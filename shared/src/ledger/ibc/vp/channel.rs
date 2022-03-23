//! IBC validity predicate for channel module

use core::time::Duration;

use prost::Message;
use sha2::Digest;
use thiserror::Error;

use super::super::handler::{
    make_close_confirm_channel_event, make_close_init_channel_event,
    make_open_ack_channel_event, make_open_confirm_channel_event,
    make_open_init_channel_event, make_open_try_channel_event,
};
use super::super::storage::{
    ack_key, channel_counter_key, channel_key, client_update_height_key,
    client_update_timestamp_key, commitment_key, is_channel_counter_key,
    next_sequence_ack_key, next_sequence_recv_key, next_sequence_send_key,
    port_channel_id, receipt_key, Error as IbcStorageError,
};
use super::{Ibc, StateChange};
use crate::ibc::core::ics02_client::client_consensus::AnyConsensusState;
use crate::ibc::core::ics02_client::client_state::AnyClientState;
use crate::ibc::core::ics02_client::context::ClientReader;
use crate::ibc::core::ics02_client::height::Height;
use crate::ibc::core::ics03_connection::connection::ConnectionEnd;
use crate::ibc::core::ics03_connection::context::ConnectionReader;
use crate::ibc::core::ics03_connection::error::Error as Ics03Error;
use crate::ibc::core::ics04_channel::channel::{
    ChannelEnd, Counterparty, State,
};
use crate::ibc::core::ics04_channel::context::ChannelReader;
use crate::ibc::core::ics04_channel::error::Error as Ics04Error;
use crate::ibc::core::ics04_channel::handler::verify::verify_channel_proofs;
use crate::ibc::core::ics04_channel::msgs::chan_close_confirm::MsgChannelCloseConfirm;
use crate::ibc::core::ics04_channel::msgs::chan_open_ack::MsgChannelOpenAck;
use crate::ibc::core::ics04_channel::msgs::chan_open_confirm::MsgChannelOpenConfirm;
use crate::ibc::core::ics04_channel::msgs::chan_open_try::MsgChannelOpenTry;
use crate::ibc::core::ics04_channel::msgs::{ChannelMsg, PacketMsg};
use crate::ibc::core::ics04_channel::packet::{Receipt, Sequence};
use crate::ibc::core::ics05_port::capabilities::Capability;
use crate::ibc::core::ics05_port::context::PortReader;
use crate::ibc::core::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortChannelId, PortId,
};
use crate::ibc::core::ics26_routing::msgs::Ics26Envelope;
use crate::ibc::proofs::Proofs;
use crate::ibc::timestamp::Timestamp;
use crate::ledger::native_vp::Error as NativeVpError;
use crate::ledger::parameters;
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::tendermint::Time;
use crate::tendermint_proto::Protobuf;
use crate::types::ibc::data::{
    Error as IbcDataError, IbcMessage, PacketAck, PacketReceipt,
};
use crate::types::storage::Key;
use crate::vm::WasmCacheAccess;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVp(NativeVpError),
    #[error("State change error: {0}")]
    InvalidStateChange(String),
    #[error("Connection error: {0}")]
    InvalidConnection(String),
    #[error("Channel error: {0}")]
    InvalidChannel(String),
    #[error("Port error: {0}")]
    InvalidPort(String),
    #[error("Version error: {0}")]
    InvalidVersion(String),
    #[error("Sequence error: {0}")]
    InvalidSequence(String),
    #[error("Packet info error: {0}")]
    InvalidPacketInfo(String),
    #[error("Client update timestamp error: {0}")]
    InvalidTimestamp(String),
    #[error("Client update hight error: {0}")]
    InvalidHeight(String),
    #[error("Proof verification error: {0}")]
    ProofVerificationFailure(Ics04Error),
    #[error("Decoding TX data error: {0}")]
    DecodingTxData(std::io::Error),
    #[error("IBC data error: {0}")]
    InvalidIbcData(IbcDataError),
    #[error("IBC storage error: {0}")]
    IbcStorage(IbcStorageError),
    #[error("IBC event error: {0}")]
    IbcEvent(String),
}

/// IBC channel functions result
pub type Result<T> = std::result::Result<T, Error>;
/// ChannelReader result
type Ics04Result<T> = core::result::Result<T, Ics04Error>;

impl<'a, DB, H, CA> Ibc<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    pub(super) fn validate_channel(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        if is_channel_counter_key(key) {
            let counter = self.channel_counter().map_err(|_| {
                Error::InvalidChannel(
                    "The channel counter doesn't exist".to_owned(),
                )
            })?;
            if self.channel_counter_pre()? < counter {
                return Ok(());
            } else {
                return Err(Error::InvalidChannel(
                    "The channel counter is invalid".to_owned(),
                ));
            }
        }

        let port_channel_id = port_channel_id(key)?;
        self.authenticated_capability(&port_channel_id.port_id)
            .map_err(|e| {
                Error::InvalidPort(format!(
                    "The port is not authenticated: ID {}, {}",
                    port_channel_id.port_id, e
                ))
            })?;

        let channel = self
            .channel_end(&(
                port_channel_id.port_id.clone(),
                port_channel_id.channel_id.clone(),
            ))
            .map_err(|_| {
                Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port/Channel {}",
                    port_channel_id
                ))
            })?;
        // check the number of hops and empty version in the channel end
        channel.validate_basic().map_err(|e| {
            Error::InvalidChannel(format!(
                "The channel is invalid: Port/Channel {}, {}",
                port_channel_id, e
            ))
        })?;

        self.validate_version(&channel)?;

        match self.get_channel_state_change(&port_channel_id)? {
            StateChange::Created => match channel.state() {
                State::Init => {
                    let ibc_msg = IbcMessage::decode(tx_data)?;
                    let msg = ibc_msg.msg_channel_open_init()?;
                    let event = make_open_init_channel_event(
                        &port_channel_id.channel_id,
                        &msg,
                    );
                    self.check_emitted_event(event)
                        .map_err(|e| Error::IbcEvent(e.to_string()))
                }
                State::TryOpen => {
                    let ibc_msg = IbcMessage::decode(tx_data)?;
                    let msg = ibc_msg.msg_channel_open_try()?;
                    self.verify_channel_try_proof(
                        &port_channel_id,
                        &channel,
                        &msg,
                    )?;
                    let event = make_open_try_channel_event(
                        &port_channel_id.channel_id,
                        &msg,
                    );
                    self.check_emitted_event(event)
                        .map_err(|e| Error::IbcEvent(e.to_string()))
                }
                _ => Err(Error::InvalidChannel(format!(
                    "The channel state is invalid: Port/Channel {}, State {}",
                    port_channel_id,
                    channel.state()
                ))),
            },
            StateChange::Updated => self.validate_updated_channel(
                &port_channel_id,
                &channel,
                tx_data,
            ),
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the channel: Port/Channel {}",
                port_channel_id
            ))),
        }
    }

    fn get_channel_state_change(
        &self,
        port_channel_id: &PortChannelId,
    ) -> Result<StateChange> {
        let key = channel_key(port_channel_id);
        self.get_state_change(&key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))
    }

    fn validate_version(&self, channel: &ChannelEnd) -> Result<()> {
        let connection = self.connection_from_channel(channel)?;
        let versions = connection.versions();
        let version = match versions {
            [version] => version,
            _ => {
                return Err(Error::InvalidVersion(
                    "Multiple versions are specified or no version".to_owned(),
                ));
            }
        };

        let feature = channel.ordering().to_string();
        if version.is_supported_feature(feature.clone()) {
            Ok(())
        } else {
            Err(Error::InvalidVersion(format!(
                "The version is unsupported: Feature {}",
                feature
            )))
        }
    }

    fn validate_updated_channel(
        &self,
        port_channel_id: &PortChannelId,
        channel: &ChannelEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        let prev_channel = self.channel_end_pre(port_channel_id)?;
        match channel.state() {
            State::Open => match prev_channel.state() {
                State::Init => {
                    let ibc_msg = IbcMessage::decode(tx_data)?;
                    let msg = ibc_msg.msg_channel_open_ack()?;
                    self.verify_channel_ack_proof(
                        port_channel_id,
                        channel,
                        &msg,
                    )?;
                    let event = make_open_ack_channel_event(&msg);
                    self.check_emitted_event(event)
                        .map_err(|e| Error::IbcEvent(e.to_string()))
                }
                State::TryOpen => {
                    let ibc_msg = IbcMessage::decode(tx_data)?;
                    let msg = ibc_msg.msg_channel_open_confirm()?;
                    self.verify_channel_confirm_proof(
                        port_channel_id,
                        channel,
                        &msg,
                    )?;
                    let event = make_open_confirm_channel_event(&msg);
                    self.check_emitted_event(event)
                        .map_err(|e| Error::IbcEvent(e.to_string()))
                }
                _ => Err(Error::InvalidStateChange(format!(
                    "The state change of the channel is invalid: Port/Channel \
                     {}",
                    port_channel_id,
                ))),
            },
            State::Closed => {
                if !prev_channel.state_matches(&State::Open) {
                    return Err(Error::InvalidStateChange(format!(
                        "The state change of the channel is invalid: \
                         Port/Channel {}",
                        port_channel_id,
                    )));
                }
                let ibc_msg = IbcMessage::decode(tx_data)?;
                match ibc_msg.0 {
                    // The timeout event will be checked in the commitment
                    // validation
                    Ics26Envelope::Ics4PacketMsg(PacketMsg::ToPacket(msg)) => {
                        let commitment_key = (
                            msg.packet.source_port,
                            msg.packet.source_channel,
                            msg.packet.sequence,
                        );
                        self.validate_commitment_absence(commitment_key)
                    }
                    Ics26Envelope::Ics4PacketMsg(PacketMsg::ToClosePacket(
                        msg,
                    )) => {
                        let commitment_key = (
                            msg.packet.source_port,
                            msg.packet.source_channel,
                            msg.packet.sequence,
                        );
                        self.validate_commitment_absence(commitment_key)
                    }
                    Ics26Envelope::Ics4ChannelMsg(
                        ChannelMsg::ChannelCloseInit(msg),
                    ) => {
                        let event = make_close_init_channel_event(&msg);
                        self.check_emitted_event(event)
                            .map_err(|e| Error::IbcEvent(e.to_string()))
                    }
                    Ics26Envelope::Ics4ChannelMsg(
                        ChannelMsg::ChannelCloseConfirm(msg),
                    ) => {
                        self.verify_channel_close_proof(
                            port_channel_id,
                            channel,
                            &msg,
                        )?;
                        let event = make_close_confirm_channel_event(&msg);
                        self.check_emitted_event(event)
                            .map_err(|e| Error::IbcEvent(e.to_string()))
                    }
                    _ => Err(Error::InvalidStateChange(format!(
                        "The state change of the channel is invalid: \
                         Port/Channel {}",
                        port_channel_id,
                    ))),
                }
            }
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the channel is invalid: Port/Channel {}",
                port_channel_id,
            ))),
        }
    }

    fn validate_commitment_absence(
        &self,
        port_channel_sequence_id: (PortId, ChannelId, Sequence),
    ) -> Result<()> {
        // check if the commitment has been deleted
        let key = commitment_key(
            &port_channel_sequence_id.0,
            &port_channel_sequence_id.1,
            port_channel_sequence_id.2,
        );
        let state_change = self
            .get_state_change(&key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))?;
        match state_change {
            // the deleted commitment is validated in validate_commitment()
            StateChange::Deleted => Ok(()),
            _ => Err(Error::InvalidStateChange(format!(
                "The commitment hasn't been deleted yet: Port {}, Channel {}, \
                 Sequence {}",
                port_channel_sequence_id.0,
                port_channel_sequence_id.1,
                port_channel_sequence_id.2,
            ))),
        }
    }

    fn verify_channel_try_proof(
        &self,
        port_channel_id: &PortChannelId,
        channel: &ChannelEnd,
        msg: &MsgChannelOpenTry,
    ) -> Result<()> {
        let expected_my_side =
            Counterparty::new(port_channel_id.port_id.clone(), None);
        self.verify_proofs(
            msg.proofs.height(),
            channel,
            expected_my_side,
            State::Init,
            &msg.proofs,
        )
    }

    fn verify_channel_ack_proof(
        &self,
        port_channel_id: &PortChannelId,
        channel: &ChannelEnd,
        msg: &MsgChannelOpenAck,
    ) -> Result<()> {
        match channel.counterparty().channel_id() {
            Some(counterpart_channel_id) => {
                if *counterpart_channel_id != msg.counterparty_channel_id {
                    return Err(Error::InvalidChannel(format!(
                        "The counterpart channel ID mismatched: ID {}",
                        counterpart_channel_id
                    )));
                }
            }
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't have the counterpart channel ID: ID \
                     {}",
                    port_channel_id
                )));
            }
        }
        let expected_my_side = Counterparty::new(
            port_channel_id.port_id.clone(),
            Some(port_channel_id.channel_id.clone()),
        );
        self.verify_proofs(
            msg.proofs.height(),
            channel,
            expected_my_side,
            State::TryOpen,
            &msg.proofs,
        )
    }

    fn verify_channel_confirm_proof(
        &self,
        port_channel_id: &PortChannelId,
        channel: &ChannelEnd,
        msg: &MsgChannelOpenConfirm,
    ) -> Result<()> {
        let expected_my_side = Counterparty::new(
            port_channel_id.port_id.clone(),
            Some(port_channel_id.channel_id.clone()),
        );
        self.verify_proofs(
            msg.proofs.height(),
            channel,
            expected_my_side,
            State::Open,
            &msg.proofs,
        )
    }

    fn verify_channel_close_proof(
        &self,
        port_channel_id: &PortChannelId,
        channel: &ChannelEnd,
        msg: &MsgChannelCloseConfirm,
    ) -> Result<()> {
        let expected_my_side = Counterparty::new(
            port_channel_id.port_id.clone(),
            Some(port_channel_id.channel_id.clone()),
        );
        self.verify_proofs(
            msg.proofs.height(),
            channel,
            expected_my_side,
            State::Closed,
            &msg.proofs,
        )
    }

    fn verify_proofs(
        &self,
        height: Height,
        channel: &ChannelEnd,
        expected_my_side: Counterparty,
        expected_state: State,
        proofs: &Proofs,
    ) -> Result<()> {
        let connection = self.connection_from_channel(channel)?;
        let counterpart_conn_id =
            match connection.counterparty().connection_id() {
                Some(id) => id.clone(),
                None => {
                    return Err(Error::InvalidConnection(
                        "The counterpart connection ID doesn't exist"
                            .to_owned(),
                    ));
                }
            };
        let expected_connection_hops = vec![counterpart_conn_id];
        let expected_channel = ChannelEnd::new(
            expected_state,
            *channel.ordering(),
            expected_my_side,
            expected_connection_hops,
            channel.version().clone(),
        );

        match verify_channel_proofs(
            self,
            height,
            channel,
            &connection,
            &expected_channel,
            proofs,
        ) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e)),
        }
    }

    fn get_sequence_pre(&self, key: &Key) -> Result<Sequence> {
        match self.ctx.read_pre(key)? {
            Some(value) => {
                // As ibc-go, u64 like a counter is encoded with big-endian
                let index: [u8; 8] = value.try_into().map_err(|_| {
                    Error::InvalidSequence(
                        "Encoding the prior sequence index failed".to_string(),
                    )
                })?;
                let index = u64::from_be_bytes(index);
                Ok(Sequence::from(index))
            }
            // The sequence is updated for the first time. The previous sequence
            // is the initial number.
            None => Ok(Sequence::from(1)),
        }
    }

    fn get_sequence(&self, key: &Key) -> Result<Sequence> {
        match self.ctx.read_post(key)? {
            Some(value) => {
                // As ibc-go, u64 like a counter is encoded with big-endian
                let index: [u8; 8] = value.try_into().map_err(|_| {
                    Error::InvalidSequence(
                        "Encoding the sequence index failed".to_string(),
                    )
                })?;
                let index = u64::from_be_bytes(index);
                Ok(Sequence::from(index))
            }
            // The sequence has not been used yet
            None => Ok(Sequence::from(1)),
        }
    }

    pub(super) fn connection_from_channel(
        &self,
        channel: &ChannelEnd,
    ) -> Result<ConnectionEnd> {
        match channel.connection_hops().get(0) {
            Some(conn_id) => ChannelReader::connection_end(self, conn_id)
                .map_err(|_| {
                    Error::InvalidConnection(format!(
                        "The connection doesn't exist: ID {}",
                        conn_id
                    ))
                }),
            _ => Err(Error::InvalidConnection(
                "the corresponding connection ID doesn't exist".to_owned(),
            )),
        }
    }

    pub(super) fn channel_end_pre(
        &self,
        port_channel_id: &PortChannelId,
    ) -> Result<ChannelEnd> {
        let key = channel_key(port_channel_id);
        match self.ctx.read_pre(&key) {
            Ok(Some(value)) => ChannelEnd::decode_vec(&value).map_err(|e| {
                Error::InvalidChannel(format!(
                    "Decoding the channel failed: Port/Channel {}, {}",
                    port_channel_id, e
                ))
            }),
            Ok(None) => Err(Error::InvalidChannel(format!(
                "The prior channel doesn't exist: Port/Channel {}",
                port_channel_id
            ))),
            Err(e) => Err(Error::InvalidChannel(format!(
                "Reading the prior channel failed: {}",
                e
            ))),
        }
    }

    pub(super) fn get_next_sequence_send_pre(
        &self,
        port_channel_id: &PortChannelId,
    ) -> Result<Sequence> {
        let key = next_sequence_send_key(port_channel_id);
        self.get_sequence_pre(&key)
    }

    pub(super) fn get_next_sequence_recv_pre(
        &self,
        port_channel_id: &PortChannelId,
    ) -> Result<Sequence> {
        let key = next_sequence_recv_key(port_channel_id);
        self.get_sequence_pre(&key)
    }

    pub(super) fn get_next_sequence_ack_pre(
        &self,
        port_channel_id: &PortChannelId,
    ) -> Result<Sequence> {
        let key = next_sequence_ack_key(port_channel_id);
        self.get_sequence_pre(&key)
    }

    pub(super) fn get_packet_commitment_pre(
        &self,
        key: &(PortId, ChannelId, Sequence),
    ) -> Result<String> {
        let key = commitment_key(&key.0, &key.1, key.2);
        match self.ctx.read_pre(&key)? {
            Some(value) => String::decode(&value[..]).map_err(|e| {
                Error::InvalidPacketInfo(format!(
                    "Decoding the prior commitment failed: {}",
                    e
                ))
            }),
            None => Err(Error::InvalidPacketInfo(format!(
                "The prior commitment doesn't exist: Key {}",
                key
            ))),
        }
    }

    pub(super) fn client_update_time_pre(
        &self,
        client_id: &ClientId,
    ) -> Result<Timestamp> {
        let key = client_update_timestamp_key(client_id);
        match self.ctx.read_pre(&key)? {
            Some(value) => {
                let time = Time::decode_vec(&value).map_err(|_| {
                    Error::InvalidTimestamp(format!(
                        "Timestamp conversion failed: ID {}",
                        client_id
                    ))
                })?;
                Ok(time.into())
            }
            None => Err(Error::InvalidTimestamp(format!(
                "Timestamp doesn't exist: ID {}",
                client_id
            ))),
        }
    }

    pub(super) fn client_update_height_pre(
        &self,
        client_id: &ClientId,
    ) -> Result<Height> {
        let key = client_update_height_key(client_id);
        match self.ctx.read_pre(&key)? {
            Some(value) => Height::decode_vec(&value).map_err(|_| {
                Error::InvalidHeight(format!(
                    "Height conversion failed: ID {}",
                    client_id
                ))
            }),
            None => Err(Error::InvalidHeight(format!(
                "Client update height doesn't exist: ID {}",
                client_id
            ))),
        }
    }

    fn channel_counter_pre(&self) -> Result<u64> {
        let key = channel_counter_key();
        self.read_counter_pre(&key)
            .map_err(|e| Error::InvalidChannel(e.to_string()))
    }
}

impl<'a, DB, H, CA> ChannelReader for Ibc<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    fn channel_end(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Ics04Result<ChannelEnd> {
        let port_channel_id = PortChannelId {
            port_id: port_channel_id.0.clone(),
            channel_id: port_channel_id.1.clone(),
        };
        let key = channel_key(&port_channel_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => ChannelEnd::decode_vec(&value)
                .map_err(|_| Ics04Error::implementation_specific()),
            Ok(None) => Err(Ics04Error::channel_not_found(
                port_channel_id.port_id,
                port_channel_id.channel_id,
            )),
            Err(_) => Err(Ics04Error::implementation_specific()),
        }
    }

    fn connection_end(
        &self,
        conn_id: &ConnectionId,
    ) -> Ics04Result<ConnectionEnd> {
        ConnectionReader::connection_end(self, conn_id)
            .map_err(Ics04Error::ics03_connection)
    }

    fn connection_channels(
        &self,
        conn_id: &ConnectionId,
    ) -> Ics04Result<Vec<(PortId, ChannelId)>> {
        let mut channels = vec![];
        let prefix = Key::parse("channelEnds/ports")
            .expect("Creating a key for the prefix shouldn't fail");
        let mut iter = self
            .ctx
            .iter_prefix(&prefix)
            .map_err(|_| Ics04Error::implementation_specific())?;
        loop {
            let next = self
                .ctx
                .iter_post_next(&mut iter)
                .map_err(|_| Ics04Error::implementation_specific())?;
            if let Some((key, value)) = next {
                let channel = ChannelEnd::decode_vec(&value)
                    .map_err(|_| Ics04Error::implementation_specific())?;
                if let Some(id) = channel.connection_hops().get(0) {
                    if id == conn_id {
                        let key = Key::parse(&key).map_err(|_| {
                            Ics04Error::implementation_specific()
                        })?;
                        let port_channel_id =
                            port_channel_id(&key).map_err(|_| {
                                Ics04Error::implementation_specific()
                            })?;
                        channels.push((
                            port_channel_id.port_id,
                            port_channel_id.channel_id,
                        ));
                    }
                }
            } else {
                break;
            }
        }
        Ok(channels)
    }

    fn client_state(
        &self,
        client_id: &ClientId,
    ) -> Ics04Result<AnyClientState> {
        ConnectionReader::client_state(self, client_id)
            .map_err(Ics04Error::ics03_connection)
    }

    fn client_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics04Result<AnyConsensusState> {
        ConnectionReader::client_consensus_state(self, client_id, height)
            .map_err(Ics04Error::ics03_connection)
    }

    fn authenticated_capability(
        &self,
        port_id: &PortId,
    ) -> Ics04Result<Capability> {
        let (_, cap) = self
            .lookup_module_by_port(port_id)
            .map_err(|_| Ics04Error::no_port_capability(port_id.clone()))?;
        if self.authenticate(port_id.clone(), &cap) {
            Ok(cap)
        } else {
            Err(Ics04Error::invalid_port_capability())
        }
    }

    fn get_next_sequence_send(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Ics04Result<Sequence> {
        let port_channel_id = PortChannelId {
            port_id: port_channel_id.0.clone(),
            channel_id: port_channel_id.1.clone(),
        };
        let key = next_sequence_send_key(&port_channel_id);
        self.get_sequence(&key).map_err(|_| {
            Ics04Error::missing_next_send_seq((
                port_channel_id.port_id,
                port_channel_id.channel_id,
            ))
        })
    }

    fn get_next_sequence_recv(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Ics04Result<Sequence> {
        let port_channel_id = PortChannelId {
            port_id: port_channel_id.0.clone(),
            channel_id: port_channel_id.1.clone(),
        };
        let key = next_sequence_recv_key(&port_channel_id);
        self.get_sequence(&key).map_err(|_| {
            Ics04Error::missing_next_recv_seq((
                port_channel_id.port_id,
                port_channel_id.channel_id,
            ))
        })
    }

    fn get_next_sequence_ack(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Ics04Result<Sequence> {
        let port_channel_id = PortChannelId {
            port_id: port_channel_id.0.clone(),
            channel_id: port_channel_id.1.clone(),
        };
        let key = next_sequence_ack_key(&port_channel_id);
        self.get_sequence(&key).map_err(|_| {
            Ics04Error::missing_next_ack_seq((
                port_channel_id.port_id,
                port_channel_id.channel_id,
            ))
        })
    }

    fn get_packet_commitment(
        &self,
        key: &(PortId, ChannelId, Sequence),
    ) -> Ics04Result<String> {
        let commitment_key = commitment_key(&key.0, &key.1, key.2);
        match self.ctx.read_post(&commitment_key) {
            Ok(Some(value)) => String::decode(&value[..])
                .map_err(|_| Ics04Error::implementation_specific()),
            Ok(None) => Err(Ics04Error::packet_commitment_not_found(key.2)),
            Err(_) => Err(Ics04Error::implementation_specific()),
        }
    }

    fn get_packet_receipt(
        &self,
        key: &(PortId, ChannelId, Sequence),
    ) -> Ics04Result<Receipt> {
        let receipt_key = receipt_key(&key.0, &key.1, key.2);
        let expect = PacketReceipt::default().as_bytes().to_vec();
        match self.ctx.read_post(&receipt_key) {
            Ok(Some(v)) if v == expect => Ok(Receipt::Ok),
            _ => Err(Ics04Error::packet_receipt_not_found(key.2)),
        }
    }

    // TODO should return Vec<u8> or Acknowledgment. fix in ibc-rs?
    fn get_packet_acknowledgement(
        &self,
        key: &(PortId, ChannelId, Sequence),
    ) -> Ics04Result<String> {
        let ack_key = ack_key(&key.0, &key.1, key.2);
        match self.ctx.read_post(&ack_key) {
            Ok(Some(_)) => Ok(PacketAck::default().to_string()),
            Ok(None) => Err(Ics04Error::packet_commitment_not_found(key.2)),
            Err(_) => Err(Ics04Error::implementation_specific()),
        }
    }

    fn hash(&self, value: String) -> String {
        let r = sha2::Sha256::digest(value.as_bytes());
        format!("{:x}", r)
    }

    fn host_height(&self) -> Height {
        ClientReader::host_height(self)
    }

    fn host_consensus_state(
        &self,
        height: Height,
    ) -> Ics04Result<AnyConsensusState> {
        ClientReader::host_consensus_state(self, height).map_err(|e| {
            Ics04Error::ics03_connection(Ics03Error::ics02_client(e))
        })
    }

    fn pending_host_consensus_state(&self) -> Ics04Result<AnyConsensusState> {
        ClientReader::pending_host_consensus_state(self).map_err(|e| {
            Ics04Error::ics03_connection(Ics03Error::ics02_client(e))
        })
    }

    fn client_update_time(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics04Result<Timestamp> {
        let key = client_update_timestamp_key(client_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => {
                let time = Time::decode_vec(&value)
                    .map_err(|_| Ics04Error::implementation_specific())?;
                Ok(time.into())
            }
            Ok(None) => Err(Ics04Error::processed_time_not_found(
                client_id.clone(),
                height,
            )),
            Err(_) => Err(Ics04Error::implementation_specific()),
        }
    }

    fn client_update_height(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics04Result<Height> {
        let key = client_update_height_key(client_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => Height::decode_vec(&value)
                .map_err(|_| Ics04Error::implementation_specific()),
            Ok(None) => Err(Ics04Error::processed_height_not_found(
                client_id.clone(),
                height,
            )),
            Err(_) => Err(Ics04Error::implementation_specific()),
        }
    }

    fn channel_counter(&self) -> Ics04Result<u64> {
        let key = channel_counter_key();
        self.read_counter(&key)
            .map_err(|_| Ics04Error::implementation_specific())
    }

    fn max_expected_time_per_block(&self) -> Duration {
        match parameters::read(self.ctx.storage) {
            Ok((parameters, gas)) => {
                match self.ctx.gas_meter.borrow_mut().add(gas) {
                    Ok(_) => parameters.max_expected_time_per_block.into(),
                    Err(_) => Duration::default(),
                }
            }
            Err(_) => Duration::default(),
        }
    }
}

impl From<NativeVpError> for Error {
    fn from(err: NativeVpError) -> Self {
        Self::NativeVp(err)
    }
}

impl From<IbcStorageError> for Error {
    fn from(err: IbcStorageError) -> Self {
        Self::IbcStorage(err)
    }
}

impl From<IbcDataError> for Error {
    fn from(err: IbcDataError) -> Self {
        Self::InvalidIbcData(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::DecodingTxData(err)
    }
}
