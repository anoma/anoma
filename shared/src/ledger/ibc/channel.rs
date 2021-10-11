//! IBC validity predicate for channel module

use borsh::BorshDeserialize;
use ibc::ics02_client::client_consensus::AnyConsensusState;
use ibc::ics02_client::client_state::AnyClientState;
use ibc::ics02_client::context::ClientReader;
use ibc::ics02_client::height::Height;
use ibc::ics03_connection::connection::ConnectionEnd;
use ibc::ics03_connection::context::ConnectionReader;
use ibc::ics04_channel::channel::{ChannelEnd, Counterparty, State};
use ibc::ics04_channel::context::ChannelReader;
use ibc::ics04_channel::error::{Error as Ics04Error, Kind as Ics04Kind};
use ibc::ics04_channel::handler::verify::verify_channel_proofs;
use ibc::ics04_channel::packet::{Receipt, Sequence};
use ibc::ics05_port::capabilities::Capability;
use ibc::ics05_port::context::PortReader;
use ibc::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortChannelId, PortId,
};
use ibc::proofs::Proofs;
use ibc::timestamp::Timestamp;
use sha2::Digest;
use thiserror::Error;

use super::storage::{
    ack_key, channel_counter_key, channel_key, commitment_key,
    is_channel_counter_key, next_sequence_ack_key, next_sequence_recv_key,
    next_sequence_send_key, port_channel_id, receipt_key,
    Error as IbcStorageError,
};
use super::{Ibc, StateChange};
use crate::ledger::native_vp::Error as NativeVpError;
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::ibc::{
    ChannelCloseConfirmData, ChannelCloseInitData, ChannelOpenAckData,
    ChannelOpenConfirmData, ChannelOpenTryData, Error as IbcDataError,
    TimeoutData,
};
use crate::types::storage::Key;

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
    #[error("Proof verification error: {0}")]
    ProofVerificationFailure(Ics04Error),
    #[error("Decoding TX data error: {0}")]
    DecodingTxData(std::io::Error),
    #[error("IBC data error: {0}")]
    InvalidIbcData(IbcDataError),
    #[error("IBC storage error: {0}")]
    IbcStorage(IbcStorageError),
}

/// IBC channel functions result
pub type Result<T> = std::result::Result<T, Error>;

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    pub(super) fn validate_channel(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        if is_channel_counter_key(key) {
            if self.channel_counter_pre()? < self.channel_counter() {
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
            .ok_or_else(|| {
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
                State::Init => Ok(()),
                State::TryOpen => self.verify_channel_try_proof(
                    port_channel_id,
                    &channel,
                    tx_data,
                ),
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
        let version = match versions.as_slice() {
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
                State::Init => self.verify_channel_ack_proof(
                    port_channel_id,
                    channel,
                    tx_data,
                ),
                State::TryOpen => self.verify_channel_confirm_proof(
                    port_channel_id,
                    channel,
                    tx_data,
                ),
                _ => Err(Error::InvalidStateChange(format!(
                    "The state change of the channel is invalid: Port {}, \
                     Channel {}",
                    port_channel_id.port_id, port_channel_id.channel_id,
                ))),
            },
            State::Closed => {
                if !prev_channel.state_matches(&State::Open) {
                    return Err(Error::InvalidStateChange(format!(
                        "The state change of the channel is invalid: Port {}, \
                         Channel {}",
                        port_channel_id.port_id, port_channel_id.channel_id,
                    )));
                }
                match TimeoutData::try_from_slice(tx_data) {
                    Ok(data) => self.validate_commitment_absence(data),
                    Err(_) => {
                        match ChannelCloseInitData::try_from_slice(tx_data) {
                            Ok(_) => Ok(()),
                            Err(_) => self.verify_channel_close_proof(
                                port_channel_id,
                                channel,
                                tx_data,
                            ),
                        }
                    }
                }
            }
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the channel is invalid: Port {}, Channel \
                 {}",
                port_channel_id.port_id, port_channel_id.channel_id
            ))),
        }
    }

    fn validate_commitment_absence(&self, data: TimeoutData) -> Result<()> {
        // check if the commitment has been deleted
        let packet = data.packet;
        let key = commitment_key(
            &packet.source_port,
            &packet.source_channel,
            packet.sequence,
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
                packet.source_port, packet.source_channel, packet.sequence
            ))),
        }
    }

    fn verify_channel_try_proof(
        &self,
        port_channel_id: PortChannelId,
        channel: &ChannelEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        let data = ChannelOpenTryData::try_from_slice(tx_data)?;
        let expected_my_side = Counterparty::new(port_channel_id.port_id, None);

        self.verify_proofs(
            channel,
            expected_my_side,
            State::Init,
            data.proofs()?,
        )
    }

    fn verify_channel_ack_proof(
        &self,
        port_channel_id: &PortChannelId,
        channel: &ChannelEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        let data = ChannelOpenAckData::try_from_slice(tx_data)?;
        let expected_my_side = Counterparty::new(
            port_channel_id.port_id.clone(),
            Some(port_channel_id.channel_id.clone()),
        );

        self.verify_proofs(
            channel,
            expected_my_side,
            State::TryOpen,
            data.proofs()?,
        )
    }

    fn verify_channel_confirm_proof(
        &self,
        port_channel_id: &PortChannelId,
        channel: &ChannelEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        let data = ChannelOpenConfirmData::try_from_slice(tx_data)?;
        let expected_my_side = Counterparty::new(
            port_channel_id.port_id.clone(),
            Some(port_channel_id.channel_id.clone()),
        );

        self.verify_proofs(
            channel,
            expected_my_side,
            State::Open,
            data.proofs()?,
        )
    }

    fn verify_channel_close_proof(
        &self,
        port_channel_id: &PortChannelId,
        channel: &ChannelEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        let data = ChannelCloseConfirmData::try_from_slice(tx_data)?;
        let expected_my_side = Counterparty::new(
            port_channel_id.port_id.clone(),
            Some(port_channel_id.channel_id.clone()),
        );

        self.verify_proofs(
            channel,
            expected_my_side,
            State::Closed,
            data.proofs()?,
        )
    }

    fn verify_proofs(
        &self,
        channel: &ChannelEnd,
        expected_my_side: Counterparty,
        expected_state: State,
        proofs: Proofs,
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
            channel.version(),
        );

        match verify_channel_proofs(
            self,
            channel,
            &connection,
            &expected_channel,
            &proofs,
        ) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e)),
        }
    }

    fn get_sequence_pre(&self, key: &Key) -> Result<Sequence> {
        match self.ctx.read_pre(key)? {
            Some(value) => {
                let index = u64::try_from_slice(&value[..]).map_err(|e| {
                    Error::InvalidSequence(format!(
                        "Decoding a prior sequece index failed: {}",
                        e
                    ))
                })?;
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
                let index = u64::try_from_slice(&value).map_err(|e| {
                    Error::InvalidSequence(format!(
                        "Decoding a sequece index failed: {}",
                        e
                    ))
                })?;
                Ok(Sequence::from(index))
            }
            // The sequence has not been used yet
            None => Ok(Sequence::from(1)),
        }
    }

    fn get_packet_info_pre(&self, key: &Key) -> Result<String> {
        match self.ctx.read_pre(key)? {
            Some(value) => String::try_from_slice(&value[..]).map_err(|e| {
                Error::InvalidPacketInfo(format!(
                    "Decoding the prior packet info failed: {}",
                    e
                ))
            }),
            None => Err(Error::InvalidPacketInfo(format!(
                "The prior packet info doesn't exist: Key {}",
                key
            ))),
        }
    }

    fn get_packet_info(&self, key: &Key) -> Result<String> {
        match self.ctx.read_post(key)? {
            Some(value) => String::try_from_slice(&value[..]).map_err(|e| {
                Error::InvalidPacketInfo(format!(
                    "Decoding the packet info failed: {}",
                    e
                ))
            }),
            None => Err(Error::InvalidPacketInfo(format!(
                "The packet info doesn't exist: Key {}",
                key
            ))),
        }
    }

    pub(super) fn connection_from_channel(
        &self,
        channel: &ChannelEnd,
    ) -> Result<ConnectionEnd> {
        match channel.connection_hops().get(0) {
            Some(conn_id) => {
                match ChannelReader::connection_end(self, conn_id) {
                    Some(conn) => Ok(conn),
                    None => Err(Error::InvalidConnection(format!(
                        "The connection doesn't exist: ID {}",
                        conn_id
                    ))),
                }
            }
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
            Ok(Some(value)) => {
                ChannelEnd::try_from_slice(&value[..]).map_err(|e| {
                    Error::InvalidChannel(format!(
                        "Decoding the channel failed: Port/Channel {}, {}",
                        port_channel_id, e
                    ))
                })
            }
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
        self.get_packet_info_pre(&key)
    }

    fn channel_counter_pre(&self) -> Result<u64> {
        let key = channel_counter_key();
        self.read_counter_pre(&key)
            .map_err(|e| Error::InvalidChannel(e.to_string()))
    }
}

impl<'a, DB, H> ChannelReader for Ibc<'a, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    fn channel_end(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Option<ChannelEnd> {
        let port_channel_id = PortChannelId {
            port_id: port_channel_id.0.clone(),
            channel_id: port_channel_id.1.clone(),
        };
        let key = channel_key(&port_channel_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => ChannelEnd::try_from_slice(&value[..]).ok(),
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn connection_end(&self, conn_id: &ConnectionId) -> Option<ConnectionEnd> {
        ConnectionReader::connection_end(self, conn_id)
    }

    fn connection_channels(
        &self,
        conn_id: &ConnectionId,
    ) -> Option<Vec<(PortId, ChannelId)>> {
        let mut channels = vec![];
        let prefix = Key::parse("channelEnds/ports")
            .expect("Creating a key for the prefix shouldn't fail");
        let mut iter = self.ctx.iter_prefix(&prefix).ok()?;
        loop {
            let next = self.ctx.iter_post_next(&mut iter).ok()?;
            if let Some((key, value)) = next {
                let channel = ChannelEnd::try_from_slice(&value[..]).ok()?;
                if let Some(id) = channel.connection_hops().get(0) {
                    if id == conn_id {
                        let key = Key::parse(&key).ok()?;
                        let port_channel_id = port_channel_id(&key).ok()?;
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
        Some(channels)
    }

    fn client_state(&self, client_id: &ClientId) -> Option<AnyClientState> {
        ClientReader::client_state(self, client_id)
    }

    fn client_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Option<AnyConsensusState> {
        ClientReader::consensus_state(self, client_id, height)
    }

    fn authenticated_capability(
        &self,
        port_id: &PortId,
    ) -> std::result::Result<Capability, Ics04Error> {
        match self.lookup_module_by_port(port_id) {
            Some(cap) => {
                if self.authenticate(&cap, port_id) {
                    Ok(cap)
                } else {
                    Err(Ics04Kind::InvalidPortCapability.into())
                }
            }
            None => Err(Ics04Kind::NoPortCapability(port_id.clone()).into()),
        }
    }

    fn get_next_sequence_send(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Option<Sequence> {
        let port_channel_id = PortChannelId {
            port_id: port_channel_id.0.clone(),
            channel_id: port_channel_id.1.clone(),
        };
        let key = next_sequence_send_key(&port_channel_id);
        self.get_sequence(&key).ok()
    }

    fn get_next_sequence_recv(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Option<Sequence> {
        let port_channel_id = PortChannelId {
            port_id: port_channel_id.0.clone(),
            channel_id: port_channel_id.1.clone(),
        };
        let key = next_sequence_recv_key(&port_channel_id);
        self.get_sequence(&key).ok()
    }

    fn get_next_sequence_ack(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Option<Sequence> {
        let port_channel_id = PortChannelId {
            port_id: port_channel_id.0.clone(),
            channel_id: port_channel_id.1.clone(),
        };
        let key = next_sequence_ack_key(&port_channel_id);
        self.get_sequence(&key).ok()
    }

    fn get_packet_commitment(
        &self,
        key: &(PortId, ChannelId, Sequence),
    ) -> Option<String> {
        let key = commitment_key(&key.0, &key.1, key.2);
        self.get_packet_info(&key).ok()
    }

    fn get_packet_receipt(
        &self,
        key: &(PortId, ChannelId, Sequence),
    ) -> Option<Receipt> {
        let key = receipt_key(&key.0, &key.1, key.2);
        match self.ctx.read_post(&key) {
            Ok(Some(_)) => Some(Receipt::Ok),
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn get_packet_acknowledgement(
        &self,
        key: &(PortId, ChannelId, Sequence),
    ) -> Option<String> {
        let key = ack_key(&key.0, &key.1, key.2);
        self.get_packet_info(&key).ok()
    }

    fn hash(&self, value: String) -> String {
        let r = sha2::Sha256::digest(value.as_bytes());
        format!("{:x}", r)
    }

    fn host_height(&self) -> Height {
        self.host_current_height()
    }

    fn host_timestamp(&self) -> Timestamp {
        match self.ctx.storage.get_block_header().0 {
            Some(h) => Timestamp::from_datetime(h.time.into()),
            None => Timestamp::none(),
        }
    }

    fn channel_counter(&self) -> u64 {
        let key = channel_counter_key();
        self.read_counter(&key)
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
