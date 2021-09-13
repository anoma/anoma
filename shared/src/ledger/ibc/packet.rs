//! IBC validity predicate for packets

use std::str::FromStr;

use borsh::BorshDeserialize;
use ibc::ics02_client::height::Height;
use ibc::ics04_channel::channel::{ChannelEnd, Counterparty, Order, State};
use ibc::ics04_channel::context::ChannelReader;
use ibc::ics04_channel::error::Error as Ics04Error;
use ibc::ics04_channel::handler::verify::{
    verify_channel_proofs, verify_next_sequence_recv,
    verify_packet_receipt_absence,
};
use ibc::ics04_channel::packet::{Packet, Sequence};
use ibc::ics24_host::identifier::{ChannelId, ClientId, PortId};
use ibc::timestamp::Expiry;
use thiserror::Error;

use super::{Ibc, StateChange};
use crate::ledger::storage::{self, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::ibc::{Error as IbcDataError, TimeoutData};
use crate::types::storage::{DbKeySeg, Key, KeySeg};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Key error: {0}")]
    InvalidKey(String),
    #[error("State change error: {0}")]
    InvalidStateChange(String),
    #[error("Client error: {0}")]
    InvalidClient(String),
    #[error("Connection error: {0}")]
    InvalidConnection(String),
    #[error("Channel error: {0}")]
    InvalidChannel(String),
    #[error("Port error: {0}")]
    InvalidPort(String),
    #[error("Packet error: {0}")]
    InvalidPacket(String),
    #[error("Proof verification error: {0}")]
    ProofVerificationFailure(Ics04Error),
    #[error("Decoding TX data error: {0}")]
    DecodingTxData(std::io::Error),
    #[error("IBC data error: {0}")]
    InvalidIbcData(IbcDataError),
}

/// IBC packet functions result
pub type Result<T> = std::result::Result<T, Error>;

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    pub(super) fn validate_commitment(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let commitment_key = Self::get_port_channel_sequence_id(key)?;
        match self
            .get_state_change(key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))?
        {
            StateChange::Created => {
                // sending a packet
                let packet = Packet::try_from_slice(tx_data)?;
                let commitment = self
                    .get_packet_commitment(&commitment_key)
                    .ok_or_else(|| {
                        Error::InvalidPacket(format!(
                            "The commitement doesn't exist: Port {}, Channel \
                             {}, Sequence {}",
                            commitment_key.0,
                            commitment_key.1,
                            commitment_key.2,
                        ))
                    })?;
                self.validate_packet_commitment(&packet, commitment)
                    .map_err(|e| Error::InvalidPacket(e.to_string()))
            }
            StateChange::Deleted => {
                self.validate_timeout(&commitment_key, tx_data)
            }
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the commitment is invalid: Key {}",
                key
            ))),
        }
    }

    pub(super) fn validate_receipt(&self, key: &Key) -> Result<()> {
        match self
            .get_state_change(key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))?
        {
            StateChange::Created => Ok(()),
            _ => Err(Error::InvalidStateChange(
                "The state change of the receipt is invalid".to_owned(),
            )),
        }
    }

    pub(super) fn validate_ack(&self, key: &Key) -> Result<()> {
        match self
            .get_state_change(key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))?
        {
            StateChange::Created => Ok(()),
            _ => Err(Error::InvalidStateChange(
                "The state change of the acknowledgment is invalid".to_owned(),
            )),
        }
    }

    fn get_port_channel_sequence_id(
        key: &Key,
    ) -> Result<(PortId, ChannelId, Sequence)> {
        match &key.segments[..] {
            [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), DbKeySeg::StringSeg(module0), DbKeySeg::StringSeg(port_id), DbKeySeg::StringSeg(module1), DbKeySeg::StringSeg(channel_id), DbKeySeg::StringSeg(module2), DbKeySeg::StringSeg(seq_index)]
                if addr == &Address::Internal(InternalAddress::Ibc)
                    && (prefix == "commitments"
                        || prefix == "receipts"
                        || prefix == "acks")
                    && module0 == "ports"
                    && module1 == "channels"
                    && module2 == "sequences" =>
            {
                let port_id = PortId::from_str(&port_id.raw())
                    .map_err(|e| Error::InvalidKey(e.to_string()))?;
                let channel_id = ChannelId::from_str(&channel_id.raw())
                    .map_err(|e| Error::InvalidKey(e.to_string()))?;
                let seq = Sequence::from_str(&seq_index.raw())
                    .map_err(|e| Error::InvalidKey(e.to_string()))?;
                Ok((port_id, channel_id, seq))
            }
            _ => Err(Error::InvalidKey(format!(
                "The key doesn't have port ID, channel ID and sequence \
                 number: Key {}",
                key
            ))),
        }
    }

    fn validate_timeout(
        &self,
        commitment_key: &(PortId, ChannelId, Sequence),
        tx_data: &[u8],
    ) -> Result<()> {
        let data = TimeoutData::try_from_slice(tx_data)?;
        let packet = data.packet.clone();
        let commitment =
            self.get_packet_commitment(commitment_key).ok_or_else(|| {
                Error::InvalidPacket(format!(
                    "The commitement doesn't exist: Port {}, Channel {}, \
                     Sequence {}",
                    commitment_key.0, commitment_key.1, commitment_key.2,
                ))
            })?;
        self.validate_packet_commitment(&packet, commitment)
            .map_err(|e| Error::InvalidPacket(e.to_string()))?;

        self.authenticated_capability(&packet.source_port)
            .map_err(|e| Error::InvalidPort(e.to_string()))?;

        let port_channel_id =
            (packet.source_port.clone(), packet.source_channel.clone());
        let channel = self.channel_end(&port_channel_id).ok_or_else(|| {
            Error::InvalidChannel(format!(
                "The channel doesn't exist: Port {}, Channel {}",
                packet.source_port, packet.source_channel
            ))
        })?;
        let counterparty = Counterparty::new(
            packet.destination_port.clone(),
            Some(packet.destination_channel.clone()),
        );
        if !channel.counterparty_matches(&counterparty) {
            return Err(Error::InvalidPacket(format!(
                "The packet is invalid for the counterparty: Port {}, Channel \
                 {}",
                packet.destination_port, packet.destination_channel
            )));
        }

        let connection = self
            .connection_from_channel(&channel)
            .map_err(|e| Error::InvalidConnection(e.to_string()))?;
        let client_id = connection.client_id().clone();

        let prev_channel = self
            .channel_end_pre(port_channel_id)
            .map_err(|e| Error::InvalidChannel(e.to_string()))?;
        match (prev_channel.state(), channel.state()) {
            (State::Open, State::Closed) => {
                // "Timeout"
                if self
                    .check_timeout(&client_id, data.proof_height, &packet)
                    .is_ok()
                {
                    return Err(Error::InvalidPacket(
                        "The timestamp has not passed yet".to_owned(),
                    ));
                }
            }
            _ => {
                // "TimeoutOnClose"
                // check that the counterpart channel has been closed
                let expected_my_side = Counterparty::new(
                    packet.source_port.clone(),
                    Some(packet.source_channel.clone()),
                );
                let counterparty = connection.counterparty();
                let conn_id =
                    counterparty.connection_id().ok_or_else(|| {
                        Error::InvalidConnection(
                            "The counterparty doesn't have a connection ID"
                                .to_owned(),
                        )
                    })?;
                let expected_conn_hops = vec![conn_id.clone()];
                let expected_channel = ChannelEnd::new(
                    State::Closed,
                    *channel.ordering(),
                    expected_my_side,
                    expected_conn_hops,
                    channel.version(),
                );

                verify_channel_proofs(
                    self,
                    &channel,
                    &connection,
                    &expected_channel,
                    &data.proofs()?,
                )
                .map_err(Error::ProofVerificationFailure)?;
            }
        }

        if channel.order_matches(&Order::Ordered) {
            if packet.sequence < data.sequence {
                return Err(Error::InvalidPacket(
                    "The sequence is invalid".to_owned(),
                ));
            }
            match verify_next_sequence_recv(
                self,
                client_id,
                packet,
                data.sequence,
                &data.proofs()?,
            ) {
                Ok(_) => Ok(()),
                Err(e) => Err(Error::ProofVerificationFailure(e)),
            }
        } else {
            match verify_packet_receipt_absence(
                self,
                client_id,
                packet,
                &data.proofs()?,
            ) {
                Ok(_) => Ok(()),
                Err(e) => Err(Error::ProofVerificationFailure(e)),
            }
        }
    }

    pub(super) fn check_timeout(
        &self,
        client_id: &ClientId,
        current_height: Height,
        packet: &Packet,
    ) -> Result<()> {
        // timeout height
        if !packet.timeout_height.is_zero()
            && packet.timeout_height < current_height
        {
            return Err(Error::InvalidPacket(format!(
                "The packet has timed out: Timeout height {}, Current height \
                 {}",
                packet.timeout_height, current_height,
            )));
        }
        // timeout timestamp
        let consensus_state =
            match self.client_consensus_state(client_id, current_height) {
                Some(c) => c,
                None => {
                    return Err(Error::InvalidClient(format!(
                        "The client consensus state doesn't exist: ID {}, \
                         Height {}",
                        client_id, current_height
                    )));
                }
            };
        let current_timestamp = consensus_state.timestamp();
        match current_timestamp.check_expiry(&packet.timeout_timestamp) {
            Expiry::NotExpired => Ok(()),
            Expiry::Expired => Err(Error::InvalidPacket(format!(
                "The packet has timed out: Timeout timestamp {}, Current \
                 timestamp {}",
                packet.timeout_timestamp, current_timestamp
            ))),
            Expiry::InvalidTimestamp => Err(Error::InvalidPacket(
                "The timestamp of the packet is invalid".to_owned(),
            )),
        }
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
