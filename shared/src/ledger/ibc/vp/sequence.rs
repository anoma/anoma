//! IBC validity predicate for sequences

use thiserror::Error;

use super::super::storage::{port_channel_id, Error as IbcStorageError};
use super::Ibc;
use crate::ibc::core::ics04_channel::channel::Order;
use crate::ibc::core::ics04_channel::context::ChannelReader;
use crate::ibc::core::ics24_host::identifier::PortChannelId;
use crate::ledger::ibc::handler::packet_from_message;
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::ibc::data::{Error as IbcDataError, IbcMessage};
use crate::types::storage::Key;
use crate::vm::WasmCacheAccess;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Key error: {0}")]
    InvalidKey(String),
    #[error("Channel error: {0}")]
    InvalidChannel(String),
    #[error("Sequence error: {0}")]
    InvalidSequence(String),
    #[error("Packet error: {0}")]
    InvalidPacket(String),
    #[error("Decoding TX data error: {0}")]
    DecodingTxData(std::io::Error),
    #[error("IBC data error: {0}")]
    InvalidIbcData(IbcDataError),
    #[error("IBC storage error: {0}")]
    IbcStorage(IbcStorageError),
}

/// IBC packet functions result
pub type Result<T> = std::result::Result<T, Error>;

impl<'a, DB, H, CA> Ibc<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    pub(super) fn validate_sequence_send(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = port_channel_id(key)?;
        let ibc_msg = IbcMessage::decode(tx_data)?;
        let msg = ibc_msg.msg_transfer()?;
        // make a packet
        let channel = self
            .channel_end(&(
                port_channel_id.port_id.clone(),
                port_channel_id.channel_id,
            ))
            .map_err(|e| Error::InvalidChannel(e.to_string()))?;
        let next_seq_pre = self
            .get_next_sequence_send_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let packet =
            packet_from_message(&msg, next_seq_pre, channel.counterparty());
        let next_seq = self
            .get_next_sequence_send(&(
                port_channel_id.port_id.clone(),
                port_channel_id.channel_id,
            ))
            .map_err(|_| {
                Error::InvalidSequence(
                    "The nextSequenceSend doesn't exit".to_owned(),
                )
            })?;
        if u64::from(next_seq_pre) + 1 != u64::from(next_seq) {
            return Err(Error::InvalidSequence(
                "The nextSequenceSend is invalid".to_owned(),
            ));
        }
        // when the ordered channel, the sequence number should be equal to
        // nextSequenceSend
        if self.is_ordered_channel(&port_channel_id)?
            && packet.sequence != next_seq_pre
        {
            return Err(Error::InvalidPacket(
                "The packet sequence is invalid".to_owned(),
            ));
        }
        // The commitment should have been stored
        let commitment_key = (
            port_channel_id.port_id,
            port_channel_id.channel_id,
            packet.sequence,
        );
        self.get_packet_commitment(&commitment_key).map_err(|_| {
            Error::InvalidSequence(format!(
                "The commitement doesn't exist: Port/Channel {}/{}, Sequence \
                 {}",
                commitment_key.0, commitment_key.1, commitment_key.2,
            ))
        })?;
        Ok(())
    }

    pub(super) fn validate_sequence_recv(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = port_channel_id(key)?;
        let ibc_msg = IbcMessage::decode(tx_data)?;
        let msg = ibc_msg.msg_recv_packet()?;
        let packet = &msg.packet;
        let next_seq_pre = self
            .get_next_sequence_recv_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let next_seq = self
            .get_next_sequence_recv(&(
                port_channel_id.port_id.clone(),
                port_channel_id.channel_id,
            ))
            .map_err(|_| {
                Error::InvalidSequence(
                    "The nextSequenceRecv doesn't exist".to_owned(),
                )
            })?;
        if u64::from(next_seq_pre) + 1 != u64::from(next_seq) {
            return Err(Error::InvalidSequence(
                "The nextSequenceRecv is invalid".to_owned(),
            ));
        }
        // when the ordered channel, the sequence number should be equal to
        // nextSequenceRecv
        if self.is_ordered_channel(&port_channel_id)?
            && packet.sequence != next_seq_pre
        {
            return Err(Error::InvalidPacket(
                "The packet sequence is invalid".to_owned(),
            ));
        }
        // The receipt and the receipt should have been stored
        let key = (
            port_channel_id.port_id,
            port_channel_id.channel_id,
            packet.sequence,
        );
        self.get_packet_receipt(&key).map_err(|_| {
            Error::InvalidSequence(format!(
                "The receipt doesn't exist: Port/Channel {}/{}, Sequence {}",
                key.0, key.1, key.2,
            ))
        })?;
        self.get_packet_acknowledgement(&key).map_err(|_| {
            Error::InvalidSequence(format!(
                "The acknowledgment doesn't exist: Port/Channel {}/{}, \
                 Sequence {}",
                key.0, key.1, key.2,
            ))
        })?;
        Ok(())
    }

    pub(super) fn validate_sequence_ack(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = port_channel_id(key)?;
        let ibc_msg = IbcMessage::decode(tx_data)?;
        let msg = ibc_msg.msg_acknowledgement()?;
        let packet = &msg.packet;
        let next_seq_pre = self
            .get_next_sequence_ack_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let next_seq = self
            .get_next_sequence_ack(&(
                port_channel_id.port_id.clone(),
                port_channel_id.channel_id,
            ))
            .map_err(|_| {
                Error::InvalidSequence(
                    "The nextSequenceAck doesn't exist".to_owned(),
                )
            })?;
        if u64::from(next_seq_pre) + 1 != u64::from(next_seq) {
            return Err(Error::InvalidSequence(
                "The sequence number is invalid".to_owned(),
            ));
        }
        // when the ordered channel, the sequence number should be equal to
        // nextSequenceAck
        if self.is_ordered_channel(&port_channel_id)?
            && packet.sequence != next_seq_pre
        {
            return Err(Error::InvalidPacket(
                "The packet sequence is invalid".to_owned(),
            ));
        }
        // The commitment should have been deleted
        let commitment_key = (
            port_channel_id.port_id,
            port_channel_id.channel_id,
            packet.sequence,
        );
        if self.get_packet_commitment(&commitment_key).is_ok() {
            return Err(Error::InvalidSequence(format!(
                "The commitement hasn't been deleted yet: Port/Channel {}/{}, \
                 Sequence {}",
                commitment_key.0, commitment_key.1, commitment_key.2,
            )));
        }
        Ok(())
    }

    pub(super) fn is_ordered_channel(
        &self,
        port_channel_id: &PortChannelId,
    ) -> Result<bool> {
        let channel = self
            .channel_end(&(
                port_channel_id.port_id.clone(),
                port_channel_id.channel_id,
            ))
            .map_err(|_| {
                Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port/Channel {}",
                    port_channel_id
                ))
            })?;
        Ok(channel.order_matches(&Order::Ordered))
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
