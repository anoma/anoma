//! IBC validity predicate for sequences

use borsh::BorshDeserialize;
use ibc::ics04_channel::channel::Order;
use ibc::ics04_channel::context::ChannelReader;
use ibc::ics24_host::identifier::PortChannelId;
use thiserror::Error;

use super::storage::{port_channel_id, Error as IbcStorageError};
use super::Ibc;
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::ibc::{PacketAckData, PacketReceiptData, PacketSendData};
use crate::types::storage::Key;

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
    #[error("IBC storage error: {0}")]
    IbcStorage(IbcStorageError),
}

/// IBC packet functions result
pub type Result<T> = std::result::Result<T, Error>;

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    pub(super) fn validate_sequence_send(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = port_channel_id(key)?;
        let data = PacketSendData::try_from_slice(tx_data)?;
        let next_seq_pre = self
            .get_next_sequence_send_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let packet = data.packet(next_seq_pre);
        let next_seq = match self.get_next_sequence_send(&(
            port_channel_id.port_id.clone(),
            port_channel_id.channel_id.clone(),
        )) {
            Some(s) => s,
            None => {
                return Err(Error::InvalidSequence(
                    "The nextSequenceSend doesn't exit".to_owned(),
                ));
            }
        };
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
        if self.get_packet_commitment(&commitment_key).is_none() {
            return Err(Error::InvalidSequence(format!(
                "The commitement doesn't exist: Port/Channel {}/{}, Sequence \
                 {}",
                commitment_key.0, commitment_key.1, commitment_key.2,
            )));
        }
        Ok(())
    }

    pub(super) fn validate_sequence_recv(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = port_channel_id(key)?;
        let data = PacketReceiptData::try_from_slice(tx_data)?;
        let packet = &data.packet;
        let next_seq_pre = self
            .get_next_sequence_recv_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let next_seq = match self.get_next_sequence_recv(&(
            port_channel_id.port_id.clone(),
            port_channel_id.channel_id.clone(),
        )) {
            Some(s) => s,
            None => {
                return Err(Error::InvalidSequence(
                    "The nextSequenceRecv doesn't exist".to_owned(),
                ));
            }
        };
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
        if self.get_packet_receipt(&key).is_none() {
            return Err(Error::InvalidSequence(format!(
                "The receipt doesn't exist: Port/Channel {}/{}, Sequence {}",
                key.0, key.1, key.2,
            )));
        }
        if self.get_packet_acknowledgement(&key).is_none() {
            return Err(Error::InvalidSequence(format!(
                "The acknowledgment doesn't exist: Port/Channel {}/{}, \
                 Sequence {}",
                key.0, key.1, key.2,
            )));
        }
        Ok(())
    }

    pub(super) fn validate_sequence_ack(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = port_channel_id(key)?;
        let data = PacketAckData::try_from_slice(tx_data)?;
        let packet = &data.packet;
        let next_seq_pre = self
            .get_next_sequence_ack_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let next_seq = match self.get_next_sequence_ack(&(
            port_channel_id.port_id.clone(),
            port_channel_id.channel_id.clone(),
        )) {
            Some(s) => s,
            None => {
                return Err(Error::InvalidSequence(
                    "The nextSequenceAck doesn't exist".to_owned(),
                ));
            }
        };
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
        if self.get_packet_commitment(&commitment_key).is_some() {
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
        let channel = match self.channel_end(&(
            port_channel_id.port_id.clone(),
            port_channel_id.channel_id.clone(),
        )) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port/Channel {}",
                    port_channel_id
                )));
            }
        };
        Ok(channel.order_matches(&Order::Ordered))
    }
}

impl From<IbcStorageError> for Error {
    fn from(err: IbcStorageError) -> Self {
        Self::IbcStorage(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::DecodingTxData(err)
    }
}
