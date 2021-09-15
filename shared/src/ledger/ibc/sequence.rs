//! IBC validity predicate for sequences

use borsh::BorshDeserialize;
use ibc::ics04_channel::channel::{Counterparty, Order};
use ibc::ics04_channel::context::ChannelReader;
use ibc::ics04_channel::handler::verify::{
    verify_packet_acknowledgement_proofs, verify_packet_recv_proofs,
};
use ibc::ics04_channel::packet::Packet;
use ibc::ics24_host::identifier::{ChannelId, PortId};
use ibc::proofs::Proofs;
use ibc::timestamp::Expiry;
use thiserror::Error;

use super::Ibc;
use crate::ledger::storage::{self, StorageHasher};
use crate::types::ibc::{
    Error as IbcDataError, PacketAckData, PacketReceiptData, PacketSendData,
};
use crate::types::storage::Key;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Key error: {0}")]
    InvalidKey(String),
    #[error("Client error: {0}")]
    InvalidClient(String),
    #[error("Connection error: {0}")]
    InvalidConnection(String),
    #[error("Channel error: {0}")]
    InvalidChannel(String),
    #[error("Port error: {0}")]
    InvalidPort(String),
    #[error("Sequence error: {0}")]
    InvalidSequence(String),
    #[error("Packet error: {0}")]
    InvalidPacket(String),
    #[error("Proof verification error: {0}")]
    ProofVerificationFailure(String),
    #[error("Decoding TX data error: {0}")]
    DecodingTxData(std::io::Error),
    #[error("IBC data error: {0}")]
    InvalidIbcData(IbcDataError),
}

/// IBC packet functions result
pub type Result<T> = std::result::Result<T, Error>;

enum Phase {
    Send,
    Recv,
    Ack,
}

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    pub(super) fn validate_sequence_send(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = Self::get_port_channel_id(key)
            .map_err(|e| Error::InvalidKey(e.to_string()))?;
        let data = PacketSendData::try_from_slice(tx_data)?;
        let next_seq_pre = self
            .get_next_sequence_send_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let packet = data.packet(next_seq_pre);
        let next_seq = match self.get_next_sequence_send(&port_channel_id) {
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
        if packet.sequence != next_seq_pre {
            return Err(Error::InvalidPacket(
                "The packet sequence is invalid".to_owned(),
            ));
        }

        self.validate_send_packet(&port_channel_id, &packet)
    }

    pub(super) fn validate_sequence_recv(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = Self::get_port_channel_id(key)
            .map_err(|e| Error::InvalidKey(e.to_string()))?;
        let data = PacketReceiptData::try_from_slice(tx_data)?;
        let packet = &data.packet;
        let next_seq_pre = self
            .get_next_sequence_recv_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let next_seq = match self.get_next_sequence_recv(&port_channel_id) {
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

        self.validate_recv_packet(&port_channel_id, packet)?;

        self.verify_recv_proof(&port_channel_id, packet, &data.proofs()?)
    }

    pub(super) fn validate_sequence_ack(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let port_channel_id = Self::get_port_channel_id(key)
            .map_err(|e| Error::InvalidKey(e.to_string()))?;
        let data = PacketAckData::try_from_slice(tx_data)?;
        let packet = &data.packet;
        let next_seq_pre = self
            .get_next_sequence_ack_pre(&port_channel_id)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        let next_seq = match self.get_next_sequence_ack(&port_channel_id) {
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

        self.validate_ack_packet(&port_channel_id, packet)?;

        self.verify_ack_proof(
            &port_channel_id,
            packet,
            data.ack.clone(),
            &data.proofs()?,
        )
    }

    fn validate_send_packet(
        &self,
        port_channel_id: &(PortId, ChannelId),
        packet: &Packet,
    ) -> Result<()> {
        self.validate_packet(packet, Phase::Send)?;

        let key = (
            port_channel_id.0.clone(),
            port_channel_id.1.clone(),
            packet.sequence,
        );
        if self.get_packet_commitment(&key).is_none() {
            return Err(Error::InvalidPacket(format!(
                "The commitment doesn't exist: Port {}, Channel {}, Sequence \
                 {}",
                port_channel_id.0, port_channel_id.1, packet.sequence
            )));
        }

        Ok(())
    }

    fn validate_recv_packet(
        &self,
        port_channel_id: &(PortId, ChannelId),
        packet: &Packet,
    ) -> Result<()> {
        self.validate_packet(packet, Phase::Recv)?;

        let key = (
            port_channel_id.0.clone(),
            port_channel_id.1.clone(),
            packet.sequence,
        );
        if self.get_packet_receipt(&key).is_none() {
            return Err(Error::InvalidPacket(format!(
                "The receipt doesn't exist: Port {}, Channel {}, Sequence {}",
                port_channel_id.0, port_channel_id.1, packet.sequence
            )));
        }
        if self.get_packet_acknowledgement(&key).is_none() {
            return Err(Error::InvalidPacket(format!(
                "The acknowledgement doesn't exist: Port {}, Channel {}, \
                 Sequence {}",
                port_channel_id.0, port_channel_id.1, packet.sequence
            )));
        }

        Ok(())
    }

    fn validate_ack_packet(
        &self,
        port_channel_id: &(PortId, ChannelId),
        packet: &Packet,
    ) -> Result<()> {
        self.validate_packet(packet, Phase::Ack)?;

        let key = (
            port_channel_id.0.clone(),
            port_channel_id.1.clone(),
            packet.sequence,
        );
        let prev_commitment = self
            .get_packet_commitment_pre(&key)
            .map_err(|e| Error::InvalidSequence(e.to_string()))?;
        self.validate_packet_commitment(packet, prev_commitment)?;

        if self.get_packet_commitment(&key).is_some() {
            return Err(Error::InvalidPacket(
                "The commitment hasn't been deleted yet".to_owned(),
            ));
        }

        Ok(())
    }

    pub(super) fn validate_packet_commitment(
        &self,
        packet: &Packet,
        commitment: String,
    ) -> Result<()> {
        let input = format!(
            "{:?},{:?},{:?}",
            packet.timeout_timestamp, packet.timeout_height, packet.data,
        );
        if commitment == self.hash(input) {
            Ok(())
        } else {
            Err(Error::InvalidPacket(
                "The commitment and the packet are mismatched".to_owned(),
            ))
        }
    }

    fn verify_recv_proof(
        &self,
        port_channel_id: &(PortId, ChannelId),
        packet: &Packet,
        proofs: &Proofs,
    ) -> Result<()> {
        let channel = match self.channel_end(port_channel_id) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port {}, Channel {}",
                    port_channel_id.0, port_channel_id.1,
                )));
            }
        };
        let connection = self
            .connection_from_channel(&channel)
            .map_err(|e| Error::InvalidConnection(e.to_string()))?;
        let client_id = connection.client_id().clone();

        match verify_packet_recv_proofs(self, packet, client_id, proofs) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e.to_string())),
        }
    }

    fn verify_ack_proof(
        &self,
        port_channel_id: &(PortId, ChannelId),
        packet: &Packet,
        ack: Vec<u8>,
        proofs: &Proofs,
    ) -> Result<()> {
        let channel = match self.channel_end(port_channel_id) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port {}, Channel {}",
                    port_channel_id.0, port_channel_id.1,
                )));
            }
        };
        let connection = self
            .connection_from_channel(&channel)
            .map_err(|e| Error::InvalidConnection(e.to_string()))?;
        let client_id = connection.client_id().clone();

        match verify_packet_acknowledgement_proofs(
            self, packet, ack, client_id, proofs,
        ) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e.to_string())),
        }
    }

    pub(super) fn is_ordered_channel(
        &self,
        port_channel_id: &(PortId, ChannelId),
    ) -> Result<bool> {
        let channel = match self.channel_end(port_channel_id) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port {}, Channel {}",
                    port_channel_id.0, port_channel_id.1
                )));
            }
        };
        Ok(channel.order_matches(&Order::Ordered))
    }

    fn validate_packet(&self, packet: &Packet, phase: Phase) -> Result<()> {
        let port_channel_id = match phase {
            Phase::Send | Phase::Ack => {
                (packet.source_port.clone(), packet.source_channel.clone())
            }
            Phase::Recv => (
                packet.destination_port.clone(),
                packet.destination_channel.clone(),
            ),
        };

        // port authentication
        self.authenticated_capability(&port_channel_id.0)
            .map_err(|e| {
                Error::InvalidPort(format!(
                    "The port is not owned: Port {}, {}",
                    port_channel_id.0, e
                ))
            })?;

        let channel = match self.channel_end(&port_channel_id) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port {}, Channel {}",
                    port_channel_id.0, port_channel_id.1,
                )));
            }
        };
        if !channel.is_open() {
            return Err(Error::InvalidChannel(format!(
                "The channel isn't open: Port {}, Channel {}",
                port_channel_id.0, port_channel_id.1
            )));
        }

        let connection = self
            .connection_from_channel(&channel)
            .map_err(|e| Error::InvalidConnection(e.to_string()))?;
        if !connection.is_open() {
            return Err(Error::InvalidConnection(
                "The connection isn't open".to_owned(),
            ));
        }

        // counterparty consistency
        let counterparty = match phase {
            Phase::Send | Phase::Ack => Counterparty::new(
                packet.destination_port.clone(),
                Some(packet.destination_channel.clone()),
            ),
            Phase::Recv => Counterparty::new(
                packet.source_port.clone(),
                Some(packet.source_channel.clone()),
            ),
        };
        if !channel.counterparty_matches(&counterparty) {
            return Err(Error::InvalidPacket(
                "The counterpart port or channel is mismatched".to_owned(),
            ));
        }

        // check timeout
        match phase {
            Phase::Send => {
                let client_id = connection.client_id();
                let height = match self.client_state(client_id) {
                    Some(s) => s.latest_height(),
                    None => {
                        return Err(Error::InvalidClient(format!(
                            "The client state doesn't exist: ID {}",
                            client_id
                        )));
                    }
                };
                self.check_timeout(client_id, height, packet)
                    .map_err(|e| Error::InvalidPacket(e.to_string()))?;
            }
            Phase::Recv => {
                // check timeout height
                if packet.timeout_height <= self.host_height() {
                    return Err(Error::InvalidPacket(
                        "The packet has timed out".to_owned(),
                    ));
                }
                // check timeout timestamp
                if self
                    .host_timestamp()
                    .check_expiry(&packet.timeout_timestamp)
                    != Expiry::NotExpired
                {
                    return Err(Error::InvalidPacket(
                        "The packet has timed out".to_owned(),
                    ));
                }
            }
            Phase::Ack => (),
        }

        Ok(())
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
