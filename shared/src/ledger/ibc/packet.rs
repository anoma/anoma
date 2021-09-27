//! IBC validity predicate for packets

use borsh::BorshDeserialize;
use ibc::ics02_client::height::Height;
use ibc::ics04_channel::channel::{ChannelEnd, Counterparty, Order, State};
use ibc::ics04_channel::context::ChannelReader;
use ibc::ics04_channel::error::Error as Ics04Error;
use ibc::ics04_channel::handler::verify::{
    verify_channel_proofs, verify_next_sequence_recv,
    verify_packet_acknowledgement_proofs, verify_packet_receipt_absence,
    verify_packet_recv_proofs,
};
use ibc::ics04_channel::packet::{Packet, Sequence};
use ibc::ics24_host::identifier::{ChannelId, ClientId, PortChannelId, PortId};
use ibc::proofs::Proofs;
use ibc::timestamp::Expiry;
use thiserror::Error;

use super::storage::{port_channel_sequence_id, Error as IbcStorageError};
use super::{Ibc, StateChange};
use crate::ledger::storage::{self, StorageHasher};
use crate::types::ibc::{
    Error as IbcDataError, PacketAckData, PacketReceiptData, PacketSendData,
    TimeoutData,
};
use crate::types::storage::Key;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
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
    #[error("IBC storage error: {0}")]
    IbcStorage(IbcStorageError),
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
    pub(super) fn validate_commitment(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        let commitment_key = port_channel_sequence_id(key)?;
        match self
            .get_state_change(key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))?
        {
            StateChange::Created => {
                // sending a packet
                let data = PacketSendData::try_from_slice(tx_data)?;
                let packet = data.packet(commitment_key.2);
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
                    .map_err(|e| Error::InvalidPacket(e.to_string()))?;

                self.validate_send_packet(&commitment_key, &packet)
            }
            StateChange::Deleted => {
                // check the channel state
                let channel = self
                    .channel_end(&(
                        commitment_key.0.clone(),
                        commitment_key.1.clone(),
                    ))
                    .ok_or_else(|| {
                        Error::InvalidChannel(format!(
                            "The channel doesn't exist: Port {}, Channel {}",
                            commitment_key.0, commitment_key.1,
                        ))
                    })?;
                match channel.state() {
                    State::Open => {
                        // "PacketAcknowledgement"
                        let data = PacketAckData::try_from_slice(tx_data)?;
                        let commitment_pre = self
                            .get_packet_commitment_pre(&commitment_key)
                            .map_err(|e| Error::InvalidPacket(e.to_string()))?;
                        self.validate_packet_commitment(
                            &data.packet,
                            commitment_pre,
                        )
                        .map_err(|e| Error::InvalidPacket(e.to_string()))?;
                        self.validate_ack_packet(
                            &commitment_key,
                            &data.packet,
                        )?;
                        let port_channel_id = PortChannelId {
                            port_id: commitment_key.0,
                            channel_id: commitment_key.1,
                        };
                        self.verify_ack_proof(
                            &port_channel_id,
                            &data.packet,
                            data.ack.clone(),
                            &data.proofs()?,
                        )
                    }
                    State::Closed => {
                        self.validate_timeout(&commitment_key, tx_data)
                    }
                    _ => Err(Error::InvalidChannel(format!(
                        "The channel state is invalid: Port {}, Channel {}",
                        commitment_key.0, commitment_key.1
                    ))),
                }
            }
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the commitment is invalid: Key {}",
                key
            ))),
        }
    }

    pub(super) fn validate_receipt(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        match self
            .get_state_change(key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))?
        {
            StateChange::Created => {
                let receipt_key = port_channel_sequence_id(key)?;
                let data = PacketReceiptData::try_from_slice(tx_data)?;
                let packet = &data.packet;
                self.validate_recv_packet(&receipt_key, packet)?;
                let port_channel_id = PortChannelId {
                    port_id: receipt_key.0,
                    channel_id: receipt_key.1,
                };
                self.verify_recv_proof(
                    &port_channel_id,
                    packet,
                    &data.proofs()?,
                )
            }
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
            StateChange::Created => {
                let ack_key = port_channel_sequence_id(key)?;
                // The receipt should have been stored
                if self
                    .get_packet_receipt(&(
                        ack_key.0.clone(),
                        ack_key.1.clone(),
                        ack_key.2,
                    ))
                    .is_none()
                {
                    return Err(Error::InvalidPacket(format!(
                        "The receipt doesn't exist: Port {}, Channel {}, \
                         Sequence {}",
                        ack_key.0, ack_key.1, ack_key.2,
                    )));
                }
                // The packet is validated in the receipt validation
                Ok(())
            }
            _ => Err(Error::InvalidStateChange(
                "The state change of the acknowledgment is invalid".to_owned(),
            )),
        }
    }

    fn validate_send_packet(
        &self,
        port_channel_seq_id: &(PortId, ChannelId, Sequence),
        packet: &Packet,
    ) -> Result<()> {
        self.validate_packet(port_channel_seq_id, packet, Phase::Send)?;

        if self.get_packet_commitment(port_channel_seq_id).is_none() {
            return Err(Error::InvalidPacket(format!(
                "The commitment doesn't exist: Port {}, Channel {}, Sequence \
                 {}",
                port_channel_seq_id.0,
                port_channel_seq_id.1,
                port_channel_seq_id.2
            )));
        }

        Ok(())
    }

    fn validate_recv_packet(
        &self,
        port_channel_seq_id: &(PortId, ChannelId, Sequence),
        packet: &Packet,
    ) -> Result<()> {
        self.validate_packet(port_channel_seq_id, packet, Phase::Recv)?;

        if self.get_packet_receipt(port_channel_seq_id).is_none() {
            return Err(Error::InvalidPacket(format!(
                "The receipt doesn't exist: Port {}, Channel {}, Sequence {}",
                port_channel_seq_id.0,
                port_channel_seq_id.1,
                port_channel_seq_id.2
            )));
        }
        if self
            .get_packet_acknowledgement(port_channel_seq_id)
            .is_none()
        {
            return Err(Error::InvalidPacket(format!(
                "The acknowledgement doesn't exist: Port {}, Channel {}, \
                 Sequence {}",
                port_channel_seq_id.0,
                port_channel_seq_id.1,
                port_channel_seq_id.2
            )));
        }

        Ok(())
    }

    fn validate_ack_packet(
        &self,
        port_channel_seq_id: &(PortId, ChannelId, Sequence),
        packet: &Packet,
    ) -> Result<()> {
        self.validate_packet(port_channel_seq_id, packet, Phase::Ack)?;

        let prev_commitment = self
            .get_packet_commitment_pre(port_channel_seq_id)
            .map_err(|e| Error::InvalidPacket(e.to_string()))?;
        self.validate_packet_commitment(packet, prev_commitment)?;

        if self.get_packet_commitment(port_channel_seq_id).is_some() {
            return Err(Error::InvalidPacket(
                "The commitment hasn't been deleted yet".to_owned(),
            ));
        }

        Ok(())
    }

    fn validate_packet(
        &self,
        port_channel_seq_id: &(PortId, ChannelId, Sequence),
        packet: &Packet,
        phase: Phase,
    ) -> Result<()> {
        let (port_id, channel_id, sequence) = port_channel_seq_id;
        let port_channel_id = match phase {
            Phase::Send | Phase::Ack => {
                if *port_id != packet.source_port
                    || *channel_id != packet.source_channel
                    || *sequence != packet.sequence
                {
                    return Err(Error::InvalidPacket(
                        "The packet info invalid".to_owned(),
                    ));
                }
                PortChannelId {
                    port_id: packet.source_port.clone(),
                    channel_id: packet.source_channel.clone(),
                }
            }
            Phase::Recv => {
                if *port_id != packet.destination_port
                    || *channel_id != packet.destination_channel
                    || *sequence != packet.sequence
                {
                    return Err(Error::InvalidPacket(
                        "The packet info invalid".to_owned(),
                    ));
                }
                PortChannelId {
                    port_id: packet.destination_port.clone(),
                    channel_id: packet.destination_channel.clone(),
                }
            }
        };

        // port authentication
        self.authenticated_capability(&port_channel_id.port_id)
            .map_err(|e| {
                Error::InvalidPort(format!(
                    "The port is not owned: Port {}, {}",
                    port_channel_id.port_id, e
                ))
            })?;

        let channel = match self.channel_end(&(
            port_channel_id.port_id.clone(),
            port_channel_id.channel_id.clone(),
        )) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port/Channel {}",
                    port_channel_id,
                )));
            }
        };
        if !channel.is_open() {
            return Err(Error::InvalidChannel(format!(
                "The channel isn't open: Port/Channel {}",
                port_channel_id
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

    fn validate_packet_commitment(
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
        port_channel_id: &PortChannelId,
        packet: &Packet,
        proofs: &Proofs,
    ) -> Result<()> {
        let channel = match self.channel_end(&(
            port_channel_id.port_id.clone(),
            port_channel_id.channel_id.clone(),
        )) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port/Channel {}",
                    port_channel_id,
                )));
            }
        };
        let connection = self
            .connection_from_channel(&channel)
            .map_err(|e| Error::InvalidConnection(e.to_string()))?;
        let client_id = connection.client_id().clone();

        verify_packet_recv_proofs(self, packet, client_id, proofs)
            .map_err(Error::ProofVerificationFailure)
    }

    fn verify_ack_proof(
        &self,
        port_channel_id: &PortChannelId,
        packet: &Packet,
        ack: Vec<u8>,
        proofs: &Proofs,
    ) -> Result<()> {
        let channel = match self.channel_end(&(
            port_channel_id.port_id.clone(),
            port_channel_id.channel_id.clone(),
        )) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidChannel(format!(
                    "The channel doesn't exist: Port/Channel {}",
                    port_channel_id,
                )));
            }
        };
        let connection = self
            .connection_from_channel(&channel)
            .map_err(|e| Error::InvalidConnection(e.to_string()))?;
        let client_id = connection.client_id().clone();

        verify_packet_acknowledgement_proofs(
            self, packet, ack, client_id, proofs,
        )
        .map_err(Error::ProofVerificationFailure)
    }

    fn validate_timeout(
        &self,
        commitment_key: &(PortId, ChannelId, Sequence),
        tx_data: &[u8],
    ) -> Result<()> {
        let data = TimeoutData::try_from_slice(tx_data)?;
        let packet = data.packet.clone();
        // deleted commitment should be for the packet sent from this channel
        let commitment = self
            .get_packet_commitment_pre(commitment_key)
            .map_err(|e| Error::InvalidPacket(e.to_string()))?;
        self.validate_packet_commitment(&packet, commitment)
            .map_err(|e| Error::InvalidPacket(e.to_string()))?;

        self.authenticated_capability(&packet.source_port)
            .map_err(|e| Error::InvalidPort(e.to_string()))?;

        // the counterparty should be equal to that of the channel
        let port_channel_id = PortChannelId {
            port_id: packet.source_port.clone(),
            channel_id: packet.source_channel.clone(),
        };
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
        let counterparty = Counterparty::new(
            packet.destination_port.clone(),
            Some(packet.destination_channel.clone()),
        );
        if !channel.counterparty_matches(&counterparty) {
            return Err(Error::InvalidPacket(format!(
                "The packet is invalid for the counterparty: Port/Channel \
                 {}/{}",
                packet.destination_port, packet.destination_channel
            )));
        }

        let connection = self
            .connection_from_channel(&channel)
            .map_err(|e| Error::InvalidConnection(e.to_string()))?;
        let client_id = connection.client_id().clone();

        // check if the packet actually timed out
        if self
            .check_timeout(&client_id, data.proof_height, &packet)
            .is_ok()
        {
            // "TimedoutOnClose" because the packet didn't time out
            // check that the counterpart channel has been closed
            let expected_my_side = Counterparty::new(
                packet.source_port.clone(),
                Some(packet.source_channel.clone()),
            );
            let counterparty = connection.counterparty();
            let conn_id = counterparty.connection_id().ok_or_else(|| {
                Error::InvalidConnection(
                    "The counterparty doesn't have a connection ID".to_owned(),
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

        if channel.order_matches(&Order::Ordered) {
            if !channel.state_matches(&State::Closed) {
                return Err(Error::InvalidChannel(format!(
                    "The channel hasn't been closed yet: Port/Channel {}",
                    port_channel_id
                )));
            }
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
