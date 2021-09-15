//! IBC-related data definitions and transaction and validity-predicate helpers.

use std::time::Duration;

use borsh::{BorshDeserialize, BorshSerialize};
use ibc::ics02_client::client_consensus::AnyConsensusState;
use ibc::ics02_client::client_state::AnyClientState;
use ibc::ics02_client::header::AnyHeader;
use ibc::ics02_client::height::Height;
use ibc::ics03_connection::connection::{
    ConnectionEnd, Counterparty as ConnCounterparty, State as ConnState,
};
use ibc::ics03_connection::version::Version;
use ibc::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order, State as ChanState,
};
use ibc::ics04_channel::packet::{Packet, Sequence};
use ibc::ics23_commitment::commitment::CommitmentProofBytes;
use ibc::ics24_host::identifier::{ChannelId, ClientId, ConnectionId, PortId};
use ibc::proofs::{ConsensusProof, Proofs};
use ibc::timestamp::Timestamp;
use ibc_proto::ibc::core::commitment::v1::MerkleProof;
use prost::Message;
use thiserror::Error;

use crate::types::time::{DateTimeUtc, DurationNanos};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Invalid proof error: {0}")]
    InvalidProof(String),
}

/// Decode result for IBC data
pub type Result<T> = std::result::Result<T, Error>;

/// States to create a new client
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ClientCreationData {
    /// The client state
    pub client_state: AnyClientState,
    /// The consensus state
    pub consensus_state: AnyConsensusState,
}

impl ClientCreationData {
    /// Returns the data to create a new client
    pub fn new(
        client_state: AnyClientState,
        consensus_state: AnyConsensusState,
    ) -> Self {
        Self {
            client_state,
            consensus_state,
        }
    }
}

/// Data to update a client
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ClientUpdateData {
    /// The updated client ID
    pub client_id: ClientId,
    /// The headers to update the client
    pub headers: Vec<AnyHeader>,
}

impl ClientUpdateData {
    /// Returns the data to update a client
    pub fn new(client_id: ClientId, headers: Vec<AnyHeader>) -> Self {
        Self { client_id, headers }
    }
}

/// Data to upgrade a client
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ClientUpgradeData {
    /// The upgraded client ID
    pub client_id: ClientId,
    /// The client state
    pub client_state: AnyClientState,
    /// The consensus state
    pub consensus_state: AnyConsensusState,
    /// The proof of the client state
    pub proof_client: Vec<u8>,
    /// The proof of the consensus state
    pub proof_consensus_state: Vec<u8>,
}

impl ClientUpgradeData {
    /// Returns the data to upgrade a client
    pub fn new(
        client_id: ClientId,
        client_state: AnyClientState,
        consensus_state: AnyConsensusState,
        client_proof: MerkleProof,
        consensus_proof: MerkleProof,
    ) -> Self {
        let mut proof_client = vec![];
        client_proof
            .encode(&mut proof_client)
            .expect("Encoding a client proof shouldn't fail");
        let mut proof_consensus_state = vec![];
        consensus_proof
            .encode(&mut proof_consensus_state)
            .expect("Encoding a consensus proof shouldn't fail");
        Self {
            client_id,
            client_state,
            consensus_state,
            proof_client,
            proof_consensus_state,
        }
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> Result<MerkleProof> {
        MerkleProof::decode(&self.proof_client[..])
            .map_err(|e| Error::InvalidProof(e.to_string()))
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus_state(&self) -> Result<MerkleProof> {
        MerkleProof::decode(&self.proof_consensus_state[..])
            .map_err(|e| Error::InvalidProof(e.to_string()))
    }
}

/// Data to initialize a connection
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ConnectionOpenInitData {
    /// The corresponding client ID
    pub client_id: ClientId,
    /// The corresponding counterparty
    pub counterparty: ConnCounterparty,
    /// The version
    pub version: Version,
    /// The delay period as (secs, nanos)
    pub delay_period: DurationNanos,
}

impl ConnectionOpenInitData {
    /// Returns the data to initalize a connection
    pub fn new(
        client_id: ClientId,
        counterparty: ConnCounterparty,
        version: Version,
        delay_period: Duration,
    ) -> Self {
        Self {
            client_id,
            counterparty,
            version,
            delay_period: delay_period.into(),
        }
    }

    /// Returns a connection end
    pub fn connection(&self) -> ConnectionEnd {
        ConnectionEnd::new(
            ConnState::Init,
            self.client_id.clone(),
            self.counterparty.clone(),
            vec![self.version.clone()],
            Duration::new(self.delay_period.secs, self.delay_period.nanos),
        )
    }
}

/// Data to try to open a connection
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ConnectionOpenTryData {
    /// The client ID
    pub client_id: ClientId,
    /// The client state
    pub client_state: AnyClientState,
    /// The counterparty
    pub counterparty: ConnCounterparty,
    /// The counterpart versions
    pub counterparty_versions: Vec<Version>,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the connection
    pub proof_connection: CommitmentProofBytes,
    /// The proof of the client state
    pub proof_client: CommitmentProofBytes,
    /// The proof of the consensus state
    pub proof_consensus: CommitmentProofBytes,
    /// The delay period
    pub delay_period: DurationNanos,
}

impl ConnectionOpenTryData {
    /// Returns the data to try to open a connection
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        client_id: ClientId,
        client_state: AnyClientState,
        counterparty: ConnCounterparty,
        counterparty_versions: Vec<Version>,
        proof_height: Height,
        proof_connection: CommitmentProofBytes,
        proof_client: CommitmentProofBytes,
        proof_consensus: CommitmentProofBytes,
        delay_period: Duration,
    ) -> Self {
        Self {
            client_id,
            client_state,
            counterparty,
            counterparty_versions,
            proof_height,
            proof_connection,
            proof_client,
            proof_consensus,
            delay_period: delay_period.into(),
        }
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let consensus_proof = ConsensusProof::new(
            self.proof_consensus.clone(),
            self.proof_height,
        )
        .map_err(Error::InvalidProof)?;
        Proofs::new(
            self.proof_connection.clone(),
            Some(self.proof_client.clone()),
            Some(consensus_proof),
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }

    /// Returns a connection end
    pub fn connection(&self) -> ConnectionEnd {
        ConnectionEnd::new(
            ConnState::TryOpen,
            self.client_id.clone(),
            self.counterparty.clone(),
            self.counterparty_versions.clone(),
            Duration::new(self.delay_period.secs, self.delay_period.nanos),
        )
    }
}

/// Data to acknowledge a connection
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ConnectionOpenAckData {
    /// The connection ID
    pub conn_id: ConnectionId,
    /// The counterpart connection ID
    pub counterpart_conn_id: ConnectionId,
    /// The client state
    pub client_state: AnyClientState,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the connection
    pub proof_connection: CommitmentProofBytes,
    /// The proof of the client state
    pub proof_client: CommitmentProofBytes,
    /// The proof of the consensus state
    pub proof_consensus: CommitmentProofBytes,
    /// The version
    pub version: Version,
}

impl ConnectionOpenAckData {
    /// Returns the data to acknowledge a connection
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        conn_id: ConnectionId,
        counterpart_conn_id: ConnectionId,
        client_state: AnyClientState,
        proof_height: Height,
        proof_connection: CommitmentProofBytes,
        proof_client: CommitmentProofBytes,
        proof_consensus: CommitmentProofBytes,
        version: Version,
    ) -> Self {
        Self {
            conn_id,
            counterpart_conn_id,
            client_state,
            proof_height,
            proof_connection,
            proof_client,
            proof_consensus,
            version,
        }
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let consensus_proof = ConsensusProof::new(
            self.proof_consensus.clone(),
            self.proof_height,
        )
        .map_err(Error::InvalidProof)?;
        Proofs::new(
            self.proof_connection.clone(),
            Some(self.proof_client.clone()),
            Some(consensus_proof),
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }
}

/// Data to confirm a connection
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ConnectionOpenConfirmData {
    /// The connection ID
    pub conn_id: ConnectionId,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the connection
    pub proof_connection: CommitmentProofBytes,
    /// The proof of the client state
    pub proof_client: CommitmentProofBytes,
    /// The proof of the consensus state
    pub proof_consensus: CommitmentProofBytes,
}

impl ConnectionOpenConfirmData {
    /// Returns the data to confirm a connection
    pub fn new(
        conn_id: ConnectionId,
        proof_height: Height,
        proof_connection: CommitmentProofBytes,
        proof_client: CommitmentProofBytes,
        proof_consensus: CommitmentProofBytes,
    ) -> Self {
        Self {
            conn_id,
            proof_height,
            proof_connection,
            proof_client,
            proof_consensus,
        }
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let consensus_proof = ConsensusProof::new(
            self.proof_consensus.clone(),
            self.proof_height,
        )
        .map_err(Error::InvalidProof)?;
        Proofs::new(
            self.proof_connection.clone(),
            Some(self.proof_client.clone()),
            Some(consensus_proof),
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }
}

/// Data to initialize a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelOpenInitData {
    /// The port ID
    pub port_id: PortId,
    /// The order feature of the channel
    pub order: Order,
    /// The counterparty
    pub counterparty: ChanCounterparty,
    /// The connection hops
    pub connection_hops: Vec<ConnectionId>,
    /// The version
    pub version: String,
}

impl ChannelOpenInitData {
    /// Returns the data to initalize a channel
    pub fn new(
        port_id: PortId,
        order: Order,
        counterparty: ChanCounterparty,
        connection_hops: Vec<ConnectionId>,
        version: String,
    ) -> Self {
        Self {
            port_id,
            order,
            counterparty,
            connection_hops,
            version,
        }
    }

    /// Returns a channel end
    pub fn channel(&self) -> ChannelEnd {
        ChannelEnd::new(
            ChanState::Init,
            self.order,
            self.counterparty.clone(),
            self.connection_hops.clone(),
            self.version.clone(),
        )
    }
}

/// Data to try to open a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelOpenTryData {
    /// The port ID
    pub port_id: PortId,
    /// The order feature of the channel
    pub order: Order,
    /// The counterparty
    pub counterparty: ChanCounterparty,
    /// The connection hops
    pub connection_hops: Vec<ConnectionId>,
    /// The version
    pub version: String,
    /// The counterparty version
    pub counterparty_version: String,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the channel
    pub proof_channel: CommitmentProofBytes,
    /// The proof of the client state
    pub proof_client: CommitmentProofBytes,
    /// The proof of the consensus state
    pub proof_consensus: CommitmentProofBytes,
}

impl ChannelOpenTryData {
    /// Returns the data to try to open a channel
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        port_id: PortId,
        order: Order,
        counterparty: ChanCounterparty,
        connection_hops: Vec<ConnectionId>,
        version: String,
        counterparty_version: String,
        proof_height: Height,
        proof_channel: CommitmentProofBytes,
        proof_client: CommitmentProofBytes,
        proof_consensus: CommitmentProofBytes,
    ) -> Self {
        Self {
            port_id,
            order,
            counterparty,
            connection_hops,
            version,
            counterparty_version,
            proof_height,
            proof_channel,
            proof_client,
            proof_consensus,
        }
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let consensus_proof = ConsensusProof::new(
            self.proof_consensus.clone(),
            self.proof_height,
        )
        .map_err(Error::InvalidProof)?;
        Proofs::new(
            self.proof_channel.clone(),
            Some(self.proof_client.clone()),
            Some(consensus_proof),
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }

    /// Returns a channel end
    pub fn channel(&self) -> ChannelEnd {
        ChannelEnd::new(
            ChanState::TryOpen,
            self.order,
            self.counterparty.clone(),
            self.connection_hops.clone(),
            self.version.clone(),
        )
    }
}

/// Data to acknowledge a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelOpenAckData {
    /// The port ID
    pub port_id: PortId,
    /// The channel ID
    pub channel_id: ChannelId,
    /// The counterpart channel ID
    pub counterpart_channel_id: ChannelId,
    /// The counterparty version
    pub counterparty_version: String,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the channel
    pub proof_channel: CommitmentProofBytes,
    /// The proof of the client state
    pub proof_client: CommitmentProofBytes,
    /// The proof of the consensus state
    pub proof_consensus: CommitmentProofBytes,
}

impl ChannelOpenAckData {
    /// Returns the data to acknowledge a channel
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        port_id: PortId,
        channel_id: ChannelId,
        counterpart_channel_id: ChannelId,
        counterparty_version: String,
        proof_height: Height,
        proof_channel: CommitmentProofBytes,
        proof_client: CommitmentProofBytes,
        proof_consensus: CommitmentProofBytes,
    ) -> Self {
        Self {
            port_id,
            channel_id,
            counterpart_channel_id,
            counterparty_version,
            proof_height,
            proof_channel,
            proof_client,
            proof_consensus,
        }
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let consensus_proof = ConsensusProof::new(
            self.proof_consensus.clone(),
            self.proof_height,
        )
        .map_err(Error::InvalidProof)?;
        Proofs::new(
            self.proof_channel.clone(),
            Some(self.proof_client.clone()),
            Some(consensus_proof),
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }
}

/// Data to confirm a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelOpenConfirmData {
    /// The port ID
    pub port_id: PortId,
    /// The channel ID
    pub channel_id: ChannelId,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the channel
    pub proof_channel: CommitmentProofBytes,
    /// The proof of the client state
    pub proof_client: CommitmentProofBytes,
    /// The proof of the consensus state
    pub proof_consensus: CommitmentProofBytes,
}

impl ChannelOpenConfirmData {
    /// Returns the data to confirm a channel
    pub fn new(
        port_id: PortId,
        channel_id: ChannelId,
        proof_height: Height,
        proof_channel: CommitmentProofBytes,
        proof_client: CommitmentProofBytes,
        proof_consensus: CommitmentProofBytes,
    ) -> Self {
        Self {
            port_id,
            channel_id,
            proof_height,
            proof_channel,
            proof_client,
            proof_consensus,
        }
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let consensus_proof = ConsensusProof::new(
            self.proof_consensus.clone(),
            self.proof_height,
        )
        .map_err(Error::InvalidProof)?;
        Proofs::new(
            self.proof_channel.clone(),
            Some(self.proof_client.clone()),
            Some(consensus_proof),
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }
}

/// Data to close a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelCloseInitData {
    /// The port ID
    pub port_id: PortId,
    /// The channel ID
    pub channel_id: ChannelId,
}

impl ChannelCloseInitData {
    /// Returns the data to close a channel
    pub fn new(port_id: PortId, channel_id: ChannelId) -> Self {
        Self {
            port_id,
            channel_id,
        }
    }
}

/// Data to confirm closing a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelCloseConfirmData {
    /// The port ID
    pub port_id: PortId,
    /// The channel ID
    pub channel_id: ChannelId,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the connection
    pub proof_connection: CommitmentProofBytes,
    /// The proof of the client state
    pub proof_client: CommitmentProofBytes,
    /// The proof of the consensus state
    pub proof_consensus: CommitmentProofBytes,
}

impl ChannelCloseConfirmData {
    /// Returns the data to confirm closing a channel
    pub fn new(
        port_id: PortId,
        channel_id: ChannelId,
        proof_height: Height,
        proof_connection: CommitmentProofBytes,
        proof_client: CommitmentProofBytes,
        proof_consensus: CommitmentProofBytes,
    ) -> Self {
        Self {
            port_id,
            channel_id,
            proof_height,
            proof_connection,
            proof_client,
            proof_consensus,
        }
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let consensus_proof = ConsensusProof::new(
            self.proof_consensus.clone(),
            self.proof_height,
        )
        .map_err(Error::InvalidProof)?;
        Proofs::new(
            self.proof_connection.clone(),
            Some(self.proof_client.clone()),
            Some(consensus_proof),
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }
}

/// Data for sending a packet
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct PacketSendData {
    /// The source port
    pub source_port: PortId,
    /// The source channel
    pub source_channel: ChannelId,
    /// The destination port
    pub destination_port: PortId,
    /// The destination channel
    pub destination_channel: ChannelId,
    /// The data of packet
    pub packet_data: Vec<u8>,
    /// The timeout height
    pub timeout_height: Height,
    /// The timeout timestamp
    pub timeout_timestamp: Option<DateTimeUtc>,
}

impl PacketSendData {
    /// Create data for sending a packet
    pub fn new(
        source_port: PortId,
        source_channel: ChannelId,
        destination_port: PortId,
        destination_channel: ChannelId,
        packet_data: Vec<u8>,
        timeout_height: Height,
        timeout_timestamp: Timestamp,
    ) -> Self {
        let timeout_timestamp =
            timeout_timestamp.as_datetime().map(DateTimeUtc);
        Self {
            source_port,
            source_channel,
            destination_port,
            destination_channel,
            packet_data,
            timeout_height,
            timeout_timestamp,
        }
    }

    /// Returns a packet
    pub fn packet(&self, sequence: Sequence) -> Packet {
        let timeout_timestamp = match self.timeout_timestamp {
            Some(timestamp) => Timestamp::from_datetime(timestamp.0),
            None => Timestamp::none(),
        };
        Packet {
            sequence,
            source_port: self.source_port.clone(),
            source_channel: self.source_channel.clone(),
            destination_port: self.destination_port.clone(),
            destination_channel: self.destination_channel.clone(),
            data: self.packet_data.clone(),
            timeout_height: self.timeout_height,
            timeout_timestamp,
        }
    }
}

/// Data for receiving a packet
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct PacketReceiptData {
    /// The packet
    pub packet: Packet,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the packet
    pub proof_packet: CommitmentProofBytes,
}

impl PacketReceiptData {
    /// Create data for receiving packet
    pub fn new(
        packet: Packet,
        proof_height: Height,
        proof_packet: CommitmentProofBytes,
    ) -> Self {
        Self {
            packet,
            proof_height,
            proof_packet,
        }
    }

    /// Returns the proofs for verification
    pub fn proofs(&self) -> Result<Proofs> {
        Proofs::new(
            self.proof_packet.clone(),
            None,
            None,
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }
}

/// Data for packet acknowledgement
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct PacketAckData {
    /// The packet
    pub packet: Packet,
    /// The acknowledgement
    pub ack: Vec<u8>,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the packet
    pub proof_packet: CommitmentProofBytes,
}

impl PacketAckData {
    /// Create data for packet acknowledgement
    pub fn new(
        packet: Packet,
        ack: Vec<u8>,
        proof_height: Height,
        proof_packet: CommitmentProofBytes,
    ) -> Self {
        Self {
            packet,
            ack,
            proof_height,
            proof_packet,
        }
    }

    /// Returns the proofs for verification
    pub fn proofs(&self) -> Result<Proofs> {
        Proofs::new(
            self.proof_packet.clone(),
            None,
            None,
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }
}

/// Data for timeout
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct TimeoutData {
    /// The packet
    pub packet: Packet,
    /// The nextSequenceRecv
    pub sequence: Sequence,
    /// The height of the proof
    pub proof_height: Height,
    /// The proof of the packet
    pub proof_packet: CommitmentProofBytes,
}

impl TimeoutData {
    /// Create data for packet acknowledgement
    pub fn new(
        packet: Packet,
        sequence: Sequence,
        proof_height: Height,
        proof_packet: CommitmentProofBytes,
    ) -> Self {
        Self {
            packet,
            sequence,
            proof_height,
            proof_packet,
        }
    }

    /// Returns the proofs for verification
    pub fn proofs(&self) -> Result<Proofs> {
        Proofs::new(
            self.proof_packet.clone(),
            None,
            None,
            None,
            self.proof_height,
        )
        .map_err(Error::InvalidProof)
    }
}
