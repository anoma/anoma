//! IBC-related data definitions and transaction and validity-predicate helpers.

use std::convert::{TryFrom, TryInto};
use std::str::FromStr;
use std::time::Duration;

use borsh::{BorshDeserialize, BorshSerialize};
use ibc::ics02_client::client_consensus::AnyConsensusState;
use ibc::ics02_client::client_state::AnyClientState;
use ibc::ics02_client::header::AnyHeader;
use ibc::ics02_client::height::Height;
use ibc::ics03_connection::connection::Counterparty as ConnCounterparty;
use ibc::ics03_connection::version::Version;
use ibc::ics04_channel::channel::{Counterparty as ChanCounterparty, Order};
use ibc::ics23_commitment::commitment::CommitmentProofBytes;
use ibc::ics24_host::identifier::{ChannelId, ClientId, ConnectionId, PortId};
use ibc::proofs::{ConsensusProof, Proofs};
use ibc_proto::ibc::core::commitment::v1::MerkleProof;
use ibc_proto::ibc::core::connection::v1::Counterparty as RawCounterparty;
use prost::Message;
use tendermint_proto::Protobuf;
use thiserror::Error;

use crate::types::time::DurationNanos;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Decoding error: {0}")]
    DecodingError(String),
}

/// Decode result for IBC data
pub type Result<T> = std::result::Result<T, Error>;

/// States to create a new client
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ClientCreationData {
    /// The client state
    client_state: Vec<u8>,
    /// The consensus state
    consensus_state: Vec<u8>,
}

impl ClientCreationData {
    /// Returns the data to create a new client
    pub fn new(
        client_state: AnyClientState,
        consensus_state: AnyConsensusState,
    ) -> Self {
        let client_state = client_state
            .encode_vec()
            .expect("Encoding a client state shouldn't fail");
        let consensus_state = consensus_state
            .encode_vec()
            .expect("Encoding a consensus state shouldn't fail");
        Self {
            client_state,
            consensus_state,
        }
    }

    /// Returns the client state
    pub fn client_state(&self) -> Result<AnyClientState> {
        AnyClientState::decode_vec(&self.client_state)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the consensus state
    pub fn consensus_state(&self) -> Result<AnyConsensusState> {
        AnyConsensusState::decode_vec(&self.consensus_state)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }
}

/// Data to update a client
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ClientUpdateData {
    /// The updated client ID
    client_id: String,
    /// The headers to update the client
    headers: Vec<Vec<u8>>,
}

impl ClientUpdateData {
    /// Returns the data to update a client
    pub fn new(client_id: ClientId, headers: Vec<AnyHeader>) -> Self {
        let client_id = client_id.to_string();
        let headers = headers
            .iter()
            .map(|h| {
                h.encode_vec()
                    .expect("Encoding a client header shouldn't fail")
            })
            .collect();
        Self { client_id, headers }
    }

    /// Returns the client ID
    pub fn client_id(&self) -> Result<ClientId> {
        ClientId::from_str(&self.client_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the header
    pub fn headers(&self) -> Result<Vec<AnyHeader>> {
        let mut headers = vec![];
        for h in &self.headers {
            let header = AnyHeader::decode_vec(h)
                .map_err(|e| Error::DecodingError(e.to_string()))?;
            headers.push(header);
        }
        Ok(headers)
    }
}

/// Data to upgrade a client
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ClientUpgradeData {
    /// The upgraded client ID
    client_id: String,
    /// The client state
    client_state: Vec<u8>,
    /// The consensus state
    consensus_state: Vec<u8>,
    /// The proof of the client state
    proof_client: Vec<u8>,
    /// The proof of the consensus state
    proof_consensus_state: Vec<u8>,
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
        let client_id = client_id.to_string();
        let client_state = client_state
            .encode_vec()
            .expect("Encoding a client state shouldn't fail");
        let consensus_state = consensus_state
            .encode_vec()
            .expect("Encoding a consensus state shouldn't fail");
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

    /// Returns the client ID
    pub fn client_id(&self) -> Result<ClientId> {
        ClientId::from_str(&self.client_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the client state
    pub fn client_state(&self) -> Result<AnyClientState> {
        AnyClientState::decode_vec(&self.client_state)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the consensus state
    pub fn consensus_state(&self) -> Result<AnyConsensusState> {
        AnyConsensusState::decode_vec(&self.consensus_state)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> Result<MerkleProof> {
        MerkleProof::decode(&self.proof_client[..])
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus_state(&self) -> Result<MerkleProof> {
        MerkleProof::decode(&self.proof_consensus_state[..])
            .map_err(|e| Error::DecodingError(e.to_string()))
    }
}

/// Data to initialize a connection
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ConnectionOpenInitData {
    /// The corresponding client ID
    client_id: String,
    /// The corresponding counterparty
    counterparty: Vec<u8>,
    /// The version
    version: Vec<u8>,
    /// The delay period as (secs, nanos)
    delay_period: DurationNanos,
}

impl ConnectionOpenInitData {
    /// Returns the data to initalize a connection
    pub fn new(
        client_id: ClientId,
        counterparty: ConnCounterparty,
        version: Version,
        delay_period: Duration,
    ) -> Self {
        let client_id = client_id.to_string();
        // TODO: Need Profobuf implementation for Counterparty in ibc-rs
        // let counterparty = counterparty.encode_vec().expect("Encoding a
        // counterparty shouldn't fail");
        let mut bytes = vec![];
        RawCounterparty::from(counterparty)
            .encode(&mut bytes)
            .expect("Encoding a counterparty shouldn't fail");
        let version = version
            .encode_vec()
            .expect("Encoding a version shouldn't fail");
        Self {
            client_id,
            counterparty: bytes,
            version,
            delay_period: delay_period.into(),
        }
    }

    /// Returns the client ID
    pub fn client_id(&self) -> Option<ClientId> {
        ClientId::from_str(&self.client_id).ok()
    }

    /// Returns the counterparty
    pub fn counterparty(&self) -> Option<ConnCounterparty> {
        // TODO: Need Profobuf implementation for Counterparty in ibc-rs
        // Counterparty::decode_vec(self.counterparty).ok()
        match RawCounterparty::decode(&self.counterparty[..]) {
            Ok(c) => c.try_into().ok(),
            Err(_) => None,
        }
    }

    /// Returns the version
    pub fn version(&self) -> Option<Version> {
        Version::decode_vec(&self.version).ok()
    }

    /// Returns the delay period
    pub fn delay_period(&self) -> Duration {
        Duration::new(self.delay_period.secs, self.delay_period.nanos)
    }
}

/// Data to try to open a connection
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ConnectionOpenTryData {
    /// The client ID
    client_id: String,
    /// The client state
    client_state: Vec<u8>,
    /// The counterparty
    counterparty: Vec<u8>,
    /// The counterpart versions
    counterparty_versions: Vec<Vec<u8>>,
    /// The height of the proof
    proof_height: (u64, u64),
    /// The proof of the connection
    proof_connection: Vec<u8>,
    /// The proof of the client state
    proof_client: Vec<u8>,
    /// The proof of the consensus state
    proof_consensus: Vec<u8>,
    /// The delay period
    delay_period: DurationNanos,
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
        let client_id = client_id.to_string();
        let client_state = client_state
            .encode_vec()
            .expect("Encoding a client state shouldn't fail");
        // TODO: Need Profobuf implementation for Counterparty in ibc-rs
        // let counterparty = counterparty.encode_vec().expect("Encoding a
        // counterparty shouldn't fail");
        let mut bytes = vec![];
        RawCounterparty::from(counterparty)
            .encode(&mut bytes)
            .expect("Encoding a counterparty shouldn't fail");
        let versions = counterparty_versions
            .iter()
            .map(|v| v.encode_vec().expect("Encoding a version shouldn't fail"))
            .collect();
        Self {
            client_id,
            client_state,
            counterparty: bytes,
            counterparty_versions: versions,
            proof_height: (
                proof_height.revision_number,
                proof_height.revision_height,
            ),
            proof_connection: proof_connection.into(),
            proof_client: proof_client.into(),
            proof_consensus: proof_consensus.into(),
            delay_period: delay_period.into(),
        }
    }

    /// Returns the client ID
    pub fn client_id(&self) -> Result<ClientId> {
        ClientId::from_str(&self.client_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the client state
    pub fn client_state(&self) -> Result<AnyClientState> {
        AnyClientState::decode_vec(&self.client_state)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the counterparty
    pub fn counterparty(&self) -> Result<ConnCounterparty> {
        // TODO: Need Profobuf implementation for Counterparty in ibc-rs
        // Counterparty::decode_vec(self.counterparty).ok()
        match RawCounterparty::decode(&self.counterparty[..]) {
            Ok(c) => ConnCounterparty::try_from(c)
                .map_err(|e| Error::DecodingError(e.to_string())),
            Err(e) => Err(Error::DecodingError(e.to_string())),
        }
    }

    /// Returns the list of versions
    pub fn counterparty_versions(&self) -> Result<Vec<Version>> {
        let mut versions = vec![];
        for v in &self.counterparty_versions {
            versions.push(
                Version::decode_vec(v)
                    .map_err(|e| Error::DecodingError(e.to_string()))?,
            )
        }
        Ok(versions)
    }

    /// Returns the height of the proofs
    pub fn proof_height(&self) -> Height {
        Height::new(self.proof_height.0, self.proof_height.1)
    }

    /// Returns the proof for connection
    pub fn proof_connection(&self) -> CommitmentProofBytes {
        self.proof_connection.clone().into()
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> CommitmentProofBytes {
        self.proof_client.clone().into()
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus(&self) -> CommitmentProofBytes {
        self.proof_consensus.clone().into()
    }

    /// Returns the delay period
    pub fn delay_period(&self) -> Duration {
        Duration::new(self.delay_period.secs, self.delay_period.nanos)
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let height = self.proof_height();
        let consensus_proof =
            ConsensusProof::new(self.proof_consensus(), height)
                .map_err(Error::DecodingError)?;
        Proofs::new(
            self.proof_connection(),
            Some(self.proof_client()),
            Some(consensus_proof),
            None,
            height,
        )
        .map_err(Error::DecodingError)
    }
}

/// Data to acknowledge a connection
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ConnectionOpenAckData {
    /// The connection ID
    conn_id: String,
    /// The counterpart connection ID
    counterpart_conn_id: String,
    /// The client state
    client_state: Vec<u8>,
    /// The height of the proof
    proof_height: (u64, u64),
    /// The proof of the connection
    proof_connection: Vec<u8>,
    /// The proof of the client state
    proof_client: Vec<u8>,
    /// The proof of the consensus state
    proof_consensus: Vec<u8>,
    /// The version
    version: Vec<u8>,
}

impl ConnectionOpenAckData {
    /// Returns the data to acknowledge a connection
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        conn_id: ConnectionId,
        counterparty_conn_id: ConnectionId,
        client_state: AnyClientState,
        proof_height: Height,
        proof_connection: CommitmentProofBytes,
        proof_client: CommitmentProofBytes,
        proof_consensus: CommitmentProofBytes,
        version: Version,
    ) -> Self {
        let conn_id = conn_id.to_string();
        let counterpart_conn_id = counterparty_conn_id.to_string();
        let client_state = client_state
            .encode_vec()
            .expect("Encoding a client state shouldn't fail");
        let version = version
            .encode_vec()
            .expect("Encoding a version shouldn't fail");
        Self {
            conn_id,
            counterpart_conn_id,
            client_state,
            proof_height: (
                proof_height.revision_number,
                proof_height.revision_height,
            ),
            proof_connection: proof_connection.into(),
            proof_client: proof_client.into(),
            proof_consensus: proof_consensus.into(),
            version,
        }
    }

    /// Returns the connection ID
    pub fn connnection_id(&self) -> Result<ConnectionId> {
        ConnectionId::from_str(&self.conn_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the counterpart connection ID
    pub fn counterpart_connection_id(&self) -> Result<ConnectionId> {
        ConnectionId::from_str(&self.counterpart_conn_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the client state
    pub fn client_state(&self) -> Result<AnyClientState> {
        AnyClientState::decode_vec(&self.client_state)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the height of the proofs
    pub fn proof_height(&self) -> Height {
        Height::new(self.proof_height.0, self.proof_height.1)
    }

    /// Returns the proof for connection
    pub fn proof_connection(&self) -> CommitmentProofBytes {
        self.proof_connection.clone().into()
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> CommitmentProofBytes {
        self.proof_client.clone().into()
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus(&self) -> CommitmentProofBytes {
        self.proof_consensus.clone().into()
    }

    /// Returns the version
    pub fn version(&self) -> Result<Version> {
        Version::decode_vec(&self.version)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let height = self.proof_height();
        let consensus_proof =
            ConsensusProof::new(self.proof_consensus(), height)
                .map_err(Error::DecodingError)?;
        Proofs::new(
            self.proof_connection(),
            Some(self.proof_client()),
            Some(consensus_proof),
            None,
            height,
        )
        .map_err(Error::DecodingError)
    }
}

/// Data to confirm a connection
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ConnectionOpenConfirmData {
    /// The connection ID
    conn_id: String,
    /// The height of the proof
    proof_height: (u64, u64),
    /// The proof of the connection
    proof_connection: Vec<u8>,
    /// The proof of the client state
    proof_client: Vec<u8>,
    /// The proof of the consensus state
    proof_consensus: Vec<u8>,
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
        let conn_id = conn_id.to_string();
        Self {
            conn_id,
            proof_height: (
                proof_height.revision_number,
                proof_height.revision_height,
            ),
            proof_connection: proof_connection.into(),
            proof_client: proof_client.into(),
            proof_consensus: proof_consensus.into(),
        }
    }

    /// Returns the connection ID
    pub fn connnection_id(&self) -> Result<ConnectionId> {
        ConnectionId::from_str(&self.conn_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the height of the proofs
    pub fn proof_height(&self) -> Height {
        Height::new(self.proof_height.0, self.proof_height.1)
    }

    /// Returns the proof for connection
    pub fn proof_connection(&self) -> CommitmentProofBytes {
        self.proof_connection.clone().into()
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> CommitmentProofBytes {
        self.proof_client.clone().into()
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus(&self) -> CommitmentProofBytes {
        self.proof_consensus.clone().into()
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let height = self.proof_height();
        let consensus_proof =
            ConsensusProof::new(self.proof_consensus(), height)
                .map_err(Error::DecodingError)?;
        Proofs::new(
            self.proof_connection(),
            Some(self.proof_client()),
            Some(consensus_proof),
            None,
            height,
        )
        .map_err(Error::DecodingError)
    }
}

/// Data to initialize a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelOpenInitData {
    /// The port ID
    port_id: String,
    /// The order feature of the channel
    order: String,
    /// The counterparty
    counterparty: Vec<u8>,
    /// The connection hops
    connection_hops: Vec<String>,
    /// The version
    version: String,
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
        let port_id = port_id.to_string();
        let order = order.to_string();
        let counterparty = counterparty
            .encode_vec()
            .expect("Encoding a counterparty shouldn't fail");
        let connection_hops =
            connection_hops.iter().map(|c| c.to_string()).collect();
        Self {
            port_id,
            order,
            counterparty,
            connection_hops,
            version,
        }
    }

    /// Returns the port ID
    pub fn port_id(&self) -> Result<PortId> {
        PortId::from_str(&self.port_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the order
    pub fn order(&self) -> Result<Order> {
        Order::from_str(&self.order)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the counterparty
    pub fn counterparty(&self) -> Result<ChanCounterparty> {
        ChanCounterparty::decode_vec(&self.counterparty)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the connection hops
    pub fn connection_hops(&self) -> Result<Vec<ConnectionId>> {
        self.connection_hops
            .iter()
            .map(|conn_str| {
                ConnectionId::from_str(conn_str)
                    .map_err(|e| Error::DecodingError(e.to_string()))
            })
            .collect()
    }

    /// Returns the version
    pub fn version(&self) -> String {
        self.version.clone()
    }
}

/// Data to try to open a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelOpenTryData {
    /// The port ID
    port_id: String,
    /// The order feature of the channel
    order: String,
    /// The counterparty
    counterparty: Vec<u8>,
    /// The connection hops
    connection_hops: Vec<String>,
    /// The version
    version: String,
    /// The counterparty version
    counterparty_version: String,
    /// The height of the proof
    proof_height: (u64, u64),
    /// The proof of the channel
    proof_channel: Vec<u8>,
    /// The proof of the client state
    proof_client: Vec<u8>,
    /// The proof of the consensus state
    proof_consensus: Vec<u8>,
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
        let port_id = port_id.to_string();
        let order = order.to_string();
        let counterparty = counterparty
            .encode_vec()
            .expect("Encoding a counterparty shouldn't fail");
        let connection_hops =
            connection_hops.iter().map(|c| c.to_string()).collect();
        Self {
            port_id,
            order,
            counterparty,
            connection_hops,
            version,
            counterparty_version,
            proof_height: (
                proof_height.revision_number,
                proof_height.revision_height,
            ),
            proof_channel: proof_channel.into(),
            proof_client: proof_client.into(),
            proof_consensus: proof_consensus.into(),
        }
    }

    /// Returns the port ID
    pub fn port_id(&self) -> Result<PortId> {
        PortId::from_str(&self.port_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the order
    pub fn order(&self) -> Result<Order> {
        Order::from_str(&self.order)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the counterparty
    pub fn counterparty(&self) -> Result<ChanCounterparty> {
        ChanCounterparty::decode_vec(&self.counterparty)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the connection hops
    pub fn connection_hops(&self) -> Result<Vec<ConnectionId>> {
        let mut hops = vec![];
        for conn_str in &self.connection_hops {
            let id = ConnectionId::from_str(conn_str)
                .map_err(|e| Error::DecodingError(e.to_string()))?;
            hops.push(id);
        }
        Ok(hops)
    }

    /// Returns the version
    pub fn version(&self) -> String {
        self.version.clone()
    }

    /// Returns the counterparty version
    pub fn counterparty_version(&self) -> String {
        self.counterparty_version.clone()
    }

    /// Returns the height of the proofs
    pub fn proof_height(&self) -> Height {
        Height::new(self.proof_height.0, self.proof_height.1)
    }

    /// Returns the proof for the channel
    pub fn proof_channel(&self) -> CommitmentProofBytes {
        self.proof_channel.clone().into()
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> CommitmentProofBytes {
        self.proof_client.clone().into()
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus(&self) -> CommitmentProofBytes {
        self.proof_consensus.clone().into()
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let height = self.proof_height();
        let consensus_proof =
            ConsensusProof::new(self.proof_consensus(), height)
                .map_err(Error::DecodingError)?;
        Proofs::new(
            self.proof_channel(),
            Some(self.proof_client()),
            Some(consensus_proof),
            None,
            height,
        )
        .map_err(Error::DecodingError)
    }
}

/// Data to acknowledge a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelOpenAckData {
    /// The port ID
    port_id: String,
    /// The channel ID
    channel_id: String,
    /// The counterpart channel ID
    counterpart_channel_id: String,
    /// The counterparty version
    counterparty_version: String,
    /// The height of the proof
    proof_height: (u64, u64),
    /// The proof of the channel
    proof_channel: Vec<u8>,
    /// The proof of the client state
    proof_client: Vec<u8>,
    /// The proof of the consensus state
    proof_consensus: Vec<u8>,
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
        let port_id = port_id.to_string();
        let channel_id = channel_id.to_string();
        let counterpart_channel_id = counterpart_channel_id.to_string();
        Self {
            port_id,
            channel_id,
            counterpart_channel_id,
            counterparty_version,
            proof_height: (
                proof_height.revision_number,
                proof_height.revision_height,
            ),
            proof_channel: proof_channel.into(),
            proof_client: proof_client.into(),
            proof_consensus: proof_consensus.into(),
        }
    }

    /// Returns the port ID
    pub fn port_id(&self) -> Result<PortId> {
        PortId::from_str(&self.port_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the port ID
    pub fn channel_id(&self) -> Result<ChannelId> {
        ChannelId::from_str(&self.channel_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the port ID
    pub fn counterpart_channel_id(&self) -> Result<ChannelId> {
        ChannelId::from_str(&self.counterpart_channel_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the counterparty version
    pub fn counterparty_version(&self) -> String {
        self.counterparty_version.clone()
    }

    /// Returns the height of the proofs
    pub fn proof_height(&self) -> Height {
        Height::new(self.proof_height.0, self.proof_height.1)
    }

    /// Returns the proof for the channel
    pub fn proof_channel(&self) -> CommitmentProofBytes {
        self.proof_channel.clone().into()
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> CommitmentProofBytes {
        self.proof_client.clone().into()
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus(&self) -> CommitmentProofBytes {
        self.proof_consensus.clone().into()
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let height = self.proof_height();
        let consensus_proof =
            ConsensusProof::new(self.proof_consensus(), height)
                .map_err(Error::DecodingError)?;
        Proofs::new(
            self.proof_channel(),
            Some(self.proof_client()),
            Some(consensus_proof),
            None,
            height,
        )
        .map_err(Error::DecodingError)
    }
}

/// Data to confirm a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelOpenConfirmData {
    /// The port ID
    port_id: String,
    /// The channel ID
    channel_id: String,
    /// The height of the proof
    proof_height: (u64, u64),
    /// The proof of the channel
    proof_channel: Vec<u8>,
    /// The proof of the client state
    proof_client: Vec<u8>,
    /// The proof of the consensus state
    proof_consensus: Vec<u8>,
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
        let port_id = port_id.to_string();
        let channel_id = channel_id.to_string();
        Self {
            port_id,
            channel_id,
            proof_height: (
                proof_height.revision_number,
                proof_height.revision_height,
            ),
            proof_channel: proof_channel.into(),
            proof_client: proof_client.into(),
            proof_consensus: proof_consensus.into(),
        }
    }

    /// Returns the port ID
    pub fn port_id(&self) -> Result<PortId> {
        PortId::from_str(&self.port_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the port ID
    pub fn channel_id(&self) -> Result<ChannelId> {
        ChannelId::from_str(&self.channel_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the height of the proofs
    pub fn proof_height(&self) -> Height {
        Height::new(self.proof_height.0, self.proof_height.1)
    }

    /// Returns the proof for connection
    pub fn proof_channel(&self) -> CommitmentProofBytes {
        self.proof_channel.clone().into()
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> CommitmentProofBytes {
        self.proof_client.clone().into()
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus(&self) -> CommitmentProofBytes {
        self.proof_consensus.clone().into()
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let height = self.proof_height();
        let consensus_proof =
            ConsensusProof::new(self.proof_consensus(), height)
                .map_err(Error::DecodingError)?;
        Proofs::new(
            self.proof_channel(),
            Some(self.proof_client()),
            Some(consensus_proof),
            None,
            height,
        )
        .map_err(Error::DecodingError)
    }
}

/// Data to close a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelCloseInitData {
    /// The port ID
    port_id: String,
    /// The channel ID
    channel_id: String,
}

impl ChannelCloseInitData {
    /// Returns the data to close a channel
    pub fn new(port_id: PortId, channel_id: ChannelId) -> Self {
        let port_id = port_id.to_string();
        let channel_id = channel_id.to_string();
        Self {
            port_id,
            channel_id,
        }
    }

    /// Returns the port ID
    pub fn port_id(&self) -> Result<PortId> {
        PortId::from_str(&self.port_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the port ID
    pub fn channel_id(&self) -> Result<ChannelId> {
        ChannelId::from_str(&self.channel_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }
}

/// Data to confirm closing a channel
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct ChannelCloseConfirmData {
    /// The port ID
    port_id: String,
    /// The channel ID
    channel_id: String,
    /// The height of the proof
    proof_height: (u64, u64),
    /// The proof of the connection
    proof_connection: Vec<u8>,
    /// The proof of the client state
    proof_client: Vec<u8>,
    /// The proof of the consensus state
    proof_consensus: Vec<u8>,
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
        let port_id = port_id.to_string();
        let channel_id = channel_id.to_string();
        Self {
            port_id,
            channel_id,
            proof_height: (
                proof_height.revision_number,
                proof_height.revision_height,
            ),
            proof_connection: proof_connection.into(),
            proof_client: proof_client.into(),
            proof_consensus: proof_consensus.into(),
        }
    }

    /// Returns the port ID
    pub fn port_id(&self) -> Result<PortId> {
        PortId::from_str(&self.port_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the port ID
    pub fn channel_id(&self) -> Result<ChannelId> {
        ChannelId::from_str(&self.channel_id)
            .map_err(|e| Error::DecodingError(e.to_string()))
    }

    /// Returns the height of the proofs
    pub fn proof_height(&self) -> Height {
        Height::new(self.proof_height.0, self.proof_height.1)
    }

    /// Returns the proof for connection
    pub fn proof_connection(&self) -> CommitmentProofBytes {
        self.proof_connection.clone().into()
    }

    /// Returns the proof for client state
    pub fn proof_client(&self) -> CommitmentProofBytes {
        self.proof_client.clone().into()
    }

    /// Returns the proof for consensus state
    pub fn proof_consensus(&self) -> CommitmentProofBytes {
        self.proof_consensus.clone().into()
    }

    /// Returns the proofs
    pub fn proofs(&self) -> Result<Proofs> {
        let height = self.proof_height();
        let consensus_proof =
            ConsensusProof::new(self.proof_consensus(), height)
                .map_err(Error::DecodingError)?;
        Proofs::new(
            self.proof_connection(),
            Some(self.proof_client()),
            Some(consensus_proof),
            None,
            height,
        )
        .map_err(Error::DecodingError)
    }
}
