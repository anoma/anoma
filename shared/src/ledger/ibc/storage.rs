//! Functions for IBC-related data to access the storage

use std::str::FromStr;

use thiserror::Error;

use crate::ibc::core::ics02_client::height::Height;
use crate::ibc::core::ics04_channel::packet::Sequence;
use crate::ibc::core::ics05_port::capabilities::Capability;
use crate::ibc::core::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortChannelId, PortId,
};
use crate::ibc::core::ics24_host::path::{
    AcksPath, ChannelEndsPath, ClientConsensusStatePath, ClientStatePath,
    ClientTypePath, CommitmentsPath, ConnectionsPath, PortsPath, ReceiptsPath,
    SeqAcksPath, SeqRecvsPath, SeqSendsPath,
};
use crate::ibc::core::ics24_host::Path;
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{self, DbKeySeg, Key, KeySeg};

const CLIENTS_COUNTER: &str = "clients/counter";
const CONNECTIONS_COUNTER: &str = "connections/counter";
const CHANNELS_COUNTER: &str = "channelEnds/counter";
const CAPABILITIES_INDEX: &str = "capabilities/index";
const CAPABILITIES: &str = "capabilities";

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Storage key error: {0}")]
    StorageKey(storage::Error),
    #[error("Invalid Key: {0}")]
    InvalidKey(String),
    #[error("Port capability error: {0}")]
    InvalidPortCapability(String),
}

/// IBC storage functions result
pub type Result<T> = std::result::Result<T, Error>;

/// IBC prefix
#[allow(missing_docs)]
pub enum IbcPrefix {
    Client,
    Connection,
    Channel,
    Port,
    Capability,
    SeqSend,
    SeqRecv,
    SeqAck,
    Commitment,
    Receipt,
    Ack,
    Event,
    Unknown,
}

/// Returns the prefix from the given key
pub fn ibc_prefix(key: &Key) -> Option<IbcPrefix> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), ..]
            if addr == &Address::Internal(InternalAddress::Ibc) =>
        {
            Some(match &*prefix.raw() {
                "clients" => IbcPrefix::Client,
                "connections" => IbcPrefix::Connection,
                "channelEnds" => IbcPrefix::Channel,
                "ports" => IbcPrefix::Port,
                "capabilities" => IbcPrefix::Capability,
                "nextSequenceSend" => IbcPrefix::SeqSend,
                "nextSequenceRecv" => IbcPrefix::SeqRecv,
                "nextSequenceAck" => IbcPrefix::SeqAck,
                "commitments" => IbcPrefix::Commitment,
                "receipts" => IbcPrefix::Receipt,
                "acks" => IbcPrefix::Ack,
                "event" => IbcPrefix::Event,
                _ => IbcPrefix::Unknown,
            })
        }
        _ => None,
    }
}

/// Check if the given key is a key of the client counter
pub fn is_client_counter_key(key: &Key) -> bool {
    *key == client_counter_key()
}

/// Check if the given key is a key of the connection counter
pub fn is_connection_counter_key(key: &Key) -> bool {
    *key == connection_counter_key()
}

/// Check if the given key is a key of the channel counter
pub fn is_channel_counter_key(key: &Key) -> bool {
    *key == channel_counter_key()
}

/// Check if the given key is a key of the capability index
pub fn is_capability_index_key(key: &Key) -> bool {
    *key == capability_index_key()
}

/// Returns a key of the IBC-related data
fn ibc_key(path: impl AsRef<str>) -> Result<Key> {
    let path = Key::parse(path).map_err(Error::StorageKey)?;
    let addr = Address::Internal(InternalAddress::Ibc);
    let key = Key::from(addr.to_db_key());
    Ok(key.join(&path))
}

/// Returns a key of the IBC client counter
pub fn client_counter_key() -> Key {
    let path = CLIENTS_COUNTER.to_owned();
    ibc_key(path).expect("Creating a key for the client counter shouldn't fail")
}

/// Returns a key of the IBC connection counter
pub fn connection_counter_key() -> Key {
    let path = CONNECTIONS_COUNTER.to_owned();
    ibc_key(path)
        .expect("Creating a key for the connection counter shouldn't fail")
}

/// Returns a key of the IBC channel counter
pub fn channel_counter_key() -> Key {
    let path = CHANNELS_COUNTER.to_owned();
    ibc_key(path)
        .expect("Creating a key for the channel counter shouldn't fail")
}

/// Returns a key of the IBC capability index
pub fn capability_index_key() -> Key {
    let path = CAPABILITIES_INDEX.to_owned();
    ibc_key(path)
        .expect("Creating a key for the capability index shouldn't fail")
}

/// Returns a key for the client type
pub fn client_type_key(client_id: &ClientId) -> Key {
    let path = Path::ClientType(ClientTypePath(client_id.clone()));
    ibc_key(path.to_string())
        .expect("Creating a key for the client state shouldn't fail")
}

/// Returns a key for the client state
pub fn client_state_key(client_id: &ClientId) -> Key {
    let path = Path::ClientState(ClientStatePath(client_id.clone()));
    ibc_key(path.to_string())
        .expect("Creating a key for the client state shouldn't fail")
}

/// Returns a key for the consensus state
pub fn consensus_state_key(client_id: &ClientId, height: Height) -> Key {
    let path = Path::ClientConsensusState(ClientConsensusStatePath {
        client_id: client_id.clone(),
        epoch: height.revision_number,
        height: height.revision_height,
    });
    ibc_key(path.to_string())
        .expect("Creating a key for the consensus state shouldn't fail")
}

/// Returns a key prefix for the consensus state
pub fn consensus_state_prefix(client_id: &ClientId) -> Key {
    let path = Path::ClientConsensusState(ClientConsensusStatePath {
        client_id: client_id.clone(),
        epoch: 0,
        height: 0,
    });
    let suffix = "/0-0".to_string();
    let path = path.to_string();
    let prefix = path.strip_suffix(&suffix).expect("The suffix should exist");
    ibc_key(prefix)
        .expect("Creating a key prefix of the consensus state shouldn't fail")
}

/// Returns a key for the connection end
pub fn connection_key(conn_id: &ConnectionId) -> Key {
    let path = Path::Connections(ConnectionsPath(conn_id.clone()));
    ibc_key(path.to_string())
        .expect("Creating a key for the connection shouldn't fail")
}

/// Returns a key for the channel end
pub fn channel_key(port_channel_id: &PortChannelId) -> Key {
    let path = Path::ChannelEnds(ChannelEndsPath(
        port_channel_id.port_id.clone(),
        port_channel_id.channel_id,
    ));
    ibc_key(path.to_string())
        .expect("Creating a key for the channel shouldn't fail")
}

/// Returns a key for the port
pub fn port_key(port_id: &PortId) -> Key {
    let path = Path::Ports(PortsPath(port_id.clone()));
    ibc_key(path.to_string())
        .expect("Creating a key for the port shouldn't fail")
}

/// Returns a key of the reversed map for IBC capabilities
pub fn capability_key(index: u64) -> Key {
    let path = format!("{}/{}", CAPABILITIES, index);
    ibc_key(path).expect("Creating a key for a capability shouldn't fail")
}

/// Returns a key for nextSequenceSend
pub fn next_sequence_send_key(port_channel_id: &PortChannelId) -> Key {
    let path = Path::SeqSends(SeqSendsPath(
        port_channel_id.port_id.clone(),
        port_channel_id.channel_id,
    ));
    ibc_key(path.to_string())
        .expect("Creating a key for nextSequenceSend shouldn't fail")
}

/// Returns a key for nextSequenceRecv
pub fn next_sequence_recv_key(port_channel_id: &PortChannelId) -> Key {
    let path = Path::SeqRecvs(SeqRecvsPath(
        port_channel_id.port_id.clone(),
        port_channel_id.channel_id,
    ));
    ibc_key(path.to_string())
        .expect("Creating a key for nextSequenceRecv shouldn't fail")
}

/// Returns a key for nextSequenceAck
pub fn next_sequence_ack_key(port_channel_id: &PortChannelId) -> Key {
    let path = Path::SeqAcks(SeqAcksPath(
        port_channel_id.port_id.clone(),
        port_channel_id.channel_id,
    ));
    ibc_key(path.to_string())
        .expect("Creating a key for nextSequenceAck shouldn't fail")
}

/// Returns a key for the commitment
pub fn commitment_key(
    port_id: &PortId,
    channel_id: &ChannelId,
    sequence: Sequence,
) -> Key {
    let path = Path::Commitments(CommitmentsPath {
        port_id: port_id.clone(),
        channel_id: *channel_id,
        sequence,
    });
    ibc_key(path.to_string())
        .expect("Creating a key for the commitment shouldn't fail")
}

/// Returns a key for the receipt
pub fn receipt_key(
    port_id: &PortId,
    channel_id: &ChannelId,
    sequence: Sequence,
) -> Key {
    let path = Path::Receipts(ReceiptsPath {
        port_id: port_id.clone(),
        channel_id: *channel_id,
        sequence,
    });
    ibc_key(path.to_string())
        .expect("Creating a key for the receipt shouldn't fail")
}

/// Returns a key for the ack
pub fn ack_key(
    port_id: &PortId,
    channel_id: &ChannelId,
    sequence: Sequence,
) -> Key {
    let path = Path::Acks(AcksPath {
        port_id: port_id.clone(),
        channel_id: *channel_id,
        sequence,
    });
    ibc_key(path.to_string())
        .expect("Creating a key for the ack shouldn't fail")
}

/// Returns a key for the timestamp for the client update
pub fn client_update_timestamp_key(client_id: &ClientId) -> Key {
    let path = format!("clients/{}/update_timestamp", client_id);
    ibc_key(path).expect("Creating a key for the ack shouldn't fail")
}

/// Returns a key for the timestamp for the client update
pub fn client_update_height_key(client_id: &ClientId) -> Key {
    let path = format!("clients/{}/update_height", client_id);
    ibc_key(path).expect("Creating a key for the ack shouldn't fail")
}

/// Returns a client ID from the given client key `#IBC/clients/<client_id>`
pub fn client_id(key: &Key) -> Result<ClientId> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(client_id),
            ..,
        ] if addr == &Address::Internal(InternalAddress::Ibc)
            && prefix == "clients" =>
        {
            ClientId::from_str(&client_id.raw())
                .map_err(|e| Error::InvalidKey(e.to_string()))
        }
        _ => Err(Error::InvalidKey(format!(
            "The key doesn't have a client ID: {}",
            key
        ))),
    }
}

/// Returns the height from the given consensus state key
/// `#IBC/clients/<client_id>/consensusState/0-<height>`
pub fn consensus_height(key: &Key) -> Result<Height> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(_client_id),
            DbKeySeg::StringSeg(module),
            DbKeySeg::StringSeg(height),
        ] if addr == &Address::Internal(InternalAddress::Ibc)
            && prefix == "clients"
            && module == "consensusStates" =>
        {
            Height::from_str(height)
                .map_err(|e| Error::InvalidKey(e.to_string()))
        }
        _ => Err(Error::InvalidKey(format!(
            "The key doesn't have a consensus height: {}",
            key
        ))),
    }
}

/// Returns a connection ID from the given connection key
/// `#IBC/connections/<conn_id>`
pub fn connection_id(key: &Key) -> Result<ConnectionId> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(conn_id),
        ] if addr == &Address::Internal(InternalAddress::Ibc)
            && prefix == "connections" =>
        {
            ConnectionId::from_str(&conn_id.raw())
                .map_err(|e| Error::InvalidKey(e.to_string()))
        }
        _ => Err(Error::InvalidKey(format!(
            "The key doesn't have a connection ID: {}",
            key
        ))),
    }
}

/// Returns a pair of port ID and channel ID from the given channel/sequence key
/// `#IBC/<prefix>/ports/<port_id>/channels/<channel_id>`
pub fn port_channel_id(key: &Key) -> Result<PortChannelId> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(module0),
            DbKeySeg::StringSeg(port),
            DbKeySeg::StringSeg(module1),
            DbKeySeg::StringSeg(channel),
        ] if addr == &Address::Internal(InternalAddress::Ibc)
            && (prefix == "channelEnds"
                || prefix == "nextSequenceSend"
                || prefix == "nextSequenceRecv"
                || prefix == "nextSequenceAck")
            && module0 == "ports"
            && module1 == "channels" =>
        {
            let port_id = PortId::from_str(&port.raw())
                .map_err(|e| Error::InvalidKey(e.to_string()))?;
            let channel_id = ChannelId::from_str(&channel.raw())
                .map_err(|e| Error::InvalidKey(e.to_string()))?;
            Ok(PortChannelId {
                port_id,
                channel_id,
            })
        }
        _ => Err(Error::InvalidKey(format!(
            "The key doesn't have port ID and channel ID: Key {}",
            key
        ))),
    }
}

/// Returns a tuple of port ID, channel ID and Sequence from the given packet
/// info key `#IBC/<info_prefix>/ports/<port_id>/channels/<channel_id>/
/// sequences/<sequence>`
pub fn port_channel_sequence_id(
    key: &Key,
) -> Result<(PortId, ChannelId, Sequence)> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(module0),
            DbKeySeg::StringSeg(port_id),
            DbKeySeg::StringSeg(module1),
            DbKeySeg::StringSeg(channel_id),
            DbKeySeg::StringSeg(module2),
            DbKeySeg::StringSeg(seq_index),
        ] if addr == &Address::Internal(InternalAddress::Ibc)
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
            "The key doesn't have port ID, channel ID and sequence number: \
             Key {}",
            key,
        ))),
    }
}

/// Returns a port ID from the given port key `#IBC/ports/<port_id>`
pub fn port_id(key: &Key) -> Result<PortId> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(port_id),
            ..,
        ] if addr == &Address::Internal(InternalAddress::Ibc)
            && prefix == "ports" =>
        {
            PortId::from_str(&port_id.raw())
                .map_err(|e| Error::InvalidKey(e.to_string()))
        }
        _ => Err(Error::InvalidKey(format!(
            "The key doesn't have a port ID: Key {}",
            key
        ))),
    }
}

/// Returns a capability from the given capability key
/// `#IBC/capabilities/<index>`
pub fn capability(key: &Key) -> Result<Capability> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(index),
            ..,
        ] if addr == &Address::Internal(InternalAddress::Ibc)
            && prefix == "capabilities" =>
        {
            let index: u64 = index.raw().parse().map_err(|e| {
                Error::InvalidPortCapability(format!(
                    "The key has a non-number index: Key {}, {}",
                    key, e
                ))
            })?;
            Ok(Capability::from(index))
        }
        _ => Err(Error::InvalidPortCapability(format!(
            "The key doesn't have a capability index: Key {}",
            key
        ))),
    }
}
