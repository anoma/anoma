//! IBC integration as a native validity predicate

mod channel;
mod client;
mod connection;
mod port;

use std::collections::HashSet;

use ibc::ics02_client::context::ClientReader;
use thiserror::Error;

use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{Key, KeySeg};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
    #[error("Key error: {0}")]
    KeyError(String),
    #[error("Counter error: {0}")]
    CounterError(String),
    #[error("Client validation error: {0}")]
    ClientError(client::Error),
    #[error("Connection validation error: {0}")]
    ConnectionError(connection::Error),
    #[error("Channel validation error: {0}")]
    ChannelError(channel::Error),
    #[error("Port validation error: {0}")]
    PortError(port::Error),
}

/// IBC functions result
pub type Result<T> = std::result::Result<T, Error>;

/// IBC VP
pub struct Ibc<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

/// Initialize storage in the genesis block.
pub fn init_genesis_storage<DB, H>(storage: &mut Storage<DB, H>)
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    // the client counter
    let key = Key::ibc_client_counter();
    let value = storage::types::encode(&0);
    storage
        .write(&key, value)
        .expect("Unable to write the initial client counter");

    // the connection counter
    let key = Key::ibc_connection_counter();
    let value = storage::types::encode(&0);
    storage
        .write(&key, value)
        .expect("Unable to write the initial connection counter");

    // the channel counter
    let key = Key::ibc_channel_counter();
    let value = storage::types::encode(&0);
    storage
        .write(&key, value)
        .expect("Unable to write the initial channel counter");

    // the capability index
    let key = Key::ibc_capability_index();
    let value = storage::types::encode(&0);
    storage
        .write(&key, value)
        .expect("Unable to write the initial capability index");
}

impl<'a, DB, H> NativeVp for Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::Ibc;

    fn validate_tx(
        &self,
        tx_data: &[u8],
        keys_changed: &HashSet<Key>,
        _verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        let mut clients = HashSet::new();

        for key in keys_changed {
            if !key.is_ibc_key() {
                continue;
            }

            let accepted = match Self::get_ibc_prefix(key) {
                IbcPrefix::Client => {
                    if key.is_ibc_client_counter() {
                        self.client_counter_pre()? < self.client_counter()
                    } else {
                        let client_id = Self::get_client_id(key)?;
                        if !clients.insert(client_id.clone()) {
                            // this client has been checked
                            continue;
                        }
                        self.validate_client(&client_id, tx_data)?
                    }
                }
                IbcPrefix::Connection => {
                    self.validate_connection(key, tx_data)?
                }
                IbcPrefix::Channel => self.validate_channel(key, tx_data)?,
                IbcPrefix::Port => self.validate_port(key)?,
                IbcPrefix::Capability => self.validate_capability(key)?,
                // TODO implement validations for modules
                IbcPrefix::SeqSend => false,
                IbcPrefix::SeqRecv => false,
                IbcPrefix::SeqAck => false,
                IbcPrefix::Commitment => false,
                IbcPrefix::Receipt => false,
                IbcPrefix::Ack => false,
                IbcPrefix::Unknown => {
                    return Err(Error::KeyError(format!(
                        "Invalid IBC-related key: {}",
                        key
                    )));
                }
            };
            if !accepted {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

enum StateChange {
    Created,
    Updated,
    Deleted,
    NotExists,
}

enum IbcPrefix {
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
    Unknown,
}

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    /// Returns the prefix after #IBC
    fn get_ibc_prefix(key: &Key) -> IbcPrefix {
        match key.segments.get(1) {
            Some(prefix) => match &*prefix.raw() {
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
                _ => IbcPrefix::Unknown,
            },
            None => IbcPrefix::Unknown,
        }
    }

    fn get_state_change(&self, key: &Key) -> Result<StateChange> {
        if self.ctx.has_key_pre(key)? {
            if self.ctx.has_key_post(key)? {
                Ok(StateChange::Updated)
            } else {
                Ok(StateChange::Deleted)
            }
        } else if self.ctx.has_key_post(key)? {
            Ok(StateChange::Created)
        } else {
            Ok(StateChange::NotExists)
        }
    }

    fn read_counter_pre(&self, key: &Key) -> Result<u64> {
        match self.ctx.read_pre(key) {
            Ok(Some(value)) => storage::types::decode(&value).map_err(|e| {
                Error::CounterError(format!(
                    "Decoding the client counter failed: {}",
                    e
                ))
            }),
            _ => Err(Error::CounterError(
                "The client counter doesn't exist".to_owned(),
            )),
        }
    }

    fn read_counter(&self, key: &Key) -> u64 {
        match self.ctx.read_post(key) {
            Ok(Some(value)) => match storage::types::decode(&value) {
                Ok(c) => c,
                Err(e) => {
                    tracing::error!("decoding a counter failed: {}", e);
                    u64::MIN
                }
            },
            _ => {
                tracing::error!("the counter doesn't exist");
                unreachable!();
            }
        }
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}

impl From<client::Error> for Error {
    fn from(err: client::Error) -> Self {
        Self::ClientError(err)
    }
}

impl From<connection::Error> for Error {
    fn from(err: connection::Error) -> Self {
        Self::ConnectionError(err)
    }
}

impl From<channel::Error> for Error {
    fn from(err: channel::Error) -> Self {
        Self::ChannelError(err)
    }
}

impl From<port::Error> for Error {
    fn from(err: port::Error) -> Self {
        Self::PortError(err)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use std::time::Duration;

    use borsh::ser::BorshSerialize;
    use ibc::ics02_client::client_consensus::ConsensusState;
    use ibc::ics02_client::client_state::ClientState;
    use ibc::ics02_client::client_type::ClientType;
    use ibc::ics02_client::header::AnyHeader;
    use ibc::ics03_connection::connection::{
        ConnectionEnd, Counterparty as ConnCounterparty, State as ConnState,
    };
    use ibc::ics03_connection::version::Version;
    use ibc::ics04_channel::channel::{
        ChannelEnd, Counterparty as ChanCounterparty, Order, State as ChanState,
    };
    use ibc::ics23_commitment::commitment::{
        CommitmentPrefix, CommitmentProofBytes,
    };
    use ibc::ics24_host::identifier::{
        ChannelId, ClientId, ConnectionId, PortId,
    };
    use ibc::ics24_host::Path;
    use ibc::mock::client_state::{MockClientState, MockConsensusState};
    use ibc::mock::header::MockHeader;
    use ibc::Height;
    use tendermint_proto::Protobuf;

    use super::*;
    use crate::ledger::gas::VpGasMeter;
    use crate::ledger::storage::testing::TestStorage;
    use crate::ledger::storage::write_log::WriteLog;
    use crate::proto::Tx;
    use crate::types::ibc::{ClientUpdateData, ConnectionOpenTryData};

    fn get_client_id() -> ClientId {
        ClientId::from_str("test_client").expect("Creating a client ID failed")
    }

    fn get_client_type_key() -> Key {
        let client_id = get_client_id();
        let path = Path::ClientType(client_id).to_string();
        Key::ibc_key(path).expect("Creating a key for a client type failed")
    }

    fn get_client_state_key() -> Key {
        let client_id = get_client_id();
        let path = Path::ClientState(client_id).to_string();
        Key::ibc_key(path).expect("Creating a key for a client state failed")
    }

    fn get_consensus_state_key(height: Height) -> Key {
        let path = Path::ClientConsensusState {
            client_id: get_client_id(),
            epoch: height.revision_number,
            height: height.revision_height,
        }
        .to_string();
        Key::ibc_key(path)
            .expect("Creating a key for a consensus state shouldn't fail")
    }

    fn insert_init_states(write_log: &mut WriteLog) {
        // insert a mock client type
        let client_type_key = get_client_type_key();
        let client_type = ClientType::Mock.as_str().to_owned();
        write_log
            .write(&client_type_key, storage::types::encode(&client_type))
            .expect("write failed");
        // insert a mock client state
        let client_state_key = get_client_state_key();
        let height = Height::new(1, 10);
        let header = MockHeader::new(height);
        let client_state = MockClientState(header).wrap_any();
        let bytes = client_state.encode_vec().expect("encoding failed");
        write_log
            .write(&client_state_key, bytes)
            .expect("write failed");
        // insert a mock consensus state
        let consensus_key = get_consensus_state_key(height);
        let consensus_state = MockConsensusState(header).wrap_any();
        let bytes = consensus_state.encode_vec().expect("encoding failed");
        write_log
            .write(&consensus_key, bytes)
            .expect("write failed");
        write_log.commit_tx();
    }

    fn get_connection_id() -> ConnectionId {
        ConnectionId::from_str("test_connection")
            .expect("Creating a connection ID failed")
    }

    fn get_connection_key() -> Key {
        let conn_id = get_connection_id();
        let path = Path::Connections(conn_id).to_string();
        Key::ibc_key(path).expect("Creating a key for a connection failed")
    }

    fn get_commitment_prefix() -> CommitmentPrefix {
        let addr = Address::Internal(InternalAddress::Ibc);
        let bytes = addr
            .raw()
            .try_to_vec()
            .expect("Encoding an address string shouldn't fail");
        CommitmentPrefix::from(bytes)
    }

    fn get_port_id() -> PortId {
        PortId::from_str("test_port").expect("Creating a port ID failed")
    }

    fn get_port_key() -> Key {
        let port_id = get_port_id();
        let path = Path::Ports(port_id).to_string();
        Key::ibc_key(path).expect("Creating a key for a port failed")
    }

    fn get_channel_id() -> ChannelId {
        ChannelId::from_str("test_channel")
            .expect("Creating a channel ID failed")
    }

    fn get_channel_key() -> Key {
        let port_id = get_port_id();
        let channel_id = get_channel_id();
        let path = Path::ChannelEnds(port_id, channel_id).to_string();
        Key::ibc_key(path).expect("Creating a key for a channel failed")
    }

    #[test]
    fn test_create_client() {
        let storage = TestStorage::default();
        let mut write_log = WriteLog::default();
        insert_init_states(&mut write_log);

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_client_state_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_create_client_fail() {
        let storage = TestStorage::default();
        let write_log = WriteLog::default();
        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_client_state_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should fail because no state is stored
        let result = ibc
            .validate_tx(&tx_data, &keys_changed, &verifiers)
            .unwrap_err();
        assert_matches!(
            result,
            Error::ClientError(client::Error::InvalidStateChange(_))
        );
    }

    #[test]
    fn test_update_client() {
        let mut storage = TestStorage::default();
        let mut write_log = WriteLog::default();
        insert_init_states(&mut write_log);
        write_log.commit_block(&mut storage).expect("commit failed");

        // update the client
        let client_id = get_client_id();
        let client_state_key = get_client_state_key();
        let height = Height::new(1, 11);
        let header = MockHeader::new(height);
        let client_state = MockClientState(header).wrap_any();
        let bytes = client_state.encode_vec().expect("encoding failed");
        write_log
            .write(&client_state_key, bytes)
            .expect("write failed");
        let consensus_key = get_consensus_state_key(height);
        let consensus_state = MockConsensusState(header).wrap_any();
        let bytes = consensus_state.encode_vec().expect("encoding failed");
        write_log
            .write(&consensus_key, bytes)
            .expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data =
            ClientUpdateData::new(client_id, vec![AnyHeader::from(header)])
                .try_to_vec()
                .expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_client_state_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_init_connection() {
        let mut storage = TestStorage::default();
        let mut write_log = WriteLog::default();
        insert_init_states(&mut write_log);
        write_log.commit_block(&mut storage).expect("commit failed");

        // insert a initial connection
        let client_id = get_client_id();
        let conn_key = get_connection_key();
        let conn = ConnectionEnd::new(
            ConnState::Init,
            client_id,
            ConnCounterparty::default(),
            vec![Version::default()],
            Duration::new(100, 0),
        );
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_connection_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_init_connection_fail() {
        let storage = TestStorage::default();
        let mut write_log = WriteLog::default();

        // insert a initial connection
        let client_id = get_client_id();
        let conn_key = get_connection_key();
        let conn = ConnectionEnd::new(
            ConnState::Init,
            client_id,
            ConnCounterparty::default(),
            vec![Version::default()],
            Duration::new(100, 0),
        );
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_connection_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should fail because no client exists
        let result = ibc
            .validate_tx(&tx_data, &keys_changed, &verifiers)
            .unwrap_err();
        assert_matches!(
            result,
            Error::ConnectionError(connection::Error::InvalidClient(_))
        );
    }

    // TODO check after implementing MockConsensusState and verification
    // functions in ibc-rs
    #[test]
    #[ignore]
    fn test_try_connection() {
        let mut storage = TestStorage::default();
        let mut write_log = WriteLog::default();
        insert_init_states(&mut write_log);
        write_log.commit_block(&mut storage).expect("commit failed");

        // insert a initial connection
        let client_id = get_client_id();
        let conn_key = get_connection_key();
        let conn = ConnectionEnd::new(
            ConnState::TryOpen,
            client_id.clone(),
            ConnCounterparty::default(),
            vec![Version::default()],
            Duration::new(100, 0),
        );
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();

        let height = Height::new(1, 10);
        let header = MockHeader::new(height);
        let client_state = MockClientState(header).wrap_any();
        let counterpart_client_id =
            ClientId::from_str("counterpart_test_client")
                .expect("Creating a client ID failed");
        let counterpart_conn_id =
            ConnectionId::from_str("counterpart_test_connection")
                .expect("Creating a connection ID failed");
        let counterparty = ConnCounterparty::new(
            counterpart_client_id,
            Some(counterpart_conn_id),
            get_commitment_prefix(),
        );

        let proof_conn = CommitmentProofBytes::from(vec![0]);
        let proof_client = CommitmentProofBytes::from(vec![0]);
        let proof_consensus = CommitmentProofBytes::from(vec![0]);
        let tx_code = vec![];
        let data = ConnectionOpenTryData::new(
            client_id,
            client_state,
            counterparty,
            vec![Version::default()],
            height,
            proof_conn,
            proof_client,
            proof_consensus,
            Duration::new(100, 0),
        );
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_connection_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_init_channel() {
        let mut storage = TestStorage::default();
        let mut write_log = WriteLog::default();
        insert_init_states(&mut write_log);
        write_log.commit_block(&mut storage).expect("commit failed");

        // insert a initial connection
        let client_id = get_client_id();
        let conn_key = get_connection_key();
        let conn = ConnectionEnd::new(
            ConnState::Open,
            client_id,
            ConnCounterparty::default(),
            vec![Version::default()],
            Duration::new(100, 0),
        );
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");

        // insert a port
        let port_key = get_port_key();
        let index = 0u64;
        write_log
            .write(&port_key, storage::types::encode(&index))
            .expect("write failed");
        // insert to the reverse map
        let path = format!("capabilities/{}", index);
        let index_key =
            Key::ibc_key(path).expect("Creating a key for a capability failed");
        let port_id = get_port_id().as_str().to_owned();
        write_log
            .write(&index_key, storage::types::encode(&port_id))
            .expect("write failed");

        // insert a initial channel
        let channel_key = get_channel_key();
        let channel = ChannelEnd::new(
            ChanState::Init,
            Order::default(),
            ChanCounterparty::default(),
            vec![get_connection_id()],
            "ORDER_ORDERED".to_string(),
        );
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_port_key());
        keys_changed.insert(get_channel_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }
}
