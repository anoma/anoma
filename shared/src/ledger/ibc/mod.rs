//! IBC integration as a native validity predicate

mod client;
mod connection;

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
    #[error("Client validation error: {0}")]
    ClientError(client::Error),
    #[error("Connection validation error: {0}")]
    ConnectionError(connection::Error),
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
    let value = crate::ledger::storage::types::encode(&0);
    storage
        .write(&key, value)
        .expect("Unable to write the initial client counter");

    // the connection counter
    let key = Key::ibc_connection_counter();
    let value = crate::ledger::storage::types::encode(&0);
    storage
        .write(&key, value)
        .expect("Unable to write the initial connection counter");
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
                // TODO implement validations for modules
                IbcPrefix::Channel => false,
                IbcPrefix::Packet => false,
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
    Packet,
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
                "packets" => IbcPrefix::Packet,
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
        ConnectionEnd, Counterparty, State,
    };
    use ibc::ics03_connection::version::Version;
    use ibc::ics23_commitment::commitment::{
        CommitmentPrefix, CommitmentProofBytes,
    };
    use ibc::ics24_host::identifier::{ClientId, ConnectionId};
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
            .write(
                &client_type_key,
                crate::ledger::storage::types::encode(&client_type),
            )
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
            State::Init,
            client_id,
            Counterparty::default(),
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
            State::Init,
            client_id,
            Counterparty::default(),
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
            State::TryOpen,
            client_id.clone(),
            Counterparty::default(),
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
        let counterparty = Counterparty::new(
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
}
