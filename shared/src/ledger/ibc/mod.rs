//! IBC integration as a native validity predicate

mod channel;
mod client;
mod connection;
mod packet;
mod port;
mod sequence;
mod storage;

use std::collections::HashSet;

use borsh::{BorshDeserialize, BorshSerialize};
use ibc::ics02_client::context::ClientReader;
use storage::{
    capability_index_key, channel_counter_key, client_counter_key, client_id,
    connection_counter_key, ibc_prefix, is_client_counter_key, IbcPrefix,
};
use thiserror::Error;

use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self as ledger_storage, Storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::Key;

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
    #[error("Packet validation error: {0}")]
    PacketError(packet::Error),
    #[error("Sequence validation error: {0}")]
    SequenceError(sequence::Error),
}

/// IBC functions result
pub type Result<T> = std::result::Result<T, Error>;

/// IBC VP
pub struct Ibc<'a, DB, H>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

/// Initialize storage in the genesis block.
pub fn init_genesis_storage<DB, H>(storage: &mut Storage<DB, H>)
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
{
    // the client counter
    let key = client_counter_key();
    let value = 0_u64.try_to_vec().unwrap();
    storage
        .write(&key, value)
        .expect("Unable to write the initial client counter");

    // the connection counter
    let key = connection_counter_key();
    let value = 0_u64.try_to_vec().unwrap();
    storage
        .write(&key, value)
        .expect("Unable to write the initial connection counter");

    // the channel counter
    let key = channel_counter_key();
    let value = 0_u64.try_to_vec().unwrap();
    storage
        .write(&key, value)
        .expect("Unable to write the initial channel counter");

    // the capability index
    let key = capability_index_key();
    let value = 0_u64.try_to_vec().unwrap();
    storage
        .write(&key, value)
        .expect("Unable to write the initial capability index");
}

impl<'a, DB, H> NativeVp for Ibc<'a, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
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
            match ibc_prefix(key) {
                IbcPrefix::Client => {
                    if is_client_counter_key(key) {
                        if self.client_counter_pre()? >= self.client_counter() {
                            return Err(Error::CounterError(
                                "The client counter is invalid".to_owned(),
                            ));
                        }
                    } else {
                        let client_id = client_id(key)
                            .map_err(|e| Error::KeyError(e.to_string()))?;
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
                IbcPrefix::SeqSend => {
                    self.validate_sequence_send(key, tx_data)?
                }
                IbcPrefix::SeqRecv => {
                    self.validate_sequence_recv(key, tx_data)?
                }
                IbcPrefix::SeqAck => {
                    self.validate_sequence_ack(key, tx_data)?
                }
                IbcPrefix::Commitment => {
                    self.validate_commitment(key, tx_data)?
                }
                IbcPrefix::Receipt => self.validate_receipt(key, tx_data)?,
                IbcPrefix::Ack => self.validate_ack(key)?,
                IbcPrefix::Unknown => {
                    return Err(Error::KeyError(format!(
                        "Invalid IBC-related key: {}",
                        key
                    )));
                }
            };
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

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
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
            Ok(Some(value)) => u64::try_from_slice(&value[..]).map_err(|e| {
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
            Ok(Some(value)) => match u64::try_from_slice(&value[..]) {
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

impl From<packet::Error> for Error {
    fn from(err: packet::Error) -> Self {
        Self::PacketError(err)
    }
}

impl From<sequence::Error> for Error {
    fn from(err: sequence::Error) -> Self {
        Self::SequenceError(err)
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;
    use std::str::FromStr;
    use std::time::Duration;

    use borsh::ser::BorshSerialize;
    use chrono::Utc;
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
    use ibc::ics04_channel::packet::{Packet, Sequence};
    use ibc::ics23_commitment::commitment::{
        CommitmentPrefix, CommitmentProofBytes,
    };
    use ibc::ics24_host::identifier::{
        ChannelId, ClientId, ConnectionId, PortChannelId, PortId,
    };
    use ibc::mock::client_state::{MockClientState, MockConsensusState};
    use ibc::mock::header::MockHeader;
    use ibc::timestamp::Timestamp;
    use ibc::Height;
    use sha2::Digest;
    use storage::{
        ack_key, capability_key, channel_key, client_state_key,
        client_type_key, commitment_key, connection_key, consensus_state_key,
        next_sequence_ack_key, next_sequence_recv_key, next_sequence_send_key,
        port_key, receipt_key,
    };
    use tendermint::account::Id as TmAccountId;
    use tendermint::block::header::{Header as TmHeader, Version as TmVersion};
    use tendermint::block::Height as TmHeight;
    use tendermint::chain::Id as TmChainId;
    use tendermint::hash::{AppHash, Hash as TmHash};
    use tendermint::time::Time as TmTime;

    use super::*;
    use crate::ledger::gas::VpGasMeter;
    use crate::ledger::storage::testing::TestStorage;
    use crate::ledger::storage::write_log::WriteLog;
    use crate::proto::Tx;
    use crate::types::ibc::{
        ChannelOpenAckData, ChannelOpenConfirmData, ChannelOpenInitData,
        ChannelOpenTryData, ClientUpdateData, ConnectionOpenAckData,
        ConnectionOpenConfirmData, ConnectionOpenInitData,
        ConnectionOpenTryData, PacketAckData, PacketReceiptData,
        PacketSendData,
    };
    use crate::types::storage::KeySeg;

    fn get_client_id() -> ClientId {
        ClientId::from_str("test_client").expect("Creating a client ID failed")
    }

    fn insert_init_states() -> (TestStorage, WriteLog) {
        let mut storage = TestStorage::default();
        let mut write_log = WriteLog::default();

        // initialize the storage
        init_genesis_storage(&mut storage);
        // set a dummy header
        storage
            .set_header(get_dummy_header())
            .expect("Setting a dummy header shouldn't fail");

        // insert a mock client type
        let client_id = get_client_id();
        let client_type_key = client_type_key(&client_id);
        let client_type =
            ClientType::Mock.try_to_vec().expect("encoding failed");
        write_log
            .write(&client_type_key, client_type)
            .expect("write failed");
        // insert a mock client state
        let client_state_key = client_state_key(&get_client_id());
        let height = Height::new(1, 10);
        let header = MockHeader {
            height,
            timestamp: Timestamp::now(),
        };
        let client_state = MockClientState(header).wrap_any();
        let bytes = client_state.try_to_vec().expect("encoding failed");
        write_log
            .write(&client_state_key, bytes)
            .expect("write failed");
        // insert a mock consensus state
        let consensus_key = consensus_state_key(&client_id, height);
        let consensus_state = MockConsensusState::new(header).wrap_any();
        let bytes = consensus_state.try_to_vec().expect("encoding failed");
        write_log
            .write(&consensus_key, bytes)
            .expect("write failed");
        write_log.commit_tx();

        (storage, write_log)
    }

    fn get_dummy_header() -> TmHeader {
        TmHeader {
            version: TmVersion { block: 10, app: 0 },
            chain_id: TmChainId::try_from("test_chain".to_owned())
                .expect("Creating an TmChainId shouldn't fail"),
            height: TmHeight::try_from(10_u64)
                .expect("Creating a height shouldn't fail"),
            time: TmTime::now(),
            last_block_id: None,
            last_commit_hash: None,
            data_hash: None,
            validators_hash: TmHash::None,
            next_validators_hash: TmHash::None,
            consensus_hash: TmHash::None,
            app_hash: AppHash::try_from(vec![0])
                .expect("Creating an AppHash shouldn't fail"),
            last_results_hash: None,
            evidence_hash: None,
            proposer_address: TmAccountId::try_from(vec![0u8; 20])
                .expect("Creating an AccountId shouldn't fail"),
        }
    }

    fn get_connection_id() -> ConnectionId {
        ConnectionId::new(0)
    }

    fn get_commitment_prefix() -> CommitmentPrefix {
        let addr = Address::Internal(InternalAddress::Ibc);
        let bytes = addr
            .raw()
            .try_to_vec()
            .expect("Encoding an address string shouldn't fail");
        CommitmentPrefix::from(bytes)
    }

    fn get_port_channel_id() -> PortChannelId {
        PortChannelId {
            port_id: get_port_id(),
            channel_id: get_channel_id(),
        }
    }

    fn get_port_id() -> PortId {
        PortId::from_str("test_port").unwrap()
    }

    fn get_channel_id() -> ChannelId {
        ChannelId::from_str("test_channel").unwrap()
    }

    fn get_connection(conn_state: ConnState) -> ConnectionEnd {
        ConnectionEnd::new(
            conn_state,
            get_client_id(),
            get_conn_counterparty(),
            vec![Version::default()],
            Duration::new(100, 0),
        )
    }

    fn get_conn_counterparty() -> ConnCounterparty {
        let counterpart_client_id =
            ClientId::from_str("counterpart_test_client")
                .expect("Creating a client ID failed");
        let counterpart_conn_id =
            ConnectionId::from_str("counterpart_test_connection")
                .expect("Creating a connection ID failed");
        ConnCounterparty::new(
            counterpart_client_id,
            Some(counterpart_conn_id),
            get_commitment_prefix(),
        )
    }

    fn get_channel(channel_state: ChanState, order: Order) -> ChannelEnd {
        ChannelEnd::new(
            channel_state,
            order,
            get_channel_counterparty(),
            vec![get_connection_id()],
            order.to_string(),
        )
    }

    fn get_channel_counterparty() -> ChanCounterparty {
        let counterpart_port_id = PortId::from_str("counterpart_test_port")
            .expect("Creating a port ID failed");
        let counterpart_channel_id =
            ChannelId::from_str("counterpart_test_channel")
                .expect("Creating a channel ID failed");
        ChanCounterparty::new(counterpart_port_id, Some(counterpart_channel_id))
    }

    fn set_port(write_log: &mut WriteLog, index: u64) {
        let port_key = port_key(&get_port_id());
        write_log
            .write(&port_key, index.try_to_vec().expect("encoding failed"))
            .expect("write failed");
        // insert to the reverse map
        let cap_key = capability_key(index);
        let port_id = get_port_id();
        let bytes = port_id.try_to_vec().expect("encoding failed");
        write_log.write(&cap_key, bytes).expect("write failed");
    }

    fn get_next_seq(storage: &TestStorage, key: &Key) -> Sequence {
        let (val, _) = storage.read(key).expect("read failed");
        match val {
            Some(v) => {
                let index =
                    u64::try_from_slice(&v[..]).expect("decoding failed");
                Sequence::from(index)
            }
            // The sequence has not been used yet
            None => Sequence::from(1),
        }
    }

    fn increment_seq(write_log: &mut WriteLog, key: &Key, seq: Sequence) {
        let seq_num = u64::from(seq.increment());
        write_log
            .write(key, seq_num.try_to_vec().unwrap())
            .expect("write failed");
    }

    fn hash(packet: &Packet) -> String {
        let input = format!(
            "{:?},{:?},{:?}",
            packet.timeout_timestamp, packet.timeout_height, packet.data,
        );
        let r = sha2::Sha256::digest(input.as_bytes());
        format!("{:x}", r)
    }

    #[test]
    fn test_create_client() {
        let (storage, write_log) = insert_init_states();

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        let client_state_key = client_state_key(&get_client_id());
        keys_changed.insert(client_state_key);

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
        let client_state_key = client_state_key(&get_client_id());
        keys_changed.insert(client_state_key);

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
        let (mut storage, mut write_log) = insert_init_states();
        write_log.commit_block(&mut storage).expect("commit failed");

        // update the client
        let client_id = get_client_id();
        let client_state_key = client_state_key(&get_client_id());
        let height = Height::new(1, 11);
        let header = MockHeader {
            height,
            timestamp: Timestamp::now(),
        };
        let client_state = MockClientState(header).wrap_any();
        let bytes = client_state.try_to_vec().expect("encoding failed");
        write_log
            .write(&client_state_key, bytes)
            .expect("write failed");
        let consensus_key = consensus_state_key(&client_id, height);
        let consensus_state = MockConsensusState::new(header).wrap_any();
        let bytes = consensus_state.try_to_vec().expect("encoding failed");
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
        keys_changed.insert(client_state_key);

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
        let (mut storage, mut write_log) = insert_init_states();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let data = ConnectionOpenInitData::new(
            get_client_id(),
            get_conn_counterparty(),
            Version::default(),
            Duration::new(100, 0),
        );

        // insert an INIT connection
        let conn_key = connection_key(&get_connection_id());
        let conn = data.connection();
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(conn_key);

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

        // prepare data
        let data = ConnectionOpenInitData::new(
            get_client_id(),
            get_conn_counterparty(),
            Version::default(),
            Duration::new(100, 0),
        );

        // insert an Init connection
        let conn_key = connection_key(&get_connection_id());
        let conn = data.connection();
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(conn_key);

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

    #[test]
    fn test_try_connection() {
        let (mut storage, mut write_log) = insert_init_states();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let height = Height::new(1, 10);
        let header = MockHeader {
            height,
            timestamp: Timestamp::now(),
        };
        let client_state = MockClientState(header).wrap_any();
        let proof_conn = CommitmentProofBytes::from(vec![0]);
        let proof_client = CommitmentProofBytes::from(vec![0]);
        let proof_consensus = CommitmentProofBytes::from(vec![0]);
        let data = ConnectionOpenTryData::new(
            get_client_id(),
            client_state,
            get_conn_counterparty(),
            vec![Version::default()],
            height,
            proof_conn,
            proof_client,
            proof_consensus,
            Duration::new(100, 0),
        );

        // insert a TryOpen connection
        let conn_key = connection_key(&get_connection_id());
        let conn = data.connection();
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(conn_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_ack_connection() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an Init connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Init);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");
        // update the connection to Open
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();

        // prepare data
        let height = Height::new(1, 10);
        let header = MockHeader {
            height,
            timestamp: Timestamp::now(),
        };
        let client_state = MockClientState(header).wrap_any();
        let counterparty = get_conn_counterparty();
        let proof_conn = CommitmentProofBytes::from(vec![0]);
        let proof_client = CommitmentProofBytes::from(vec![0]);
        let proof_consensus = CommitmentProofBytes::from(vec![0]);
        let tx_code = vec![];
        let data = ConnectionOpenAckData::new(
            get_connection_id(),
            counterparty.connection_id().unwrap().clone(),
            client_state,
            height,
            proof_conn,
            proof_client,
            proof_consensus,
            Version::default(),
        );

        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(conn_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_confirm_connection() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert a TryOpen connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::TryOpen);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");
        // update the connection to Open
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();

        // prepare data
        let height = Height::new(1, 10);
        let proof_conn = CommitmentProofBytes::from(vec![0]);
        let proof_client = CommitmentProofBytes::from(vec![0]);
        let proof_consensus = CommitmentProofBytes::from(vec![0]);
        let tx_code = vec![];
        let data = ConnectionOpenConfirmData::new(
            get_connection_id(),
            height,
            proof_conn,
            proof_client,
            proof_consensus,
        );

        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(conn_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_init_channel() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let data = ChannelOpenInitData::new(
            get_port_id(),
            Order::Ordered,
            get_channel_counterparty(),
            vec![get_connection_id()],
            Order::Ordered.to_string(),
        );

        // insert an Init channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = data.channel();
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(channel_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_try_channel() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opend connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let proof_channel = CommitmentProofBytes::from(vec![0]);
        let proof_client = CommitmentProofBytes::from(vec![0]);
        let proof_consensus = CommitmentProofBytes::from(vec![0]);
        let data = ChannelOpenTryData::new(
            get_port_id(),
            Order::Ordered,
            get_channel_counterparty(),
            vec![get_connection_id()],
            Order::Ordered.to_string(),
            Order::Ordered.to_string(),
            Height::new(1, 10),
            proof_channel,
            proof_client,
            proof_consensus,
        );

        // insert a TryOpen channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = data.channel();
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(channel_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_ack_channel() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opend connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an Init channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Init, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let proof_channel = CommitmentProofBytes::from(vec![0]);
        let proof_client = CommitmentProofBytes::from(vec![0]);
        let proof_consensus = CommitmentProofBytes::from(vec![0]);
        let data = ChannelOpenAckData::new(
            get_port_id(),
            get_channel_id(),
            get_channel_counterparty().channel_id().unwrap().clone(),
            Order::Ordered.to_string(),
            Height::new(1, 10),
            proof_channel,
            proof_client,
            proof_consensus,
        );

        // update the channel to Open
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(channel_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_confirm_channel() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opend connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert a TryOpen channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::TryOpen, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let proof_channel = CommitmentProofBytes::from(vec![0]);
        let proof_client = CommitmentProofBytes::from(vec![0]);
        let proof_consensus = CommitmentProofBytes::from(vec![0]);
        let data = ChannelOpenConfirmData::new(
            get_port_id(),
            get_channel_id(),
            Height::new(1, 10),
            proof_channel,
            proof_client,
            proof_consensus,
        );

        // update the channel to Open
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(channel_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_port() {
        let (storage, mut write_log) = insert_init_states();
        // insert a port
        set_port(&mut write_log, 0);
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(port_key(&get_port_id()));

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_capability() {
        let (storage, mut write_log) = insert_init_states();
        // insert a port
        let index = 0;
        set_port(&mut write_log, index);
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        let cap_key = capability_key(index);
        keys_changed.insert(cap_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_seq_send() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let counterparty = get_channel_counterparty();
        let timestamp = Utc::now() + chrono::Duration::seconds(100);
        let timeout_timestamp = Timestamp::from_datetime(timestamp);
        let data = PacketSendData::new(
            get_port_id(),
            get_channel_id(),
            counterparty.port_id().clone(),
            counterparty.channel_id().unwrap().clone(),
            vec![0],
            Height::new(1, 100),
            timeout_timestamp,
        );

        // get and increment the nextSequenceSend
        let seq_key = next_sequence_send_key(&get_port_channel_id());
        let sequence = get_next_seq(&storage, &seq_key);
        increment_seq(&mut write_log, &seq_key, sequence);
        // make a packet
        let packet = data.packet(sequence);
        // insert a commitment
        let commitment = hash(&packet);
        let key = commitment_key(&get_port_id(), &get_channel_id(), sequence);
        write_log
            .write(&key, commitment.try_to_vec().expect("encoding failed"))
            .expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(seq_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_seq_recv() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // get and increment the nextSequenceRecv
        let seq_key = next_sequence_recv_key(&get_port_channel_id());
        let sequence = get_next_seq(&storage, &seq_key);
        increment_seq(&mut write_log, &seq_key, sequence);
        // make a packet and data
        let counterparty = get_channel_counterparty();
        let timestamp = Utc::now() + chrono::Duration::seconds(100);
        let timeout_timestamp = Timestamp::from_datetime(timestamp);
        let packet = Packet {
            sequence,
            source_port: counterparty.port_id().clone(),
            source_channel: counterparty.channel_id().unwrap().clone(),
            destination_port: get_port_id(),
            destination_channel: get_channel_id(),
            data: vec![0],
            timeout_height: Height::new(1, 100),
            timeout_timestamp,
        };
        let proof_packet = CommitmentProofBytes::from(vec![0]);
        let data =
            PacketReceiptData::new(packet, Height::new(1, 10), proof_packet);

        // insert a receipt and an ack
        let key = receipt_key(&get_port_id(), &get_channel_id(), sequence);
        write_log
            .write(&key, 0_u64.try_to_vec().unwrap())
            .expect("write failed");
        let key = ack_key(&get_port_id(), &get_channel_id(), sequence);
        write_log
            .write(&key, "test_ack".to_owned().try_to_vec().unwrap())
            .expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(seq_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_seq_ack() {
        let (mut storage, mut write_log) = insert_init_states();
        // get the nextSequenceAck
        let seq_key = next_sequence_ack_key(&get_port_channel_id());
        let sequence = get_next_seq(&storage, &seq_key);
        // make a packet
        let counterparty = get_channel_counterparty();
        let timestamp = Utc::now() + chrono::Duration::seconds(100);
        let timeout_timestamp = Timestamp::from_datetime(timestamp);
        let packet = Packet {
            sequence,
            source_port: get_port_id(),
            source_channel: get_channel_id(),
            destination_port: counterparty.port_id().clone(),
            destination_channel: counterparty.channel_id().unwrap().clone(),
            data: vec![0],
            timeout_height: Height::new(1, 100),
            timeout_timestamp,
        };
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        // insert a commitment
        let commitment = hash(&packet);
        let commitment_key =
            commitment_key(&get_port_id(), &get_channel_id(), sequence);
        write_log
            .write(
                &commitment_key,
                commitment.try_to_vec().expect("encoding failed"),
            )
            .expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let ack = "test_ack".try_to_vec().expect("encoding failed");
        let proof_packet = CommitmentProofBytes::from(vec![0]);
        let data =
            PacketAckData::new(packet, ack, Height::new(1, 10), proof_packet);

        // increment the nextSequenceAck
        increment_seq(&mut write_log, &seq_key, sequence);
        // delete the commitment
        write_log.delete(&commitment_key).expect("delete failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(seq_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_commitment() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let counterparty = get_channel_counterparty();
        let timestamp = Utc::now() + chrono::Duration::seconds(100);
        let timeout_timestamp = Timestamp::from_datetime(timestamp);
        let data = PacketSendData::new(
            get_port_id(),
            get_channel_id(),
            counterparty.port_id().clone(),
            counterparty.channel_id().unwrap().clone(),
            vec![0],
            Height::new(1, 100),
            timeout_timestamp,
        );

        // make a packet
        let seq_key = next_sequence_send_key(&get_port_channel_id());
        let sequence = get_next_seq(&storage, &seq_key);
        let packet = data.packet(sequence);
        // insert a commitment
        let commitment = hash(&packet);
        let commitment_key = commitment_key(
            &packet.source_port,
            &packet.source_channel,
            sequence,
        );
        write_log
            .write(
                &commitment_key,
                commitment.try_to_vec().expect("encoding failed"),
            )
            .expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(commitment_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_receipt() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.try_to_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.try_to_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // make a packet and data
        let counterparty = get_channel_counterparty();
        let timestamp = Utc::now() + chrono::Duration::seconds(100);
        let timeout_timestamp = Timestamp::from_datetime(timestamp);
        let packet = Packet {
            sequence: Sequence::from(1),
            source_port: counterparty.port_id().clone(),
            source_channel: counterparty.channel_id().unwrap().clone(),
            destination_port: get_port_id(),
            destination_channel: get_channel_id(),
            data: vec![0],
            timeout_height: Height::new(1, 100),
            timeout_timestamp,
        };
        let proof_packet = CommitmentProofBytes::from(vec![0]);
        let data =
            PacketReceiptData::new(packet, Height::new(1, 10), proof_packet);

        // insert a receipt and an ack
        let receipt_key = receipt_key(
            &data.packet.destination_port,
            &data.packet.destination_channel,
            data.packet.sequence,
        );
        write_log
            .write(&receipt_key, 0_u64.try_to_vec().unwrap())
            .expect("write failed");
        let ack_key = ack_key(
            &data.packet.destination_port,
            &data.packet.destination_channel,
            data.packet.sequence,
        );
        write_log
            .write(&ack_key, "test_ack".to_owned().try_to_vec().unwrap())
            .expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = data.try_to_vec().expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(receipt_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_ack() {
        let (storage, mut write_log) = insert_init_states();

        // insert a receipt and an ack
        let receipt_key =
            receipt_key(&get_port_id(), &get_channel_id(), Sequence::from(1));
        write_log
            .write(&receipt_key, 0_u64.try_to_vec().unwrap())
            .expect("write failed");
        let ack_key =
            ack_key(&get_port_id(), &get_channel_id(), Sequence::from(1));
        write_log
            .write(&ack_key, "test_ack".to_owned().try_to_vec().unwrap())
            .expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(ack_key);

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }
}
