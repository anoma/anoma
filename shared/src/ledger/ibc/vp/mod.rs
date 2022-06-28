//! IBC integration as a native validity predicate

mod channel;
mod client;
mod connection;
mod packet;
mod port;
mod sequence;
mod token;

use std::collections::{BTreeSet, HashSet};

use borsh::BorshDeserialize;
use thiserror::Error;
pub use token::{Error as IbcTokenError, IbcToken};

use super::storage::{client_id, ibc_prefix, is_client_counter_key, IbcPrefix};
use crate::ibc::core::ics02_client::context::ClientReader;
use crate::ibc::events::IbcEvent;
use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::proto::SignedTxData;
use crate::types::address::{Address, InternalAddress};
use crate::types::ibc::IbcEvent as WrappedIbcEvent;
use crate::types::storage::Key;
use crate::vm::WasmCacheAccess;

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
    #[error("IBC event error: {0}")]
    IbcEvent(String),
    #[error("Decoding transaction data error: {0}")]
    TxDataDecoding(std::io::Error),
    #[error("IBC message is required as transaction data")]
    NoTxData,
}

/// IBC functions result
pub type Result<T> = std::result::Result<T, Error>;

/// IBC VP
pub struct Ibc<'a, DB, H, CA>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H, CA>,
}

impl<'a, DB, H, CA> NativeVp for Ibc<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::Ibc;

    fn validate_tx(
        &self,
        tx_data: &[u8],
        keys_changed: &BTreeSet<Key>,
        _verifiers: &BTreeSet<Address>,
    ) -> Result<bool> {
        let signed = SignedTxData::try_from_slice(tx_data)
            .map_err(Error::TxDataDecoding)?;
        let tx_data = &signed.data.ok_or(Error::NoTxData)?;
        let mut clients = HashSet::new();

        for key in keys_changed {
            if let Some(ibc_prefix) = ibc_prefix(key) {
                match ibc_prefix {
                    IbcPrefix::Client => {
                        if is_client_counter_key(key) {
                            let counter =
                                self.client_counter().map_err(|_| {
                                    Error::CounterError(
                                        "The client counter doesn't exist"
                                            .to_owned(),
                                    )
                                })?;
                            if self.client_counter_pre()? >= counter {
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
                    IbcPrefix::Channel => {
                        self.validate_channel(key, tx_data)?
                    }
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
                    IbcPrefix::Receipt => {
                        self.validate_receipt(key, tx_data)?
                    }
                    IbcPrefix::Ack => self.validate_ack(key)?,
                    IbcPrefix::Event => {}
                    IbcPrefix::Unknown => {
                        return Err(Error::KeyError(format!(
                            "Invalid IBC-related key: {}",
                            key
                        )));
                    }
                };
            }
        }

        Ok(true)
    }
}

#[derive(Debug, PartialEq, Eq)]
enum StateChange {
    Created,
    Updated,
    Deleted,
    NotExists,
}

impl<'a, DB, H, CA> Ibc<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
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
            Ok(Some(value)) => {
                // As ibc-go, u64 like a counter is encoded with big-endian
                let counter: [u8; 8] = value.try_into().map_err(|_| {
                    Error::CounterError(
                        "Encoding the counter failed".to_string(),
                    )
                })?;
                Ok(u64::from_be_bytes(counter))
            }
            Ok(None) => {
                Err(Error::CounterError("The counter doesn't exist".to_owned()))
            }
            Err(e) => Err(Error::CounterError(format!(
                "Reading the counter failed: {}",
                e
            ))),
        }
    }

    fn read_counter(&self, key: &Key) -> Result<u64> {
        match self.ctx.read_post(key) {
            Ok(Some(value)) => {
                // As ibc-go, u64 like a counter is encoded with big-endian
                let counter: [u8; 8] = value.try_into().map_err(|_| {
                    Error::CounterError(
                        "Encoding the counter failed".to_string(),
                    )
                })?;
                Ok(u64::from_be_bytes(counter))
            }
            Ok(None) => {
                Err(Error::CounterError("The counter doesn't exist".to_owned()))
            }
            Err(e) => Err(Error::CounterError(format!(
                "Reading the counter failed: {}",
                e
            ))),
        }
    }

    fn check_emitted_event(&self, expected_event: IbcEvent) -> Result<()> {
        match self.ctx.write_log.get_ibc_event() {
            Some(event) => {
                let expected = WrappedIbcEvent::try_from(expected_event)
                    .map_err(|e| Error::IbcEvent(e.to_string()))?;
                if *event == expected {
                    Ok(())
                } else {
                    Err(Error::IbcEvent(format!(
                        "The IBC event is invalid: Event {}",
                        event
                    )))
                }
            }
            None => {
                Err(Error::IbcEvent("No event has been emitted".to_owned()))
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

/// A dummy header used for testing
#[cfg(any(feature = "test", feature = "testing"))]
pub fn get_dummy_header() -> crate::types::storage::Header {
    use crate::tendermint::time::Time as TmTime;
    crate::types::storage::Header {
        hash: crate::types::hash::Hash([0; 32]),
        time: TmTime::now().try_into().unwrap(),
        next_validators_hash: crate::types::hash::Hash([0; 32]),
    }
}

#[cfg(test)]
mod tests {
    use core::time::Duration;
    use std::convert::TryFrom;
    use std::str::FromStr;

    use crate::ibc::applications::ics20_fungible_token_transfer::msgs::transfer::MsgTransfer;
    use crate::ibc::core::ics02_client::client_consensus::ConsensusState;
    use crate::ibc::core::ics02_client::client_state::ClientState;
    use crate::ibc::core::ics02_client::client_type::ClientType;
    use crate::ibc::core::ics02_client::header::Header;
    use crate::ibc::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
    use crate::ibc::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
    use crate::ibc::core::ics03_connection::connection::{
        ConnectionEnd, Counterparty as ConnCounterparty, State as ConnState,
    };
    use crate::ibc::core::ics03_connection::msgs::conn_open_ack::MsgConnectionOpenAck;
    use crate::ibc::core::ics03_connection::msgs::conn_open_confirm::MsgConnectionOpenConfirm;
    use crate::ibc::core::ics03_connection::msgs::conn_open_init::MsgConnectionOpenInit;
    use crate::ibc::core::ics03_connection::msgs::conn_open_try::MsgConnectionOpenTry;
    use crate::ibc::core::ics03_connection::version::Version as ConnVersion;
    use crate::ibc::core::ics04_channel::channel::{
        ChannelEnd, Counterparty as ChanCounterparty, Order, State as ChanState,
    };
    use crate::ibc::core::ics04_channel::msgs::acknowledgement::MsgAcknowledgement;
    use crate::ibc::core::ics04_channel::msgs::chan_open_ack::MsgChannelOpenAck;
    use crate::ibc::core::ics04_channel::msgs::chan_open_confirm::MsgChannelOpenConfirm;
    use crate::ibc::core::ics04_channel::msgs::chan_open_init::MsgChannelOpenInit;
    use crate::ibc::core::ics04_channel::msgs::chan_open_try::MsgChannelOpenTry;
    use crate::ibc::core::ics04_channel::msgs::recv_packet::MsgRecvPacket;
    use crate::ibc::core::ics04_channel::packet::{Packet, Sequence};
    use crate::ibc::core::ics04_channel::Version as ChanVersion;
    use crate::ibc::core::ics23_commitment::commitment::CommitmentProofBytes;
    use crate::ibc::core::ics24_host::identifier::{
        ChannelId, ClientId, ConnectionId, PortChannelId, PortId,
    };
    use crate::ibc::mock::client_state::{MockClientState, MockConsensusState};
    use crate::ibc::mock::header::MockHeader;
    use crate::ibc::proofs::{ConsensusProof, Proofs};
    use crate::ibc::signer::Signer;
    use crate::ibc::timestamp::Timestamp;
    use crate::ibc::tx_msg::Msg;
    use crate::ibc::Height;
    use crate::ibc_proto::cosmos::base::v1beta1::Coin;
    use prost::Message;
    use sha2::Digest;
    use crate::tendermint_proto::Protobuf;
    use crate::tendermint::time::Time as TmTime;

    use super::get_dummy_header;
    use super::super::handler::{
        commitment_prefix, init_connection, make_create_client_event,
        make_open_ack_channel_event, make_open_ack_connection_event,
        make_open_confirm_channel_event, make_open_confirm_connection_event,
        make_open_init_channel_event, make_open_init_connection_event,
        make_open_try_channel_event, make_open_try_connection_event,
        make_send_packet_event, make_update_client_event, packet_from_message,
        try_connection,
    };
    use super::super::storage::{
        ack_key, capability_key, channel_key, client_state_key,
        client_type_key, client_update_height_key, client_update_timestamp_key,
        commitment_key, connection_key, consensus_state_key,
        next_sequence_ack_key, next_sequence_recv_key, next_sequence_send_key,
        port_key, receipt_key,
    };
    use super::*;
    use crate::types::key::testing::keypair_1;
    use crate::ledger::gas::VpGasMeter;
    use crate::ledger::storage::testing::TestStorage;
    use crate::ledger::storage::write_log::WriteLog;
    use crate::proto::Tx;
    use crate::types::ibc::data::{PacketAck, PacketReceipt};
    use crate::vm::wasm;
    use crate::types::storage::{BlockHash, BlockHeight};

    fn get_client_id() -> ClientId {
        ClientId::from_str("test_client").expect("Creating a client ID failed")
    }

    fn insert_init_states() -> (TestStorage, WriteLog) {
        let mut storage = TestStorage::default();
        let mut write_log = WriteLog::default();

        // initialize the storage
        super::super::init_genesis_storage(&mut storage);
        // set a dummy header
        storage
            .set_header(get_dummy_header())
            .expect("Setting a dummy header shouldn't fail");
        storage
            .begin_block(BlockHash::default(), BlockHeight(1))
            .unwrap();

        // insert a mock client type
        let client_id = get_client_id();
        let client_type_key = client_type_key(&client_id);
        let client_type = ClientType::Mock.as_str().as_bytes().to_vec();
        write_log
            .write(&client_type_key, client_type)
            .expect("write failed");
        // insert a mock client state
        let client_state_key = client_state_key(&get_client_id());
        let height = Height::new(0, 1);
        let header = MockHeader {
            height,
            timestamp: Timestamp::now(),
        };
        let client_state = MockClientState::new(header).wrap_any();
        let bytes = client_state.encode_vec().expect("encoding failed");
        write_log
            .write(&client_state_key, bytes)
            .expect("write failed");
        // insert a mock consensus state
        let consensus_key = consensus_state_key(&client_id, height);
        let consensus_state = MockConsensusState::new(header).wrap_any();
        let bytes = consensus_state.encode_vec().expect("encoding failed");
        write_log
            .write(&consensus_key, bytes)
            .expect("write failed");
        // insert update time and height
        let client_update_time_key = client_update_timestamp_key(&client_id);
        let bytes = TmTime::now().encode_vec().expect("encoding failed");
        write_log
            .write(&client_update_time_key, bytes)
            .expect("write failed");
        let client_update_height_key = client_update_height_key(&client_id);
        let host_height = Height::new(10, 100);
        write_log
            .write(
                &client_update_height_key,
                host_height.encode_vec().expect("encoding failed"),
            )
            .expect("write failed");
        write_log.commit_tx();

        (storage, write_log)
    }

    fn get_connection_id() -> ConnectionId {
        ConnectionId::new(0)
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
            vec![ConnVersion::default()],
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
            commitment_prefix(),
        )
    }

    fn get_channel(channel_state: ChanState, order: Order) -> ChannelEnd {
        ChannelEnd::new(
            channel_state,
            order,
            get_channel_counterparty(),
            vec![get_connection_id()],
            ChanVersion::ics20(),
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
            .write(&port_key, index.to_be_bytes().to_vec())
            .expect("write failed");
        // insert to the reverse map
        let cap_key = capability_key(index);
        let port_id = get_port_id();
        let bytes = port_id.as_str().as_bytes().to_vec();
        write_log.write(&cap_key, bytes).expect("write failed");
    }

    fn get_next_seq(storage: &TestStorage, key: &Key) -> Sequence {
        let (val, _) = storage.read(key).expect("read failed");
        match val {
            Some(v) => {
                // IBC related data is encoded without borsh
                let index: [u8; 8] = v.try_into().expect("decoding failed");
                let index = u64::from_be_bytes(index);
                Sequence::from(index)
            }
            // The sequence has not been used yet
            None => Sequence::from(1),
        }
    }

    fn increment_seq(write_log: &mut WriteLog, key: &Key, seq: Sequence) {
        let seq_num = u64::from(seq.increment());
        write_log
            .write(key, seq_num.to_be_bytes().to_vec())
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
        let (storage, mut write_log) = insert_init_states();

        let height = Height::new(0, 1);
        let header = MockHeader {
            height,
            timestamp: Timestamp::now(),
        };
        let client_state = MockClientState::new(header).wrap_any();
        let consensus_state = MockConsensusState::new(header).wrap_any();
        let msg = MsgCreateAnyClient {
            client_state,
            consensus_state,
            signer: Signer::new("account0"),
        };
        let event = make_create_client_event(&get_client_id(), &msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        let client_state_key = client_state_key(&get_client_id());
        keys_changed.insert(client_state_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_create_client_fail() {
        let storage = TestStorage::default();
        let write_log = WriteLog::default();
        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        let client_state_key = client_state_key(&get_client_id());
        keys_changed.insert(client_state_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        // this should fail because no state is stored
        let result = ibc
            .validate_tx(tx.data.as_ref().unwrap(), &keys_changed, &verifiers)
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
        let msg = MsgUpdateAnyClient {
            client_id: client_id.clone(),
            header: header.wrap_any(),
            signer: Signer::new("account0"),
        };
        let client_state = MockClientState::new(header).wrap_any();
        let bytes = client_state.encode_vec().expect("encoding failed");
        write_log
            .write(&client_state_key, bytes)
            .expect("write failed");
        let consensus_key = consensus_state_key(&client_id, height);
        let consensus_state = MockConsensusState::new(header).wrap_any();
        let bytes = consensus_state.encode_vec().expect("encoding failed");
        write_log
            .write(&consensus_key, bytes)
            .expect("write failed");
        let event = make_update_client_event(&client_id, &msg);
        write_log.set_ibc_event(event.try_into().unwrap());
        // update time and height for this updating
        let key = client_update_timestamp_key(&client_id);
        write_log
            .write(&key, TmTime::now().encode_vec().expect("encoding failed"))
            .expect("write failed");
        let key = client_update_height_key(&client_id);
        write_log
            .write(
                &key,
                Height::new(10, 101).encode_vec().expect("encoding failed"),
            )
            .expect("write failed");

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(client_state_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_init_connection() {
        let (mut storage, mut write_log) = insert_init_states();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare a message
        let msg = MsgConnectionOpenInit {
            client_id: get_client_id(),
            counterparty: get_conn_counterparty(),
            version: ConnVersion::default(),
            delay_period: Duration::new(100, 0),
            signer: Signer::new("account0"),
        };

        // insert an INIT connection
        let conn_id = get_connection_id();
        let conn_key = connection_key(&conn_id);
        let conn = init_connection(&msg);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        let event = make_open_init_connection_event(&conn_id, &msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(conn_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_init_connection_fail() {
        let storage = TestStorage::default();
        let mut write_log = WriteLog::default();

        // prepare data
        let msg = MsgConnectionOpenInit {
            client_id: get_client_id(),
            counterparty: get_conn_counterparty(),
            version: ConnVersion::default(),
            delay_period: Duration::new(100, 0),
            signer: Signer::new("account0"),
        };

        // insert an Init connection
        let conn_key = connection_key(&get_connection_id());
        let conn = init_connection(&msg);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(conn_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        // this should fail because no client exists
        let result = ibc
            .validate_tx(tx.data.as_ref().unwrap(), &keys_changed, &verifiers)
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
        let height = Height::new(0, 1);
        let header = MockHeader {
            height,
            timestamp: Timestamp::now(),
        };
        let client_state = MockClientState::new(header).wrap_any();
        let proof_conn = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_client = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_consensus = ConsensusProof::new(
            CommitmentProofBytes::try_from(vec![0]).unwrap(),
            height,
        )
        .unwrap();
        let proofs = Proofs::new(
            proof_conn,
            Some(proof_client),
            Some(proof_consensus),
            None,
            Height::new(0, 1),
        )
        .unwrap();
        let msg = MsgConnectionOpenTry {
            previous_connection_id: None,
            client_id: get_client_id(),
            client_state: Some(client_state),
            counterparty: get_conn_counterparty(),
            counterparty_versions: vec![ConnVersion::default()],
            proofs,
            delay_period: Duration::new(100, 0),
            signer: Signer::new("account0"),
        };

        // insert a TryOpen connection
        let conn_id = get_connection_id();
        let conn_key = connection_key(&conn_id);
        let conn = try_connection(&msg);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        let event = make_open_try_connection_event(&conn_id, &msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(conn_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_ack_connection() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an Init connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Init);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");
        // update the connection to Open
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");

        // prepare data
        let height = Height::new(0, 1);
        let header = MockHeader {
            height,
            timestamp: Timestamp::now(),
        };
        let client_state = MockClientState::new(header).wrap_any();
        let counterparty = get_conn_counterparty();
        let proof_conn = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_client = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_consensus = ConsensusProof::new(
            CommitmentProofBytes::try_from(vec![0]).unwrap(),
            height,
        )
        .unwrap();
        let proofs = Proofs::new(
            proof_conn,
            Some(proof_client),
            Some(proof_consensus),
            None,
            Height::new(0, 1),
        )
        .unwrap();
        let tx_code = vec![];
        let msg = MsgConnectionOpenAck {
            connection_id: get_connection_id(),
            counterparty_connection_id: counterparty
                .connection_id()
                .unwrap()
                .clone(),
            client_state: Some(client_state),
            proofs,
            version: ConnVersion::default(),
            signer: Signer::new("account0"),
        };
        let event = make_open_ack_connection_event(&msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(conn_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_confirm_connection() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert a TryOpen connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::TryOpen);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");
        // update the connection to Open
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");

        // prepare data
        let height = Height::new(0, 1);
        let proof_conn = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_client = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_consensus = ConsensusProof::new(
            CommitmentProofBytes::try_from(vec![0]).unwrap(),
            height,
        )
        .unwrap();
        let proofs = Proofs::new(
            proof_conn,
            Some(proof_client),
            Some(proof_consensus),
            None,
            height,
        )
        .unwrap();
        let tx_code = vec![];
        let msg = MsgConnectionOpenConfirm {
            connection_id: get_connection_id(),
            proofs,
            signer: Signer::new("account0"),
        };
        let event = make_open_confirm_connection_event(&msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(conn_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_init_channel() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let channel = get_channel(ChanState::Init, Order::Ordered);
        let msg = MsgChannelOpenInit {
            port_id: get_port_id(),
            channel: channel.clone(),
            signer: Signer::new("account0"),
        };

        // insert an Init channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        let event = make_open_init_channel_event(&get_channel_id(), &msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(channel_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_try_channel() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opend connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let height = Height::new(0, 1);
        let proof_channel = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_client = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_consensus = ConsensusProof::new(
            CommitmentProofBytes::try_from(vec![0]).unwrap(),
            height,
        )
        .unwrap();
        let proofs = Proofs::new(
            proof_channel,
            Some(proof_client),
            Some(proof_consensus),
            None,
            height,
        )
        .unwrap();
        let channel = get_channel(ChanState::TryOpen, Order::Ordered);
        let msg = MsgChannelOpenTry {
            port_id: get_port_id(),
            previous_channel_id: None,
            channel: channel.clone(),
            counterparty_version: ChanVersion::ics20(),
            proofs,
            signer: Signer::new("account0"),
        };

        // insert a TryOpen channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        let event = make_open_try_channel_event(&get_channel_id(), &msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(channel_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_ack_channel() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opend connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an Init channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Init, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let height = Height::new(0, 1);
        let proof_channel = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_client = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_consensus = ConsensusProof::new(
            CommitmentProofBytes::try_from(vec![0]).unwrap(),
            height,
        )
        .unwrap();
        let proofs = Proofs::new(
            proof_channel,
            Some(proof_client),
            Some(proof_consensus),
            None,
            height,
        )
        .unwrap();
        let msg = MsgChannelOpenAck {
            port_id: get_port_id(),
            channel_id: get_channel_id(),
            counterparty_channel_id: get_channel_counterparty()
                .channel_id()
                .unwrap()
                .clone(),
            counterparty_version: ChanVersion::ics20(),
            proofs,
            signer: Signer::new("account0"),
        };

        // update the channel to Open
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        let event = make_open_ack_channel_event(&msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(channel_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_confirm_channel() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opend connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert a TryOpen channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::TryOpen, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let height = Height::new(0, 1);
        let proof_channel = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_client = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proof_consensus = ConsensusProof::new(
            CommitmentProofBytes::try_from(vec![0]).unwrap(),
            height,
        )
        .unwrap();
        let proofs = Proofs::new(
            proof_channel,
            Some(proof_client),
            Some(proof_consensus),
            None,
            height,
        )
        .unwrap();
        let msg = MsgChannelOpenConfirm {
            port_id: get_port_id(),
            channel_id: get_channel_id(),
            proofs,
            signer: Signer::new("account0"),
        };

        // update the channel to Open
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        let event = make_open_confirm_channel_event(&msg);
        write_log.set_ibc_event(event.try_into().unwrap());

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(channel_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
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
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(port_key(&get_port_id()));

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
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
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        let cap_key = capability_key(index);
        keys_changed.insert(cap_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_seq_send() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare a message
        let timeout_timestamp =
            (Timestamp::now() + Duration::from_secs(100)).unwrap();
        let msg = MsgTransfer {
            source_port: get_port_id(),
            source_channel: get_channel_id(),
            token: Some(Coin {
                denom: "XAN".to_string(),
                amount: 100u64.to_string(),
            }),
            sender: Signer::new("sender"),
            receiver: Signer::new("receiver"),
            timeout_height: Height::new(0, 100),
            timeout_timestamp,
        };

        // get and increment the nextSequenceSend
        let seq_key = next_sequence_send_key(&get_port_channel_id());
        let sequence = get_next_seq(&storage, &seq_key);
        increment_seq(&mut write_log, &seq_key, sequence);
        // make a packet
        let counterparty = get_channel_counterparty();
        let packet = packet_from_message(&msg, sequence, &counterparty);
        // insert a commitment
        let commitment = hash(&packet);
        let mut commitment_bytes = vec![];
        commitment
            .encode(&mut commitment_bytes)
            .expect("encoding shouldn't fail");
        let key = commitment_key(&get_port_id(), &get_channel_id(), sequence);
        write_log
            .write(&key, commitment_bytes)
            .expect("write failed");

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(seq_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_seq_recv() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // get and increment the nextSequenceRecv
        let seq_key = next_sequence_recv_key(&get_port_channel_id());
        let sequence = get_next_seq(&storage, &seq_key);
        increment_seq(&mut write_log, &seq_key, sequence);
        // make a packet and data
        let counterparty = get_channel_counterparty();
        let timeout_timestamp =
            (Timestamp::now() + Duration::from_secs(100)).unwrap();
        let packet = Packet {
            sequence,
            source_port: counterparty.port_id().clone(),
            source_channel: counterparty.channel_id().unwrap().clone(),
            destination_port: get_port_id(),
            destination_channel: get_channel_id(),
            data: vec![0],
            timeout_height: Height::new(0, 100),
            timeout_timestamp,
        };
        let proof_packet = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proofs =
            Proofs::new(proof_packet, None, None, None, Height::new(0, 1))
                .unwrap();
        let msg = MsgRecvPacket {
            packet,
            proofs,
            signer: Signer::new("account0"),
        };

        // insert a receipt and an ack
        let key = receipt_key(&get_port_id(), &get_channel_id(), sequence);
        write_log
            .write(&key, PacketReceipt::default().as_bytes().to_vec())
            .expect("write failed");
        let key = ack_key(&get_port_id(), &get_channel_id(), sequence);
        let ack = PacketAck::default().encode_to_vec();
        write_log.write(&key, ack).expect("write failed");

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(seq_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
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
        let timeout_timestamp =
            (Timestamp::now() + core::time::Duration::from_secs(100)).unwrap();
        let packet = Packet {
            sequence,
            source_port: get_port_id(),
            source_channel: get_channel_id(),
            destination_port: counterparty.port_id().clone(),
            destination_channel: counterparty.channel_id().unwrap().clone(),
            data: vec![0],
            timeout_height: Height::new(0, 100),
            timeout_timestamp,
        };
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        // insert a commitment
        let commitment = hash(&packet);
        let commitment_key =
            commitment_key(&get_port_id(), &get_channel_id(), sequence);
        write_log
            .write(&commitment_key, commitment.as_bytes().to_vec())
            .expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare data
        let ack = PacketAck::default().encode_to_vec();
        let proof_packet = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proofs =
            Proofs::new(proof_packet, None, None, None, Height::new(0, 1))
                .unwrap();
        let msg = MsgAcknowledgement {
            packet,
            acknowledgement: ack,
            proofs,
            signer: Signer::new("account0"),
        };

        // increment the nextSequenceAck
        increment_seq(&mut write_log, &seq_key, sequence);
        // delete the commitment
        write_log.delete(&commitment_key).expect("delete failed");

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(seq_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_commitment() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // prepare a message
        let timeout_timestamp =
            (Timestamp::now() + Duration::from_secs(100)).unwrap();
        let msg = MsgTransfer {
            source_port: get_port_id(),
            source_channel: get_channel_id(),
            token: Some(Coin {
                denom: "XAN".to_string(),
                amount: 100u64.to_string(),
            }),
            sender: Signer::new("sender"),
            receiver: Signer::new("receiver"),
            timeout_height: Height::new(0, 100),
            timeout_timestamp,
        };

        // make a packet
        let seq_key = next_sequence_send_key(&get_port_channel_id());
        let sequence = get_next_seq(&storage, &seq_key);
        let counterparty = get_channel_counterparty();
        let packet = packet_from_message(&msg, sequence, &counterparty);
        // insert a commitment
        let commitment = hash(&packet);
        let commitment_key = commitment_key(
            &packet.source_port,
            &packet.source_channel,
            sequence,
        );
        let mut commitment_bytes = vec![];
        commitment
            .encode(&mut commitment_bytes)
            .expect("encoding shouldn't fail");
        write_log
            .write(&commitment_key, commitment_bytes)
            .expect("write failed");
        let event = make_send_packet_event(packet);
        write_log.set_ibc_event(event.try_into().unwrap());

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(commitment_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }

    #[test]
    fn test_validate_receipt() {
        let (mut storage, mut write_log) = insert_init_states();
        // insert an opened connection
        let conn_key = connection_key(&get_connection_id());
        let conn = get_connection(ConnState::Open);
        let bytes = conn.encode_vec().expect("encoding failed");
        write_log.write(&conn_key, bytes).expect("write failed");
        // insert an opened channel
        set_port(&mut write_log, 0);
        let channel_key = channel_key(&get_port_channel_id());
        let channel = get_channel(ChanState::Open, Order::Ordered);
        let bytes = channel.encode_vec().expect("encoding failed");
        write_log.write(&channel_key, bytes).expect("write failed");
        write_log.commit_tx();
        write_log.commit_block(&mut storage).expect("commit failed");

        // make a packet and data
        let counterparty = get_channel_counterparty();
        let timeout_timestamp =
            (Timestamp::now() + Duration::from_secs(100)).unwrap();
        let packet = Packet {
            sequence: Sequence::from(1),
            source_port: counterparty.port_id().clone(),
            source_channel: counterparty.channel_id().unwrap().clone(),
            destination_port: get_port_id(),
            destination_channel: get_channel_id(),
            data: vec![0],
            timeout_height: Height::new(0, 100),
            timeout_timestamp,
        };
        let proof_packet = CommitmentProofBytes::try_from(vec![0]).unwrap();
        let proofs =
            Proofs::new(proof_packet, None, None, None, Height::new(0, 1))
                .unwrap();
        let msg = MsgRecvPacket {
            packet,
            proofs,
            signer: Signer::new("account0"),
        };

        // insert a receipt and an ack
        let receipt_key = receipt_key(
            &msg.packet.destination_port,
            &msg.packet.destination_channel,
            msg.packet.sequence,
        );
        write_log
            .write(&receipt_key, PacketReceipt::default().as_bytes().to_vec())
            .expect("write failed");
        let ack_key = ack_key(
            &msg.packet.destination_port,
            &msg.packet.destination_channel,
            msg.packet.sequence,
        );
        let ack = PacketAck::default().encode_to_vec();
        write_log.write(&ack_key, ack).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(receipt_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
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
            .write(&receipt_key, PacketReceipt::default().as_bytes().to_vec())
            .expect("write failed");
        let ack_key =
            ack_key(&get_port_id(), &get_channel_id(), Sequence::from(1));
        let ack = PacketAck::default().encode_to_vec();
        write_log.write(&ack_key, ack).expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair_1());
        let gas_meter = VpGasMeter::new(0);
        let (vp_wasm_cache, _vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter, vp_wasm_cache);

        let mut keys_changed = BTreeSet::new();
        keys_changed.insert(ack_key);

        let verifiers = BTreeSet::new();

        let ibc = Ibc { ctx };
        assert!(
            ibc.validate_tx(
                tx.data.as_ref().unwrap(),
                &keys_changed,
                &verifiers
            )
            .expect("validation failed")
        );
    }
}
