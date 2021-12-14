use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::str::FromStr;
use std::time::Duration;

use anoma::ledger::gas::VpGasMeter;
pub use anoma::ledger::ibc::storage::{
    ack_key, capability_index_key, capability_key, channel_counter_key,
    channel_key, client_counter_key, client_state_key, client_type_key,
    commitment_key, connection_counter_key, connection_key,
    consensus_state_key, next_sequence_ack_key, next_sequence_recv_key,
    next_sequence_send_key, port_key, receipt_key,
};
use anoma::ledger::ibc::Ibc;
use anoma::ledger::native_vp::{Ctx, NativeVp};
use anoma::ledger::storage::mockdb::MockDB;
use anoma::ledger::storage::testing::Sha256Hasher;
use anoma::proto::Tx;
use anoma::types::address::{Address, InternalAddress};
pub use anoma::types::ibc::*;
use anoma::types::storage::Key;
use anoma::vm::{wasm, WasmCacheRwAccess};
use anoma_vm_env::tx_prelude::BorshSerialize;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_consensus::ConsensusState;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_state::{AnyClientState, ClientState};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::header::Header;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::connection::Counterparty as ConnCounterparty;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::version::Version;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order,
};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::packet::{Packet, Sequence};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics23_commitment::commitment::CommitmentProofBytes;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortId,
};
#[cfg(not(feature = "ABCI"))]
use ibc::mock::client_state::{MockClientState, MockConsensusState};
#[cfg(not(feature = "ABCI"))]
use ibc::mock::header::MockHeader;
#[cfg(not(feature = "ABCI"))]
use ibc::timestamp::Timestamp;
#[cfg(not(feature = "ABCI"))]
use ibc::Height;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_consensus::ConsensusState;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_state::{AnyClientState, ClientState};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::header::Header;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::connection::Counterparty as ConnCounterparty;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::version::Version;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order,
};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::packet::{Packet, Sequence};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics23_commitment::commitment::CommitmentProofBytes;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortId,
};
#[cfg(feature = "ABCI")]
use ibc_abci::mock::client_state::{MockClientState, MockConsensusState};
#[cfg(feature = "ABCI")]
use ibc_abci::mock::header::MockHeader;
#[cfg(feature = "ABCI")]
use ibc_abci::timestamp::Timestamp;
#[cfg(feature = "ABCI")]
use ibc_abci::Height;
#[cfg(not(feature = "ABCI"))]
use ibc_proto::ibc::core::commitment::v1::MerkleProof;
#[cfg(feature = "ABCI")]
use ibc_proto_abci::ibc::core::commitment::v1::MerkleProof;
use tempfile::TempDir;
#[cfg(not(feature = "ABCI"))]
use tendermint::account::Id as TmAccountId;
#[cfg(not(feature = "ABCI"))]
use tendermint::block::header::{Header as TmHeader, Version as TmVersion};
#[cfg(not(feature = "ABCI"))]
use tendermint::block::Height as TmHeight;
#[cfg(not(feature = "ABCI"))]
use tendermint::chain::Id as TmChainId;
#[cfg(not(feature = "ABCI"))]
use tendermint::hash::{AppHash, Hash as TmHash};
#[cfg(not(feature = "ABCI"))]
use tendermint::time::Time as TmTime;
#[cfg(feature = "ABCI")]
use tendermint_stable::account::Id as TmAccountId;
#[cfg(feature = "ABCI")]
use tendermint_stable::block::header::{
    Header as TmHeader, Version as TmVersion,
};
#[cfg(feature = "ABCI")]
use tendermint_stable::block::Height as TmHeight;
#[cfg(feature = "ABCI")]
use tendermint_stable::chain::Id as TmChainId;
#[cfg(feature = "ABCI")]
use tendermint_stable::hash::{AppHash, Hash as TmHash};
#[cfg(feature = "ABCI")]
use tendermint_stable::time::Time as TmTime;

use crate::tx::TestTxEnv;

pub struct TestIbcVp<'a> {
    pub ibc: Ibc<'a, MockDB, Sha256Hasher, WasmCacheRwAccess>,
    pub keys_changed: HashSet<Key>,
}

impl<'a> TestIbcVp<'a> {
    pub fn validate(
        &self,
        tx_data: &[u8],
    ) -> std::result::Result<bool, anoma::ledger::ibc::Error> {
        self.ibc
            .validate_tx(tx_data, &self.keys_changed, &HashSet::new())
    }
}

/// Initialize IBC VP by running a transaction.
pub fn init_ibc_vp_from_tx<'a>(
    tx_env: &'a TestTxEnv,
    tx: &'a Tx,
) -> (TestIbcVp<'a>, TempDir) {
    let keys_changed = tx_env
        .write_log
        .verifiers_changed_keys(&HashSet::new())
        .get(&Address::Internal(InternalAddress::Ibc))
        .cloned()
        .expect("no IBC address");
    let (vp_wasm_cache, vp_cache_dir) =
        wasm::compilation_cache::common::testing::cache();

    let ctx = Ctx::new(
        &tx_env.storage,
        &tx_env.write_log,
        tx,
        VpGasMeter::new(0),
        vp_wasm_cache,
    );
    let ibc = Ibc { ctx };

    (TestIbcVp { ibc, keys_changed }, vp_cache_dir)
}

pub fn tm_dummy_header() -> TmHeader {
    TmHeader {
        version: TmVersion { block: 10, app: 0 },
        chain_id: TmChainId::try_from("test_chain".to_owned())
            .expect("Creating an TmChainId shouldn't fail"),
        height: TmHeight::try_from(10_u64)
            .expect("Creating a height shouldn't fail"),
        time: TmTime::from_str("2021-11-01T18:14:32.024837Z")
            .expect("Setting the time shouldn't fail"),
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

pub fn prepare_client() -> (ClientId, AnyClientState, HashMap<Key, Vec<u8>>) {
    let mut writes = HashMap::new();

    let data = client_creation_data();
    // client state
    let client_state = data.client_state.clone();
    let client_id = data.client_id(0).expect("invalid client ID");
    let key = client_state_key(&client_id);
    let bytes = data.client_state.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // client type
    let key = client_type_key(&client_id);
    let client_type = client_state.client_type();
    let bytes = client_type.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // consensus state
    let height = client_state.latest_height();
    let key = consensus_state_key(&client_id, height);
    let bytes = data.consensus_state.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // client counter
    let key = client_counter_key();
    let bytes = 1_u64.try_to_vec().unwrap();
    writes.insert(key, bytes);

    (client_id, client_state, writes)
}

pub fn prepare_opened_connection(
    client_id: &ClientId,
) -> (ConnectionId, HashMap<Key, Vec<u8>>) {
    let mut writes = HashMap::new();

    let conn_id = connection_id(0);
    let key = connection_key(&conn_id);
    let data = connection_open_init_data(client_id.clone());
    let mut conn = data.connection();
    open_connection(&mut conn);
    let bytes = conn.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // connection counter
    let key = connection_counter_key();
    let bytes = 1_u64.try_to_vec().unwrap();
    writes.insert(key, bytes);

    (conn_id, writes)
}

pub fn prepare_opened_channel(
    conn_id: &ConnectionId,
) -> (PortId, ChannelId, HashMap<Key, Vec<u8>>) {
    let mut writes = HashMap::new();

    // port
    let port_id = port_id("test_port").expect("invalid port ID");
    let key = port_key(&port_id);
    writes.insert(key, 0_u64.try_to_vec().unwrap());
    // capability
    let key = capability_key(0);
    let bytes = port_id.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // channel
    let channel_id = channel_id(0);
    let port_channel_id = port_channel_id(port_id.clone(), channel_id.clone());
    let key = channel_key(&port_channel_id);
    let data = channel_open_init_data(port_id.clone(), conn_id.clone());
    let mut channel = data.channel();
    open_channel(&mut channel);
    let bytes = channel.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);

    (port_id, channel_id, writes)
}

pub fn client_creation_data() -> ClientCreationData {
    let height = Height::new(1, 10);
    let header = MockHeader {
        height,
        timestamp: Timestamp::now(),
    };
    let client_state = MockClientState(header).wrap_any();
    let consensus_state = MockConsensusState::new(header).wrap_any();
    ClientCreationData::new(client_state, consensus_state)
}

pub fn client_update_data(client_id: ClientId) -> ClientUpdateData {
    let height = Height::new(1, 11);
    let header = MockHeader {
        height,
        timestamp: Timestamp::now(),
    };
    ClientUpdateData::new(client_id, vec![header.wrap_any()])
}

pub fn client_upgrade_data(client_id: ClientId) -> ClientUpgradeData {
    let height = Height::new(0, 1);
    let header = MockHeader {
        height,
        timestamp: Timestamp::now(),
    };
    let client_state = MockClientState(header).wrap_any();
    let consensus_state = MockConsensusState::new(header).wrap_any();
    let client_proof =
        MerkleProof::try_from(CommitmentProofBytes::from(vec![])).unwrap();
    let consensus_proof =
        MerkleProof::try_from(CommitmentProofBytes::from(vec![])).unwrap();
    ClientUpgradeData::new(
        client_id,
        client_state,
        consensus_state,
        client_proof,
        consensus_proof,
    )
}

pub fn connection_open_init_data(
    client_id: ClientId,
) -> ConnectionOpenInitData {
    ConnectionOpenInitData::new(
        client_id,
        dummy_connection_counterparty(),
        Version::default(),
        Duration::new(100, 0),
    )
}

pub fn connection_open_try_data(
    client_id: ClientId,
    client_state: AnyClientState,
) -> ConnectionOpenTryData {
    ConnectionOpenTryData::new(
        client_id,
        client_state,
        dummy_connection_counterparty(),
        vec![Version::default()],
        Height::new(1, 10),
        vec![0].into(),
        vec![0].into(),
        vec![0].into(),
        Duration::new(100, 0),
    )
}

pub fn connection_open_ack_data(
    conn_id: ConnectionId,
    client_state: AnyClientState,
) -> ConnectionOpenAckData {
    let counterparty_id = ConnectionId::from_str("counterpart_test_connection")
        .expect("Creating a connection ID failed");
    ConnectionOpenAckData::new(
        conn_id,
        counterparty_id,
        client_state,
        Height::new(1, 10),
        vec![0].into(),
        vec![0].into(),
        vec![0].into(),
        Version::default(),
    )
}

pub fn connection_open_confirm_data(
    conn_id: ConnectionId,
) -> ConnectionOpenConfirmData {
    ConnectionOpenConfirmData::new(
        conn_id,
        Height::new(1, 10),
        vec![0].into(),
        vec![0].into(),
        vec![0].into(),
    )
}

fn dummy_connection_counterparty() -> ConnCounterparty {
    let counterpart_client_id = ClientId::from_str("counterpart_test_client")
        .expect("Creating a client ID failed");
    let counterpart_conn_id =
        ConnectionId::from_str("counterpart_test_connection")
            .expect("Creating a connection ID failed");
    connection_counterparty(counterpart_client_id, counterpart_conn_id)
}

pub fn channel_open_init_data(
    port_id: PortId,
    conn_id: ConnectionId,
) -> ChannelOpenInitData {
    ChannelOpenInitData::new(
        port_id,
        Order::Ordered,
        dummy_channel_counterparty(),
        vec![conn_id],
        Order::Ordered.to_string(),
    )
}

pub fn channel_open_try_data(
    port_id: PortId,
    conn_id: ConnectionId,
) -> ChannelOpenTryData {
    ChannelOpenTryData::new(
        port_id,
        Order::Ordered,
        dummy_channel_counterparty(),
        vec![conn_id],
        Order::Ordered.to_string(),
        Order::Ordered.to_string(),
        Height::new(1, 10),
        vec![0].into(),
        vec![0].into(),
        vec![0].into(),
    )
}

pub fn channel_open_ack_data(
    port_id: PortId,
    channel_id: ChannelId,
) -> ChannelOpenAckData {
    ChannelOpenAckData::new(
        port_id,
        channel_id,
        dummy_channel_counterparty().channel_id().unwrap().clone(),
        Order::Ordered.to_string(),
        Height::new(1, 10),
        vec![0].into(),
        vec![0].into(),
        vec![0].into(),
    )
}

pub fn channel_open_confirm_data(
    port_id: PortId,
    channel_id: ChannelId,
) -> ChannelOpenConfirmData {
    ChannelOpenConfirmData::new(
        port_id,
        channel_id,
        Height::new(1, 10),
        vec![0].into(),
        vec![0].into(),
        vec![0].into(),
    )
}

pub fn channel_close_init_data(
    port_id: PortId,
    channel_id: ChannelId,
) -> ChannelCloseInitData {
    ChannelCloseInitData::new(port_id, channel_id)
}

pub fn channel_close_confirm_data(
    port_id: PortId,
    channel_id: ChannelId,
) -> ChannelCloseConfirmData {
    ChannelCloseConfirmData::new(
        port_id,
        channel_id,
        Height::new(1, 10),
        vec![0].into(),
        vec![0].into(),
        vec![0].into(),
    )
}

fn dummy_channel_counterparty() -> ChanCounterparty {
    let counterpart_port_id = PortId::from_str("counterpart_test_port")
        .expect("Creating a port ID failed");
    let counterpart_channel_id =
        ChannelId::from_str("counterpart_test_channel")
            .expect("Creating a channel ID failed");
    channel_counterparty(counterpart_port_id, counterpart_channel_id)
}

pub fn unorder_channel(channel: &mut ChannelEnd) {
    channel.ordering = Order::Unordered;
}

pub fn packet_send_data(
    port_id: PortId,
    channel_id: ChannelId,
) -> PacketSendData {
    let counterparty = dummy_channel_counterparty();
    let timestamp = chrono::Utc::now() + chrono::Duration::seconds(100);
    let timeout_timestamp = Timestamp::from_datetime(timestamp);
    PacketSendData::new(
        port_id,
        channel_id,
        counterparty.port_id().clone(),
        counterparty.channel_id().unwrap().clone(),
        vec![0],
        Height::new(1, 10),
        timeout_timestamp,
    )
}

pub fn set_timeout_height(data: &mut PacketSendData) {
    data.timeout_height = Height::new(1, 1);
}

pub fn packet_receipt_data(packet: Packet) -> PacketReceiptData {
    PacketReceiptData::new(packet, Height::new(1, 10), vec![0].into())
}

pub fn packet_ack_data(packet: Packet) -> PacketAckData {
    PacketAckData::new(packet, vec![0], Height::new(1, 10), vec![0].into())
}

pub fn received_packet(
    port_id: PortId,
    channel_id: ChannelId,
    sequence: Sequence,
) -> Packet {
    let counterparty = dummy_channel_counterparty();
    let timestamp = chrono::Utc::now() + chrono::Duration::seconds(100);
    let timeout_timestamp = Timestamp::from_datetime(timestamp);
    Packet {
        sequence,
        source_port: counterparty.port_id().clone(),
        source_channel: counterparty.channel_id().unwrap().clone(),
        destination_port: port_id,
        destination_channel: channel_id,
        data: vec![0],
        timeout_height: Height::new(1, 10),
        timeout_timestamp,
    }
}

pub fn timeout_data(packet: Packet, next_seq_recv: Sequence) -> TimeoutData {
    TimeoutData::new(packet, next_seq_recv, Height::new(1, 10), vec![0].into())
}
