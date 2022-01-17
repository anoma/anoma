use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::str::FromStr;
use std::time::Duration;

use anoma::ledger::gas::VpGasMeter;
pub use anoma::ledger::ibc::handler::*;
pub use anoma::ledger::ibc::storage::{
    ack_key, capability_index_key, capability_key, channel_counter_key,
    channel_key, client_counter_key, client_state_key, client_type_key,
    commitment_key, connection_counter_key, connection_key,
    consensus_state_key, next_sequence_ack_key, next_sequence_recv_key,
    next_sequence_send_key, port_key, receipt_key,
};
use anoma::ledger::ibc::vp::Ibc;
use anoma::ledger::native_vp::{Ctx, NativeVp};
use anoma::ledger::storage::mockdb::MockDB;
use anoma::ledger::storage::Sha256Hasher;
use anoma::proto::Tx;
use anoma::types::address::{Address, InternalAddress};
pub use anoma::types::ibc::data::*;
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
use ibc::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::connection::Counterparty as ConnCounterparty;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::msgs::conn_open_ack::MsgConnectionOpenAck;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::msgs::conn_open_confirm::MsgConnectionOpenConfirm;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::msgs::conn_open_init::MsgConnectionOpenInit;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::msgs::conn_open_try::MsgConnectionOpenTry;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::version::Version;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::channel::State as ChanState;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order,
};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::acknowledgement::MsgAcknowledgement;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_close_confirm::MsgChannelCloseConfirm;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_close_init::MsgChannelCloseInit;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_open_ack::MsgChannelOpenAck;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_open_confirm::MsgChannelOpenConfirm;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_open_init::MsgChannelOpenInit;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_open_try::MsgChannelOpenTry;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::recv_packet::MsgRecvPacket;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::timeout::MsgTimeout;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::timeout_on_close::MsgTimeoutOnClose;
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
use ibc::proofs::{ConsensusProof, Proofs};
#[cfg(not(feature = "ABCI"))]
use ibc::signer::Signer;
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
use ibc_abci::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::connection::Counterparty as ConnCounterparty;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::msgs::conn_open_ack::MsgConnectionOpenAck;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::msgs::conn_open_confirm::MsgConnectionOpenConfirm;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::msgs::conn_open_init::MsgConnectionOpenInit;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::msgs::conn_open_try::MsgConnectionOpenTry;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::version::Version;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::channel::State as ChanState;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order,
};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::acknowledgement::MsgAcknowledgement;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_close_confirm::MsgChannelCloseConfirm;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_close_init::MsgChannelCloseInit;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_open_ack::MsgChannelOpenAck;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_open_confirm::MsgChannelOpenConfirm;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_open_init::MsgChannelOpenInit;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_open_try::MsgChannelOpenTry;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::recv_packet::MsgRecvPacket;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::timeout::MsgTimeout;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::timeout_on_close::MsgTimeoutOnClose;
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
use ibc_abci::proofs::{ConsensusProof, Proofs};
#[cfg(feature = "ABCI")]
use ibc_abci::signer::Signer;
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
    ) -> std::result::Result<bool, anoma::ledger::ibc::vp::Error> {
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

    let msg = msg_create_client();
    // client state
    let client_state = msg.client_state.clone();
    let client_id =
        client_id(client_state.client_type(), 0).expect("invalid client ID");
    let key = client_state_key(&client_id);
    let bytes = msg.client_state.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // client type
    let key = client_type_key(&client_id);
    let client_type = client_state.client_type();
    let bytes = client_type.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // consensus state
    let height = client_state.latest_height();
    let key = consensus_state_key(&client_id, height);
    let bytes = msg.consensus_state.try_to_vec().expect("encoding failed");
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
    let msg = msg_connection_open_init(client_id.clone());
    let mut conn = init_connection(&msg);
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
    let msg = msg_channel_open_init(port_id.clone(), conn_id.clone());
    let mut channel = msg.channel;
    open_channel(&mut channel);
    let bytes = channel.try_to_vec().expect("encoding failed");
    writes.insert(key, bytes);

    (port_id, channel_id, writes)
}

pub fn msg_create_client() -> MsgCreateAnyClient {
    let height = Height::new(1, 10);
    let header = MockHeader {
        height,
        timestamp: Timestamp::now(),
    };
    let client_state = MockClientState(header).wrap_any();
    let consensus_state = MockConsensusState::new(header).wrap_any();
    MsgCreateAnyClient {
        client_state,
        consensus_state,
        signer: Signer::new("test"),
    }
}

pub fn msg_update_client(client_id: ClientId) -> MsgUpdateAnyClient {
    let height = Height::new(1, 11);
    let header = MockHeader {
        height,
        timestamp: Timestamp::now(),
    }
    .wrap_any();
    MsgUpdateAnyClient {
        client_id,
        header,
        signer: Signer::new("test"),
    }
}

pub fn msg_upgrade_client(client_id: ClientId) -> MsgUpgradeAnyClient {
    let height = Height::new(0, 1);
    let header = MockHeader {
        height,
        timestamp: Timestamp::now(),
    };
    let client_state = MockClientState(header).wrap_any();
    let consensus_state = MockConsensusState::new(header).wrap_any();
    let proof_upgrade_client =
        MerkleProof::try_from(CommitmentProofBytes::from(vec![])).unwrap();
    let proof_upgrade_consensus_state =
        MerkleProof::try_from(CommitmentProofBytes::from(vec![])).unwrap();
    MsgUpgradeAnyClient {
        client_id,
        client_state,
        consensus_state,
        proof_upgrade_client,
        proof_upgrade_consensus_state,
        signer: Signer::new("test"),
    }
}

pub fn msg_connection_open_init(client_id: ClientId) -> MsgConnectionOpenInit {
    MsgConnectionOpenInit {
        client_id,
        counterparty: dummy_connection_counterparty(),
        version: Version::default(),
        delay_period: Duration::new(100, 0),
        signer: Signer::new("test"),
    }
}

pub fn msg_connection_open_try(
    client_id: ClientId,
    client_state: AnyClientState,
) -> MsgConnectionOpenTry {
    MsgConnectionOpenTry {
        previous_connection_id: None,
        client_id,
        client_state: Some(client_state),
        counterparty: dummy_connection_counterparty(),
        counterparty_versions: vec![Version::default()],
        proofs: dummy_proofs(),
        delay_period: Duration::new(100, 0),
        signer: Signer::new("test"),
    }
}

pub fn msg_connection_open_ack(
    connection_id: ConnectionId,
    client_state: AnyClientState,
) -> MsgConnectionOpenAck {
    let counterparty_connection_id =
        ConnectionId::from_str("counterpart_test_connection")
            .expect("Creating a connection ID failed");
    MsgConnectionOpenAck {
        connection_id,
        counterparty_connection_id,
        client_state: Some(client_state),
        proofs: dummy_proofs(),
        version: Version::default(),
        signer: Signer::new("test"),
    }
}

pub fn msg_connection_open_confirm(
    connection_id: ConnectionId,
) -> MsgConnectionOpenConfirm {
    MsgConnectionOpenConfirm {
        connection_id,
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
}

fn dummy_proofs() -> Proofs {
    let height = Height::new(1, 10);
    let consensus_proof = ConsensusProof::new(vec![0].into(), height).unwrap();
    Proofs::new(
        vec![0].into(),
        Some(vec![0].into()),
        Some(consensus_proof),
        None,
        height,
    )
    .unwrap()
}

fn dummy_connection_counterparty() -> ConnCounterparty {
    let counterpart_client_id = ClientId::from_str("counterpart_test_client")
        .expect("Creating a client ID failed");
    let counterpart_conn_id =
        ConnectionId::from_str("counterpart_test_connection")
            .expect("Creating a connection ID failed");
    connection_counterparty(counterpart_client_id, counterpart_conn_id)
}

pub fn msg_channel_open_init(
    port_id: PortId,
    conn_id: ConnectionId,
) -> MsgChannelOpenInit {
    MsgChannelOpenInit {
        port_id,
        channel: dummy_channel(ChanState::Init, Order::Ordered, conn_id),
        signer: Signer::new("test"),
    }
}

pub fn msg_channel_open_try(
    port_id: PortId,
    conn_id: ConnectionId,
) -> MsgChannelOpenTry {
    MsgChannelOpenTry {
        port_id,
        previous_channel_id: None,
        channel: dummy_channel(ChanState::TryOpen, Order::Ordered, conn_id),
        counterparty_version: Order::Ordered.to_string(),
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
}

pub fn msg_channel_open_ack(
    port_id: PortId,
    channel_id: ChannelId,
) -> MsgChannelOpenAck {
    MsgChannelOpenAck {
        port_id,
        channel_id,
        counterparty_channel_id: dummy_channel_counterparty()
            .channel_id()
            .unwrap()
            .clone(),
        counterparty_version: Order::Ordered.to_string(),
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
}

pub fn msg_channel_open_confirm(
    port_id: PortId,
    channel_id: ChannelId,
) -> MsgChannelOpenConfirm {
    MsgChannelOpenConfirm {
        port_id,
        channel_id,
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
}

pub fn msg_channel_close_init(
    port_id: PortId,
    channel_id: ChannelId,
) -> MsgChannelCloseInit {
    MsgChannelCloseInit {
        port_id,
        channel_id,
        signer: Signer::new("test"),
    }
}

pub fn msg_channel_close_confirm(
    port_id: PortId,
    channel_id: ChannelId,
) -> MsgChannelCloseConfirm {
    MsgChannelCloseConfirm {
        port_id,
        channel_id,
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
}

fn dummy_channel(
    state: ChanState,
    order: Order,
    connection_id: ConnectionId,
) -> ChannelEnd {
    ChannelEnd::new(
        state,
        order,
        dummy_channel_counterparty(),
        vec![connection_id],
        order.to_string(),
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

pub fn msg_packet_recv(packet: Packet) -> MsgRecvPacket {
    MsgRecvPacket {
        packet,
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
}

pub fn msg_packet_ack(packet: Packet) -> MsgAcknowledgement {
    MsgAcknowledgement {
        packet,
        acknowledgement: vec![0],
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
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

pub fn msg_timeout(packet: Packet, next_sequence_recv: Sequence) -> MsgTimeout {
    MsgTimeout {
        packet,
        next_sequence_recv,
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
}

pub fn msg_timeout_on_close(
    packet: Packet,
    next_sequence_recv: Sequence,
) -> MsgTimeoutOnClose {
    MsgTimeoutOnClose {
        packet,
        next_sequence_recv,
        proofs: dummy_proofs(),
        signer: Signer::new("test"),
    }
}
