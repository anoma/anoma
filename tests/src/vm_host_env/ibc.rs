use core::time::Duration;
use std::collections::{BTreeSet, HashMap};
use std::convert::TryFrom;
use std::str::FromStr;

use anoma::ibc::applications::ics20_fungible_token_transfer::msgs::transfer::MsgTransfer;
use anoma::ibc::core::ics02_client::client_consensus::ConsensusState;
use anoma::ibc::core::ics02_client::client_state::{
    AnyClientState, ClientState,
};
use anoma::ibc::core::ics02_client::header::Header;
use anoma::ibc::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
use anoma::ibc::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
use anoma::ibc::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
use anoma::ibc::core::ics03_connection::connection::Counterparty as ConnCounterparty;
use anoma::ibc::core::ics03_connection::msgs::conn_open_ack::MsgConnectionOpenAck;
use anoma::ibc::core::ics03_connection::msgs::conn_open_confirm::MsgConnectionOpenConfirm;
use anoma::ibc::core::ics03_connection::msgs::conn_open_init::MsgConnectionOpenInit;
use anoma::ibc::core::ics03_connection::msgs::conn_open_try::MsgConnectionOpenTry;
use anoma::ibc::core::ics03_connection::version::Version as ConnVersion;
use anoma::ibc::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order, State as ChanState,
};
use anoma::ibc::core::ics04_channel::msgs::acknowledgement::MsgAcknowledgement;
use anoma::ibc::core::ics04_channel::msgs::chan_close_confirm::MsgChannelCloseConfirm;
use anoma::ibc::core::ics04_channel::msgs::chan_close_init::MsgChannelCloseInit;
use anoma::ibc::core::ics04_channel::msgs::chan_open_ack::MsgChannelOpenAck;
use anoma::ibc::core::ics04_channel::msgs::chan_open_confirm::MsgChannelOpenConfirm;
use anoma::ibc::core::ics04_channel::msgs::chan_open_init::MsgChannelOpenInit;
use anoma::ibc::core::ics04_channel::msgs::chan_open_try::MsgChannelOpenTry;
use anoma::ibc::core::ics04_channel::msgs::recv_packet::MsgRecvPacket;
use anoma::ibc::core::ics04_channel::msgs::timeout::MsgTimeout;
use anoma::ibc::core::ics04_channel::msgs::timeout_on_close::MsgTimeoutOnClose;
use anoma::ibc::core::ics04_channel::packet::{Packet, Sequence};
use anoma::ibc::core::ics04_channel::Version as ChanVersion;
use anoma::ibc::core::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortId,
};
use anoma::ibc::mock::client_state::{MockClientState, MockConsensusState};
use anoma::ibc::mock::header::MockHeader;
use anoma::ibc::proofs::{ConsensusProof, Proofs};
use anoma::ibc::signer::Signer;
use anoma::ibc::timestamp::Timestamp;
use anoma::ibc::Height;
use anoma::ibc_proto::cosmos::base::v1beta1::Coin;
use anoma::ibc_proto::ibc::core::commitment::v1::MerkleProof;
use anoma::ibc_proto::ics23::CommitmentProof;
use anoma::ledger::gas::VpGasMeter;
pub use anoma::ledger::ibc::handler::*;
use anoma::ledger::ibc::init_genesis_storage;
pub use anoma::ledger::ibc::storage::{
    ack_key, capability_index_key, capability_key, channel_counter_key,
    channel_key, client_counter_key, client_state_key, client_type_key,
    commitment_key, connection_counter_key, connection_key,
    consensus_state_key, next_sequence_ack_key, next_sequence_recv_key,
    next_sequence_send_key, port_key, receipt_key,
};
use anoma::ledger::ibc::vp::{Ibc, IbcToken};
use anoma::ledger::native_vp::{Ctx, NativeVp};
use anoma::ledger::storage::mockdb::MockDB;
use anoma::ledger::storage::testing::TestStorage;
use anoma::ledger::storage::Sha256Hasher;
use anoma::proto::Tx;
use anoma::tendermint::account::Id as TmAccountId;
use anoma::tendermint::block::header::{
    Header as TmHeader, Version as TmVersion,
};
use anoma::tendermint::block::Height as TmHeight;
use anoma::tendermint::chain::Id as TmChainId;
use anoma::tendermint::hash::{AppHash, Hash as TmHash};
use anoma::tendermint::time::Time as TmTime;
use anoma::tendermint_proto::Protobuf;
use anoma::types::address::{self, Address, InternalAddress};
use anoma::types::ibc::data::FungibleTokenPacketData;
use anoma::types::ibc::IbcEvent;
use anoma::types::storage::{BlockHash, BlockHeight, Key, TxIndex};
use anoma::types::time::Rfc3339String;
use anoma::types::token::{self, Amount};
use anoma::vm::{wasm, WasmCacheRwAccess};
use tempfile::TempDir;

use crate::tx::*;

const VP_ALWAYS_TRUE_WASM: &str = "../wasm_for_tests/vp_always_true.wasm";

pub struct TestIbcVp<'a> {
    pub ibc: Ibc<'a, MockDB, Sha256Hasher, WasmCacheRwAccess>,
    pub keys_changed: BTreeSet<Key>,
}

impl<'a> TestIbcVp<'a> {
    pub fn validate(
        &self,
        tx_data: &[u8],
    ) -> std::result::Result<bool, anoma::ledger::ibc::vp::Error> {
        self.ibc
            .validate_tx(tx_data, &self.keys_changed, &BTreeSet::new())
    }
}

pub struct TestIbcTokenVp<'a> {
    pub token: IbcToken<'a, MockDB, Sha256Hasher, WasmCacheRwAccess>,
    pub keys_changed: BTreeSet<Key>,
}

impl<'a> TestIbcTokenVp<'a> {
    pub fn validate(
        &self,
        tx_data: &[u8],
    ) -> std::result::Result<bool, anoma::ledger::ibc::vp::IbcTokenError> {
        self.token
            .validate_tx(tx_data, &self.keys_changed, &BTreeSet::new())
    }
}

pub struct TestIbcActions;

impl IbcActions for TestIbcActions {
    /// Read IBC-related data
    fn read_ibc_data(&self, key: &Key) -> Option<Vec<u8>> {
        tx_host_env::read_bytes(key.to_string())
    }

    /// Write IBC-related data
    fn write_ibc_data(&self, key: &Key, data: impl AsRef<[u8]>) {
        tx_host_env::write_bytes(key.to_string(), data)
    }

    /// Delete IBC-related data
    fn delete_ibc_data(&self, key: &Key) {
        tx_host_env::delete(key.to_string())
    }

    /// Emit an IBC event
    fn emit_ibc_event(&self, event: IbcEvent) {
        tx_host_env::emit_ibc_event(&event)
    }

    fn transfer_token(
        &self,
        src: &Address,
        dest: &Address,
        token: &Address,
        amount: Amount,
    ) {
        let src_key = token::balance_key(token, src);
        let dest_key = token::balance_key(token, dest);
        let src_bal: Option<Amount> = tx_host_env::read(&src_key.to_string());
        let mut src_bal = src_bal.unwrap_or_else(|| match src {
            Address::Internal(InternalAddress::IbcMint) => Amount::max(),
            _ => unreachable!(),
        });
        src_bal.spend(&amount);
        let mut dest_bal: Amount =
            tx_host_env::read(&dest_key.to_string()).unwrap_or_default();
        dest_bal.receive(&amount);
        match src {
            Address::Internal(InternalAddress::IbcMint) => {
                tx_host_env::write_temp(&src_key.to_string(), src_bal)
            }
            Address::Internal(InternalAddress::IbcBurn) => unreachable!(),
            _ => tx_host_env::write(&src_key.to_string(), src_bal),
        }
        match dest {
            Address::Internal(InternalAddress::IbcMint) => unreachable!(),
            Address::Internal(InternalAddress::IbcBurn) => {
                tx_host_env::write_temp(&dest_key.to_string(), dest_bal)
            }
            _ => tx_host_env::write(&dest_key.to_string(), dest_bal),
        }
    }

    fn get_height(&self) -> BlockHeight {
        tx_host_env::get_block_height()
    }

    fn get_header_time(&self) -> Rfc3339String {
        tx_host_env::get_block_time()
    }
}

/// Initialize IBC VP by running a transaction.
pub fn init_ibc_vp_from_tx<'a>(
    tx_env: &'a TestTxEnv,
    tx: &'a Tx,
) -> (TestIbcVp<'a>, TempDir) {
    let keys_changed = tx_env
        .write_log
        .verifiers_changed_keys(&BTreeSet::new())
        .get(&Address::Internal(InternalAddress::Ibc))
        .cloned()
        .expect("no IBC address");
    let (vp_wasm_cache, vp_cache_dir) =
        wasm::compilation_cache::common::testing::cache();

    let ctx = Ctx::new(
        &tx_env.storage,
        &tx_env.write_log,
        tx,
        &TxIndex(0),
        VpGasMeter::new(0),
        vp_wasm_cache,
    );
    let ibc = Ibc { ctx };

    (TestIbcVp { ibc, keys_changed }, vp_cache_dir)
}

/// Initialize the native token VP for the given address
pub fn init_token_vp_from_tx<'a>(
    tx_env: &'a TestTxEnv,
    tx: &'a Tx,
    addr: &Address,
) -> (TestIbcTokenVp<'a>, TempDir) {
    let keys_changed = tx_env
        .write_log
        .verifiers_changed_keys(&BTreeSet::new())
        .get(addr)
        .cloned()
        .expect("no token address");
    let (vp_wasm_cache, vp_cache_dir) =
        wasm::compilation_cache::common::testing::cache();

    let ctx = Ctx::new(
        &tx_env.storage,
        &tx_env.write_log,
        tx,
        &TxIndex(0),
        VpGasMeter::new(0),
        vp_wasm_cache,
    );
    let token = IbcToken { ctx };

    (
        TestIbcTokenVp {
            token,
            keys_changed,
        },
        vp_cache_dir,
    )
}

/// Initialize the test storage
pub fn init_storage(storage: &mut TestStorage) -> (Address, Address) {
    init_genesis_storage(storage);
    // block header to check timeout timestamp
    storage.set_header(tm_dummy_header()).unwrap();
    storage
        .begin_block(BlockHash::default(), BlockHeight(1))
        .unwrap();

    // initialize a token
    let code = std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");
    let token = tx_host_env::init_account(code.clone());

    // initialize an account
    let account = tx_host_env::init_account(code);
    let key = token::balance_key(&token, &account);
    let init_bal = Amount::from(1_000_000_000u64);
    tx_host_env::write(key.to_string(), init_bal);
    (token, account)
}

pub fn tm_dummy_header() -> TmHeader {
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

pub fn prepare_client() -> (ClientId, AnyClientState, HashMap<Key, Vec<u8>>) {
    let mut writes = HashMap::new();

    let msg = msg_create_client();
    // client state
    let client_state = msg.client_state.clone();
    let client_id =
        client_id(client_state.client_type(), 0).expect("invalid client ID");
    let key = client_state_key(&client_id);
    let bytes = msg.client_state.encode_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // client type
    let key = client_type_key(&client_id);
    let client_type = client_state.client_type();
    let bytes = client_type.as_str().as_bytes().to_vec();
    writes.insert(key, bytes);
    // consensus state
    let height = client_state.latest_height();
    let key = consensus_state_key(&client_id, height);
    let bytes = msg.consensus_state.encode_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // client counter
    let key = client_counter_key();
    let bytes = 1_u64.to_be_bytes().to_vec();
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
    let bytes = conn.encode_vec().expect("encoding failed");
    writes.insert(key, bytes);
    // connection counter
    let key = connection_counter_key();
    let bytes = 1_u64.to_be_bytes().to_vec();
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
    writes.insert(key, 0_u64.to_be_bytes().to_vec());
    // capability
    let key = capability_key(0);
    let bytes = port_id.as_bytes().to_vec();
    writes.insert(key, bytes);
    // channel
    let channel_id = channel_id(0);
    let port_channel_id = port_channel_id(port_id.clone(), channel_id.clone());
    let key = channel_key(&port_channel_id);
    let msg = msg_channel_open_init(port_id.clone(), conn_id.clone());
    let mut channel = msg.channel;
    open_channel(&mut channel);
    let bytes = channel.encode_vec().expect("encoding failed");
    writes.insert(key, bytes);

    (port_id, channel_id, writes)
}

pub fn msg_create_client() -> MsgCreateAnyClient {
    let height = Height::new(0, 1);
    let header = MockHeader {
        height,
        timestamp: Timestamp::now(),
    };
    let client_state = MockClientState::new(header).wrap_any();
    let consensus_state = MockConsensusState::new(header).wrap_any();
    MsgCreateAnyClient {
        client_state,
        consensus_state,
        signer: Signer::new("test"),
    }
}

pub fn msg_update_client(client_id: ClientId) -> MsgUpdateAnyClient {
    let height = Height::new(0, 2);
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
    let client_state = MockClientState::new(header).wrap_any();
    let consensus_state = MockConsensusState::new(header).wrap_any();
    let proof_upgrade_client = MerkleProof {
        proofs: vec![CommitmentProof { proof: None }],
    };
    let proof_upgrade_consensus_state = MerkleProof {
        proofs: vec![CommitmentProof { proof: None }],
    };
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
        version: ConnVersion::default(),
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
        counterparty_versions: vec![ConnVersion::default()],
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
        version: ConnVersion::default(),
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
    let height = Height::new(0, 1);
    let consensus_proof =
        ConsensusProof::new(vec![0].try_into().unwrap(), height).unwrap();
    Proofs::new(
        vec![0].try_into().unwrap(),
        Some(vec![0].try_into().unwrap()),
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
        counterparty_version: ChanVersion::ics20(),
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
        counterparty_version: ChanVersion::ics20(),
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
        ChanVersion::ics20(),
    )
}

pub fn dummy_channel_counterparty() -> ChanCounterparty {
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

pub fn msg_transfer(
    port_id: PortId,
    channel_id: ChannelId,
    token: String,
    sender: &Address,
) -> MsgTransfer {
    let timeout_timestamp =
        (Timestamp::now() + Duration::from_secs(100)).unwrap();
    MsgTransfer {
        source_port: port_id,
        source_channel: channel_id,
        token: Some(Coin {
            denom: token,
            amount: 100u64.to_string(),
        }),
        sender: Signer::new(sender.to_string()),
        receiver: Signer::new(
            address::testing::gen_established_address().to_string(),
        ),
        timeout_height: Height::new(1, 100),
        timeout_timestamp,
    }
}

pub fn set_timeout_height(msg: &mut MsgTransfer) {
    msg.timeout_height = Height::new(1, 1);
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
    token: String,
    receiver: &Address,
) -> Packet {
    let counterparty = dummy_channel_counterparty();
    let timeout_timestamp =
        (Timestamp::now() + Duration::from_secs(100)).unwrap();
    let data = FungibleTokenPacketData {
        denomination: token,
        amount: 100u64.to_string(),
        sender: address::testing::gen_established_address().to_string(),
        receiver: receiver.to_string(),
    };
    Packet {
        sequence,
        source_port: counterparty.port_id().clone(),
        source_channel: counterparty.channel_id().unwrap().clone(),
        destination_port: port_id,
        destination_channel: channel_id,
        data: serde_json::to_vec(&data).unwrap(),
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
