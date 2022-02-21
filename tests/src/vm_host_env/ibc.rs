use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::str::FromStr;
use std::time::Duration;

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
use anoma::types::address::{self, Address, InternalAddress};
use anoma::types::ibc::data::FungibleTokenPacketData;
use anoma::types::ibc::IbcEvent;
use anoma::types::storage::Key;
use anoma::types::time::{DateTimeUtc, DurationSecs};
use anoma::types::token::{self, Amount};
use anoma::vm::{wasm, WasmCacheRwAccess};
#[cfg(not(feature = "ABCI"))]
use ibc::applications::ics20_fungible_token_transfer::msgs::transfer::MsgTransfer;
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
use ibc_abci::applications::ics20_fungible_token_transfer::msgs::transfer::MsgTransfer;
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
use ibc_proto::cosmos::base::v1beta1::Coin;
#[cfg(not(feature = "ABCI"))]
use ibc_proto::ibc::core::commitment::v1::MerkleProof;
#[cfg(feature = "ABCI")]
use ibc_proto_abci::cosmos::base::v1beta1::Coin;
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
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::Protobuf;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::Protobuf;
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

use crate::tx::*;

const VP_ALWAYS_TRUE_WASM: &str = "../wasm_for_tests/vp_always_true.wasm";

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

pub struct TestIbcTokenVp<'a> {
    pub token: IbcToken<'a, MockDB, Sha256Hasher, WasmCacheRwAccess>,
    pub keys_changed: HashSet<Key>,
}

impl<'a> TestIbcTokenVp<'a> {
    pub fn validate(
        &self,
        tx_data: &[u8],
    ) -> std::result::Result<bool, anoma::ledger::ibc::vp::IbcTokenError> {
        self.token
            .validate_tx(tx_data, &self.keys_changed, &HashSet::new())
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

/// Initialize the native token VP for the given address
pub fn init_token_vp_from_tx<'a>(
    tx_env: &'a TestTxEnv,
    tx: &'a Tx,
    addr: &Address,
) -> (TestIbcTokenVp<'a>, TempDir) {
    let keys_changed = tx_env
        .write_log
        .verifiers_changed_keys(&HashSet::new())
        .get(addr)
        .cloned()
        .expect("no token address");
    let (vp_wasm_cache, vp_cache_dir) =
        wasm::compilation_cache::common::testing::cache();

    let ctx = Ctx::new(
        &tx_env.storage,
        &tx_env.write_log,
        tx,
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
    let timestamp = DateTimeUtc::now() + DurationSecs(100);
    let timeout_timestamp = Timestamp::from_datetime(timestamp.0);
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
    let timestamp = chrono::Utc::now() + chrono::Duration::seconds(100);
    let timeout_timestamp = Timestamp::from_datetime(timestamp);
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
