//! The ledger shell connects the ABCI++ interface with the Anoma ledger app.
//!
//! Any changes applied before [`Shell::finalize_block`] might have to be
//! reverted, so any changes applied in the methods `Shell::prepare_proposal`
//! (ABCI++), [`Shell::process_proposal`] must be also reverted (unless we can
//! simply overwrite them in the next block).
//! More info in <https://github.com/anoma/anoma/issues/362>.
mod finalize_block;
mod init_chain;
#[cfg(not(feature = "ABCI"))]
mod prepare_proposal;
mod process_proposal;
mod queries;

use std::convert::{TryFrom, TryInto};
use std::mem;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anoma::ledger::gas::BlockGasMeter;
use anoma::ledger::pos::anoma_proof_of_stake::types::{
    ActiveValidator, ValidatorSetUpdate,
};
use anoma::ledger::pos::anoma_proof_of_stake::PosBase;
use anoma::ledger::storage::write_log::WriteLog;
use anoma::ledger::storage::{DBIter, Storage, StorageHasher, DB};
use anoma::ledger::{ibc, parameters, pos};
use anoma::proto::{self, Tx};
use anoma::types::chain::ChainId;
use anoma::types::storage::{BlockHeight, Key};
use anoma::types::time::{DateTime, DateTimeUtc, TimeZone, Utc};
use anoma::types::transaction::{
    hash_tx, process_tx, verify_decrypted_correctly, AffineCurve, DecryptedTx,
    EllipticCurve, PairingEngine, TxType, WrapperTx,
};
use anoma::types::{address, key, token};
use anoma::vm::wasm::{TxCache, VpCache};
use anoma::vm::WasmCacheRwAccess;
use borsh::BorshSerialize;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::abci::{
    self, Evidence, RequestPrepareProposal, ValidatorUpdate,
};
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::types::ConsensusParams;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::ConsensusParams;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::{self, Evidence, ValidatorUpdate};
use thiserror::Error;
#[cfg(not(feature = "ABCI"))]
use tower_abci::{request, response};
#[cfg(feature = "ABCI")]
use tower_abci_old::{request, response};

use super::rpc;
use crate::config;
use crate::config::genesis;
use crate::node::ledger::events::Event;
use crate::node::ledger::shims::abcipp_shim_types::shim;
use crate::node::ledger::shims::abcipp_shim_types::shim::response::TxResult;
use crate::node::ledger::{protocol, storage, tendermint_node};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Error removing the DB data: {0}")]
    RemoveDB(std::io::Error),
    #[error("chain ID mismatch: {0}")]
    ChainId(String),
    #[error("Error decoding a transaction from bytes: {0}")]
    TxDecoding(proto::Error),
    #[error("Error trying to apply a transaction: {0}")]
    TxApply(protocol::Error),
    #[error("Gas limit exceeding while applying transactions in block")]
    GasOverflow,
    #[error("{0}")]
    Tendermint(tendermint_node::Error),
    #[error("Server error: {0}")]
    TowerServer(String),
}

/// The different error codes that the ledger may
/// send back to a client indicating the status
/// of their submitted tx
#[derive(Debug, Clone, FromPrimitive, ToPrimitive, PartialEq)]
pub enum ErrorCodes {
    Ok = 0,
    InvalidTx = 1,
    InvalidSig = 2,
    WasmRuntimeError = 3,
    InvalidOrder = 4,
    ExtraTxs = 5,
}

impl From<ErrorCodes> for u32 {
    fn from(code: ErrorCodes) -> u32 {
        code.to_u32().unwrap()
    }
}

impl From<ErrorCodes> for String {
    fn from(code: ErrorCodes) -> String {
        u32::from(code).to_string()
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn reset(config: config::Ledger) -> Result<()> {
    // simply nuke the DB files
    let db_path = &config.db_dir();
    match std::fs::remove_dir_all(&db_path) {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => (),
        res => res.map_err(Error::RemoveDB)?,
    };
    // reset Tendermint state
    tendermint_node::reset(config.tendermint_dir())
        .map_err(Error::Tendermint)?;
    Ok(())
}

#[derive(Clone, Debug)]
pub enum MempoolTxType {
    /// A transaction that has not been validated by this node before
    NewTransaction,
    /// A transaction that has been validated at some previous level that may
    /// need to be validated again
    RecheckTransaction,
}

#[derive(Debug)]
pub struct Shell<
    D = storage::PersistentDB,
    H = storage::PersistentStorageHasher,
> where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    /// The persistent storage
    pub(super) storage: Storage<D, H>,
    /// Gas meter for the current block
    gas_meter: BlockGasMeter,
    /// Write log for the current block
    write_log: WriteLog,
    /// Byzantine validators given from ABCI++ `prepare_proposal` are stored in
    /// this field. They will be slashed when we finalize the block.
    byzantine_validators: Vec<Evidence>,
    /// Path to the base directory with DB data and configs
    #[allow(dead_code)]
    base_dir: PathBuf,
    /// Path to the WASM directory for files used in the genesis block.
    wasm_dir: PathBuf,
    /// VP WASM compilation cache
    vp_wasm_cache: VpCache<WasmCacheRwAccess>,
    /// Tx WASM compilation cache
    tx_wasm_cache: TxCache<WasmCacheRwAccess>,
}

impl<D, H> Shell<D, H>
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    /// Create a new shell from a path to a database and a chain id. Looks
    /// up the database with this data and tries to load the last state.
    pub fn new(
        base_dir: PathBuf,
        db_path: impl AsRef<Path>,
        chain_id: ChainId,
        wasm_dir: PathBuf,
        db_cache: Option<&D::Cache>,
        vp_wasm_compilation_cache: u64,
        tx_wasm_compilation_cache: u64,
    ) -> Self {
        if !Path::new(&base_dir).is_dir() {
            std::fs::create_dir(&base_dir)
                .expect("Creating directory for Anoma should not fail");
        }
        let mut storage = Storage::open(db_path, chain_id.clone(), db_cache);
        storage
            .load_last_state()
            .map_err(|e| {
                tracing::error!("Cannot load the last state from the DB {}", e);
            })
            .expect("PersistentStorage cannot be initialized");

        let vp_wasm_cache_dir =
            base_dir.join(chain_id.as_str()).join("vp_wasm_cache");
        let tx_wasm_cache_dir =
            base_dir.join(chain_id.as_str()).join("tx_wasm_cache");
        Self {
            storage,
            gas_meter: BlockGasMeter::default(),
            write_log: WriteLog::default(),
            byzantine_validators: vec![],
            base_dir,
            wasm_dir,
            vp_wasm_cache: VpCache::new(
                vp_wasm_cache_dir,
                vp_wasm_compilation_cache as usize,
            ),
            tx_wasm_cache: TxCache::new(
                tx_wasm_cache_dir,
                tx_wasm_compilation_cache as usize,
            ),
        }
    }

    /// Iterate lazily over the wrapper txs in order
    #[cfg(not(feature = "ABCI"))]
    fn next_wrapper(&mut self) -> Option<&WrapperTx> {
        self.storage.tx_queue.lazy_next()
    }

    /// Iterate lazily over the wrapper txs in order
    #[cfg(feature = "ABCI")]
    fn next_wrapper(&mut self) -> Option<WrapperTx> {
        self.storage.tx_queue.pop()
    }

    /// If we reject the decrypted txs because they were out of
    /// order, reset the iterator.
    pub fn reset_tx_queue_iter(&mut self) {
        self.storage.tx_queue.rewind()
    }

    /// Load the Merkle root hash and the height of the last committed block, if
    /// any. This is returned when ABCI sends an `info` request.
    pub fn last_state(&mut self) -> response::Info {
        let mut response = response::Info::default();
        let result = self.storage.get_state();

        match result {
            Some((root, height)) => {
                tracing::info!(
                    "Last state root hash: {}, height: {}",
                    root,
                    height
                );
                response.last_block_app_hash = root.0;
                response.last_block_height =
                    height.try_into().expect("Invalid block height");
            }
            None => {
                tracing::info!(
                    "No state could be found, chain is not initialized"
                );
            }
        };

        response
    }

    /// Apply PoS slashes from the evidence
    fn slash(&mut self) {
        if !self.byzantine_validators.is_empty() {
            let byzantine_validators =
                mem::take(&mut self.byzantine_validators);
            let pos_params = self.storage.read_pos_params();
            let current_epoch = self.storage.block.epoch;
            for evidence in byzantine_validators {
                let evidence_height = match u64::try_from(evidence.height) {
                    Ok(height) => height,
                    Err(err) => {
                        tracing::error!(
                            "Unexpected evidence block height {}",
                            err
                        );
                        continue;
                    }
                };
                let evidence_epoch = match self
                    .storage
                    .block
                    .pred_epochs
                    .get_epoch(BlockHeight(evidence_height))
                {
                    Some(epoch) => epoch,
                    None => {
                        tracing::error!(
                            "Couldn't find epoch for evidence block height {}",
                            evidence_height
                        );
                        continue;
                    }
                };
                let slash_type =
                    match abci::EvidenceType::from_i32(evidence.r#type) {
                        Some(r#type) => match r#type {
                            abci::EvidenceType::DuplicateVote => {
                                pos::types::SlashType::DuplicateVote
                            }
                            abci::EvidenceType::LightClientAttack => {
                                pos::types::SlashType::LightClientAttack
                            }
                            abci::EvidenceType::Unknown => {
                                tracing::error!(
                                    "Unknown evidence: {:#?}",
                                    evidence
                                );
                                continue;
                            }
                        },
                        None => {
                            tracing::error!(
                                "Unexpected evidence type {}",
                                evidence.r#type
                            );
                            continue;
                        }
                    };
                let validator_raw_hash = match evidence.validator {
                    Some(validator) => {
                        match String::from_utf8(validator.address) {
                            Ok(raw_hash) => raw_hash,
                            Err(err) => {
                                tracing::error!(
                                    "Evidence failed to decode validator \
                                     address from utf-8 with {}",
                                    err
                                );
                                continue;
                            }
                        }
                    }
                    None => {
                        tracing::error!(
                            "Evidence without a validator {:#?}",
                            evidence
                        );
                        continue;
                    }
                };
                let validator = match self
                    .storage
                    .read_validator_address_raw_hash(&validator_raw_hash)
                {
                    Some(validator) => validator,
                    None => {
                        tracing::error!(
                            "Cannot find validator's address from raw hash {}",
                            validator_raw_hash
                        );
                        continue;
                    }
                };
                tracing::info!(
                    "Slashing {} for {} in epoch {}, block height {}",
                    evidence_epoch,
                    slash_type,
                    validator,
                    evidence_height
                );
                if let Err(err) = self.storage.slash(
                    &pos_params,
                    current_epoch,
                    evidence_epoch,
                    evidence_height,
                    slash_type,
                    &validator,
                ) {
                    tracing::error!("Error in slashing: {}", err);
                }
            }
        }
    }

    #[cfg(not(feature = "ABCI"))]
    /// INVARIANT: This method must be stateless.
    pub fn extend_vote(
        &self,
        _req: request::ExtendVote,
    ) -> response::ExtendVote {
        Default::default()
    }

    #[cfg(not(feature = "ABCI"))]
    /// INVARIANT: This method must be stateless.
    pub fn verify_vote_extension(
        &self,
        _req: request::VerifyVoteExtension,
    ) -> response::VerifyVoteExtension {
        Default::default()
    }

    /// Commit a block. Persist the application state and return the Merkle root
    /// hash.
    pub fn commit(&mut self) -> response::Commit {
        let mut response = response::Commit::default();
        // commit changes from the write-log to storage
        self.write_log
            .commit_block(&mut self.storage)
            .expect("Expected committing block write log success");
        // store the block's data in DB
        self.storage.commit().unwrap_or_else(|e| {
            tracing::error!(
                "Encountered a storage error while committing a block {:?}",
                e
            )
        });

        let root = self.storage.merkle_root();
        tracing::info!(
            "Committed block hash: {}, height: {}",
            root,
            self.storage.last_height,
        );
        response.data = root.0;
        response
    }

    /// Validate a transaction request. On success, the transaction will
    /// included in the mempool and propagated to peers, otherwise it will be
    /// rejected.
    pub fn mempool_validate(
        &self,
        tx_bytes: &[u8],
        r#_type: MempoolTxType,
    ) -> response::CheckTx {
        let mut response = response::CheckTx::default();
        match Tx::try_from(tx_bytes).map_err(Error::TxDecoding) {
            Ok(_) => response.log = String::from("Mempool validation passed"),
            Err(msg) => {
                response.code = 1;
                response.log = msg.to_string();
            }
        }
        response
    }

    /// Simulate validation and application of a transaction.
    fn dry_run_tx(&self, tx_bytes: &[u8]) -> response::Query {
        let mut response = response::Query::default();
        let mut gas_meter = BlockGasMeter::default();
        let mut write_log = WriteLog::default();
        let mut vp_wasm_cache = self.vp_wasm_cache.read_only();
        let mut tx_wasm_cache = self.tx_wasm_cache.read_only();
        match Tx::try_from(tx_bytes) {
            Ok(tx) => {
                let tx = TxType::Decrypted(DecryptedTx::Decrypted(tx));
                match protocol::apply_tx(
                    tx,
                    tx_bytes.len(),
                    &mut gas_meter,
                    &mut write_log,
                    &self.storage,
                    &mut vp_wasm_cache,
                    &mut tx_wasm_cache,
                )
                .map_err(Error::TxApply)
                {
                    Ok(result) => response.info = result.to_string(),
                    Err(error) => {
                        response.code = 1;
                        response.log = format!("{}", error);
                    }
                }
                response
            }
            Err(err) => {
                response.code = 1;
                response.log = format!("{}", Error::TxDecoding(err));
                response
            }
        }
    }
}

/// Helper functions and types for writing unit tests
/// for the shell
#[cfg(test)]
mod test_utils {
    use std::path::PathBuf;

    use anoma::ledger::storage::mockdb::MockDB;
    use anoma::ledger::storage::testing::Sha256Hasher;
    use anoma::ledger::storage::BlockStateWrite;
    use anoma::types::address::{xan, EstablishedAddressGen};
    use anoma::types::key::ed25519::Keypair;
    use anoma::types::storage::{BlockHash, Epoch};
    use anoma::types::transaction::Fee;
    use tempfile::tempdir;
    #[cfg(not(feature = "ABCI"))]
    use tendermint_proto::abci::{
        Event as TmEvent, RequestInitChain, ResponsePrepareProposal,
    };
    #[cfg(not(feature = "ABCI"))]
    use tendermint_proto::google::protobuf::Timestamp;
    #[cfg(feature = "ABCI")]
    use tendermint_proto_abci::abci::{Event as TmEvent, RequestInitChain};
    #[cfg(feature = "ABCI")]
    use tendermint_proto_abci::google::protobuf::Timestamp;

    use super::*;
    use crate::node::ledger::shims::abcipp_shim_types::shim::request::{
        FinalizeBlock, ProcessProposal,
    };
    use crate::node::ledger::storage::{PersistentDB, PersistentStorageHasher};

    /// Gets the absolute path to root directory
    pub fn top_level_directory() -> PathBuf {
        let mut current_path = std::env::current_dir()
            .expect("Current directory should exist")
            .canonicalize()
            .expect("Current directory should exist");
        while current_path.file_name().unwrap() != "apps" {
            current_path.pop();
        }
        current_path.pop();
        current_path
    }

    /// Generate a random public/private keypair
    pub(super) fn gen_keypair() -> Keypair {
        use rand::prelude::ThreadRng;
        use rand::thread_rng;

        let mut rng: ThreadRng = thread_rng();
        Keypair::generate(&mut rng)
    }

    /// A wrapper around the shell that implements
    /// Drop so as to clean up the files that it
    /// generates. Also allows illegal state
    /// modifications for testing purposes
    pub(super) struct TestShell {
        pub shell: Shell<MockDB, Sha256Hasher>,
    }

    impl TestShell {
        /// Create a new shell
        pub fn new() -> Self {
            let base_dir = tempdir().unwrap().as_ref().canonicalize().unwrap();
            let vp_wasm_compilation_cache = 50 * 1024 * 1024; // 50 kiB
            let tx_wasm_compilation_cache = 50 * 1024 * 1024; // 50 kiB
            Self {
                shell: Shell::<MockDB, Sha256Hasher>::new(
                    base_dir.clone(),
                    base_dir.join("db").join("anoma-devchain-00000"),
                    Default::default(),
                    top_level_directory().join("wasm"),
                    None,
                    vp_wasm_compilation_cache,
                    tx_wasm_compilation_cache,
                ),
            }
        }

        /// Forward a InitChain request and expect a success
        pub fn init_chain(&mut self, req: RequestInitChain) {
            self.shell
                .init_chain(req)
                .expect("Test shell failed to initialize");
        }

        /// Forward the prepare proposal request and return the response
        #[cfg(not(feature = "ABCI"))]
        pub fn prepare_proposal(
            &mut self,
            req: RequestPrepareProposal,
        ) -> ResponsePrepareProposal {
            self.shell.prepare_proposal(req)
        }

        /// Forward a ProcessProposal request and extract the relevant
        /// response data to return
        pub fn process_proposal(
            &mut self,
            req: ProcessProposal,
        ) -> shim::response::ProcessProposal {
            #[cfg(not(feature = "ABCI"))]
            {
                self.shell.process_proposal(req)
            }
            #[cfg(feature = "ABCI")]
            {
                self.shell.process_and_decode_proposal(req)
            }
        }

        /// Forward a FinalizeBlock request return a vector of
        /// the events created for each transaction
        pub fn finalize_block(
            &mut self,
            req: FinalizeBlock,
        ) -> Result<Vec<TmEvent>> {
            match self.shell.finalize_block(req) {
                Ok(resp) => Ok(resp.events),
                Err(err) => Err(err),
            }
        }

        /// Add a wrapper tx to the queue of txs to be decrypted
        /// in the current block proposal
        #[cfg(test)]
        pub fn enqueue_tx(&mut self, wrapper: WrapperTx) {
            self.shell.storage.tx_queue.push(wrapper);
            self.shell.reset_tx_queue_iter();
        }

        #[cfg(not(feature = "ABCI"))]
        /// Get the next wrapper tx to be decoded
        pub fn next_wrapper(&mut self) -> Option<&WrapperTx> {
            self.shell.next_wrapper()
        }

        #[cfg(feature = "ABCI")]
        /// Get the next wrapper tx to be decoded
        pub fn next_wrapper(&mut self) -> Option<WrapperTx> {
            self.shell.next_wrapper()
        }
    }

    /// Start a new test shell and initialize it
    pub(super) fn setup() -> TestShell {
        let mut test = TestShell::new();
        test.init_chain(RequestInitChain {
            time: Some(Timestamp {
                seconds: 0,
                nanos: 0,
            }),
            chain_id: ChainId::default().to_string(),
            ..Default::default()
        });
        test
    }

    /// We test that on shell shutdown, the tx queue gets persisted in a DB, and
    /// on startup it is read successfully
    #[test]
    fn test_tx_queue_persistence() {
        let base_dir = tempdir().unwrap().as_ref().canonicalize().unwrap();
        // we have to use RocksDB for this test
        let vp_wasm_compilation_cache = 50 * 1024 * 1024; // 50 kiB
        let tx_wasm_compilation_cache = 50 * 1024 * 1024; // 50 kiB
        let mut shell = Shell::<PersistentDB, PersistentStorageHasher>::new(
            base_dir.clone(),
            base_dir.join("db").join("anoma-devchain-00000"),
            Default::default(),
            top_level_directory().join("wasm"),
            None,
            vp_wasm_compilation_cache,
            tx_wasm_compilation_cache,
        );
        let keypair = gen_keypair();
        // enqueue a wrapper tx
        let tx = Tx::new(
            "wasm_code".as_bytes().to_owned(),
            Some("transaction data".as_bytes().to_owned()),
        );
        let wrapper = WrapperTx::new(
            Fee {
                amount: 0.into(),
                token: xan(),
            },
            &keypair,
            Epoch(0),
            0.into(),
            tx,
        );
        shell.storage.tx_queue.push(wrapper);
        // Artificially increase the block height so that chain
        // will read the new block when restarted
        let store = Default::default();
        let hash = BlockHash([0; 32]);
        let pred_epochs = Default::default();
        let address_gen = EstablishedAddressGen::new("test");
        shell
            .storage
            .db
            .write_block(BlockStateWrite {
                root: [0; 32].into(),
                store: &store,
                hash: &hash,
                height: BlockHeight(1),
                epoch: Epoch(0),
                pred_epochs: &pred_epochs,
                next_epoch_min_start_height: BlockHeight(3),
                next_epoch_min_start_time: DateTimeUtc::now(),
                address_gen: &address_gen,
                tx_queue: &shell.storage.tx_queue,
            })
            .expect("Test failed");

        // Drop the shell
        std::mem::drop(shell);

        // Reboot the shell and check that the queue was restored from DB
        let shell = Shell::<PersistentDB, PersistentStorageHasher>::new(
            base_dir.clone(),
            base_dir.join("db").join("anoma-devchain-00000"),
            Default::default(),
            top_level_directory().join("wasm"),
            None,
            vp_wasm_compilation_cache,
            tx_wasm_compilation_cache,
        );
        assert!(!shell.storage.tx_queue.is_empty());
    }
}
