//! The ledger shell connects the ABCI++ interface with the Anoma ledger app.
//!
//! Any changes applied before [`Shell::finalize_block`] might have to be
//! reverted, so any changes applied in the methods [`Shell::prepare_proposal`],
//! [`Shell::process_proposal`] must be also reverted (unless we can simply
//! overwrite them in the next block).
//! More info in <https://github.com/anoma/anoma/issues/362>.

use std::cmp::max;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::hash::Hash;
use std::mem;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anoma::ledger::gas::BlockGasMeter;
use anoma::ledger::parameters::Parameters;
use anoma::ledger::pos::anoma_proof_of_stake::types::{
    ActiveValidator, ValidatorSetUpdate,
};
use anoma::ledger::pos::anoma_proof_of_stake::PosBase;
use anoma::ledger::pos::PosParams;
use anoma::ledger::storage::write_log::WriteLog;
use anoma::ledger::{ibc, parameters, pos};
use anoma::proto::{self, Tx};
use anoma::types::address::Address;
use anoma::types::chain::ChainId;
use anoma::types::storage::{BlockHash, BlockHeight, Key};
use anoma::types::time::{DateTime, DateTimeUtc, TimeZone, Utc};
use anoma::types::transaction::{process_tx, TxType, WrapperTx};
use anoma::types::{address, key, token};
use borsh::{BorshDeserialize, BorshSerialize};
#[cfg(not(feature = "dev"))]
use sha2::{Digest, Sha256};
use tendermint::block::Header;
use tendermint_proto::abci::{
    self, ConsensusParams, Evidence, ValidatorUpdate,
};
use tendermint_proto::types::EvidenceParams;
use thiserror::Error;
use tower_abci::{request, response};

use super::rpc;
use crate::config::genesis;
use crate::node::ledger::events::{Event, EventType};
use crate::node::ledger::rpc::PrefixValue;
use crate::node::ledger::shims::abcipp_shim_types::shim;
use crate::node::ledger::shims::abcipp_shim_types::shim::response::TxResult;
use crate::node::ledger::{protocol, storage, tendermint_node};
use crate::{config, wasm_loader};

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

pub type Result<T> = std::result::Result<T, Error>;

pub fn reset(config: config::Ledger) -> Result<()> {
    // simply nuke the DB files
    let db_path = config.db_dir();
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
pub struct Shell {
    /// The persistent storage
    pub(super) storage: storage::PersistentStorage,
    /// Gas meter for the current block
    gas_meter: BlockGasMeter,
    /// Write log for the current block
    write_log: WriteLog,
    /// Byzantine validators given from ABCI++ `prepare_proposal` are stored in
    /// this field. They will be slashed when we finalize the block.
    byzantine_validators: Vec<Evidence>,
    /// Path to the base directory with DB data and configs
    base_dir: PathBuf,
    /// Path to the WASM directory for files used in the genesis block.
    wasm_dir: PathBuf,
}

impl Shell {
    /// Create a new shell from a path to a database and a chain id. Looks
    /// up the database with this data and tries to load the last state.
    pub fn new(
        base_dir: PathBuf,
        db_path: impl AsRef<Path>,
        chain_id: ChainId,
        wasm_dir: PathBuf,
    ) -> Self {
        let mut storage = storage::open(db_path, chain_id);
        storage
            .load_last_state()
            .map_err(|e| {
                tracing::error!("Cannot load the last state from the DB {}", e);
            })
            .expect("PersistentStorage cannot be initialized");

        Self {
            storage,
            gas_meter: BlockGasMeter::default(),
            write_log: WriteLog::default(),
            byzantine_validators: vec![],
            base_dir,
            wasm_dir,
        }
    }

    /// Create a new genesis for the chain with specified id. This includes
    /// 1. A set of initial users and tokens
    /// 2. Setting up the validity predicates for both users and tokens
    /// 3. A matchmaker
    pub fn init_chain(
        &mut self,
        init: request::InitChain,
    ) -> Result<response::InitChain> {
        let mut response = response::InitChain::default();
        let (current_chain_id, _) = self.storage.get_chain_id();
        if current_chain_id != init.chain_id {
            return Err(Error::ChainId(format!(
                "Current chain ID: {}, Tendermint chain ID: {}",
                current_chain_id, init.chain_id
            )));
        }
        #[cfg(not(feature = "dev"))]
        let genesis = genesis::genesis(&self.base_dir, &self.storage.chain_id);
        #[cfg(not(feature = "dev"))]
        {
            let genesis_bytes = genesis.try_to_vec().unwrap();
            let errors = self.storage.chain_id.validate(genesis_bytes);
            use itertools::Itertools;
            assert!(
                errors.is_empty(),
                "Chain ID validation failed: {}",
                errors.into_iter().format(". ")
            );
        }
        #[cfg(feature = "dev")]
        let genesis = genesis::genesis();

        let ts: tendermint_proto::google::protobuf::Timestamp =
            init.time.expect("Missing genesis time");
        let initial_height = init
            .initial_height
            .try_into()
            .expect("Unexpected block height");
        // TODO hacky conversion, depends on https://github.com/informalsystems/tendermint-rs/issues/870
        let genesis_time: DateTimeUtc =
            (Utc.timestamp(ts.seconds, ts.nanos as u32)).into();

        parameters::init_genesis_storage(
            &mut self.storage,
            &genesis.parameters,
        );
        // Depends on parameters being initialized
        self.storage
            .init_genesis_epoch(
                initial_height,
                genesis_time,
                &genesis.parameters,
            )
            .expect("Initializing genesis epoch must not fail");

        // Loaded VP code cache to avoid loading the same files multiple times
        let mut vp_code_cache: HashMap<String, Vec<u8>> = HashMap::default();

        // Initialize genesis established accounts
        for genesis::EstablishedAccount {
            address,
            vp_code_path,
            vp_sha256,
            public_key,
            storage,
        } in genesis.established_accounts
        {
            let vp_code = vp_code_cache
                .get_or_insert_with(vp_code_path.clone(), || {
                    wasm_loader::read_wasm(&self.wasm_dir, &vp_code_path)
                });

            // In dev, we don't check the hash
            #[cfg(feature = "dev")]
            let _ = vp_sha256;
            #[cfg(not(feature = "dev"))]
            {
                let mut hasher = Sha256::new();
                hasher.update(&vp_code);
                let vp_code_hash = hasher.finalize();
                assert_eq!(
                    vp_code_hash.as_slice(),
                    &vp_sha256,
                    "Invalid established account's VP sha256 hash for {}",
                    vp_code_path
                );
            }

            self.storage
                .write(&Key::validity_predicate(&address), vp_code)
                .unwrap();

            if let Some(pk) = public_key {
                let pk_storage_key = key::ed25519::pk_key(&address);
                self.storage
                    .write(&pk_storage_key, pk.try_to_vec().unwrap())
                    .unwrap();
            }

            for (key, value) in storage {
                self.storage.write(&key, value).unwrap();
            }
        }

        // Initialize genesis implicit
        for genesis::ImplicitAccount { public_key } in genesis.implicit_accounts
        {
            let address: address::Address = (&public_key).into();
            let pk_storage_key = key::ed25519::pk_key(&address);
            self.storage
                .write(&pk_storage_key, public_key.try_to_vec().unwrap())
                .unwrap();
        }

        // Initialize genesis token accounts
        for genesis::TokenAccount {
            address,
            vp_code_path,
            vp_sha256,
            balances,
        } in genesis.token_accounts
        {
            let vp_code = vp_code_cache
                .get_or_insert_with(vp_code_path.clone(), || {
                    wasm_loader::read_wasm(&self.wasm_dir, &vp_code_path)
                });

            // In dev, we don't check the hash
            #[cfg(feature = "dev")]
            let _ = vp_sha256;
            #[cfg(not(feature = "dev"))]
            {
                let mut hasher = Sha256::new();
                hasher.update(&vp_code);
                let vp_code_hash = hasher.finalize();
                assert_eq!(
                    vp_code_hash.as_slice(),
                    &vp_sha256,
                    "Invalid token account's VP sha256 hash for {}",
                    vp_code_path
                );
            }

            self.storage
                .write(&Key::validity_predicate(&address), vp_code)
                .unwrap();

            for (owner, amount) in balances {
                self.storage
                    .write(
                        &token::balance_key(&address, &owner),
                        amount.try_to_vec().unwrap(),
                    )
                    .unwrap();
            }
        }

        // Initialize genesis validator accounts
        for validator in &genesis.validators {
            let vp_code = vp_code_cache.get_or_insert_with(
                validator.validator_vp_code_path.clone(),
                || {
                    std::fs::read(
                        self.wasm_dir.join(&validator.validator_vp_code_path),
                    )
                    .unwrap_or_else(|_| {
                        panic!(
                            "cannot load genesis VP {}.",
                            validator.validator_vp_code_path
                        )
                    })
                },
            );

            #[cfg(not(feature = "dev"))]
            {
                let mut hasher = Sha256::new();
                hasher.update(&vp_code);
                let vp_code_hash = hasher.finalize();
                assert_eq!(
                    vp_code_hash.as_slice(),
                    &validator.validator_vp_sha256,
                    "Invalid validator VP sha256 hash for {}",
                    validator.validator_vp_code_path
                );
            }

            let addr = &validator.pos_data.address;
            self.storage
                .write(&Key::validity_predicate(addr), vp_code)
                .expect("Unable to write user VP");
            // Validator account key
            let pk_key = key::ed25519::pk_key(addr);
            self.storage
                .write(
                    &pk_key,
                    validator
                        .account_key
                        .try_to_vec()
                        .expect("encode public key"),
                )
                .expect("Unable to set genesis user public key");
            // Account balance (tokens no staked in PoS)
            self.storage
                .write(
                    &token::balance_key(&address::xan(), addr),
                    validator
                        .non_staked_balance
                        .try_to_vec()
                        .expect("encode token amount"),
                )
                .expect("Unable to set genesis balance");
        }

        // PoS system depends on epoch being initialized
        let (current_epoch, _gas) = self.storage.get_current_epoch();
        pos::init_genesis_storage(
            &mut self.storage,
            &genesis.pos_params,
            genesis
                .validators
                .iter()
                .map(|validator| &validator.pos_data),
            current_epoch,
        );
        ibc::init_genesis_storage(&mut self.storage);

        let evidence_params =
            self.get_evidence_params(&genesis.parameters, &genesis.pos_params);
        response.consensus_params = Some(ConsensusParams {
            evidence: Some(evidence_params),
            ..response.consensus_params.unwrap_or_default()
        });

        // Set the initial validator set
        for validator in genesis.validators {
            let mut abci_validator =
                tendermint_proto::abci::ValidatorUpdate::default();
            let consensus_key: ed25519_dalek::PublicKey =
                validator.pos_data.consensus_key.clone().into();
            let pub_key = tendermint_proto::crypto::PublicKey {
                sum: Some(tendermint_proto::crypto::public_key::Sum::Ed25519(
                    consensus_key.to_bytes().to_vec(),
                )),
            };
            abci_validator.pub_key = Some(pub_key);
            let power: u64 =
                validator.pos_data.voting_power(&genesis.pos_params).into();
            abci_validator.power = power
                .try_into()
                .expect("unexpected validator's voting power");
            response.validators.push(abci_validator);
        }

        Ok(response)
    }

    /// Load the Merkle root hash and the height of the last committed block, if
    /// any. This is returned when ABCI sends an `info` request.
    pub fn last_state(&self) -> response::Info {
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

    /// Uses `path` in the query to forward the request to the
    /// right query method and returns the result (which may be
    /// the default if `path` is not a supported string.
    /// INVARIANT: This method must be stateless.
    pub fn query(&self, query: request::Query) -> response::Query {
        use rpc::Path;
        match Path::from_str(&query.path) {
            Ok(path) => match path {
                Path::DryRunTx => self.dry_run_tx(&query.data),
                Path::Epoch => {
                    let (epoch, _gas) = self.storage.get_last_epoch();
                    let value = anoma::ledger::storage::types::encode(&epoch);
                    response::Query {
                        value,
                        ..Default::default()
                    }
                }
                Path::Value(storage_key) => {
                    self.read_storage_value(&storage_key)
                }
                Path::Prefix(storage_key) => {
                    self.read_storage_prefix(&storage_key)
                }
                Path::HasKey(storage_key) => self.has_storage_key(&storage_key),
            },
            Err(err) => response::Query {
                code: 1,
                info: format!("RPC error: {}", err),
                ..Default::default()
            },
        }
    }

    /// Begin a new block.
    ///
    /// INVARIANT: Any changes applied in this method must be reverted if the
    /// proposal is rejected (unless we can simply overwrite them in the
    /// next block).
    pub fn prepare_proposal(
        &mut self,
        hash: BlockHash,
        header: Header,
        byzantine_validators: Vec<Evidence>,
    ) {
        let height = BlockHeight(header.height.into());

        // We can safely reset meter, because if the block is rejected, we'll
        // reset again on the next proposal, until the proposal is accepted
        self.gas_meter.reset();

        // The values set will be overwritten if this proposal is rejected.
        self.storage
            .begin_block(hash, height)
            .expect("Beginning a block shouldn't fail");

        // The value set will be overwritten if this proposal is rejected.
        self.storage
            .set_header(header)
            .expect("Setting a header shouldn't fail");

        // The value set will be overwritten if this proposal is rejected.
        self.byzantine_validators = byzantine_validators;
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

    /// INVARIANT: This method must be stateless.
    pub fn verify_header(
        &self,
        _req: shim::request::VerifyHeader,
    ) -> shim::response::VerifyHeader {
        Default::default()
    }

    /// Validate a transaction request. On success, the transaction will
    /// included in the mempool and propagated to peers, otherwise it will be
    /// rejected.
    ///
    /// Checks if the Tx can be deserialized from bytes. Checks the fees and
    /// signatures of the fee payer for a transaction if it is a wrapper tx.
    ///
    /// INVARIANT: Any changes applied in this method must be reverted if the
    /// proposal is rejected (unless we can simply overwrite them in the
    /// next block).
    pub fn process_proposal(
        &mut self,
        req: shim::request::ProcessProposal,
    ) -> shim::response::ProcessProposal {
        let tx = Tx::try_from(req.tx.as_ref()).map_err(Error::TxDecoding);
        // If we could not deserialize the Tx, return an error response
        if let Err(err) = tx {
            return shim::response::ProcessProposal {
                result: err.into(),
                tx: req.tx,
            };
        }

        let (result, processed_tx) = match process_tx(tx.unwrap()) {
            // This occurs if the wrapper tx signature is invalid
            Err(err) => (TxResult::from(err), None),
            Ok(result) => match result {
                // If it is a raw transaction, we do no further validation
                TxType::Raw(_) => (
                    TxResult {
                        code: 0,
                        info: "Process proposal accepted this transaction"
                            .into(),
                    },
                    None,
                ),
                TxType::Wrapper(tx) => {
                    let wrapper = &WrapperTx::try_from(&tx).unwrap();
                    // validate the ciphertext via Ferveo
                    if !wrapper.validate_ciphertext() {
                        (
                            shim::response::TxResult {
                                code: 1,
                                info: "The ciphertext of the wrapped tx is \
                                       invalid"
                                    .into(),
                            },
                            None,
                        )
                    } else {
                        // check that the fee payer has sufficient balance
                        match self.get_balance(
                            &wrapper.fee.token,
                            &wrapper.fee_payer(),
                        ) {
                            Ok(balance) if wrapper.fee.amount <= balance => (
                                shim::response::TxResult {
                                    code: 0,
                                    info: "Process proposal accepted this \
                                           transaction"
                                        .into(),
                                },
                                Some(tx),
                            ),
                            Ok(_) => (
                                shim::response::TxResult {
                                    code: 1,
                                    info: "The address given does not have \
                                           sufficient balance to pay fee"
                                        .into(),
                                },
                                None,
                            ),
                            Err(err) => (
                                shim::response::TxResult { code: 1, info: err },
                                None,
                            ),
                        }
                    }
                }
            },
        };
        shim::response::ProcessProposal {
            result,
            tx: processed_tx.map(|tx| tx.to_bytes()).unwrap_or(req.tx),
        }
    }

    /// Simple helper function for the ledger to get balances
    /// of the specified token at the specified address
    fn get_balance(
        &self,
        token: &Address,
        owner: &Address,
    ) -> std::result::Result<token::Amount, String> {
        let query_resp =
            self.read_storage_value(&token::balance_key(token, owner));
        if query_resp.code != 0 {
            Err("Unable to read balance of the given address".into())
        } else {
            BorshDeserialize::try_from_slice(&query_resp.value[..]).map_err(
                |_| {
                    "Unable to deserialize the balance of the given address"
                        .into()
                },
            )
        }
    }

    pub fn revert_proposal(
        &mut self,
        _req: shim::request::RevertProposal,
    ) -> shim::response::RevertProposal {
        Default::default()
    }

    /// INVARIANT: This method must be stateless.
    pub fn extend_vote(
        &self,
        _req: shim::request::ExtendVote,
    ) -> shim::response::ExtendVote {
        Default::default()
    }

    /// Validate and apply transactions.
    pub fn finalize_block(
        &mut self,
        req: shim::request::FinalizeBlock,
    ) -> Result<shim::response::FinalizeBlock> {
        let header = self
            .storage
            .header
            .as_ref()
            .expect("Header must have been set in prepare_proposal.");
        let height = BlockHeight(header.height.into());
        let time: DateTime<Utc> = header.time.into();
        let time: DateTimeUtc = time.into();
        let new_epoch = self
            .storage
            .update_epoch(height, time)
            .expect("Must be able to update epoch");

        self.slash();

        let mut response = shim::response::FinalizeBlock::default();
        for tx in &req.txs {
            let mut tx_result =
                Event::new_tx_event(EventType::Applied, tx, req.height);
            match protocol::apply_tx(
                tx,
                &mut self.gas_meter,
                &mut self.write_log,
                &self.storage,
            )
            .map_err(Error::TxApply)
            {
                Ok(result) => {
                    if result.is_accepted() {
                        tracing::info!(
                            "all VPs accepted apply_tx storage modification \
                             {:#?}",
                            result
                        );
                        self.write_log.commit_tx();
                        tx_result["code"] = "0".into();
                        match serde_json::to_string(
                            &result.initialized_accounts,
                        ) {
                            Ok(initialized_accounts) => {
                                tx_result["initialized_accounts"] =
                                    initialized_accounts;
                            }
                            Err(err) => {
                                tracing::error!(
                                    "Failed to serialize the initialized \
                                     accounts: {}",
                                    err
                                );
                            }
                        }
                    } else {
                        tracing::info!(
                            "some VPs rejected apply_tx storage modification \
                             {:#?}",
                            result.vps_result.rejected_vps
                        );
                        self.write_log.drop_tx();
                        tx_result["code"] = "1".into();
                    }
                    tx_result["gas_used"] = result.gas_used.to_string();
                    tx_result["info"] = result.to_string();
                }
                Err(msg) => {
                    tracing::info!("Transaction failed with: {}", msg);
                    self.write_log.drop_tx();
                    tx_result["gas_used"] = self
                        .gas_meter
                        .get_current_transaction_gas()
                        .to_string();
                    tx_result["info"] = msg.to_string();
                    tx_result["code"] = "2".into();
                }
            }
            response.events.push(tx_result.into());
        }

        if new_epoch {
            // Apply validator set update
            let (current_epoch, _gas) = self.storage.get_current_epoch();
            // TODO ABCI validator updates on block H affects the validator set
            // on block H+2, do we need to update a block earlier?
            self.storage.validator_set_update(current_epoch, |update| {
                let (consensus_key, power) = match update {
                    ValidatorSetUpdate::Active(ActiveValidator {
                        consensus_key,
                        voting_power,
                    }) => {
                        let power: u64 = voting_power.into();
                        let power: i64 = power
                            .try_into()
                            .expect("unexpected validator's voting power");
                        (consensus_key, power)
                    }
                    ValidatorSetUpdate::Deactivated(consensus_key) => {
                        // Any validators that have become inactive must
                        // have voting power set to 0 to remove them from
                        // the active set
                        let power = 0_i64;
                        (consensus_key, power)
                    }
                };
                let consensus_key: ed25519_dalek::PublicKey =
                    consensus_key.into();
                let pub_key = tendermint_proto::crypto::PublicKey {
                    sum: Some(
                        tendermint_proto::crypto::public_key::Sum::Ed25519(
                            consensus_key.to_bytes().to_vec(),
                        ),
                    ),
                };
                let pub_key = Some(pub_key);
                let update = ValidatorUpdate { pub_key, power };
                response.validator_updates.push(update);
            });

            // Update evidence parameters
            let (parameters, _gas) = parameters::read(&self.storage)
                .expect("Couldn't read protocol parameters");
            let pos_params = self.storage.read_pos_params();
            let evidence_params =
                self.get_evidence_params(&parameters, &pos_params);
            response.consensus_param_updates = Some(ConsensusParams {
                evidence: Some(evidence_params),
                ..response.consensus_param_updates.unwrap_or_default()
            });
        }

        response.gas_used = self
            .gas_meter
            .finalize_transaction()
            .map_err(|_| Error::GasOverflow)?;
        Ok(response)
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
        match protocol::apply_tx(
            tx_bytes,
            &mut gas_meter,
            &mut write_log,
            &self.storage,
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

    /// Query to read a value from storage
    fn read_storage_value(&self, key: &Key) -> response::Query {
        match self.storage.read(key) {
            Ok((Some(value), _gas)) => response::Query {
                value,
                ..Default::default()
            },
            Ok((None, _gas)) => response::Query {
                code: 1,
                info: format!("No value found for key: {}", key),
                ..Default::default()
            },
            Err(err) => response::Query {
                code: 2,
                info: format!("Storage error: {}", err),
                ..Default::default()
            },
        }
    }

    /// Query to read a range of values from storage with a matching prefix. The
    /// value in successful response is a [`Vec<PrefixValue>`] encoded with
    /// [`BorshSerialize`].
    fn read_storage_prefix(&self, key: &Key) -> response::Query {
        let (iter, _gas) = self.storage.iter_prefix(key);
        let mut iter = iter.peekable();
        if iter.peek().is_none() {
            response::Query {
                code: 1,
                info: format!("No value found for key: {}", key),
                ..Default::default()
            }
        } else {
            let values: std::result::Result<
                Vec<PrefixValue>,
                anoma::types::storage::Error,
            > = iter
                .map(|(key, value, _gas)| {
                    let key = Key::parse(key)?;
                    Ok(PrefixValue { key, value })
                })
                .collect();
            match values {
                Ok(values) => {
                    let value = values.try_to_vec().unwrap();
                    response::Query {
                        value,
                        ..Default::default()
                    }
                }
                Err(err) => response::Query {
                    code: 1,
                    info: format!(
                        "Error parsing a storage key {}: {}",
                        key, err
                    ),
                    ..Default::default()
                },
            }
        }
    }

    /// Query to check if a storage key exists.
    fn has_storage_key(&self, key: &Key) -> response::Query {
        match self.storage.has_key(key) {
            Ok((has_key, _gas)) => response::Query {
                value: has_key.try_to_vec().unwrap(),
                ..Default::default()
            },
            Err(err) => response::Query {
                code: 2,
                info: format!("Storage error: {}", err),
                ..Default::default()
            },
        }
    }

    fn get_evidence_params(
        &self,
        protocol_params: &Parameters,
        pos_params: &PosParams,
    ) -> EvidenceParams {
        // Minimum number of epochs before tokens are unbonded and can be
        // withdrawn. This must be greater than 0, otherwise Tendermint won't be
        // happy and will fail updating the consensus parameters.
        let len_before_unbonded = max(pos_params.unbonding_len as i64 - 1, 1);
        let max_age_num_blocks: i64 =
            protocol_params.epoch_duration.min_num_of_blocks as i64
                * len_before_unbonded;
        let min_duration_secs =
            protocol_params.epoch_duration.min_duration.0 as i64;
        let max_age_duration =
            Some(tendermint_proto::google::protobuf::Duration {
                seconds: min_duration_secs * len_before_unbonded,
                nanos: 0,
            });
        EvidenceParams {
            max_age_num_blocks,
            max_age_duration,
            ..EvidenceParams::default()
        }
    }
}

trait HashMapExt<K, V>
where
    K: Eq + Hash,
    V: Clone,
{
    /// Inserts a value computed from `f` into the map if the given `key` is not
    /// present, then returns a clone of the value from the map.
    fn get_or_insert_with(&mut self, key: K, f: impl FnOnce() -> V) -> V;
}

impl<K, V> HashMapExt<K, V> for HashMap<K, V>
where
    K: Eq + Hash,
    V: Clone,
{
    fn get_or_insert_with(&mut self, key: K, f: impl FnOnce() -> V) -> V {
        use std::collections::hash_map::Entry;
        match self.entry(key) {
            Entry::Occupied(o) => o.get().clone(),
            Entry::Vacant(v) => v.insert(f()).clone(),
        }
    }
}
