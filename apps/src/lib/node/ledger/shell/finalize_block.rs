//! Implementation of the [`FinalizeBlock`] ABCI++ method for the Shell

use anoma::types::storage::BlockHash;
#[cfg(not(feature = "ABCI"))]
use tendermint::block::Header;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::abci::Evidence;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::crypto::{public_key, PublicKey as TendermintPublicKey};
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::Evidence;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::crypto::{
    public_key, PublicKey as TendermintPublicKey,
};
#[cfg(feature = "ABCI")]
use tendermint_stable::block::Header;

use super::*;
use crate::node::ledger::events::EventType;

impl<D, H> Shell<D, H>
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    /// Updates the chain with new header, height, etc. Also keeps track
    /// of epoch changes and applies associated updates to validator sets,
    /// etc. as necessary.
    ///
    /// Validate and apply decrypted transactions unless [`process_proposal`]
    /// detected that they were not submitted in correct order or more
    /// decrypted txs arrived than expected. In that case, all decrypted
    /// transactions are not applied and must be included in the next
    /// [`prepare_proposal`] call.
    ///
    /// Incoming wrapper txs need no further validation. They
    /// are added to the block.
    ///
    /// Error codes:
    ///   0: Ok
    ///   1: Invalid tx
    ///   2: Tx is invalidly signed
    ///   3: Wasm runtime error
    ///   4: Invalid order of decrypted txs
    ///   5. More decrypted txs than expected
    pub fn finalize_block(
        &mut self,
        req: shim::request::FinalizeBlock,
    ) -> Result<shim::response::FinalizeBlock> {
        let mut response = shim::response::FinalizeBlock::default();
        // begin the next block and check if a new epoch began
        let (height, new_epoch) =
            self.update_state(req.header, req.hash, req.byzantine_validators);

        for processed_tx in &req.txs {
            let tx = if let Ok(tx) = Tx::try_from(processed_tx.tx.as_ref()) {
                tx
            } else {
                tracing::error!(
                    "Internal logic error: FinalizeBlock received a tx that could not \
                     be deserialized to a Tx type"
                    );
                continue
            };
            let tx_length = processed_tx.tx.len();
            if ErrorCodes::from_u32(tx.result.code).unwrap()
                == ErrorCodes::InvalidSig
            {

                if let Ok(TxType::Wrapper(wrapper)) = TxType::try_from(inner_tx)
                {
                    let mut tx_result =
                        Event::new_tx_event(&TxType::Wrapper(wrapper), height.0);
                    tx_result["code"] = processed_tx.result.code.to_string();
                    tx_result["info"] = format!("Tx rejected: {}", &processed_tx.result.info);
                    tx_result["gas_used"] = "0".into();
                    response.events.push(tx_result.into());
                    continue;
                } else {
                    tracing::error!(
                        "Internal logic error: FinalizeBlock received a tx with an invalid \
                        signature error code that could not be deserialized to a WrapperTx type"
                    );
                    continue
                }
            }

            let tx_type = if let Ok(tx_type) = process_tx(tx) {
                tx_type
            } else {
                tracing::error!(
                    "Internal logic error: FinalizeBlock received tx that could not be \
                    deserialized to a valid TxType"
                );
                continue
            };
            // If [`process_proposal`] rejected a Tx, emit an event here and
            // move on to next tx
            // If we are rejecting all decrypted txs because they were submitted
            // in an incorrect order, we do that later.
            if ErrorCodes::from_u32(processed_tx.result.code).unwrap() != ErrorCodes::Ok
                && !req.reject_all_decrypted
            {
                let mut tx_result =
                    Event::new_tx_event(&tx_type, height.0);
                tx_result["code"] = processed_tx.result.code.to_string();
                tx_result["info"] = format!("Tx rejected: {}", &processed_tx.result.info);
                tx_result["gas_used"] = "0".into();
                response.events.push(tx_result.into());
                // if the rejected tx was decrypted, remove it
                // from the queue of txs to be processed
                if let TxType::Decrypted(_) = &tx_type {
                    self.tx_queue.pop();
                }
                continue;
            }

            let mut tx_result = match &tx_type {
                TxType::Wrapper(_wrapper) => {
                    if !cfg!(feature = "ABCI") {
                        self.tx_queue.push(_wrapper.clone());
                    }
                    Event::new_tx_event(&tx_type, height.0)
                }
                TxType::Decrypted(_) => {
                    // If [`process_proposal`] detected that decrypted txs were
                    // submitted out of order, we apply none
                    // of those. New encrypted txs may still
                    // be accepted.
                    if req.reject_all_decrypted {
                        let mut tx_result =
                            Event::new_tx_event(&tx_type, height.0);
                        tx_result["code"] = ErrorCodes::InvalidOrder.into();
                        tx_result["info"] = "All decrypted txs rejected as \
                                             they were not submitted in \
                                             correct order"
                            .into();
                        tx_result["gas_used"] = "0".into();
                        response.events.push(tx_result.into());
                        continue;
                    }
                    // We remove the corresponding wrapper tx from the queue
                    if !cfg!(feature = "ABCI") {
                        self.tx_queue.pop();
                    }
                    Event::new_tx_event(&tx_type, height.0)
                }
                TxType::Protocol(protocol_tx) => {
                    todo!()
                }
                TxType::Raw(_) => {
                    tracing::error!(
                        "Internal logic error: FinalizeBlock received a TxType::Raw transaction"
                    );
                    continue
                },
            };

            match protocol::apply_tx(
                tx_type,
                tx_length,
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
                        tx_result["code"] = ErrorCodes::Ok.into();
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
                        tx_result["code"] = ErrorCodes::InvalidTx.into();
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
                    tx_result["code"] = ErrorCodes::WasmRuntimeError.into();
                }
            }
            response.events.push(tx_result.into());
        }

        if new_epoch {
            self.update_epoch(&mut response);
            self.update_dkg();
        }

        response.gas_used = self
            .gas_meter
            .finalize_transaction()
            .map_err(|_| Error::GasOverflow)?;
        self.reset_queue();
        Ok(response)
    }

    /// Sets the metadata necessary for a new block, including
    /// the hash, height, validator changes, and evidence of
    /// byzantine behavior. Applies slashes if necessary.
    /// Returns a bool indicating if a new epoch began and
    /// the height of the new block.
    fn update_state(
        &mut self,
        header: Header,
        hash: BlockHash,
        byzantine_validators: Vec<Evidence>,
    ) -> (BlockHeight, bool) {
        let height = BlockHeight(header.height.into());

        self.gas_meter.reset();

        self.storage
            .begin_block(hash, height)
            .expect("Beginning a block shouldn't fail");

        self.storage
            .set_header(header)
            .expect("Setting a header shouldn't fail");

        self.byzantine_validators = byzantine_validators;

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
        (height, new_epoch)
    }

    /// If a new epoch begins, we update the response to include
    /// changes to the validator sets and consensus parameters
    fn update_epoch(&self, response: &mut shim::response::FinalizeBlock) {
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
            let consensus_key: ed25519_dalek::PublicKey = consensus_key.into();
            let pub_key = TendermintPublicKey {
                sum: Some(public_key::Sum::Ed25519(
                    consensus_key.to_bytes().to_vec(),
                )),
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
            ..response.consensus_param_updates.take().unwrap_or_default()
        });
    }

    fn update_dkg(&mut self) {
        use tendermint::validator::Set;
        let (current_epoch, _) = self.storage.get_current_epoch();
        // TODO: The total weight should likely be a config, not hardcoded.
        let params = DkgParams {
            tau: current_epoch.0,
            security_threshold: 2^12 / 3,
            total_weight: 2^12,
        };
        // get the new validator for the next epoch
        let validators = self.storage.read_validator_set();
        let validators = validators
            .get(current_epoch + 1)
            .expect("Validators for the next epoch should be known");
        self.dkg =  DkgStateMachine::new(

        )

    }
}

/// We test the failure cases of [`finalize_block`]. The happy flows
/// are covered by the e2e tests.
#[cfg(test)]
mod test_finalize_block {
    use anoma::types::address::xan;
    use anoma::types::storage::Epoch;
    use anoma::types::transaction::Fee;
    #[cfg(not(feature = "ABCI"))]
    use tendermint::block::header::Version;
    #[cfg(not(feature = "ABCI"))]
    use tendermint::{Hash, Time};
    #[cfg(feature = "ABCI")]
    use tendermint_stable::block::header::Version;
    #[cfg(feature = "ABCI")]
    use tendermint_stable::{Hash, Time};

    use super::*;
    use crate::node::ledger::shell::test_utils::*;
    use crate::node::ledger::shims::abcipp_shim_types::shim::request::{
        FinalizeBlock, ProcessedTx,
    };

    /// This is just to be used in testing. It is not
    /// a meaningful default.
    impl Default for FinalizeBlock {
        fn default() -> Self {
            FinalizeBlock {
                hash: BlockHash([0u8; 32]),
                header: Header {
                    version: Version { block: 0, app: 0 },
                    chain_id: String::from("test")
                        .try_into()
                        .expect("Should not fail"),
                    height: 0u64.try_into().expect("Should not fail"),
                    time: Time::now(),
                    last_block_id: None,
                    last_commit_hash: None,
                    data_hash: None,
                    validators_hash: Hash::None,
                    next_validators_hash: Hash::None,
                    consensus_hash: Hash::None,
                    app_hash: Vec::<u8>::new()
                        .try_into()
                        .expect("Should not fail"),
                    last_results_hash: None,
                    evidence_hash: None,
                    proposer_address: vec![0u8; 20]
                        .try_into()
                        .expect("Should not fail"),
                },
                byzantine_validators: vec![],
                txs: vec![],
                reject_all_decrypted: false,
            }
        }
    }

    #[cfg(not(feature = "ABCI"))]
    /// Check that if a wrapper tx was rejected by [`process_proposal`],
    /// check that the correct event is returned. Check that it does
    /// not appear in the queue of txs to be decrypted
    #[test]
    fn test_process_proposal_rejected_wrapper_tx() {
        let mut shell = setup();
        let keypair = gen_keypair();
        let mut processed_txs = vec![];
        let mut valid_wrappers = vec![];
        // create some wrapper txs
        for i in 1..5 {
            let raw_tx = Tx::new(
                "wasm_code".as_bytes().to_owned(),
                Some(format!("transaction data: {}", i).as_bytes().to_owned()),
            );
            let wrapper = WrapperTx::new(
                Fee {
                    amount: i.into(),
                    token: xan(),
                },
                &keypair,
                Epoch(0),
                0.into(),
                raw_tx.clone(),
            );
            let tx = wrapper.sign(&keypair).expect("Test failed");
            if i > 1 {
                processed_txs.push(ProcessedTx {
                    tx: tx.to_bytes(),
                    result: TxResult {
                        code: u32::try_from(i.rem_euclid(2))
                            .expect("Test failed"),
                        info: "".into(),
                    },
                });
            } else {
                shell.enqueue_tx(wrapper.clone());
            }

            if i != 3 {
                valid_wrappers.push(wrapper)
            }
        }

        // check that the correct events were created
        for (index, event) in shell
            .finalize_block(FinalizeBlock {
                txs: processed_txs.clone(),
                reject_all_decrypted: false,
                ..Default::default()
            })
            .expect("Test failed")
            .iter()
            .enumerate()
        {
            assert_eq!(event.r#type, "accepted");
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key.as_str() == "code")
                .expect("Test failed")
                .value
                .as_str();
            assert_eq!(code, &index.rem_euclid(2).to_string());
        }
        // verify that the queue of wrapper txs to be processed is correct
        let mut valid_tx = valid_wrappers.iter();
        let mut counter = 0;
        while let Some(wrapper) = shell.next_wrapper() {
            // we cannot easily implement the PartialEq trait for WrapperTx
            // so we check the hashes of the inner txs for equality
            assert_eq!(
                wrapper.tx_hash,
                valid_tx.next().expect("Test failed").tx_hash
            );
            counter += 1;
        }
        assert_eq!(counter, 3);
    }

    #[cfg(feature = "ABCI")]
    /// Check that if a wrapper tx was rejected by [`process_proposal`],
    /// check that the correct event is returned.
    #[test]
    fn test_process_proposal_rejected_wrapper_tx() {
        let mut shell = setup();
        let keypair = gen_keypair();
        let mut processed_txs = vec![];
        // create some wrapper txs
        for i in 1..5 {
            let raw_tx = Tx::new(
                "wasm_code".as_bytes().to_owned(),
                Some(format!("transaction data: {}", i).as_bytes().to_owned()),
            );
            let wrapper = WrapperTx::new(
                Fee {
                    amount: i.into(),
                    token: xan(),
                },
                &keypair,
                Epoch(0),
                0.into(),
                raw_tx.clone(),
            );
            let tx = wrapper.sign(&keypair).expect("Test failed");
            if i > 1 {
                processed_txs.push(ProcessedTx {
                    tx: tx.to_bytes(),
                    result: TxResult {
                        code: u32::try_from(i.rem_euclid(2))
                            .expect("Test failed"),
                        info: "".into(),
                    },
                });
            }
        }

        // check that the correct events were created
        for (index, event) in shell
            .finalize_block(FinalizeBlock {
                txs: processed_txs.clone(),
                reject_all_decrypted: false,
                ..Default::default()
            })
            .expect("Test failed")
            .iter()
            .enumerate()
        {
            assert_eq!(event.r#type, "applied");
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key == "code".as_bytes())
                .expect("Test failed")
                .value
                .clone();
            assert_eq!(
                String::from_utf8(code).expect("Test failed"),
                index.rem_euclid(2).to_string()
            );
        }
    }

    #[cfg(not(feature = "ABCI"))]
    /// Check that if a decrypted tx was rejected by [`process_proposal`],
    /// check that the correct event is returned. Check that it is still
    /// removed from the queue of txs to be included in the next block
    /// proposal
    #[test]
    fn test_process_proposal_rejected_decrypted_tx() {
        let mut shell = setup();
        let keypair = gen_keypair();
        let raw_tx = Tx::new(
            "wasm_code".as_bytes().to_owned(),
            Some(String::from("transaction data").as_bytes().to_owned()),
        );
        let wrapper = WrapperTx::new(
            Fee {
                amount: 0.into(),
                token: xan(),
            },
            &keypair,
            Epoch(0),
            0.into(),
            raw_tx.clone(),
        );

        let processed_tx = ProcessedTx {
            tx: Tx::from(TxType::Decrypted(DecryptedTx::Decrypted(raw_tx)))
                .to_bytes(),
            result: TxResult {
                code: ErrorCodes::InvalidTx.into(),
                info: "".into(),
            },
        };
        shell.enqueue_tx(wrapper);

        // check that the decrypted tx was not applied
        for event in shell
            .finalize_block(FinalizeBlock {
                txs: vec![processed_tx],
                reject_all_decrypted: false,
                ..Default::default()
            })
            .expect("Test failed")
        {
            assert_eq!(event.r#type, "applied");
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key.as_str() == "code")
                .expect("Test failed")
                .value
                .as_str();
            assert_eq!(code, String::from(ErrorCodes::InvalidTx).as_str());
        }
        // check that the corresponding wrapper tx was removed from the queue
        assert!(shell.next_wrapper().is_none());
    }

    #[cfg(feature = "ABCI")]
    /// Check that if a decrypted tx was rejected by [`process_proposal`],
    /// check that the correct event is returned.
    #[test]
    fn test_process_proposal_rejected_decrypted_tx() {
        let mut shell = setup();
        let raw_tx = Tx::new(
            "wasm_code".as_bytes().to_owned(),
            Some(String::from("transaction data").as_bytes().to_owned()),
        );
        let processed_tx = ProcessedTx {
            tx: Tx::from(TxType::Decrypted(DecryptedTx::Decrypted(raw_tx)))
                .to_bytes(),
            result: TxResult {
                code: ErrorCodes::InvalidTx.into(),
                info: "".into(),
            },
        };

        // check that the decrypted tx was not applied
        for event in shell
            .finalize_block(FinalizeBlock {
                txs: vec![processed_tx],
                reject_all_decrypted: false,
                ..Default::default()
            })
            .expect("Test failed")
        {
            assert_eq!(event.r#type, "applied");
            let code = event
                .attributes
                .iter()
                .find(|attr| attr.key == "code".as_bytes())
                .expect("Test failed")
                .value
                .clone();
            assert_eq!(
                String::from_utf8(code).expect("Test failed"),
                String::from(ErrorCodes::InvalidTx)
            );
        }
        // check that the corresponding wrapper tx was removed from the queue
        assert!(shell.next_wrapper().is_none());
    }

    /// Test that the wrapper txs are queued in the order they
    /// are received from the block. Tests that the previously
    /// decrypted txs are de-queued.
    #[test]
    fn test_mixed_txs_queued_in_correct_order() {
        let mut shell = setup();
        let keypair = gen_keypair();
        let mut processed_txs = vec![];
        let mut valid_txs = vec![];

        // create two decrypted txs
        let mut wasm_path = top_level_directory();
        wasm_path.push("wasm_for_tests/tx_no_op.wasm");
        let tx_code = std::fs::read(wasm_path)
            .expect("Expected a file at given code path");
        for i in 0..2 {
            let raw_tx = Tx::new(
                tx_code.clone(),
                Some(
                    format!("Decrypted transaction data: {}", i)
                        .as_bytes()
                        .to_owned(),
                ),
            );
            let wrapper_tx = WrapperTx::new(
                Fee {
                    amount: 0.into(),
                    token: xan(),
                },
                &keypair,
                Epoch(0),
                0.into(),
                raw_tx.clone(),
            );
            shell.enqueue_tx(wrapper_tx);
            processed_txs.push(ProcessedTx {
                tx: Tx::from(TxType::Decrypted(DecryptedTx::Decrypted(raw_tx)))
                    .to_bytes(),
                result: TxResult {
                    code: ErrorCodes::Ok.into(),
                    info: "".into(),
                },
            });
        }
        // create two wrapper txs
        for i in 0..2 {
            let raw_tx = Tx::new(
                "wasm_code".as_bytes().to_owned(),
                Some(
                    format!("Encrypted transaction data: {}", i)
                        .as_bytes()
                        .to_owned(),
                ),
            );
            let wrapper_tx = WrapperTx::new(
                Fee {
                    amount: 0.into(),
                    token: xan(),
                },
                &keypair,
                Epoch(0),
                0.into(),
                raw_tx.clone(),
            );
            let wrapper = wrapper_tx.sign(&keypair).expect("Test failed");
            valid_txs.push(wrapper_tx);
            processed_txs.push(ProcessedTx {
                tx: wrapper.to_bytes(),
                result: TxResult {
                    code: ErrorCodes::Ok.into(),
                    info: "".into(),
                },
            });
        }
        // Put the wrapper txs in front of the decrypted txs
        processed_txs.rotate_left(2);
        // check that the correct events were created
        for (index, event) in shell
            .finalize_block(FinalizeBlock {
                txs: processed_txs,
                reject_all_decrypted: false,
                ..Default::default()
            })
            .expect("Test failed")
            .iter()
            .enumerate()
        {
            if index < 2 {
                // these should be accepted wrapper txs

                #[cfg(not(feature = "ABCI"))]
                {
                    assert_eq!(event.r#type, "accepted");
                    let code = event
                        .attributes
                        .iter()
                        .find(|attr| attr.key.as_str() == "code")
                        .expect("Test failed")
                        .value
                        .as_str();
                    assert_eq!(code, String::from(ErrorCodes::Ok).as_str());
                }
                #[cfg(feature = "ABCI")]
                {
                    assert_eq!(event.r#type, "applied");
                    let code = event
                        .attributes
                        .iter()
                        .find(|attr| attr.key == "code".as_bytes())
                        .expect("Test failed")
                        .value
                        .clone();
                    assert_eq!(
                        String::from_utf8(code).expect("Test failed"),
                        String::from(ErrorCodes::Ok)
                    );
                }
            } else {
                // these should be accepted decrypted txs
                assert_eq!(event.r#type, "applied");
                #[cfg(not(feature = "ABCI"))]
                {
                    let code = event
                        .attributes
                        .iter()
                        .find(|attr| attr.key.as_str() == "code")
                        .expect("Test failed")
                        .value
                        .as_str();
                    assert_eq!(code, String::from(ErrorCodes::Ok).as_str());
                }
                #[cfg(feature = "ABCI")]
                {
                    let code = event
                        .attributes
                        .iter()
                        .find(|attr| attr.key == "code".as_bytes())
                        .expect("Test failed")
                        .value
                        .clone();
                    assert_eq!(
                        String::from_utf8(code).expect("Test failed"),
                        String::from(ErrorCodes::Ok)
                    );
                }
            }
        }

        #[cfg(not(feature = "ABCI"))]
        {
            // check that the applied decrypted txs were dequeued and the
            // accepted wrappers were enqueued in correct order
            let mut txs = valid_txs.iter();

            let mut counter = 0;
            while let Some(wrapper) = shell.next_wrapper() {
                assert_eq!(
                    wrapper.tx_hash,
                    txs.next().expect("Test failed").tx_hash
                );
                counter += 1;
            }
            assert_eq!(counter, 2);
        }
    }

    #[cfg(not(feature = "ABCI"))]
    /// Tests that if the decrypted txs are submitted out of
    /// order then
    ///  1. They are still enqueued in order
    ///  2. New wrapper txs are enqueued in correct order
    #[test]
    fn test_decrypted_txs_out_of_order() {
        let mut shell = setup();
        let keypair = gen_keypair();
        let mut processed_txs = vec![];
        let mut valid_txs = vec![];
        // create a wrapper tx to be included in block proposal
        let raw_tx = Tx::new(
            "wasm_code".as_bytes().to_owned(),
            Some(String::from("transaction data").as_bytes().to_owned()),
        );
        let wrapper_tx = WrapperTx::new(
            Fee {
                amount: 0.into(),
                token: xan(),
            },
            &keypair,
            Epoch(0),
            0.into(),
            raw_tx,
        );
        let wrapper = wrapper_tx.sign(&keypair).expect("Test failed");
        valid_txs.push(wrapper_tx);
        processed_txs.push(ProcessedTx {
            tx: wrapper.to_bytes(),
            result: TxResult {
                code: ErrorCodes::Ok.into(),
                info: "".into(),
            },
        });
        // Create two decrypted txs to be part of block proposal.
        // We give them an error code of two to indicate that order
        // was not respected (although actually it was, but the job
        // of detecting this lies with process_proposal so at this stage
        // we can just lie to finalize_block to get the desired behavior)
        for i in 0..2 {
            let raw_tx = Tx::new(
                "wasm_code".as_bytes().to_owned(),
                Some(format!("transaction data: {}", i).as_bytes().to_owned()),
            );
            let wrapper = WrapperTx::new(
                Fee {
                    amount: 0.into(),
                    token: xan(),
                },
                &keypair,
                Epoch(0),
                0.into(),
                raw_tx.clone(),
            );
            // add the corresponding wrapper tx to the queue
            shell.enqueue_tx(wrapper.clone());
            valid_txs.push(wrapper);
            processed_txs.push(ProcessedTx {
                tx: Tx::from(TxType::Decrypted(DecryptedTx::Decrypted(raw_tx)))
                    .to_bytes(),
                result: TxResult {
                    code: ErrorCodes::InvalidOrder.into(),
                    info: "".into(),
                },
            })
        }
        // We tell [`finalize_block`] that the decrypted txs are out of
        // order although in fact they are not. This should not affect
        // the expected behavior
        // We check that the correct events are created.
        for (index, event) in shell
            .finalize_block(FinalizeBlock {
                txs: processed_txs.clone(),
                reject_all_decrypted: true,
                ..Default::default()
            })
            .expect("Test failed")
            .iter()
            .enumerate()
        {
            if index == 0 {
                // the wrapper tx should be accepted
                assert_eq!(event.r#type, "accepted");
                #[cfg(not(feature = "ABCI"))]
                {
                    let code = event
                        .attributes
                        .iter()
                        .find(|attr| attr.key.as_str() == "code")
                        .expect("Test failed")
                        .value
                        .as_str();
                    assert_eq!(code, String::from(ErrorCodes::Ok).as_str());
                }
                #[cfg(feature = "ABCI")]
                {
                    let code = event
                        .attributes
                        .iter()
                        .find(|attr| attr.key == "code".as_bytes())
                        .expect("Test failed")
                        .value
                        .clone();
                    assert_eq!(
                        String::from_utf8(code).expect("Test failed"),
                        String::from(ErrorCodes::Ok)
                    );
                }
            } else {
                // both decrypted txs should be rejected
                assert_eq!(event.r#type, "applied");
                #[cfg(not(feature = "ABCI"))]
                {
                    let code = event
                        .attributes
                        .iter()
                        .find(|attr| attr.key.as_str() == "code")
                        .expect("Test failed")
                        .value
                        .as_str();
                    assert_eq!(
                        code,
                        String::from(ErrorCodes::InvalidOrder).as_str()
                    );
                }
                #[cfg(feature = "ABCI")]
                {
                    let code = event
                        .attributes
                        .iter()
                        .find(|attr| attr.key == "code".as_bytes())
                        .expect("Test failed")
                        .value
                        .clone();
                    assert_eq!(
                        String::from_utf8(code).expect("Test failed"),
                        String::from(ErrorCodes::InvalidOrder)
                    );
                }
            }
        }
        // the wrapper tx should appear at the end of the queue
        valid_txs.rotate_left(1);
        // check that the queue has 3 wrappers in correct order
        let mut counter = 0;
        let mut txs = valid_txs.iter();
        while let Some(wrapper) = shell.next_wrapper() {
            assert_eq!(
                wrapper.tx_hash,
                txs.next().expect("Test failed").tx_hash
            );
            counter += 1;
        }
        assert_eq!(counter, 3);
    }
}
