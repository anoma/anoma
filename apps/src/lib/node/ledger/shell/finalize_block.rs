//! Implementation of the `FinalizeBlock` ABCI++ method for the Shell

use anoma::ledger::governance::storage as gov_storage;
use anoma::ledger::governance::utils::{
    compute_tally, get_proposal_votes, ProposalEvent,
};
use anoma::ledger::governance::vp::ADDRESS as gov_address;
use anoma::ledger::storage::types::encode;
use anoma::ledger::treasury::ADDRESS as treasury_address;
use anoma::types::address::{xan as m1t, Address};
use anoma::types::governance::TallyResult;
use anoma::types::storage::{BlockHash, Epoch};
use anoma::types::transaction::protocol::ProtocolTxType;
#[cfg(not(feature = "ABCI"))]
use tendermint::block::Header;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::abci::Evidence;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::crypto::PublicKey as TendermintPublicKey;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::Evidence;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::crypto::PublicKey as TendermintPublicKey;
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
    /// Validate and apply decrypted transactions unless
    /// [`Shell::process_proposal`] detected that they were not submitted in
    /// correct order or more decrypted txs arrived than expected. In that
    /// case, all decrypted transactions are not applied and must be
    /// included in the next `Shell::prepare_proposal` call.
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

        if new_epoch {
            for id in std::mem::take(&mut self.proposal_data) {
                let proposal_funds_key = gov_storage::get_funds_key(id);
                let proposal_start_epoch_key =
                    gov_storage::get_voting_start_epoch_key(id);

                let funds = self
                    .read_storage_key::<token::Amount>(&proposal_funds_key)
                    .ok_or_else(|| {
                        Error::BadProposal(
                            id,
                            "Invalid proposal funds.".to_string(),
                        )
                    })?;
                let proposal_start_epoch = self
                    .read_storage_key::<Epoch>(&proposal_start_epoch_key)
                    .ok_or_else(|| {
                        Error::BadProposal(
                            id,
                            "Invalid proposal start_epoch.".to_string(),
                        )
                    })?;

                let votes =
                    get_proposal_votes(&self.storage, proposal_start_epoch, id);
                let tally_result =
                    compute_tally(&self.storage, proposal_start_epoch, votes);

                let transfer_address = match tally_result {
                    TallyResult::Passed => {
                        let proposal_author_key =
                            gov_storage::get_author_key(id);
                        let proposal_author = self
                            .read_storage_key::<Address>(&proposal_author_key)
                            .ok_or_else(|| {
                                Error::BadProposal(
                                    id,
                                    "Invalid proposal author.".to_string(),
                                )
                            })?;

                        let proposal_code_key =
                            gov_storage::get_proposal_code_key(id);
                        let proposal_code =
                            self.read_storage_key_bytes(&proposal_code_key);
                        match proposal_code {
                            Some(proposal_code) => {
                                let tx =
                                    Tx::new(proposal_code, Some(encode(&id)));
                                let tx_type = TxType::Decrypted(
                                    DecryptedTx::Decrypted(tx),
                                );
                                let pending_execution_key =
                                    gov_storage::get_proposal_execution_key(id);
                                self.storage
                                    .write(&pending_execution_key, "")
                                    .expect(
                                        "Should be able to write to storage.",
                                    );
                                let tx_result = protocol::apply_tx(
                                    tx_type,
                                    0, /*  this is used to compute the fee
                                        * based on the code size. We dont
                                        * need it here. */
                                    &mut BlockGasMeter::default(),
                                    &mut self.write_log,
                                    &self.storage,
                                    &mut self.vp_wasm_cache,
                                    &mut self.tx_wasm_cache,
                                );
                                self.storage
                                    .delete(&pending_execution_key)
                                    .expect(
                                        "Should be able to delete the storage.",
                                    );
                                match tx_result {
                                    Ok(tx_result) => {
                                        if tx_result.is_accepted() {
                                            self.write_log.commit_tx();
                                            let proposal_event: Event =
                                                ProposalEvent::new(
                                                    EventType::Proposal
                                                        .to_string(),
                                                    TallyResult::Passed,
                                                    id,
                                                    true,
                                                    true,
                                                )
                                                .into();
                                            response
                                                .events
                                                .push(proposal_event.into());

                                            proposal_author
                                        } else {
                                            self.write_log.drop_tx();
                                            let proposal_event: Event =
                                                ProposalEvent::new(
                                                    EventType::Proposal
                                                        .to_string(),
                                                    TallyResult::Passed,
                                                    id,
                                                    true,
                                                    false,
                                                )
                                                .into();
                                            response
                                                .events
                                                .push(proposal_event.into());

                                            treasury_address
                                        }
                                    }
                                    Err(_e) => {
                                        self.write_log.drop_tx();
                                        let proposal_event: Event =
                                            ProposalEvent::new(
                                                EventType::Proposal.to_string(),
                                                TallyResult::Passed,
                                                id,
                                                true,
                                                false,
                                            )
                                            .into();
                                        response
                                            .events
                                            .push(proposal_event.into());

                                        treasury_address
                                    }
                                }
                            }
                            None => {
                                let proposal_event: Event = ProposalEvent::new(
                                    EventType::Proposal.to_string(),
                                    TallyResult::Passed,
                                    id,
                                    false,
                                    false,
                                )
                                .into();
                                response.events.push(proposal_event.into());

                                proposal_author
                            }
                        }
                    }
                    TallyResult::Rejected | TallyResult::Unknown => {
                        let proposal_event: Event = ProposalEvent::new(
                            EventType::Proposal.to_string(),
                            TallyResult::Rejected,
                            id,
                            false,
                            false,
                        )
                        .into();
                        response.events.push(proposal_event.into());

                        treasury_address
                    }
                };

                // transfer proposal locked funds
                self.storage.transfer(
                    &m1t(),
                    funds,
                    &gov_address,
                    &transfer_address,
                );
            }
        }

        for processed_tx in &req.txs {
            let tx = if let Ok(tx) = Tx::try_from(processed_tx.tx.as_ref()) {
                tx
            } else {
                tracing::error!(
                    "Internal logic error: FinalizeBlock received a tx that \
                     could not be deserialized to a Tx type"
                );
                continue;
            };
            let tx_length = processed_tx.tx.len();
            // If [`process_proposal`] rejected a Tx due to invalid signature,
            // emit an event here and move on to next tx. If we are
            // rejecting all decrypted txs because they were
            // submitted in an incorrect order, we do that later.
            if ErrorCodes::from_u32(processed_tx.result.code).unwrap()
                == ErrorCodes::InvalidSig
            {
                let mut tx_result = match process_tx(tx.clone()) {
                    Ok(tx @ TxType::Wrapper(_))
                    | Ok(tx @ TxType::Protocol(_)) => {
                        Event::new_tx_event(&tx, height.0)
                    }
                    _ => match TxType::try_from(tx) {
                        Ok(tx @ TxType::Wrapper(_))
                        | Ok(tx @ TxType::Protocol(_)) => {
                            Event::new_tx_event(&tx, height.0)
                        }
                        _ => {
                            tracing::error!(
                                "Internal logic error: FinalizeBlock received \
                                 a tx with an invalid signature error code \
                                 that could not be deserialized to a \
                                 WrapperTx / ProtocolTx type"
                            );
                            continue;
                        }
                    },
                };
                tx_result["code"] = processed_tx.result.code.to_string();
                tx_result["info"] =
                    format!("Tx rejected: {}", &processed_tx.result.info);
                tx_result["gas_used"] = "0".into();
                response.events.push(tx_result.into());
                continue;
            }

            let tx_type = if let Ok(tx_type) = process_tx(tx) {
                tx_type
            } else {
                tracing::error!(
                    "Internal logic error: FinalizeBlock received tx that \
                     could not be deserialized to a valid TxType"
                );
                continue;
            };
            // If [`process_proposal`] rejected a Tx, emit an event here and
            // move on to next tx
            // If we are rejecting all decrypted txs because they were submitted
            // in an incorrect order, we do that later.
            if ErrorCodes::from_u32(processed_tx.result.code).unwrap()
                != ErrorCodes::Ok
                && !req.reject_all_decrypted
            {
                let mut tx_result = Event::new_tx_event(&tx_type, height.0);
                tx_result["code"] = processed_tx.result.code.to_string();
                tx_result["info"] =
                    format!("Tx rejected: {}", &processed_tx.result.info);
                tx_result["gas_used"] = "0".into();
                response.events.push(tx_result.into());
                // if the rejected tx was decrypted, remove it
                // from the queue of txs to be processed
                if let TxType::Decrypted(_) = &tx_type {
                    self.storage.tx_queue.pop();
                }
                continue;
            }

            let mut tx_result = match &tx_type {
                TxType::Wrapper(_wrapper) => {
                    if !cfg!(feature = "ABCI") {
                        self.storage.tx_queue.push(_wrapper.clone());
                    }
                    Event::new_tx_event(&tx_type, height.0)
                }
                TxType::Decrypted(inner) => {
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
                        self.storage.tx_queue.pop();
                    }
                    let mut event = Event::new_tx_event(&tx_type, height.0);
                    if let DecryptedTx::Undecryptable(_) = inner {
                        event["log"] =
                            "Transaction could not be decrypted.".into();
                        event["code"] = ErrorCodes::Undecryptable.into();
                    }
                    event
                }
                TxType::Protocol(protocol_tx) => match protocol_tx.tx {
                    ProtocolTxType::EthereumBridgeUpdate(_) => {
                        Event::new_tx_event(&tx_type, height.0)
                    }
                    _ => {
                        tracing::error!(
                            "Internal logic error: FinalizeBlock received an \
                             unsupported TxType::Protocol transaction"
                        );
                        continue;
                    }
                },
                TxType::Raw(_) => {
                    tracing::error!(
                        "Internal logic error: FinalizeBlock received a \
                         TxType::Raw transaction"
                    );
                    continue;
                }
            };

            match protocol::apply_tx(
                tx_type,
                tx_length,
                &mut self.gas_meter,
                &mut self.write_log,
                &self.storage,
                &mut self.vp_wasm_cache,
                &mut self.tx_wasm_cache,
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
                        if !tx_result.contains_key("code") {
                            tx_result["code"] = ErrorCodes::Ok.into();
                        }
                        if let Some(ibc_event) = &result.ibc_event {
                            // Add the IBC event besides the tx_result
                            let event = Event::from(ibc_event.clone());
                            response.events.push(event.into());
                        }
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
        self.reset_tx_queue_iter();

        if new_epoch {
            self.update_epoch(&mut response);
        }

        response.gas_used = self
            .gas_meter
            .finalize_transaction()
            .map_err(|_| Error::GasOverflow)?;
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
        let time: DateTimeUtc = header
            .time
            .try_into()
            .expect("Time conversion shouldn't failed");
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
            let pub_key = TendermintPublicKey {
                sum: Some(key_to_tendermint(&consensus_key).unwrap()),
            };
            let pub_key = Some(pub_key);
            let update = ValidatorUpdate { pub_key, power };
            response.validator_updates.push(update);
        });

        // Update evidence parameters
        let (epoch_duration, _gas) =
            parameters::read_epoch_parameter(&self.storage)
                .expect("Couldn't read epoch duration parameters");
        let pos_params = self.storage.read_pos_params();
        let evidence_params =
            self.get_evidence_params(&epoch_duration, &pos_params);
        response.consensus_param_updates = Some(ConsensusParams {
            evidence: Some(evidence_params),
            ..response.consensus_param_updates.take().unwrap_or_default()
        });
    }
}

/// We test the failure cases of [`finalize_block`]. The happy flows
/// are covered by the e2e tests.
#[cfg(test)]
mod test_finalize_block {
    use anoma::types::address::xan;
    use anoma::types::storage::Epoch;
    use anoma::types::transaction::{EncryptionKey, Fee};

    use super::*;
    use crate::node::ledger::shell::test_utils::*;
    use crate::node::ledger::shims::abcipp_shim_types::shim::request::{
        FinalizeBlock, ProcessedTx,
    };

    #[cfg(not(feature = "ABCI"))]
    /// Check that if a wrapper tx was rejected by [`process_proposal`],
    /// check that the correct event is returned. Check that it does
    /// not appear in the queue of txs to be decrypted
    #[test]
    fn test_process_proposal_rejected_wrapper_tx() {
        let (mut shell, _) = setup();
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
                Default::default(),
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
        let (mut shell, _) = setup();
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
                Default::default(),
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
        let (mut shell, _) = setup();
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
            Default::default(),
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
        let (mut shell, _) = setup();
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

    #[cfg(not(feature = "ABCI"))]
    /// Test that if a tx is undecryptable, it is applied
    /// but the tx result contains the appropriate error code.
    #[test]
    fn test_undecryptable_returns_error_code() {
        let (mut shell, _) = setup();

        let keypair = crate::wallet::defaults::daewon_keypair();
        let pubkey = EncryptionKey::default();
        // not valid tx bytes
        let tx = "garbage data".as_bytes().to_owned();
        let inner_tx =
            anoma::types::transaction::encrypted::EncryptedTx::encrypt(
                &tx, pubkey,
            );
        let wrapper = WrapperTx {
            fee: Fee {
                amount: 0.into(),
                token: xan(),
            },
            pk: keypair.ref_to(),
            epoch: Epoch(0),
            gas_limit: 0.into(),
            inner_tx,
            tx_hash: hash_tx(&tx),
        };
        let processed_tx = ProcessedTx {
            tx: Tx::from(TxType::Decrypted(DecryptedTx::Undecryptable(
                wrapper.clone(),
            )))
            .to_bytes(),
            result: TxResult {
                code: ErrorCodes::Ok.into(),
                info: "".into(),
            },
        };

        shell.enqueue_tx(wrapper);

        // check that correct error message is returned
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
            assert_eq!(code, String::from(ErrorCodes::Undecryptable).as_str());
            let log = event
                .attributes
                .iter()
                .find(|attr| attr.key.as_str() == "log")
                .expect("Test failed")
                .value
                .as_str();
            assert!(log.contains("Transaction could not be decrypted."))
        }
        // check that the corresponding wrapper tx was removed from the queue
        assert!(shell.next_wrapper().is_none());
    }

    #[cfg(feature = "ABCI")]
    /// Test that if a tx is undecryptable, it is applied
    /// but the tx result contains the appropriate error code.
    #[test]
    fn test_undecryptable_returns_error_code() {
        let (mut shell, _) = setup();

        let keypair = crate::wallet::defaults::daewon_keypair();
        let pubkey = EncryptionKey::default();
        // not valid tx bytes
        let tx = "garbage data".as_bytes().to_owned();
        let inner_tx =
            anoma::types::transaction::encrypted::EncryptedTx::encrypt(
                &tx, pubkey,
            );
        let wrapper = WrapperTx {
            fee: Fee {
                amount: 0.into(),
                token: xan(),
            },
            pk: keypair.ref_to(),
            epoch: Epoch(0),
            gas_limit: 0.into(),
            inner_tx,
            tx_hash: hash_tx(&tx),
        };
        let processed_tx = ProcessedTx {
            tx: Tx::from(TxType::Decrypted(DecryptedTx::Undecryptable(
                wrapper,
            )))
            .to_bytes(),
            result: TxResult {
                code: ErrorCodes::Ok.into(),
                info: "".into(),
            },
        };

        // check that correct error message is returned
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
                String::from(ErrorCodes::Undecryptable)
            );

            let log = String::from_utf8(
                event
                    .attributes
                    .iter()
                    .find(|attr| attr.key == "log".as_bytes())
                    .expect("Test failed")
                    .value
                    .clone(),
            )
            .expect("Test failed");
            assert!(log.contains("Transaction could not be decrypted."))
        }
        // check that the corresponding wrapper tx was removed from the queue
        assert!(shell.next_wrapper().is_none());
    }

    /// Test that the wrapper txs are queued in the order they
    /// are received from the block. Tests that the previously
    /// decrypted txs are de-queued.
    #[test]
    fn test_mixed_txs_queued_in_correct_order() {
        let (mut shell, _) = setup();
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
                Default::default(),
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
                Default::default(),
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
        let (mut shell, _) = setup();
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
            Default::default(),
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
                Default::default(),
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
