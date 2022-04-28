//! Implementation of the [`PrepareProposal`] ABCI++ method for the Shell

#[cfg(not(feature = "ABCI"))]
mod prepare_block {
    use std::collections::hash_map::Entry;
    use std::collections::HashMap;

    use anoma::types::ethereum_headers::{MultiSignedEthHeader, SignedHeader};
    use anoma::types::transaction::protocol::ProtocolTxType;
    use anoma::types::vote_extensions::VoteExtension;

    use super::super::*;
    use crate::node::ledger::shims::abcipp_shim_types::shim::TxBytes;

    impl<D, H> Shell<D, H>
    where
        D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
        H: StorageHasher + Sync + 'static,
    {
        /// Begin a new block.
        ///
        /// We include half of the new wrapper txs given to us from the mempool
        /// by tendermint. The rest of the block is filled with decryptions
        /// of the wrapper txs from the previously committed block.
        ///
        /// INVARIANT: Any changes applied in this method must be reverted if
        /// the proposal is rejected (unless we can simply overwrite
        /// them in the next block).
        pub fn prepare_proposal(
            &mut self,
            req: RequestPrepareProposal,
        ) -> response::PrepareProposal {
            // We can safely reset meter, because if the block is rejected,
            // we'll reset again on the next proposal, until the
            // proposal is accepted
            self.gas_meter.reset();
            let txs = if let ShellMode::Validator { .. } = self.mode {
                // TODO: This should not be hardcoded
                let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();

                // Add the vote extensions in as protocol txs
                let mut txs: Vec<TxBytes> = self
                    .mode
                    .get_protocol_key()
                    .map(|protocol_key| {
                        self.prepare_ethereum_headers(req.votes, protocol_key)
                    })
                    .unwrap();

                // filter in half of the new txs from Tendermint, only keeping
                // wrappers
                let number_of_new_txs = 1 + req.block_data.len() / 2;
                let mut new_txs: Vec<TxBytes> = req
                    .block_data
                    .into_iter()
                    .take(number_of_new_txs)
                    .filter(|tx_bytes| {
                        if let Ok(tx) = Tx::try_from(tx_bytes.as_slice()) {
                            matches!(process_tx(tx), Ok(TxType::Wrapper(_)))
                        } else {
                            false
                        }
                    })
                    .collect();
                txs.append(&mut new_txs);
                // decrypt the wrapper txs included in the previous block
                let mut decrypted_txs = self
                    .storage
                    .tx_queue
                    .iter()
                    .map(|tx| {
                        Tx::from(match tx.decrypt(privkey) {
                            Ok(tx) => DecryptedTx::Decrypted(tx),
                            _ => DecryptedTx::Undecryptable(tx.clone()),
                        })
                        .to_bytes()
                    })
                    .collect();

                txs.append(&mut decrypted_txs);
                txs
            } else {
                vec![]
            };
            response::PrepareProposal { block_data: txs }
        }

        /// Extract ethereum headers from vote extensions. Create a
        /// protocol tx for each header correctly signed by a number of
        /// valid validators. The correctness of the headers will be handled
        /// by the validity predicate.
        fn prepare_ethereum_headers(
            &self,
            votes: Vec<tendermint_proto::types::Vote>,
            signing_key: &common::SecretKey,
        ) -> Vec<TxBytes> {
            votes
                .into_iter()
                // extract vote extension data from vote
                .filter_map(|vote| {
                    VoteExtension::try_from(vote)
                        .ok()
                        .map(|data| data.ethereum_headers)
                })
                // flatten into a list of signed ethereum headers
                .flatten()
                // remove those headers that aren't properly signed by
                // validators
                .filter(|header| {
                    self.validate_ethereum_header(
                        self.storage.last_height.0,
                        header,
                    )
                })
                // group equivalent headers together
                .fold(HashMap::new(), |mut headers, header| {
                    if let Entry::Vacant(e) = headers.entry(header.hash()) {
                        e.insert(MultiSignedEthHeader::from(header));
                    } else {
                        headers
                            .get_mut(&header.hash())
                            .unwrap()
                            .add(header)
                            .unwrap();
                    }
                    headers
                })
                .drain()
                // put each multi-signed header into a separate signed
                // transaction
                .map(|(_, header)| {
                    ProtocolTxType::EthereumHeaders(header)
                        .sign(&signing_key.ref_to(), signing_key)
                        .to_bytes()
                })
                .collect()
        }
    }

    #[cfg(test)]
    mod test_prepare_proposal {
        use anoma::types::address::xan;
        use anoma::types::ethereum_headers::EthereumHeader;
        use anoma::types::hash::Hash;
        use anoma::types::storage::Epoch;
        use anoma::types::transaction::protocol::ProtocolTx;
        use anoma::types::transaction::Fee;

        use super::*;
        use crate::node::ledger::shell::test_utils::{
            gen_keypair, setup, TestShell,
        };

        /// Test that if a tx from the mempool is not a
        /// WrapperTx type, it is not included in the
        /// proposed block.
        #[test]
        fn test_prepare_proposal_rejects_non_wrapper_tx() {
            let (mut shell, _) = TestShell::new();
            let tx = Tx::new(
                "wasm_code".as_bytes().to_owned(),
                Some("transaction_data".as_bytes().to_owned()),
            );
            let req = RequestPrepareProposal {
                block_data: vec![tx.to_bytes()],
                block_data_size: 0,
                votes: vec![],
            };
            let block_data = shell.prepare_proposal(req).block_data;
            assert_eq!(block_data.len(), 0);
        }

        /// Test that if an error is encountered while
        /// trying to process a tx from the mempool,
        /// we simply exclude it from the proposal
        #[test]
        fn test_error_in_processing_tx() {
            let (mut shell, _) = TestShell::new();
            let keypair = gen_keypair();
            let tx = Tx::new(
                "wasm_code".as_bytes().to_owned(),
                Some("transaction_data".as_bytes().to_owned()),
            );
            // an unsigned wrapper will cause an error in processing
            let wrapper = Tx::new(
                "".as_bytes().to_owned(),
                Some(
                    WrapperTx::new(
                        Fee {
                            amount: 0.into(),
                            token: xan(),
                        },
                        &keypair,
                        Epoch(0),
                        0.into(),
                        tx,
                        Default::default(),
                    )
                    .try_to_vec()
                    .expect("Test failed"),
                ),
            )
            .to_bytes();
            let req = RequestPrepareProposal {
                block_data: vec![wrapper],
                block_data_size: 0,
                votes: vec![],
            };
            let block_data = shell.prepare_proposal(req).block_data;
            assert_eq!(block_data.len(), 0);
        }

        /// Test that the decrypted txs are included
        /// in the proposal in the same order as their
        /// corresponding wrappers
        #[test]
        fn test_decrypted_txs_in_correct_order() {
            let (mut shell, _) = TestShell::new();
            let keypair = gen_keypair();
            let mut expected_wrapper = vec![];
            let mut expected_decrypted = vec![];

            let mut req = RequestPrepareProposal {
                block_data: vec![],
                block_data_size: 0,
                votes: vec![],
            };
            // create a request with two new wrappers from mempool and
            // two wrappers from the previous block to be decrypted
            for i in 0..2 {
                let tx = Tx::new(
                    "wasm_code".as_bytes().to_owned(),
                    Some(
                        format!("transaction data: {}", i)
                            .as_bytes()
                            .to_owned(),
                    ),
                );
                expected_decrypted
                    .push(Tx::from(DecryptedTx::Decrypted(tx.clone())));
                let wrapper_tx = WrapperTx::new(
                    Fee {
                        amount: 0.into(),
                        token: xan(),
                    },
                    &keypair,
                    Epoch(0),
                    0.into(),
                    tx,
                    Default::default(),
                );
                let wrapper = wrapper_tx.sign(&keypair).expect("Test failed");
                shell.enqueue_tx(wrapper_tx);
                expected_wrapper.push(wrapper.clone());
                req.block_data.push(wrapper.to_bytes());
            }
            // we extract the inner data from the txs for testing
            // equality since otherwise changes in timestamps would
            // fail the test
            expected_wrapper.append(&mut expected_decrypted);
            let expected_txs: Vec<Vec<u8>> = expected_wrapper
                .iter()
                .map(|tx| tx.data.clone().expect("Test failed"))
                .collect();

            let block_data = shell.prepare_proposal(req).block_data;

            let received: Vec<Vec<u8>> = block_data
                .iter()
                .map(|tx_bytes| {
                    Tx::try_from(tx_bytes.as_slice())
                        .expect("Test failed")
                        .data
                        .expect("Test failed")
                })
                .collect();
            // check that the order of the txs is correct
            assert_eq!(received, expected_txs);
        }

        /// Check that the protocol txs are made correctly from ethereum
        /// headers. They should
        /// 1. Remove invalidly signed headers.
        /// 2. Group headers with the same hash together
        /// 3. Headers that don't have the same hash should be separate
        #[test]
        fn test_prepare_ethereum_headers() {
            let (shell, _) = setup();
            let signing_key =
                shell.mode.get_protocol_key().expect("Test failed");
            let address = shell.mode.get_validator_address().unwrap().clone();
            let voting_power = shell.get_validator_voting_power().unwrap();

            // we will submit two copies of this header which should get
            // combined into a single multi-signed header in a protocol tx
            let header_1 = EthereumHeader {
                hash: Hash([0; 32]),
                parent_hash: Hash([0; 32]),
                number: 0u64,
                difficulty: 0.into(),
                mix_hash: Hash([0; 32]),
                nonce: Default::default(),
                state_root: Hash([0; 32]),
                transactions_root: Hash([0; 32]),
            };

            // This is an invalid header that should appear in no protocol tx
            let incorrect = header_1.clone().sign(
                voting_power,
                crate::wallet::defaults::bertha_address(),
                shell.storage.last_height.0,
                signing_key,
            );

            let signed_1 = header_1.clone().sign(
                voting_power,
                address.clone(),
                shell.storage.last_height.0,
                signing_key,
            );

            // This is a separate header that should appear by itself in
            // in a protocol tx.
            let header_2 = EthereumHeader {
                hash: Hash([0; 32]),
                parent_hash: Hash([1; 32]),
                number: 0u64,
                difficulty: 0.into(),
                mix_hash: Hash([0; 32]),
                nonce: Default::default(),
                state_root: Hash([0; 32]),
                transactions_root: Hash([0; 32]),
            };

            let signed_2 = header_2.clone().sign(
                voting_power,
                address.clone(),
                shell.storage.last_height.0,
                signing_key,
            );

            let vote_extension = VoteExtension {
                ethereum_headers: vec![
                    signed_1.clone(),
                    incorrect,
                    signed_1,
                    signed_2,
                ],
            }
            .try_to_vec()
            .ok()
            .map(|bytes| tendermint_proto::types::VoteExtension {
                app_data_to_sign: bytes,
                app_data_self_authenticating: vec![],
            });

            let vote = tendermint_proto::types::Vote {
                r#type: 0,
                height: 0,
                round: 0,
                block_id: None,
                timestamp: None,
                validator_address: vec![],
                validator_index: 0,
                signature: vec![],
                vote_extension,
            };

            let protocol_txs = shell.prepare_ethereum_headers(
                vec![vote],
                shell.mode.get_protocol_key().unwrap(),
            );
            assert_eq!(protocol_txs.len(), 2);
            for protocol_tx in protocol_txs {
                let tx =
                    Tx::try_from(protocol_tx.as_slice()).expect("Test failed");
                match process_tx(tx) {
                    Ok(TxType::Protocol(ProtocolTx {
                        tx: ProtocolTxType::EthereumHeaders(tx),
                        ..
                    })) => {
                        let height = shell.storage.last_height.0;
                        if (&header_1, height)
                            == (
                                &tx.signed_header.data.0,
                                tx.signed_header.data.1,
                            )
                        {
                            assert_eq!(
                                tx.signers,
                                vec![
                                    (address.clone(), voting_power),
                                    (address.clone(), voting_power)
                                ]
                            );
                        } else if (&header_2, height)
                            == (
                                &tx.signed_header.data.0,
                                tx.signed_header.data.1,
                            )
                        {
                            assert_eq!(
                                tx.signers,
                                vec![(address.clone(), voting_power)]
                            )
                        } else {
                            panic!("Test failed");
                        }
                    }
                    _ => panic!("Test failed"),
                }
            }
        }
    }
}

#[cfg(not(feature = "ABCI"))]
pub use prepare_block::*;
