#[cfg(not(feature = "ABCI"))]
mod extend_votes {
    use anoma::types::ethereum_headers::{
        EpochPower, SignedEthereumHeader, SignedHeader,
    };
    use anoma::types::vote_extensions::VoteExtension;
    use tendermint_proto::types::Vote;

    use super::super::*;

    impl<D, H> Shell<D, H>
    where
        D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
        H: StorageHasher + Sync + 'static,
    {
        /// INVARIANT: This method must be stateless.
        pub fn extend_vote(
            &mut self,
            _req: request::ExtendVote,
        ) -> response::ExtendVote {
            response::ExtendVote {
                vote_extension: VoteExtension {
                    ethereum_headers: self.new_ethereum_headers(),
                }
                .try_to_vec()
                .ok()
                .map(|bytes| {
                    tendermint_proto::types::VoteExtension {
                        app_data_to_sign: bytes,
                        app_data_self_authenticating: vec![],
                    }
                }),
            }
        }

        /// At present this checks the signature on all Ethereum headers
        ///
        /// INVARIANT: This method must be stateless.
        pub fn verify_vote_extension(
            &self,
            req: request::VerifyVoteExtension,
        ) -> response::VerifyVoteExtension {
            if let Some(Vote {
                vote_extension: Some(ext),
                ..
            }) = req.vote
            {
                // Verify all the Ethereum headers are signed correctly
                if let Ok(VoteExtension { ethereum_headers }) =
                VoteExtension::try_from(ext)
                {
                    return if ethereum_headers.iter().all(|header| {
                        self.validate_ethereum_header(
                            self.storage.last_height.0 + 1,
                            header,
                        )
                    }) {
                        response::VerifyVoteExtension { result: 0 }
                    } else {
                        response::VerifyVoteExtension { result: 1 }
                    };
                }
            }
            Default::default()
        }

        /// Checks the channel to the Ethereum fullnode and
        /// retrieves all messages sent. Extracts and signs
        /// all the headers and logs contained therein.
        pub fn new_ethereum_headers(&mut self) -> Vec<SignedEthereumHeader> {
            let mut header_buffer = vec![];
            let voting_power = self.get_validator_voting_power();
            let address = self.mode.get_validator_address().cloned();
            if let ShellMode::Validator {
                ref mut ethereum_recv,
                data:
                    ValidatorData {
                        keys:
                            ValidatorKeys {
                                protocol_keypair, ..
                            },
                        ..
                    },
                ..
            } = &mut self.mode
            {
                let (voting_power, address) =
                    (voting_power.unwrap(), address.unwrap());
                while let Ok(eth_result) = ethereum_recv.try_recv() {
                    if let Some(header) = eth_result.new_header {
                        header_buffer.push(header.sign(
                            voting_power,
                            address.clone(),
                            self.storage.last_height.0 + 1,
                            protocol_keypair,
                        ));
                    }
                    if let Some(err) = eth_result.error {
                        tracing::error!(
                            "Received error from Ethereum: {:?}",
                            err
                        );
                    }
                }
            }
            header_buffer
        }

        /// Verify that each ethereum header in a vote extension was signed by
        /// a validator in the correct epoch, the stated voting power is
        /// correct, and the signature is correct.
        #[cfg(not(feature = "ABCI"))]
        pub fn validate_ethereum_header(
            &self,
            block_number: u64,
            header: &impl SignedHeader,
        ) -> bool {
            let eth_verifier = match &self.storage.eth_verifier {
                Some(verifier) => verifier,
                _ => return false,
            };
            // This should come from a vote extension sent during the
            // finalization of the last committed block.
            if block_number != header.get_height() {
                return false;
            }
            // Get the public keys of each validator. Filter out those that
            // inaccurately stated their voting power at a given block height
            let public_keys: Vec<common::PublicKey> = header
                .get_voting_powers()
                .into_iter()
                .filter_map(
                    |EpochPower {
                         validator,
                         voting_power,
                         block_height,
                     }| {
                        let epoch = self
                            .storage
                            .block
                            .pred_epochs
                            .get_epoch(block_height.into());
                        if let Some((power, pk)) =
                            self.get_validator_from_address(&validator, epoch)
                        {
                            if u64::from(power) == voting_power {
                                Some(pk)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    },
                )
                .collect();

            // check that we found all the public keys and
            // check that the signatures are valid
            public_keys.len() == header.get_addresses().len()
                && header.verify_signatures(&public_keys).is_ok()
                && eth_verifier.verify_header(header.get_header())
        }
    }
}

#[cfg(not(feature = "ABCI"))]
pub use extend_votes::*;

#[cfg(all(test, not(feature = "eth-fullnode"), not(feature = "ABCI")))]
mod test_vote_extensions {
    use std::convert::TryFrom;

    use anoma::ledger::pos;
    use anoma::ledger::pos::anoma_proof_of_stake::PosBase;
    use anoma::types::ethereum_headers::{
        EthereumHeader, MultiSignedEthHeader,
    };
    use anoma::types::hash::Hash;
    use anoma::types::key::*;
    use anoma::types::storage::Epoch;
    use anoma::types::vote_extensions::VoteExtension;
    use tendermint_proto::types::Vote;
    use tower_abci::request;

    use crate::node::ledger::shell::test_utils::*;
    use crate::node::ledger::shims::abcipp_shim_types::shim::request::FinalizeBlock;

    /// Test that we successfully receive ethereum headers
    /// from the channel to fullnode process
    ///
    /// We further check that ledger side buffering is done if multiple
    /// headers are in the channel
    #[test]
    #[cfg(not(feature = "eth-fullnode"))]
    fn test_get_eth_headers() {
        let (mut shell, _) =
            tokio_test::block_on(setup_with_mocked_eth_fullnode());
        // a header is sent every 2 seconds.
        // We should receive at least 2 headers as such
        std::thread::sleep(std::time::Duration::from_secs(5));
        let headers = shell.new_ethereum_headers();
        assert!(headers.len() > 1);
    }

    /// Test that ethereum headers are added to vote extensions.
    /// Check that these vote extensions pass verification
    #[test]
    #[cfg(not(feature = "eth-fullnode"))]
    fn test_eth_headers_vote_extensions() {
        let (mut shell, _) =
            tokio_test::block_on(setup_with_mocked_eth_fullnode());
        // a header is sent every 2 seconds.
        // We should receive at least 2 headers as such
        std::thread::sleep(std::time::Duration::from_secs(5));
        let vote_extension = shell
            .shell
            .extend_vote(Default::default())
            .vote_extension
            .expect("Test failed");
        let vote_extension_data =
            VoteExtension::try_from(vote_extension.clone())
                .expect("Test failed");

        assert!(vote_extension_data.ethereum_headers.len() > 1);
        let vote = Vote {
            r#type: 0,
            height: 0,
            round: 0,
            block_id: None,
            timestamp: None,
            validator_address: vec![],
            validator_index: 0,
            signature: vec![],
            vote_extension: Some(vote_extension),
        };
        let req = request::VerifyVoteExtension { vote: Some(vote) };
        let res = shell.verify_vote_extension(req);
        assert_eq!(res.result, 0);
    }

    #[cfg(not(feature = "ABCI"))]
    /// Test that Ethereum headers signed by a non-validator is rejected
    #[test]
    fn test_eth_headers_must_be_signed_by_validator() {
        let (shell, _) = setup();
        let signing_key = gen_keypair();
        let address = shell.mode.get_validator_address().unwrap().clone();
        let voting_power = shell.get_validator_voting_power().unwrap();
        let signed_header = EthereumHeader {
            hash: Hash([0; 32]),
            parent_hash: Hash([0; 32]),
            number: 0u64,
            difficulty: 0.into(),
            mix_hash: Hash([0; 32]),
            nonce: Default::default(),
            state_root: Hash([0; 32]),
            transactions_root: Hash([0; 32]),
        }
        .sign(
            voting_power,
            address,
            shell.storage.last_height.0 + 1,
            &signing_key,
        );
        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &signed_header
        ));
        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &MultiSignedEthHeader::from(signed_header)
        ));
    }

    #[cfg(not(feature = "ABCI"))]
    /// Test that validation of vote extensions cast during the previous
    /// block are accepted for the current block. This should pass even
    /// if the epoch changed resulting in a change to the validator set.
    #[test]
    fn test_validate_vote_extensions() {
        let (mut shell, _) = setup();
        let protocol_key = shell.mode.get_protocol_key().unwrap().clone();
        let address = shell.mode.get_validator_address().unwrap().clone();
        let voting_power = shell.get_validator_voting_power().unwrap();
        let height = shell.storage.last_height.0 + 1;
        let signed_header = EthereumHeader {
            hash: Hash([0; 32]),
            parent_hash: Hash([0; 32]),
            number: 0u64,
            difficulty: 0.into(),
            mix_hash: Hash([0; 32]),
            nonce: Default::default(),
            state_root: Hash([0; 32]),
            transactions_root: Hash([0; 32]),
        }
        .sign(voting_power, address, height, &protocol_key);
        assert_eq!(shell.storage.get_current_epoch().0.0, 0);
        // We make a change so that there are no
        // validators in the next epoch
        let mut current_validators = shell.storage.read_validator_set();
        current_validators.data.insert(
            1,
            Some(pos::types::ValidatorSet {
                active: Default::default(),
                inactive: Default::default(),
            }),
        );
        shell.storage.write_validator_set(&current_validators);
        // we advance forward to the next epoch
        let mut req = FinalizeBlock::default();
        req.header.height = 11u32.into();
        req.header.time = tendermint::time::Time::now();
        shell.finalize_block(req).expect("Test failed");
        shell.commit();

        assert!(
            shell
                .get_validator_from_protocol_pk(&protocol_key.ref_to(), None)
                .is_none()
        );
        let prev_epoch = Epoch(shell.storage.get_current_epoch().0.0 - 1);
        assert!(
            shell
                .shell
                .get_validator_from_protocol_pk(
                    &protocol_key.ref_to(),
                    Some(prev_epoch)
                )
                .is_some()
        );
        assert!(shell.validate_ethereum_header(height, &signed_header));
        assert!(shell.validate_ethereum_header(
            height,
            &MultiSignedEthHeader::from(signed_header)
        ));
    }

    /// Test that if the declared voting power is not correct,
    /// the signed header is rejected
    #[test]
    fn reject_incorrect_voting_power() {
        let (shell, _) = setup();
        let signing_key = shell.mode.get_protocol_key().expect("Test failed");
        let address = shell.mode.get_validator_address().unwrap().clone();
        let voting_power = 99;
        let header = EthereumHeader {
            hash: Hash([0; 32]),
            parent_hash: Hash([0; 32]),
            number: 0u64,
            difficulty: 0.into(),
            mix_hash: Hash([0; 32]),
            nonce: Default::default(),
            state_root: Hash([0; 32]),
            transactions_root: Hash([0; 32]),
        }
        .sign(
            voting_power,
            address,
            shell.storage.last_height.0 + 1,
            &signing_key,
        );

        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &header
        ));
        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &MultiSignedEthHeader::from(header)
        ));
    }

    /// Test that that a header that incorrectly labels what block it was
    /// included in a vote extension on is rejected
    #[test]
    fn reject_incorrect_block_number() {
        let (shell, _) = setup();
        let signing_key = shell.mode.get_protocol_key().expect("Test failed");
        let address = shell.mode.get_validator_address().unwrap().clone();
        let voting_power = shell.get_validator_voting_power().unwrap();
        let header = EthereumHeader {
            hash: Hash([0; 32]),
            parent_hash: Hash([0; 32]),
            number: 0u64,
            difficulty: 0.into(),
            mix_hash: Hash([0; 32]),
            nonce: Default::default(),
            state_root: Hash([0; 32]),
            transactions_root: Hash([0; 32]),
        }
        .sign(
            voting_power,
            address,
            shell.storage.last_height.0,
            &signing_key,
        );

        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &header
        ));
        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &MultiSignedEthHeader::from(header)
        ));
    }

    /// Test that that a header with an incorrect address
    /// included in a vote extension is rejected
    #[test]
    fn reject_incorrect_address() {
        let (shell, _) = setup();
        let signing_key = shell.mode.get_protocol_key().expect("Test failed");
        let voting_power = shell.get_validator_voting_power().unwrap();
        let header = EthereumHeader {
            hash: Hash([0; 32]),
            parent_hash: Hash([0; 32]),
            number: 0u64,
            difficulty: 0.into(),
            mix_hash: Hash([0; 32]),
            nonce: Default::default(),
            state_root: Hash([0; 32]),
            transactions_root: Hash([0; 32]),
        }
        .sign(
            voting_power,
            crate::wallet::defaults::bertha_address(),
            shell.storage.last_height.0 + 1,
            &signing_key,
        );

        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &header
        ));
        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &MultiSignedEthHeader::from(header)
        ));
    }

    /// Test that the ethash algorithm is called on Ethereum headers
    /// as part of validation
    #[test]
    fn test_eth_verifier_is_called() {
        let (shell, _) = setup();
        let address = shell.mode.get_validator_address().unwrap().clone();
        let signing_key = shell.mode.get_protocol_key().expect("Test failed");
        let voting_power = shell.get_validator_voting_power().unwrap();
        let header = EthereumHeader {
            hash: Hash([1; 32]),
            parent_hash: Hash([0; 32]),
            number: 0u64,
            difficulty: 0.into(),
            mix_hash: Hash([0; 32]),
            nonce: Default::default(),
            state_root: Hash([0; 32]),
            transactions_root: Hash([0; 32]),
        }
        .sign(
            voting_power,
            address,
            shell.storage.last_height.0 + 1,
            &signing_key,
        );

        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &header
        ));
        assert!(!shell.validate_ethereum_header(
            shell.storage.last_height.0 + 1,
            &MultiSignedEthHeader::from(header)
        ));
    }
}
