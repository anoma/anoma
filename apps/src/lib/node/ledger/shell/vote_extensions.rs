#[cfg(not(feature = "ABCI"))]
mod extend_votes {
    use anoma::types::transaction::protocol::VoteExtension;
    use borsh::{BorshDeserialize, BorshSerialize};
    use tendermint_proto::types::Vote;

    use super::super::*;
    use crate::node::ledger::ethereum_node::ethereum_types::SignedEthereumHeader;

    #[derive(BorshSerialize, BorshDeserialize)]
    pub struct VoteExtensionData {
        pub ethereum_headers: Vec<SignedEthereumHeader>,
    }

    impl TryFrom<VoteExtension> for VoteExtensionData {
        type Error = Error;

        fn try_from(ext: VoteExtension) -> Result<Self> {
            BorshDeserialize::try_from_slice(&ext.signed_data[..])
                .map_err(Error::VoteExtDecoding)
        }
    }

    impl TryFrom<&VoteExtension> for VoteExtensionData {
        type Error = Error;

        fn try_from(ext: &VoteExtension) -> Result<Self> {
            BorshDeserialize::try_from_slice(&ext.signed_data[..])
                .map_err(Error::VoteExtDecoding)
        }
    }

    impl TryFrom<tendermint_proto::types::VoteExtension> for VoteExtensionData {
        type Error = Error;

        fn try_from(
            ext: tendermint_proto::types::VoteExtension,
        ) -> Result<Self> {
            Self::try_from(VoteExtension::from(ext))
        }
    }

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
                vote_extension: VoteExtensionData {
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
                if let Ok(VoteExtensionData { ethereum_headers }) =
                    VoteExtensionData::try_from(ext)
                {
                    return if ethereum_headers
                        .iter()
                        .all(|header| header.verify_signature().is_ok())
                    {
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
                while let Ok(eth_result) = ethereum_recv.try_recv() {
                    if let Some(header) = eth_result.new_header {
                        header_buffer.push(header.sign(protocol_keypair));
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
    }
}

#[cfg(not(feature = "ABCI"))]
pub use extend_votes::*;

#[cfg(all(test, not(feature = "eth-fullnode"), not(feature = "ABCI")))]
mod test_vote_extensions {
    use std::convert::TryFrom;

    use tendermint_proto::types::Vote;
    use tower_abci::request;

    use super::*;
    use crate::node::ledger::shell::test_utils::*;

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
        let headers = shell.shell.new_ethereum_headers();
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
            VoteExtensionData::try_from(vote_extension.clone())
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
        let res = shell.shell.verify_vote_extension(req);
        assert_eq!(res.result, 0);
    }
}
