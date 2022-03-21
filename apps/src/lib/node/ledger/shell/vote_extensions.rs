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
