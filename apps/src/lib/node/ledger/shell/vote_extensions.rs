
#[cfg(not(feature = "ABCI"))]
mod extend_votes {
    use super::super::*;
    impl<D, H> Shell<D, H>
        where
            D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
            H: StorageHasher + Sync + 'static,
    {
        /// INVARIANT: This method must be stateless.
        pub fn extend_vote(
            &self,
            _req: request::ExtendVote,
        ) -> response::ExtendVote {
            tracing::info!("\n\n\n Hey! Vote Extensions do get called!\n\n\n");
            Default::default()
        }


        /// INVARIANT: This method must be stateless.
        pub fn verify_vote_extension(
            &self,
            _req: request::VerifyVoteExtension,
        ) -> response::VerifyVoteExtension {
            Default::default()
        }

        pub fn new_ethereum_headers(&mut self) {
            let mut header_buffer = vec![];
            if let ShellMode::Validator {ref mut ethereum_recv, ..} = &mut self.mode {
                while let Ok(eth_result) = ethereum_recv.try_recv() {
                    if let Some(header) = eth_result.new_header {
                        header_buffer.push(header);
                    }
                    if let Some(err) = eth_result.error {
                        tracing::error!("Received error from Ethereum: {:?}", err);
                    }
                }
            }
        }
    }
}

#[cfg(not(feature = "ABCI"))]
pub use extend_votes::*;