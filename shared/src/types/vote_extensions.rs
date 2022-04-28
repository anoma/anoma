//! Anoma native vote extensions types

#[cfg(feature = "ethereum-headers")]
mod vote_exts {
    use std::convert::TryFrom;

    use borsh::{BorshDeserialize, BorshSerialize};

    use crate::types::ethereum_headers::SignedEthereumHeader;

    /// The data type vote extension bytes should deserialize to
    #[derive(BorshSerialize, BorshDeserialize)]
    pub struct VoteExtension {
        /// Ethereum headers seen by validators that they have signed
        pub ethereum_headers: Vec<SignedEthereumHeader>,
    }

    impl TryFrom<&[u8]> for VoteExtension {
        type Error = std::io::Error;

        fn try_from(ext: &[u8]) -> Result<Self, Self::Error> {
            BorshDeserialize::try_from_slice(ext)
        }
    }

    #[cfg(not(feature = "ABCI"))]
    impl TryFrom<tendermint_proto::types::Vote> for VoteExtension {
        type Error = std::io::Error;

        fn try_from(
            vote: tendermint_proto::types::Vote,
        ) -> Result<Self, Self::Error> {
            Self::try_from(vote.extension)
        }
    }
}

#[cfg(feature = "ethereum-headers")]
pub use vote_exts::*;
