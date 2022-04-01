//! Anoma native vote extensions types

#[cfg(feature = "ethereum-headers")]
mod vote_exts {
    use std::convert::TryFrom;

    use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};

    use crate::types::ethereum_headers::SignedEthereumHeader;

    /// Convert Tendermint vote extensions to an Anoma native type
    #[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
    pub struct VoteExtension {
        /// Data whose authenticity if given by a validator signature
        pub signed_data: Vec<u8>,
        /// Data that can be self-authenticated
        pub self_authenticating_data: Vec<u8>,
    }

    #[cfg(not(feature = "ABCI"))]
    impl From<tendermint_proto::types::VoteExtension> for VoteExtension {
        fn from(ext: tendermint_proto::types::VoteExtension) -> Self {
            Self {
                signed_data: ext.app_data_to_sign,
                self_authenticating_data: ext.app_data_self_authenticating,
            }
        }
    }

    /// The data type vote extension bytes should deserialize to
    #[derive(BorshSerialize, BorshDeserialize)]
    pub struct VoteExtensionData {
        /// Ethereum headers seen by validators that they have signed
        pub ethereum_headers: Vec<SignedEthereumHeader>,
    }

    impl TryFrom<VoteExtension> for VoteExtensionData {
        type Error = std::io::Error;

        fn try_from(ext: VoteExtension) -> Result<Self, Self::Error> {
            BorshDeserialize::try_from_slice(&ext.signed_data[..])
        }
    }

    impl TryFrom<&VoteExtension> for VoteExtensionData {
        type Error = std::io::Error;

        fn try_from(ext: &VoteExtension) -> Result<Self, Self::Error> {
            BorshDeserialize::try_from_slice(&ext.signed_data[..])
        }
    }

    #[cfg(not(feature = "ABCI"))]
    impl TryFrom<tendermint_proto::types::VoteExtension> for VoteExtensionData {
        type Error = std::io::Error;

        fn try_from(
            ext: tendermint_proto::types::VoteExtension,
        ) -> Result<Self, Self::Error> {
            Self::try_from(VoteExtension::from(ext))
        }
    }

    #[cfg(not(feature = "ABCI"))]
    impl TryFrom<tendermint_proto::types::Vote> for VoteExtensionData {
        type Error = std::io::Error;

        fn try_from(
            vote: tendermint_proto::types::Vote,
        ) -> Result<Self, Self::Error> {
            if let Some(ext) = vote.vote_extension {
                Self::try_from(VoteExtension::from(ext))
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "No vote extension in vote",
                ))
            }
        }
    }
}

#[cfg(feature = "ethereum-headers")]
pub use vote_exts::*;
