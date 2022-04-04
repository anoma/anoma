//! Types needed to work with Ethereum headers
//! Includes tooling for transforming external types
//! to Anoma native types and back.

#[cfg(feature = "ethereum-headers")]
#[allow(missing_docs)]
pub mod eth_header_types {
    use std::convert::TryFrom;

    use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
    use ethereum_types::{H256, H64, U256};
    use thiserror::Error;
    use web3::types::BlockHeader;

    use crate::proto::{MultiSigned, Signed};
    use crate::types::address::Address;
    use crate::types::hash::Hash;
    use crate::types::key::*;
    use crate::types::transaction::hash_tx;

    /// Errors in transforming types related to Ethereum headers
    #[derive(Error, Debug)]
    pub enum Error {
        /// Error for an invalid Ethereum header.
        #[error("Encountered an invalid Ethereum header")]
        InvalidHeader,
        #[error("Could not combine Signed headers due to incompatibility")]
        IncompatibleHeaders,
    }

    type Result<T> = std::result::Result<T, Error>;

    /// The difficulty of an Ethereum block
    #[derive(
        Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema, PartialEq,
    )]
    pub struct Difficulty([u64; 4]);

    impl From<U256> for Difficulty {
        fn from(difficulty: U256) -> Self {
            Self(difficulty.0)
        }
    }

    impl From<web3::types::U256> for Difficulty {
        fn from(difficulty: web3::types::U256) -> Self {
            Self(difficulty.0)
        }
    }

    impl From<u64> for Difficulty {
        fn from(difficulty: u64) -> Self {
            Self::from(U256::from(difficulty))
        }
    }

    impl From<Difficulty> for U256 {
        fn from(difficulty: Difficulty) -> Self {
            Self(difficulty.0)
        }
    }

    /// The nonce found by mining an Ethereum block
    #[derive(
        Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema, PartialEq,
    )]
    pub struct Nonce([u8; 8]);

    impl Default for Nonce {
        fn default() -> Self {
            Self::from(H64::zero())
        }
    }

    impl From<H64> for Nonce {
        fn from(nonce: H64) -> Self {
            Nonce(nonce.0)
        }
    }

    impl From<Nonce> for H64 {
        fn from(nonce: Nonce) -> Self {
            Self(nonce.0)
        }
    }

    impl From<web3::types::H64> for Nonce {
        fn from(nonce: web3::types::H64) -> Self {
            Nonce(nonce.0)
        }
    }

    impl From<Hash> for H256 {
        fn from(hash: Hash) -> Self {
            H256(hash.0)
        }
    }

    impl From<H256> for Hash {
        fn from(hash: H256) -> Self {
            Hash(hash.0)
        }
    }

    impl From<web3::types::H256> for Hash {
        fn from(hash: web3::types::H256) -> Self {
            Hash(hash.0)
        }
    }

    #[derive(
        Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema, PartialEq,
    )]
    /// Pared down information from an Ethereum block header.
    /// Should only represent headers of mined blocks.
    pub struct EthereumHeader {
        /// Hash of the block
        pub hash: Hash,
        /// Hash of the parent
        pub parent_hash: Hash,
        /// Block number
        pub number: u64,
        /// Difficulty of the block
        pub difficulty: Difficulty,
        /// nonce
        pub nonce: Nonce,
        /// mix hash
        pub mix_hash: Hash,
        /// State root hash
        pub state_root: Hash,
        /// Transactions root hash
        pub transactions_root: Hash,
    }

    impl EthereumHeader {
        /// Create a signature for the Ethereum header and return
        /// the signed data.
        pub fn sign(
            self,
            voting_power: u64,
            address: Address,
            height: u64,
            signing_key: &common::SecretKey,
        ) -> SignedEthereumHeader {
            SignedEthereumHeader {
                voting_power,
                address,
                signed_header: Signed::new(signing_key, (self, height)),
            }
        }
    }

    impl TryFrom<BlockHeader> for EthereumHeader {
        type Error = Error;

        fn try_from(header: BlockHeader) -> Result<Self> {
            Ok(EthereumHeader {
                hash: header
                    .hash
                    .map(Hash::from)
                    .ok_or(Error::InvalidHeader)?,
                parent_hash: Hash(header.parent_hash.0),
                nonce: header
                    .nonce
                    .map(Nonce::from)
                    .ok_or(Error::InvalidHeader)?,
                mix_hash: header
                    .mix_hash
                    .map(Hash::from)
                    .ok_or(Error::InvalidHeader)?,
                number: header
                    .number
                    .map(|num| num.as_u64())
                    .ok_or(Error::InvalidHeader)?,
                difficulty: header.difficulty.into(),
                state_root: header.state_root.into(),
                transactions_root: header.transactions_root.into(),
            })
        }
    }

    /// A uniform interface for signed and multi-signed ethereum headers
    pub trait SignedHeader {
        /// Get the sum of all the voting power of validators who have
        /// signed this header.
        fn get_voting_power(&self) -> u64;
        /// Get the address of the validators who have signed this data
        fn get_addresses(&self) -> Vec<&Address>;
        /// Get the height of the block that this data was created
        /// as part of a vote extension
        fn get_height(&self) -> u64;
        /// Check that validity of the signatures
        fn verify_signatures(
            &self,
            public_keys: &[common::PublicKey],
        ) -> std::result::Result<(), VerifySigError>;
        /// Get the hash of the inner signed Ethereum header
        fn hash(&self) -> Hash;
    }

    #[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
    /// A verifiable signed instance of the EthereumHeader.
    pub struct SignedEthereumHeader {
        /// Voting power of the signing validator
        pub voting_power: u64,
        /// Address of the signing validator
        pub address: Address,
        /// A signed Ethereum header and the block height
        /// that this header appeared in a vote extension.
        /// This guards against replays.
        pub signed_header: Signed<(EthereumHeader, u64)>,
    }

    impl SignedHeader for SignedEthereumHeader {
        fn get_voting_power(&self) -> u64 {
            self.voting_power
        }

        fn get_addresses(&self) -> Vec<&Address> {
            vec![&self.address]
        }

        fn get_height(&self) -> u64 {
            let Signed {
                data: (_, height), ..
            } = self.signed_header;
            height
        }

        fn verify_signatures(
            &self,
            public_keys: &[common::PublicKey],
        ) -> std::result::Result<(), VerifySigError> {
            self.signed_header.verify(&public_keys[0])
        }

        fn hash(&self) -> Hash {
            let Signed { data, .. } = &self.signed_header;
            hash_tx(&data.try_to_vec().unwrap())
        }
    }

    #[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
    /// An EthereumHeader with multiple validator signatures.
    pub struct MultiSignedEthHeader {
        /// Voting power of all signing validators
        pub voting_power: u64,
        /// Address of the signing validators
        pub signers: Vec<Address>,
        /// A multi-signed Ethereum header and the block height
        /// that these headers appeared in a vote extension.
        /// This guards against replays.
        pub signed_header: MultiSigned<(EthereumHeader, u64)>,
    }

    impl From<SignedEthereumHeader> for MultiSignedEthHeader {
        fn from(
            SignedEthereumHeader {
                voting_power,
                address,
                signed_header,
            }: SignedEthereumHeader,
        ) -> Self {
            Self {
                voting_power,
                signers: vec![address],
                signed_header: signed_header.into(),
            }
        }
    }

    impl MultiSignedEthHeader {
        /// Add a new signature for the same (block header, block height)
        /// to this instance.
        pub fn add(&mut self, other: SignedEthereumHeader) -> Result<()> {
            if self.hash() == other.hash()
                && self.get_height() == other.get_height()
            {
                self.voting_power += other.voting_power;
                self.signers.push(other.address);
                self.signed_header.sigs.push(other.signed_header.sig);
                Ok(())
            } else {
                Err(Error::IncompatibleHeaders)
            }
        }
    }

    impl SignedHeader for MultiSignedEthHeader {
        fn get_voting_power(&self) -> u64 {
            self.voting_power
        }

        fn get_addresses(&self) -> Vec<&Address> {
            self.signers.iter().collect()
        }

        fn get_height(&self) -> u64 {
            let MultiSigned {
                data: (_, height), ..
            } = self.signed_header;
            height
        }

        fn verify_signatures(
            &self,
            public_keys: &[common::PublicKey],
        ) -> std::result::Result<(), VerifySigError> {
            self.signed_header.verify(public_keys)
        }

        fn hash(&self) -> Hash {
            let MultiSigned { data, .. } = &self.signed_header;
            hash_tx(&data.try_to_vec().unwrap())
        }
    }
}

#[cfg(feature = "ethereum-headers")]
pub use eth_header_types::*;
