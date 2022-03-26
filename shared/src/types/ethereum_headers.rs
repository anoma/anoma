//! Types needed to work with Ethereum headers
//! Includes tooling for transforming external types
//! to Anoma native types and back.
use std::convert::TryFrom;

use borsh::{BorshDeserialize, BorshSerialize};
use ethereum_types::{H256, H64, U256};
use thiserror::Error;
use web3::types::BlockHeader;

#[cfg(not(feature = "ABCI"))]
use crate::proto::Signed;
use crate::types::hash::Hash;
#[cfg(not(feature = "ABCI"))]
use crate::types::key::*;

/// Errors in transforming types related to Ethereum headers
#[derive(Error, Debug)]
pub enum Error {
    /// Error for an invalid Ethereum header.
    #[error("Encountered an invalid Ethereum header")]
    InvalidHeader,
}

type Result<T> = std::result::Result<T, Error>;

/// The difficulty of an Ethereum block
#[derive(BorshSerialize, BorshDeserialize)]
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
#[derive(BorshSerialize, BorshDeserialize)]
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

#[derive(BorshSerialize, BorshDeserialize)]
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

#[cfg(not(feature = "ABCI"))]
impl EthereumHeader {
    /// Create a signature for the Ethereum header and return
    /// the signed data.
    pub fn sign(self, signing_key: &common::SecretKey) -> SignedEthereumHeader {
        let sig =
            common::SigScheme::sign(signing_key, &self.try_to_vec().unwrap());
        SignedEthereumHeader {
            public_key: signing_key.ref_to(),
            signed_header: Signed { data: self, sig },
        }
    }
}

impl TryFrom<BlockHeader> for EthereumHeader {
    type Error = Error;

    fn try_from(header: BlockHeader) -> Result<Self> {
        Ok(EthereumHeader {
            hash: header.hash.map(Hash::from).ok_or(Error::InvalidHeader)?,
            parent_hash: Hash(header.parent_hash.0),
            nonce: header.nonce.map(Nonce::from).ok_or(Error::InvalidHeader)?,
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

#[cfg(not(feature = "ABCI"))]
#[derive(BorshSerialize, BorshDeserialize)]
/// A verifiable signed instance of the EthereumHeader.
pub struct SignedEthereumHeader {
    /// The public key use to verify the signature
    pub public_key: common::PublicKey,
    /// A signed Ethereum header
    pub signed_header: Signed<EthereumHeader>,
}

#[cfg(not(feature = "ABCI"))]
impl SignedEthereumHeader {
    /// Check that validity of the signature
    pub fn verify_signature(&self) -> std::result::Result<(), VerifySigError> {
        let Signed { data, sig } = &self.signed_header;
        common::SigScheme::verify_signature_raw(
            &self.public_key,
            &data.try_to_vec().unwrap(),
            sig,
        )
    }

    /// Get the Ethereum header out and return it
    pub fn extract_header(self) -> EthereumHeader {
        let Signed { data, .. } = self.signed_header;
        data
    }
}
