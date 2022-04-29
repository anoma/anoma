//! Types for working with 32 bytes hashes.

use std::fmt::{self, Display};

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
#[cfg(not(feature = "ABCI"))]
use tendermint::abci::transaction;
#[cfg(feature = "ABCI")]
use tendermint_stable::abci::transaction;
use thiserror::Error;

/// The length of the transaction hash string
pub const HASH_LENGTH: usize = 32;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("TEMPORARY error: {error}")]
    Temporary { error: String },
    #[error("Failed trying to convert slice to a hash: {0}")]
    ConversionFailed(std::array::TryFromSliceError),
}

/// Result for functions that may fail
pub type HashResult<T> = std::result::Result<T, Error>;

#[derive(
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    BorshSerialize,
    BorshDeserialize,
    BorshSchema,
    Serialize,
    Deserialize,
)]
/// A hash, typically a sha-2 hash of a tx
pub struct Hash(pub [u8; 32]);

impl Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in &self.0 {
            write!(f, "{:02X}", byte)?;
        }
        Ok(())
    }
}

impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl TryFrom<&[u8]> for Hash {
    type Error = self::Error;

    fn try_from(value: &[u8]) -> HashResult<Self> {
        if value.len() != HASH_LENGTH {
            return Err(Error::Temporary {
                error: format!(
                    "Unexpected tx hash length {}, expected {}",
                    value.len(),
                    HASH_LENGTH
                ),
            });
        }
        let hash: [u8; 32] =
            TryFrom::try_from(value).map_err(Error::ConversionFailed)?;
        Ok(Hash(hash))
    }
}

impl From<Hash> for transaction::Hash {
    fn from(hash: Hash) -> Self {
        Self::new(hash.0)
    }
}

impl Hash {
    /// Compute sha256 of some bytes
    pub fn sha256(data: impl AsRef<[u8]>) -> Self {
        let digest = Sha256::digest(data.as_ref());
        Self(*digest.as_ref())
    }
}
