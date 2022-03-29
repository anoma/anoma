//! Chain related data types
// TODO move BlockHash and BlockHeight here from the storage types

use std::fmt::Display;
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

/// The length of chain ID string
pub const CHAIN_ID_LENGTH: usize = 30;
/// The maximum length of chain ID prefix
pub const CHAIN_ID_PREFIX_MAX_LEN: usize = 19;
/// Separator between chain ID prefix and the generated hash
pub const CHAIN_ID_PREFIX_SEP: char = '.';

/// Development default chain ID. Must be [`CHAIN_ID_LENGTH`] long.
#[cfg(feature = "dev")]
pub const DEFAULT_CHAIN_ID: &str = "anoma-devchain.000000000000000";

/// Release default chain ID. Must be [`CHAIN_ID_LENGTH`] long.
#[cfg(not(feature = "dev"))]
pub const DEFAULT_CHAIN_ID: &str = "anoma-internal.000000000000000";

/// Chain ID
#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    BorshSerialize,
    BorshDeserialize,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
    Hash,
)]
#[serde(transparent)]
pub struct ChainId(String);

impl ChainId {
    /// Extracts a string slice containing the entire chain ID.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Derive the chain ID from the genesis hash and release version.
    pub fn from_genesis(
        ChainIdPrefix(prefix): ChainIdPrefix,
        genesis_bytes: impl AsRef<[u8]>,
    ) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(genesis_bytes);
        // less `1` for chain ID prefix separator char
        let width = CHAIN_ID_LENGTH - 1 - prefix.len();
        // lowercase hex of the first `width` chars of the hash
        let hash = format!("{:.width$x}", hasher.finalize(), width = width,);
        let raw = format!("{}{}{}", prefix, CHAIN_ID_PREFIX_SEP, hash);
        ChainId(raw)
    }

    /// Validate that chain ID is matching the expected value derived from the
    /// genesis hash and release version.
    pub fn validate(
        &self,
        genesis_bytes: impl AsRef<[u8]>,
    ) -> Vec<ChainIdValidationError> {
        let mut errors = vec![];
        match self.0.rsplit_once(CHAIN_ID_PREFIX_SEP) {
            Some((prefix, hash)) => {
                let mut hasher = Sha256::new();
                hasher.update(genesis_bytes);
                // less `1` for chain ID prefix separator char
                let width = CHAIN_ID_LENGTH - 1 - prefix.len();
                // lowercase hex of the first `width` chars of the hash
                let expected_hash =
                    format!("{:.width$x}", hasher.finalize(), width = width,);
                if hash != expected_hash {
                    errors.push(ChainIdValidationError::InvalidHash(
                        expected_hash,
                        hash.to_string(),
                    ));
                }
            }
            None => {
                errors.push(ChainIdValidationError::MissingSeparator);
            }
        }
        errors
    }
}

#[allow(missing_docs)]
#[derive(Debug, Error)]
pub enum ChainIdValidationError {
    #[error(
        "The prefix separator character '{CHAIN_ID_PREFIX_SEP}' is missing"
    )]
    MissingSeparator,
    #[error("The chain ID hash is not valid, expected {0}, got {1}")]
    InvalidHash(String, String),
}

impl Default for ChainId {
    fn default() -> Self {
        Self(DEFAULT_CHAIN_ID.to_string())
    }
}

impl Display for ChainId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[allow(missing_docs)]
#[derive(Debug, Error)]
pub enum ChainIdParseError {
    #[error("Chain ID must be {CHAIN_ID_LENGTH} long, got {0}")]
    UnexpectedLen(usize),
    #[error(
        "The chain ID contains forbidden characters: {0:?}. Only alphanumeric \
         characters and `-`, `_` and `.` are allowed."
    )]
    ForbiddenCharacters(Vec<char>),
}

impl FromStr for ChainId {
    type Err = ChainIdParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let len = s.len();
        if len != CHAIN_ID_LENGTH {
            return Err(ChainIdParseError::UnexpectedLen(len));
        }
        let mut forbidden_chars = s
            .chars()
            .filter(|char| {
                !matches!(*char as u8, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'-' | b'_' | b'.')
            })
            .peekable();
        if forbidden_chars.peek().is_some() {
            return Err(ChainIdParseError::ForbiddenCharacters(
                forbidden_chars.collect(),
            ));
        }
        Ok(Self(s.to_owned()))
    }
}

/// Chain ID prefix
#[derive(
    Debug, Clone, Serialize, Deserialize, BorshSerialize, BorshDeserialize,
)]
#[serde(transparent)]
pub struct ChainIdPrefix(String);

impl Display for ChainIdPrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ChainIdPrefix {
    /// Extracts a string slice containing the entire chain ID prefix.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Return a temporary chain ID made only from the prefix. This is not a
    /// valid chain ID and is only to be used temporarily in a network setup.
    pub fn temp_chain_id(&self) -> ChainId {
        ChainId(self.0.clone())
    }
}

#[allow(missing_docs)]
#[derive(Debug, Error)]
pub enum ChainIdPrefixParseError {
    #[error(
        "Chain ID prefix must at least 1 and up to {CHAIN_ID_PREFIX_MAX_LEN} \
         characters long, got {0}"
    )]
    UnexpectedLen(usize),
    #[error(
        "The prefix contains forbidden characters: {0:?}. Only alphanumeric \
         characters and `-`, `_` and `.` are allowed."
    )]
    ForbiddenCharacters(Vec<char>),
}

impl FromStr for ChainIdPrefix {
    type Err = ChainIdPrefixParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let len = s.len();
        if !(1..=CHAIN_ID_PREFIX_MAX_LEN).contains(&len) {
            return Err(ChainIdPrefixParseError::UnexpectedLen(len));
        }
        let mut forbidden_chars = s
            .chars()
            .filter(|char| {
                !matches!(*char as u8, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'-' | b'_' | b'.')
            })
            .peekable();
        if forbidden_chars.peek().is_some() {
            return Err(ChainIdPrefixParseError::ForbiddenCharacters(
                forbidden_chars.collect(),
            ));
        }
        Ok(Self(s.to_owned()))
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    proptest! {
        /// Test any chain ID that is generated via `from_genesis` function is valid.
        #[test]
        fn test_any_generated_chain_id_is_valid(
            prefix in proptest::string::string_regex(r#"[A-Za-z0-9\.\-_]{1,19}"#).unwrap(),
            genesis_bytes in any::<Vec<u8>>(),
        ) {
            let chain_id_prefix = ChainIdPrefix::from_str(&prefix).unwrap();
            let chain_id = ChainId::from_genesis(chain_id_prefix, &genesis_bytes);
            // There should be no validation errors
            let errors = chain_id.validate(&genesis_bytes);
            assert!(errors.is_empty(), "There should be no validation errors {:#?}", errors);
        }
    }
}
