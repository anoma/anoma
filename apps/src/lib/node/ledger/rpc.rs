//! RPC endpoint is used for ledger state queries

use std::fmt::Display;
use std::str::FromStr;

use anoma::types::address::Address;
use anoma::types::storage;
use borsh::{BorshDeserialize, BorshSerialize};
use thiserror::Error;

/// RPC query path
#[derive(Debug, Clone)]
pub enum Path {
    /// Dry run a transaction
    DryRunTx,
    /// Epoch of the last committed block
    Epoch,
    /// Read a storage value with exact storage key
    Value(storage::Key),
    /// Read a range of storage values with a matching key prefix
    Prefix(storage::Key),
}

/// RPC query path
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct PrefixValue {
    pub key: storage::Key,
    pub value: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct BalanceQuery {
    owner: Option<Address>,
    token: Option<Address>,
}

const DRY_RUN_TX_PATH: &str = "dry_run_tx";
const EPOCH_PATH: &str = "epoch";
const VALUE_PREFIX: &str = "value";
const PREFIX_PREFIX: &str = "prefix";

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Path::DryRunTx => write!(f, "{}", DRY_RUN_TX_PATH),
            Path::Epoch => write!(f, "{}", EPOCH_PATH),
            Path::Value(storage_key) => {
                write!(f, "{}/{}", VALUE_PREFIX, storage_key)
            }
            Path::Prefix(storage_key) => {
                write!(f, "{}/{}", PREFIX_PREFIX, storage_key)
            }
        }
    }
}

impl FromStr for Path {
    type Err = PathParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let path = s.to_lowercase();
        match path.as_str() {
            DRY_RUN_TX_PATH => Ok(Self::DryRunTx),
            EPOCH_PATH => Ok(Self::Epoch),
            _ => match path.split_once("/") {
                Some((VALUE_PREFIX, storage_key)) => {
                    let key = storage::Key::parse(storage_key)
                        .map_err(PathParseError::InvalidStorageKey)?;
                    Ok(Self::Value(key))
                }
                Some((PREFIX_PREFIX, storage_key)) => {
                    let key = storage::Key::parse(storage_key)
                        .map_err(PathParseError::InvalidStorageKey)?;
                    Ok(Self::Prefix(key))
                }
                _ => Err(PathParseError::InvalidPath(s.to_string())),
            },
        }
    }
}

impl From<Path> for tendermint::abci::Path {
    fn from(path: Path) -> Self {
        let path = path.to_string();
        // TODO: update in tendermint-rs to allow to construct this from owned
        // string. It's what `from_str` does anyway
        tendermint::abci::Path::from_str(&path).unwrap()
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum PathParseError {
    #[error("Unrecognized query path: {0}")]
    InvalidPath(String),
    #[error("Invalid storage key: {0}")]
    InvalidStorageKey(storage::Error),
}
