//! Protocol parameters

use std::collections::HashSet;

use borsh::{BorshDeserialize, BorshSerialize};
use thiserror::Error;

use super::storage::types::decode;
use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::types::{self, encode};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{DbKeySeg, Key};
use crate::types::time::DurationSecs;

const ADDR: InternalAddress = InternalAddress::Parameters;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// Parameters functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Parameters VP
pub struct ParametersVp<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

/// Protocol parameters
#[derive(
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshSerialize,
    BorshDeserialize,
)]
pub struct Parameters {
    /// Epoch duration
    pub epoch_duration: EpochDuration,
}

/// Epoch duration. A new epoch begins as soon as both the `min_num_of_blocks`
/// and `min_duration` have passed since the beginning of the current epoch.
#[derive(
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshSerialize,
    BorshDeserialize,
)]
pub struct EpochDuration {
    /// Minimum number of blocks in an epoch
    pub min_num_of_blocks: u64,
    /// Minimum duration of an epoch
    pub min_duration: DurationSecs,
}

/// Initialize parameters in storage in the genesis block.
pub fn init_genesis_storage<DB, H>(
    storage: &mut Storage<DB, H>,
    parameters: &Parameters,
) where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    let params_key = storage_key();
    let params_value = encode(parameters);
    storage
        .write(&params_key, params_value)
        .expect("Protocol parameters must be initialized in the genesis block");
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum ReadError {
    #[error("Storage error: {0}")]
    StorageError(storage::Error),
    #[error("Storage type error: {0}")]
    StorageTypeError(types::Error),
    #[error("Protocol parameters are missing, they must be always set")]
    ParametersMissing,
}

/// Read the current parameters from storage. Returns the parameters and gas
/// cost.
pub fn read<DB, H>(
    storage: &Storage<DB, H>,
) -> std::result::Result<(Parameters, u64), ReadError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    let key = storage_key();
    let (value, gas) = storage.read(&key).map_err(ReadError::StorageError)?;
    let parameters = decode(value.ok_or(ReadError::ParametersMissing)?)
        .map_err(ReadError::StorageTypeError)?;
    Ok((parameters, gas))
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum WriteError {
    #[error("Storage error: {0}")]
    StorageError(storage::Error),
}

/// Update the current parameters in storage. Returns the parameters and gas
/// cost.
pub fn update<DB, H>(
    storage: &mut Storage<DB, H>,
    parameters: &Parameters,
) -> std::result::Result<u64, WriteError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    let key = storage_key();
    let value = encode(parameters);
    // TODO charge storage size diff
    let (gas, _size_diff) = storage
        .write(&key, value)
        .map_err(WriteError::StorageError)?;
    Ok(gas)
}

impl<'a, DB, H> NativeVp for ParametersVp<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    type Error = Error;

    const ADDR: InternalAddress = ADDR;

    fn validate_tx(
        &self,
        _tx_data: &[u8],
        _keys_changed: &HashSet<Key>,
        _verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        // TODO allow parameters change by over 2/3 validator voting power
        // No changes are currently permitted
        Ok(false)
    }
}

/// Storage key used for parameters.
fn storage_key() -> Key {
    Key {
        segments: vec![DbKeySeg::AddressSeg(Address::Internal(ADDR))],
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}
