//! Protocol parameters

use std::collections::BTreeSet;

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
use thiserror::Error;

use super::storage::types::decode;
use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::types::{self, encode};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{DbKeySeg, Key};
use crate::types::time::DurationSecs;
use crate::vm::WasmCacheAccess;

const ADDR: InternalAddress = InternalAddress::Parameters;
const EPOCH_DURATION_KEY: &str = "epoch_duration";
const VP_WHITELIST_KEY: &str = "vp_whitelist";
const TX_WHITELIST_KEY: &str = "tx_whitelist";
const MAX_EXPECTED_TIME_PER_BLOCK_KEY: &str = "max_expected_time_per_block";

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// Parameters functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Parameters VP
pub struct ParametersVp<'a, DB, H, CA>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H, CA>,
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
    BorshSchema,
)]
pub struct Parameters {
    /// Epoch duration
    pub epoch_duration: EpochDuration,
    /// Maximum expected time per block
    pub max_expected_time_per_block: DurationSecs,
    /// Whitelisted validity predicate hashes
    pub vp_whitelist: Vec<String>,
    /// Whitelisted tx hashes
    pub tx_whitelist: Vec<String>,
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
    BorshSchema,
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
    // write epoch parameters
    let epoch_key = epoch_storage_key();
    let epoch_value = encode(&parameters.epoch_duration);
    storage
        .write(&epoch_key, epoch_value)
        .expect("Epoch parameters must be initialized in the genesis block");

    // write vp whitelist parameter
    let vp_whitelist_key = vp_whitelist_storage_key();
    let vp_whitelist_value = encode(&parameters.vp_whitelist);
    storage.write(&vp_whitelist_key, vp_whitelist_value).expect(
        "Vp whitelist parameters must be initialized in the genesis block",
    );

    // write tx whitelist parameter
    let tx_whitelist_key = tx_whitelist_storage_key();
    let tx_whitelist_value = encode(&parameters.tx_whitelist);
    storage.write(&tx_whitelist_key, tx_whitelist_value).expect(
        "Tx whitelist parameters must be initialized in the genesis block",
    );

    // write tx whitelist parameter
    let max_expected_time_per_block_key = max_expected_time_per_block_key();
    let max_expected_time_per_block_value =
        encode(&parameters.max_expected_time_per_block);
    storage
        .write(
            &max_expected_time_per_block_key,
            max_expected_time_per_block_value,
        )
        .expect(
            "Max expected time per block parameters must be initialized in \
             the genesis block",
        );
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

// Read the all the parameters from storage. Returns the parameters and gas
/// cost.
pub fn read<DB, H>(
    storage: &Storage<DB, H>,
) -> std::result::Result<(Parameters, u64), ReadError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    // read epoch
    let (epoch_duration, gas_epoch) = read_epoch_parameter(storage)
        .expect("Couldn't read epoch duration parameters");

    // read vp whitelist
    let vp_whitelist_key = vp_whitelist_storage_key();
    let (value, gas_vp) = storage
        .read(&vp_whitelist_key)
        .map_err(ReadError::StorageError)?;
    let vp_whitelist: Vec<String> =
        decode(value.ok_or(ReadError::ParametersMissing)?)
            .map_err(ReadError::StorageTypeError)?;

    // read tx whitelist
    let tx_whitelist_key = tx_whitelist_storage_key();
    let (value, gas_tx) = storage
        .read(&tx_whitelist_key)
        .map_err(ReadError::StorageError)?;
    let tx_whitelist: Vec<String> =
        decode(value.ok_or(ReadError::ParametersMissing)?)
            .map_err(ReadError::StorageTypeError)?;

    let max_expected_time_per_block_key = max_expected_time_per_block_key();
    let (value, gas_time) = storage
        .read(&max_expected_time_per_block_key)
        .map_err(ReadError::StorageError)?;
    let max_expected_time_per_block: DurationSecs =
        decode(value.ok_or(ReadError::ParametersMissing)?)
            .map_err(ReadError::StorageTypeError)?;

    Ok((
        Parameters {
            epoch_duration,
            max_expected_time_per_block,
            vp_whitelist,
            tx_whitelist,
        },
        gas_epoch + gas_tx + gas_vp + gas_time,
    ))
}

/// Read the the epoch duration parameter from store
pub fn read_epoch_parameter<DB, H>(
    storage: &Storage<DB, H>,
) -> std::result::Result<(EpochDuration, u64), ReadError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    // read epoch
    let epoch_key = epoch_storage_key();
    let (value, gas) =
        storage.read(&epoch_key).map_err(ReadError::StorageError)?;
    let epoch_duration: EpochDuration =
        decode(value.ok_or(ReadError::ParametersMissing)?)
            .map_err(ReadError::StorageTypeError)?;

    Ok((epoch_duration, gas))
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum WriteError {
    #[error("Storage error: {0}")]
    StorageError(storage::Error),
    #[error("Serialize error: {0}")]
    SerializeError(String),
}

/// Update the  parameters in storage. Returns the parameters and gas
/// cost.
pub fn update<DB, H, T>(
    storage: &mut Storage<DB, H>,
    value: &T,
    key: Key,
) -> std::result::Result<u64, WriteError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
    T: BorshSerialize,
{
    let serialized_value = value
        .try_to_vec()
        .map_err(|e| WriteError::SerializeError(e.to_string()))?;
    let (gas, _size_diff) = storage
        .write(&key, serialized_value)
        .map_err(WriteError::StorageError)?;
    Ok(gas)
}

/// Update the epoch parameter in storage. Returns the parameters and gas
/// cost.
pub fn update_epoch_parameter<DB, H>(
    storage: &mut Storage<DB, H>,
    value: &EpochDuration,
) -> std::result::Result<u64, WriteError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    let key = epoch_storage_key();
    update(storage, value, key)
}

/// Update the tx whitelist parameter in storage. Returns the parameters and gas
/// cost.
pub fn update_tx_whitelist_parameter<DB, H>(
    storage: &mut Storage<DB, H>,
    value: Vec<String>,
) -> std::result::Result<u64, WriteError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    let key = tx_whitelist_storage_key();
    update(storage, &value, key)
}

/// Update the vp whitelist parameter in storage. Returns the parameters and gas
/// cost.
pub fn update_vp_whitelist_parameter<DB, H>(
    storage: &mut Storage<DB, H>,
    value: Vec<String>,
) -> std::result::Result<u64, WriteError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    let key = vp_whitelist_storage_key();
    update(storage, &value, key)
}

/// Update the max_expected_time_per_block parameter in storage. Returns the
/// parameters and gas cost.
pub fn update_max_expected_time_per_block_parameter<DB, H>(
    storage: &mut Storage<DB, H>,
    value: &DurationSecs,
) -> std::result::Result<u64, WriteError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    let key = max_expected_time_per_block_key();
    update(storage, value, key)
}

impl<'a, DB, H, CA> NativeVp for ParametersVp<'a, DB, H, CA>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Error = Error;

    const ADDR: InternalAddress = ADDR;

    fn validate_tx(
        &self,
        _tx_data: &[u8],
        _keys_changed: &BTreeSet<Key>,
        _verifiers: &BTreeSet<Address>,
    ) -> Result<bool> {
        // TODO allow parameters change by over 2/3 validator voting power
        // No changes are currently permitted
        Ok(false)
    }
}

/// Storage key used for epoch parameter.
pub fn epoch_storage_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(Address::Internal(ADDR)),
            DbKeySeg::StringSeg(EPOCH_DURATION_KEY.to_string()),
        ],
    }
}

/// Storage key used for vp whitelist parameter.
pub fn vp_whitelist_storage_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(Address::Internal(ADDR)),
            DbKeySeg::StringSeg(VP_WHITELIST_KEY.to_string()),
        ],
    }
}

/// Storage key used for tx whitelist parameter.
pub fn tx_whitelist_storage_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(Address::Internal(ADDR)),
            DbKeySeg::StringSeg(TX_WHITELIST_KEY.to_string()),
        ],
    }
}

/// Storage key used for tx whitelist parameter.
pub fn max_expected_time_per_block_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(Address::Internal(ADDR)),
            DbKeySeg::StringSeg(MAX_EXPECTED_TIME_PER_BLOCK_KEY.to_string()),
        ],
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}
