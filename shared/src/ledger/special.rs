//! Internal VP for storing special VPs

use std::collections::HashSet;

use thiserror::Error;

use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{DbKeySeg, Key};
use crate::vm::WasmCacheAccess;

const ADDR: InternalAddress = InternalAddress::Special;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// Function result type
pub type Result<T> = std::result::Result<T, Error>;

/// This internal address's VP
pub struct SpecialVp<'a, DB, H, CA>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H, CA>,
}

/// Special VPs
pub struct Special {
    /// The VP for implicit addresses
    pub implicit_vp: Vec<u8>,
}

/// The storage key for the implicit VP
pub fn implicit_vp_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(Address::Internal(ADDR)),
            DbKeySeg::StringSeg(String::from("implicit")),
        ],
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum ReadError {
    #[error("Storage read error: {0}")]
    StorageError(storage::Error),
    #[error("Missing implicit VP")]
    MissingImplicitVp,
}

/// Read the VP for implicit addresses, with gas meter.
pub fn read_implicit_vp<DB, H>(
    storage: &Storage<DB, H>,
) -> std::result::Result<(Vec<u8>, u64), ReadError>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: storage::StorageHasher,
{
    let key = implicit_vp_key();
    let (value, gas) = storage.read(&key).map_err(ReadError::StorageError)?;
    let value = value.ok_or(ReadError::MissingImplicitVp)?;

    Ok((value, gas))
}

impl<'a, DB, H, CA> NativeVp for SpecialVp<'a, DB, H, CA>
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
        _keys_changed: &HashSet<Key>,
        _verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        // todo: update the implicit vp on a 2/3 majority
        Ok(false)
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}
