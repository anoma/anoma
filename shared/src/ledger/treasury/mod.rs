//! Treasury VP

use std::collections::BTreeSet;
/// treasury parameters
pub mod parameters;
/// treasury storage
pub mod storage;

use thiserror::Error;

use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::Key;
use crate::vm::WasmCacheAccess;

/// Internal treasury address
pub const ADDRESS: Address = Address::Internal(InternalAddress::Treasury);

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// Treasury functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Treasury VP
pub struct TreasuryVp<'a, DB, H, CA>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H, CA>,
}

impl<'a, DB, H, CA> NativeVp for TreasuryVp<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::Treasury;

    fn validate_tx(
        &self,
        _tx_data: &[u8],
        _keys_changed: &BTreeSet<Key>,
        _verifiers: &BTreeSet<Address>,
    ) -> Result<bool> {
        Ok(false)
    }
}
