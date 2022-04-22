//! Bridge from Ethereum

use std::collections::BTreeSet;

use crate::ledger::native_vp::{Ctx, NativeVp};
use crate::ledger::storage;
use crate::ledger::storage::StorageHasher;
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::Key;
use crate::vm::WasmCacheAccess;

/// Internal address for the Ethereum bridge VP
pub const ADDRESS: Address = Address::Internal(InternalAddress::EthBridge);

/// Validity predicate for the Ethereum bridge
pub struct EthBridge<'ctx, DB, H, CA>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'ctx, DB, H, CA>,
}

#[allow(missing_docs)]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Internal error")]
    Internal,
}

impl<'a, DB, H, CA> NativeVp for EthBridge<'a, DB, H, CA>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::EthBridge;

    fn validate_tx(
        &self,
        _tx_data: &[u8],
        _keys_changed: &BTreeSet<Key>,
        _verifiers: &BTreeSet<Address>,
    ) -> Result<bool, Self::Error> {
        Ok(true)
    }
}
