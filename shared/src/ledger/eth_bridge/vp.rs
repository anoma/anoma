//! Validity predicate for the Ethereum bridge

use std::collections::BTreeSet;

use crate::ledger::native_vp::{Ctx, NativeVp};
use crate::ledger::storage as ledger_storage;
use crate::ledger::storage::StorageHasher;
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::Key;
use crate::vm::WasmCacheAccess;

/// Internal address for the Ethereum bridge VP
pub const ADDRESS: Address = Address::Internal(InternalAddress::EthBridge);

/// Validity predicate for the Ethereum bridge
pub struct EthBridge<'ctx, DB, H, CA>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
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
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
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
        tracing::debug!(
            tx_data_len = _tx_data.len(),
            keys_changed_len = _keys_changed.len(),
            verifiers_len = _verifiers.len(),
            "Validity predicate triggered",
        );
        Ok(false)
    }
}
