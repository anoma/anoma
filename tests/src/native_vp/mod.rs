mod pos;

use std::collections::BTreeSet;

use anoma::ledger::native_vp::{Ctx, NativeVp};
use anoma::ledger::storage::mockdb::MockDB;
use anoma::ledger::storage::Sha256Hasher;
use anoma::types::address::Address;
use anoma::types::storage;
use anoma::vm::WasmCacheRwAccess;

use crate::tx::TestTxEnv;

type NativeVpCtx<'a> = Ctx<'a, MockDB, Sha256Hasher, WasmCacheRwAccess>;

#[derive(Debug)]
pub struct TestNativeVpEnv {
    pub tx_env: TestTxEnv,
    pub address: Address,
    pub verifiers: BTreeSet<Address>,
    pub keys_changed: BTreeSet<storage::Key>,
}

impl TestNativeVpEnv {
    pub fn from_tx_env(tx_env: TestTxEnv, address: Address) -> Self {
        // Find the tx verifiers and keys_changes the same way as protocol would
        let verifiers = tx_env.get_verifiers();

        let keys_changed = tx_env.all_touched_storage_keys();

        Self {
            address,
            tx_env,
            verifiers,
            keys_changed,
        }
    }
}

impl TestNativeVpEnv {
    /// Run some transaction code `apply_tx` and validate it with a native VP
    pub fn validate_tx<'a, T>(
        &'a self,
        init_native_vp: impl Fn(NativeVpCtx<'a>) -> T,
    ) -> Result<bool, <T as NativeVp>::Error>
    where
        T: NativeVp,
    {
        let ctx = Ctx {
            iterators: Default::default(),
            gas_meter: Default::default(),
            storage: &self.tx_env.storage,
            write_log: &self.tx_env.write_log,
            tx: &self.tx_env.tx,
            vp_wasm_cache: self.tx_env.vp_wasm_cache.clone(),
            address: &self.address,
            keys_changed: &self.keys_changed,
            verifiers: &self.verifiers,
        };
        let tx_data = self.tx_env.tx.data.as_ref().cloned().unwrap_or_default();
        let native_vp = init_native_vp(ctx);

        native_vp.validate_tx(&tx_data, &self.keys_changed, &self.verifiers)
    }
}
