mod pos;

use anoma::ledger::native_vp::{Ctx, NativeVp};
use anoma::ledger::storage::mockdb::MockDB;
use anoma::ledger::storage::Sha256Hasher;
use anoma::vm::wasm::compilation_cache;
use anoma::vm::wasm::compilation_cache::common::Cache;
use anoma::vm::{wasm, WasmCacheRwAccess};
use tempfile::TempDir;

use crate::tx::TestTxEnv;

type NativeVpCtx<'a> = Ctx<'a, MockDB, Sha256Hasher, WasmCacheRwAccess>;
type VpCache = Cache<compilation_cache::vp::Name, WasmCacheRwAccess>;

#[derive(Debug)]
pub struct TestNativeVpEnv {
    pub vp_cache_dir: TempDir,
    pub vp_wasm_cache: VpCache,
    pub tx_env: TestTxEnv,
}

impl TestNativeVpEnv {
    pub fn new(tx_env: TestTxEnv) -> Self {
        let (vp_wasm_cache, vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();

        Self {
            vp_cache_dir,
            vp_wasm_cache,
            tx_env,
        }
    }
}

impl Default for TestNativeVpEnv {
    fn default() -> Self {
        let (vp_wasm_cache, vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();

        Self {
            vp_cache_dir,
            vp_wasm_cache,
            tx_env: TestTxEnv::default(),
        }
    }
}

impl TestNativeVpEnv {
    /// Run some transaction code `apply_tx` and validate it with a native VP
    pub fn validate_tx<'a, T>(
        &'a self,
        init_native_vp: impl Fn(NativeVpCtx<'a>) -> T,
        // The function is applied on the `tx_data` when called
        mut apply_tx: impl FnMut(&[u8]),
    ) -> Result<bool, <T as NativeVp>::Error>
    where
        T: NativeVp,
    {
        let tx_data = self.tx_env.tx.data.as_ref().cloned().unwrap_or_default();
        apply_tx(&tx_data);

        // Find the tx verifiers and keys_changes the same way as protocol would
        let verifiers = self.tx_env.get_verifiers();

        let keys_changed = self.tx_env.all_touched_storage_keys();

        let ctx = Ctx {
            iterators: Default::default(),
            gas_meter: Default::default(),
            storage: &self.tx_env.storage,
            write_log: &self.tx_env.write_log,
            tx: &self.tx_env.tx,
            vp_wasm_cache: self.tx_env.vp_wasm_cache.clone(),
        };
        let tx_data = self.tx_env.tx.data.as_ref().cloned().unwrap_or_default();
        let native_vp = init_native_vp(ctx);

        native_vp.validate_tx(&tx_data, &keys_changed, &verifiers)
    }
}
