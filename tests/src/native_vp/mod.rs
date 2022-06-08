mod pos;

use std::collections::BTreeSet;

use anoma::ledger::native_vp::{Ctx, NativeVp};
use anoma::ledger::storage::mockdb::MockDB;
use anoma::ledger::storage::Sha256Hasher;
use anoma::vm::wasm::compilation_cache;
use anoma::vm::wasm::compilation_cache::common::Cache;
use anoma::vm::{wasm, WasmCacheRwAccess};
use anoma_vm_env::tx_prelude::Address;
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
        // // The address of the native VP we're testing
        let self_addr = Address::Internal(<T as NativeVp>::ADDR);

        let tx_data = self.tx_env.tx.data.as_ref().cloned().unwrap_or_default();
        apply_tx(&tx_data);

        // Find the tx verifiers and keys_changes the same way as protocol would
        let verifiers = self
            .tx_env
            .write_log
            .verifiers_changed_keys(&self.tx_env.verifiers);

        let keys_changed = verifiers
            .get(&self_addr)
            .expect(
                "The transaction didn't touch any keys of this native VP and \
                 it isn't set as the verifier of the tx",
            )
            .clone();
        let verifiers = verifiers
            .iter()
            .map(|(addr, _)| addr)
            .cloned()
            .collect::<BTreeSet<_>>();

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
