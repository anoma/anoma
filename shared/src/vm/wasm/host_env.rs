//! The wasm host environment.
//!
//! Here, we expose the host functions into wasm's
//! imports, so they can be called from inside the wasm.

use wasmer::{
    Function, HostEnvInitError, ImportObject, Instance, Memory, Store,
    WasmerEnv,
};

use crate::ledger::storage::{self, StorageHasher};
use crate::vm::host_env::{TxVmEnv, VpEvaluator, VpVmEnv};
use crate::vm::wasm::memory::WasmMemory;
use crate::vm::{host_env, WasmCacheAccess};

impl<DB, H, CA> WasmerEnv for TxVmEnv<'_, WasmMemory, DB, H, CA>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    fn init_with_instance(
        &mut self,
        instance: &Instance,
    ) -> std::result::Result<(), HostEnvInitError> {
        self.memory.init_env_memory(&instance.exports)
    }
}

impl<DB, H, EVAL, CA> WasmerEnv for VpVmEnv<'_, WasmMemory, DB, H, EVAL, CA>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    EVAL: VpEvaluator,
    CA: WasmCacheAccess,
{
    fn init_with_instance(
        &mut self,
        instance: &Instance,
    ) -> std::result::Result<(), HostEnvInitError> {
        self.memory.init_env_memory(&instance.exports)
    }
}

/// Prepare imports (memory and host functions) exposed to the vm guest running
/// transaction code
#[allow(clippy::too_many_arguments)]
pub fn tx_imports<DB, H, CA>(
    wasm_store: &Store,
    initial_memory: Memory,
    env: TxVmEnv<'static, WasmMemory, DB, H, CA>,
) -> ImportObject
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    wasmer::imports! {
        // default namespace
        "env" => {
            "memory" => initial_memory,
            "gas" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_charge_gas),
            "anoma_tx_read" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_read),
            "anoma_tx_result_buffer" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_result_buffer),
            "anoma_tx_has_key" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_has_key),
            "anoma_tx_write" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_write),
            "anoma_tx_write_temp" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_write_temp),
            "anoma_tx_delete" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_delete),
            "anoma_tx_iter_prefix" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_iter_prefix),
            "anoma_tx_iter_next" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_iter_next),
            "anoma_tx_insert_verifier" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_insert_verifier),
            "anoma_tx_update_validity_predicate" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_update_validity_predicate),
            "anoma_tx_init_account" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_init_account),
            "anoma_tx_emit_ibc_event" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_emit_ibc_event),
            "anoma_tx_get_chain_id" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_chain_id),
            "anoma_tx_get_block_height" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_block_height),
            "anoma_tx_get_block_time" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_block_time),
            "anoma_tx_get_block_hash" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_block_hash),
            "anoma_tx_get_block_epoch" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_block_epoch),
            "anoma_tx_log_string" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_log_string),
        },
    }
}

/// Prepare imports (memory and host functions) exposed to the vm guest running
/// validity predicate code
pub fn vp_imports<DB, H, EVAL, CA>(
    wasm_store: &Store,
    initial_memory: Memory,
    env: VpVmEnv<'static, WasmMemory, DB, H, EVAL, CA>,
) -> ImportObject
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    EVAL: VpEvaluator<Db = DB, H = H, Eval = EVAL, CA = CA>,
    CA: WasmCacheAccess,
{
    wasmer::imports! {
        // default namespace
        "env" => {
            "memory" => initial_memory,
            "gas" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_charge_gas),
            "anoma_vp_read_pre" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_read_pre),
            "anoma_vp_read_post" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_read_post),
            "anoma_vp_read_temp" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_read_temp),
            "anoma_vp_result_buffer" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_result_buffer),
            "anoma_vp_has_key_pre" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_has_key_pre),
            "anoma_vp_has_key_post" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_has_key_post),
            "anoma_vp_iter_prefix" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_iter_prefix),
            "anoma_vp_iter_pre_next" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_iter_pre_next),
            "anoma_vp_iter_post_next" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_iter_post_next),
            "anoma_vp_get_chain_id" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_chain_id),
            "anoma_vp_get_block_height" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_block_height),
            "anoma_vp_get_block_hash" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_block_hash),
            "anoma_vp_get_tx_code_hash" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_tx_code_hash),
            "anoma_vp_get_block_epoch" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_block_epoch),
            "anoma_vp_verify_tx_signature" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_verify_tx_signature),
            "anoma_vp_eval" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_eval),
            "anoma_vp_log_string" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_log_string),
        },
    }
}
