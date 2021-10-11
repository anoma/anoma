//! The wasm host environment.
//!
//! Here, we expose the host functions into wasm's
//! imports, so they can be called from inside the wasm.

use wasmer::{
    Function, HostEnvInitError, ImportObject, Instance, Memory, Store,
    WasmerEnv,
};

use crate::gossip::mm::MmHost;
use crate::ledger::storage::{self, StorageHasher};
use crate::vm::host_env;
use crate::vm::host_env::{
    FilterEnv, MatchmakerEnv, TxEnv, VpEnv, VpEvaluator,
};
use crate::vm::wasm::memory::WasmMemory;

impl<DB, H> WasmerEnv for TxEnv<'_, WasmMemory, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    fn init_with_instance(
        &mut self,
        instance: &Instance,
    ) -> std::result::Result<(), HostEnvInitError> {
        self.memory.init_env_memory(&instance.exports)
    }
}

impl<DB, H, EVAL> WasmerEnv for VpEnv<'_, WasmMemory, DB, H, EVAL>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    EVAL: VpEvaluator,
{
    fn init_with_instance(
        &mut self,
        instance: &Instance,
    ) -> std::result::Result<(), HostEnvInitError> {
        self.memory.init_env_memory(&instance.exports)
    }
}

impl<MM> WasmerEnv for MatchmakerEnv<WasmMemory, MM>
where
    MM: MmHost,
{
    fn init_with_instance(
        &mut self,
        instance: &Instance,
    ) -> std::result::Result<(), HostEnvInitError> {
        self.memory.init_env_memory(&instance.exports)
    }
}

impl WasmerEnv for FilterEnv<WasmMemory> {
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
pub fn tx_imports<DB, H>(
    wasm_store: &Store,
    initial_memory: Memory,
    env: TxEnv<'static, WasmMemory, DB, H>,
) -> ImportObject
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
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
            "anoma_tx_delete" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_delete),
            "anoma_tx_iter_prefix" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_iter_prefix),
            "anoma_tx_iter_next" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_iter_next),
            "anoma_tx_insert_verifier" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_insert_verifier),
            "anoma_tx_update_validity_predicate" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_update_validity_predicate),
            "anoma_tx_init_account" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_init_account),
            "anoma_tx_get_chain_id" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_chain_id),
            "anoma_tx_get_block_height" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_block_height),
            "anoma_tx_get_block_hash" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_block_hash),
            "anoma_tx_get_block_epoch" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_get_block_epoch),
            "anoma_tx_log_string" => Function::new_native_with_env(wasm_store, env.clone(), host_env::tx_log_string),
        },
    }
}

/// Prepare imports (memory and host functions) exposed to the vm guest running
/// validity predicate code
pub fn vp_imports<DB, H, EVAL>(
    wasm_store: &Store,
    initial_memory: Memory,
    env: VpEnv<'static, WasmMemory, DB, H, EVAL>,
) -> ImportObject
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    EVAL: VpEvaluator<Db = DB, H = H, Eval = EVAL>,
{
    wasmer::imports! {
        // default namespace
        "env" => {
            "memory" => initial_memory,
            "gas" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_charge_gas),
            "anoma_vp_read_pre" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_read_pre),
            "anoma_vp_read_post" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_read_post),
            "anoma_vp_result_buffer" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_result_buffer),
            "anoma_vp_has_key_pre" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_has_key_pre),
            "anoma_vp_has_key_post" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_has_key_post),
            "anoma_vp_iter_prefix" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_iter_prefix),
            "anoma_vp_iter_pre_next" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_iter_pre_next),
            "anoma_vp_iter_post_next" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_iter_post_next),
            "anoma_vp_get_chain_id" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_chain_id),
            "anoma_vp_get_block_height" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_block_height),
            "anoma_vp_get_block_hash" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_block_hash),
            "anoma_vp_get_block_epoch" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_get_block_epoch),
            "anoma_vp_verify_tx_signature" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_verify_tx_signature),
            "anoma_vp_eval" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_eval),
            "anoma_vp_log_string" => Function::new_native_with_env(wasm_store, env.clone(), host_env::vp_log_string),
        },
    }
}

/// Prepare imports (memory and host functions) exposed to the vm guest running
/// matchmaker code
pub fn mm_imports<MM>(
    wasm_store: &Store,
    initial_memory: Memory,
    mm: MM,
) -> ImportObject
where
    MM: 'static + MmHost,
{
    let env = MatchmakerEnv {
        memory: WasmMemory::default(),
        mm,
    };
    wasmer::imports! {
        // default namespace
        "env" => {
            "memory" => initial_memory,
            "anoma_mm_send_match" => Function::new_native_with_env(wasm_store, env.clone(), host_env::mm_send_match),
            "anoma_mm_update_state" => Function::new_native_with_env(wasm_store, env.clone(), host_env::mm_update_state),
            "anoma_mm_remove_intents" => Function::new_native_with_env(wasm_store, env.clone(), host_env::mm_remove_intents),
            "anoma_mm_log_string" => Function::new_native_with_env(wasm_store, env, host_env::mm_log_string),
        },
    }
}

/// Prepare imports (memory and host functions) exposed to the vm guest running
/// filter code
pub fn mm_filter_imports(
    wasm_store: &Store,
    initial_memory: Memory,
) -> ImportObject {
    let env = FilterEnv {
        memory: WasmMemory::default(),
    };
    wasmer::imports! {
        // default namespace
        "env" => {
            "memory" => initial_memory,
            "anoma_filter_log_string" => Function::new_native_with_env(wasm_store, env, host_env::mm_filter_log_string),
        },
    }
}
