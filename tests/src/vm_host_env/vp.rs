use std::collections::BTreeSet;

use anoma::ledger::gas::VpGasMeter;
use anoma::ledger::storage::mockdb::MockDB;
use anoma::ledger::storage::testing::TestStorage;
use anoma::ledger::storage::write_log::WriteLog;
use anoma::proto::Tx;
use anoma::types::address::{self, Address};
use anoma::types::storage::{self, Key, TxIndex};
use anoma::vm::prefix_iter::PrefixIterators;
use anoma::vm::wasm::{self, VpCache};
use anoma::vm::{self, WasmCacheRwAccess};
use tempfile::TempDir;

use crate::tx::{tx_host_env, TestTxEnv};

/// This module combines the native host function implementations from
/// `native_vp_host_env` with the functions exposed to the vp wasm
/// that will call to the native functions, instead of interfacing via a
/// wasm runtime. It can be used for host environment integration tests.
pub mod vp_host_env {
    pub use anoma_vm_env::vp_prelude::*;

    pub use super::native_vp_host_env::*;
}

/// Host environment structures required for transactions.
pub struct TestVpEnv {
    pub addr: Address,
    pub storage: TestStorage,
    pub write_log: WriteLog,
    pub iterators: PrefixIterators<'static, MockDB>,
    pub gas_meter: VpGasMeter,
    pub tx: Tx,
    pub tx_index: TxIndex,
    pub keys_changed: BTreeSet<storage::Key>,
    pub verifiers: BTreeSet<Address>,
    pub eval_runner: native_vp_host_env::VpEval,
    pub result_buffer: Option<Vec<u8>>,
    pub vp_wasm_cache: VpCache<WasmCacheRwAccess>,
    pub vp_cache_dir: TempDir,
}

impl Default for TestVpEnv {
    fn default() -> Self {
        #[cfg(feature = "wasm-runtime")]
        let eval_runner = anoma::vm::wasm::run::VpEvalWasm::default();
        #[cfg(not(feature = "wasm-runtime"))]
        let eval_runner = native_vp_host_env::VpEval;

        let (vp_wasm_cache, vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();

        Self {
            addr: address::testing::established_address_1(),
            storage: TestStorage::default(),
            write_log: WriteLog::default(),
            iterators: PrefixIterators::default(),
            gas_meter: VpGasMeter::default(),
            tx: Tx::new(vec![], None),
            tx_index: TxIndex::default(),
            keys_changed: BTreeSet::default(),
            verifiers: BTreeSet::default(),
            eval_runner,
            result_buffer: None,
            vp_wasm_cache,
            vp_cache_dir,
        }
    }
}

impl TestVpEnv {
    pub fn all_touched_storage_keys(&self) -> BTreeSet<Key> {
        self.write_log.get_keys()
    }

    pub fn get_verifiers(&self) -> BTreeSet<Address> {
        self.write_log.verifiers_and_changed_keys(&self.verifiers).0
    }
}

/// This module allows to test code with vp host environment functions.
/// It keeps a thread-local global `VpEnv`, which is passed to any of
/// invoked host environment functions and so it must be initialized
/// before the test.
mod native_vp_host_env {

    use std::cell::RefCell;
    use std::pin::Pin;

    use anoma::ledger::storage::Sha256Hasher;
    use anoma::vm::host_env::*;
    use anoma::vm::WasmCacheRwAccess;
    // TODO replace with `std::concat_idents` once stabilized (https://github.com/rust-lang/rust/issues/29599)
    use concat_idents::concat_idents;

    use super::*;

    #[cfg(feature = "wasm-runtime")]
    pub type VpEval = anoma::vm::wasm::run::VpEvalWasm<
        MockDB,
        Sha256Hasher,
        WasmCacheRwAccess,
    >;
    #[cfg(not(feature = "wasm-runtime"))]
    pub struct VpEval;

    thread_local! {
        /// A [`TestVpEnv`] that can be used for VP host env functions calls
        /// that implements the WASM host environment in native environment.
        pub static ENV: RefCell<Option<Pin<Box<TestVpEnv>>>> =
            RefCell::new(None);
    }

    /// Initialize the VP environment in [`ENV`]. This will be used in the
    /// host env function calls via macro `native_host_fn!`.
    pub fn init() {
        ENV.with(|env| {
            let test_env = TestVpEnv::default();
            *env.borrow_mut() = Some(Box::pin(test_env));
        });
    }

    /// Set the VP host environment in [`ENV`] from the given [`TestVpEnv`].
    /// This will be used in the host env function calls via
    /// macro `native_host_fn!`.
    pub fn set(test_env: TestVpEnv) {
        ENV.with(|env| {
            *env.borrow_mut() = Some(Box::pin(test_env));
        });
    }

    /// Mutably borrow the [`TestVpEnv`] from [`ENV`]. The [`ENV`] must be
    /// initialized.
    pub fn with<T>(f: impl Fn(&mut TestVpEnv) -> T) -> T {
        ENV.with(|env| {
            let mut env = env.borrow_mut();
            let mut env = env
                .as_mut()
                .expect(
                    "Did you forget to initialize the ENV? (e.g. call to \
                     `vp_host_env::init()`)",
                )
                .as_mut();
            f(&mut *env)
        })
    }

    /// Take the [`TestVpEnv`] out of [`ENV`]. The [`ENV`] must be initialized.
    pub fn take() -> TestVpEnv {
        ENV.with(|env| {
            let mut env = env.borrow_mut();
            let env = env.take().expect(
                "Did you forget to initialize the ENV? (e.g. call to \
                 `vp_host_env::init()`)",
            );
            let env = Pin::into_inner(env);
            *env
        })
    }

    /// Initialize the VP host environment in [`ENV`] by running a transaction.
    /// The transaction is expected to modify the storage sub-space of the given
    /// address `addr` or to add it to the set of verifiers using
    /// [`tx_host_env::insert_verifier`].
    pub fn init_from_tx(
        addr: Address,
        mut tx_env: TestTxEnv,
        mut apply_tx: impl FnMut(&Address),
    ) {
        // Write an empty validity predicate for the address, because it's used
        // to check if the address exists when we write into its storage
        let vp_key = Key::validity_predicate(&addr);
        tx_env.storage.write(&vp_key, vec![]).unwrap();

        tx_host_env::set(tx_env);
        apply_tx(&addr);

        let tx_env = tx_host_env::take();
        let verifiers_from_tx = &tx_env.verifiers;
        let (verifiers, keys_changed) = tx_env
            .write_log
            .verifiers_and_changed_keys(verifiers_from_tx);
        if !verifiers.contains(&addr) {
            panic!(
                "The VP for the given address has not been triggered by the \
                 transaction, {:#?}",
                keys_changed
            );
        }

        let vp_env = TestVpEnv {
            addr,
            storage: tx_env.storage,
            write_log: tx_env.write_log,
            keys_changed,
            verifiers,
            ..Default::default()
        };

        set(vp_env);
    }

    #[cfg(not(feature = "wasm-runtime"))]
    impl VpEvaluator for VpEval {
        type CA = WasmCacheRwAccess;
        type Db = MockDB;
        type Eval = VpEval;
        type H = Sha256Hasher;

        fn eval(
            &self,
            _ctx: VpCtx<'static, Self::Db, Self::H, Self::Eval, Self::CA>,
            _vp_code: Vec<u8>,
            _input_data: Vec<u8>,
        ) -> anoma::types::internal::HostEnvResult {
            unimplemented!(
                "The \"wasm-runtime\" feature must be enabled to test with \
                 the `eval` function."
            )
        }
    }

    /// A helper macro to create implementations of the host environment
    /// functions exported to wasm, which uses the environment from the
    /// `ENV` variable.
    macro_rules! native_host_fn {
            // unit return type
            ( $fn:ident ( $($arg:ident : $type:ty),* $(,)?) ) => {
                concat_idents!(extern_fn_name = anoma, _, $fn {
                    #[no_mangle]
                    extern "C" fn extern_fn_name( $($arg: $type),* ) {
                        with(|TestVpEnv {
                                addr,
                                storage,
                                write_log,
                                iterators,
                                gas_meter,
                            tx,
                            tx_index,
                                keys_changed,
                                verifiers,
                                eval_runner,
                                result_buffer,
                                vp_wasm_cache,
                                vp_cache_dir: _,
                            }: &mut TestVpEnv| {

                            let env = vm::host_env::testing::vp_env(
                                addr,
                                storage,
                                write_log,
                                iterators,
                                gas_meter,
                                tx,
                                tx_index,
                                verifiers,
                                result_buffer,
                                keys_changed,
                                eval_runner,
                                vp_wasm_cache,
                            );

                            // Call the `host_env` function and unwrap any
                            // runtime errors
                            $fn( &env, $($arg),* ).unwrap()
                        })
                    }
                });
            };

            // non-unit return type
            ( $fn:ident ( $($arg:ident : $type:ty),* $(,)?) -> $ret:ty ) => {
                concat_idents!(extern_fn_name = anoma, _, $fn {
                    #[no_mangle]
                    extern "C" fn extern_fn_name( $($arg: $type),* ) -> $ret {
                        with(|TestVpEnv {
                                addr,
                                storage,
                                write_log,
                                iterators,
                                gas_meter,
                            tx,
                            tx_index,
                                keys_changed,
                                verifiers,
                                eval_runner,
                                result_buffer,
                                vp_wasm_cache,
                                vp_cache_dir: _,
                            }: &mut TestVpEnv| {

                            let env = vm::host_env::testing::vp_env(
                                addr,
                                storage,
                                write_log,
                                iterators,
                                gas_meter,
                                tx,
                                tx_index,
                                verifiers,
                                result_buffer,
                                keys_changed,
                                eval_runner,
                                vp_wasm_cache,
                            );

                            // Call the `host_env` function and unwrap any
                            // runtime errors
                            $fn( &env, $($arg),* ).unwrap()
                        })
                    }
                });
            }
        }

    // Implement all the exported functions from
    // [`anoma_vm_env::imports::vp`] `extern "C"` section.
    native_host_fn!(vp_read_pre(key_ptr: u64, key_len: u64) -> i64);
    native_host_fn!(vp_read_post(key_ptr: u64, key_len: u64) -> i64);
    native_host_fn!(vp_result_buffer(result_ptr: u64));
    native_host_fn!(vp_has_key_pre(key_ptr: u64, key_len: u64) -> i64);
    native_host_fn!(vp_has_key_post(key_ptr: u64, key_len: u64) -> i64);
    native_host_fn!(vp_iter_prefix(prefix_ptr: u64, prefix_len: u64) -> u64);
    native_host_fn!(vp_iter_pre_next(iter_id: u64) -> i64);
    native_host_fn!(vp_iter_post_next(iter_id: u64) -> i64);
    native_host_fn!(vp_get_chain_id(result_ptr: u64));
    native_host_fn!(vp_get_block_height() -> u64);
    native_host_fn!(vp_get_block_hash(result_ptr: u64));
    native_host_fn!(vp_get_tx_code_hash(result_ptr: u64));
    native_host_fn!(vp_get_block_epoch() -> u64);
    native_host_fn!(vp_verify_tx_signature(
            pk_ptr: u64,
            pk_len: u64,
            sig_ptr: u64,
            sig_len: u64,
        ) -> i64);
    native_host_fn!(vp_eval(
            vp_code_ptr: u64,
            vp_code_len: u64,
            input_data_ptr: u64,
            input_data_len: u64,
        ) -> i64);
    native_host_fn!(vp_log_string(str_ptr: u64, str_len: u64));
}
