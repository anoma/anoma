use std::borrow::Borrow;
use std::collections::BTreeSet;

use anoma::ledger::gas::BlockGasMeter;
use anoma::ledger::parameters::{self, EpochDuration};
use anoma::ledger::storage::mockdb::MockDB;
use anoma::ledger::storage::testing::TestStorage;
use anoma::ledger::storage::write_log::WriteLog;
use anoma::proto::Tx;
use anoma::types::address::Address;
use anoma::types::storage::Key;
use anoma::types::time::DurationSecs;
use anoma::types::{key, token};
use anoma::vm::prefix_iter::PrefixIterators;
use anoma::vm::wasm::{self, TxCache, VpCache};
use anoma::vm::{self, WasmCacheRwAccess};
use anoma_vm_env::tx_prelude::BorshSerialize;
use derivative::Derivative;
use tempfile::TempDir;

/// This module combines the native host function implementations from
/// `native_tx_host_env` with the functions exposed to the tx wasm
/// that will call to the native functions, instead of interfacing via a
/// wasm runtime. It can be used for host environment integration tests.
pub mod tx_host_env {
    pub use anoma_vm_env::tx_prelude::*;

    pub use super::native_tx_host_env::*;
}

/// Host environment structures required for transactions.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct TestTxEnv {
    #[derivative(Debug = "ignore")]
    pub storage: TestStorage,
    pub write_log: WriteLog,
    pub iterators: PrefixIterators<'static, MockDB>,
    pub verifiers: BTreeSet<Address>,
    pub gas_meter: BlockGasMeter,
    pub result_buffer: Option<Vec<u8>>,
    pub vp_wasm_cache: VpCache<WasmCacheRwAccess>,
    pub vp_cache_dir: TempDir,
    pub tx_wasm_cache: TxCache<WasmCacheRwAccess>,
    pub tx_cache_dir: TempDir,
    pub tx: Tx,
}
impl Default for TestTxEnv {
    fn default() -> Self {
        let (vp_wasm_cache, vp_cache_dir) =
            wasm::compilation_cache::common::testing::cache();
        let (tx_wasm_cache, tx_cache_dir) =
            wasm::compilation_cache::common::testing::cache();

        Self {
            storage: TestStorage::default(),
            write_log: WriteLog::default(),
            iterators: PrefixIterators::default(),
            gas_meter: BlockGasMeter::default(),
            verifiers: BTreeSet::default(),
            result_buffer: None,
            vp_wasm_cache,
            vp_cache_dir,
            tx_wasm_cache,
            tx_cache_dir,
            tx: Tx::new(vec![], None),
        }
    }
}

impl TestTxEnv {
    pub fn all_touched_storage_keys(&self) -> BTreeSet<Key> {
        self.write_log.get_keys()
    }

    pub fn get_verifiers(&self) -> BTreeSet<Address> {
        self.write_log.verifiers_and_changed_keys(&self.verifiers).0
    }

    pub fn init_parameters(
        &mut self,
        epoch_duration: Option<EpochDuration>,
        vp_whitelist: Option<Vec<String>>,
        tx_whitelist: Option<Vec<String>>,
    ) {
        let _ = parameters::update_epoch_parameter(
            &mut self.storage,
            &epoch_duration.unwrap_or(EpochDuration {
                min_num_of_blocks: 1,
                min_duration: DurationSecs(5),
            }),
        );
        let _ = parameters::update_tx_whitelist_parameter(
            &mut self.storage,
            tx_whitelist.unwrap_or_default(),
        );
        let _ = parameters::update_vp_whitelist_parameter(
            &mut self.storage,
            vp_whitelist.unwrap_or_default(),
        );
    }

    /// Fake accounts existence by initializating their VP storage.
    /// This is needed for accounts that are being modified by a tx test to
    /// pass account existence check in `tx_write` function.
    pub fn spawn_accounts(
        &mut self,
        addresses: impl IntoIterator<Item = impl Borrow<Address>>,
    ) {
        for address in addresses {
            let key = Key::validity_predicate(address.borrow());
            let vp_code = vec![];
            self.storage
                .write(&key, vp_code)
                .expect("Unable to write VP");
        }
    }

    pub fn commit_tx_and_block(&mut self) {
        self.write_log.commit_tx();
        self.write_log
            .commit_block(&mut self.storage)
            .map_err(|err| println!("{:?}", err))
            .ok();
        self.iterators = PrefixIterators::default();
        self.verifiers = BTreeSet::default();
        self.gas_meter = BlockGasMeter::default();
    }

    /// Credit tokens to the target account.
    pub fn credit_tokens(
        &mut self,
        target: &Address,
        token: &Address,
        amount: token::Amount,
    ) {
        let storage_key = token::balance_key(token, target);
        self.storage
            .write(&storage_key, amount.try_to_vec().unwrap())
            .unwrap();
    }

    /// Set public key for the address.
    pub fn write_public_key(
        &mut self,
        address: &Address,
        public_key: &key::common::PublicKey,
    ) {
        let storage_key = key::pk_key(address);
        self.storage
            .write(&storage_key, public_key.try_to_vec().unwrap())
            .unwrap();
    }
}

/// This module allows to test code with tx host environment functions.
/// It keeps a thread-local global `TxEnv`, which is passed to any of
/// invoked host environment functions and so it must be initialized
/// before the test.
mod native_tx_host_env {

    use std::cell::RefCell;
    use std::pin::Pin;

    use anoma::vm::host_env::*;
    // TODO replace with `std::concat_idents` once stabilized (https://github.com/rust-lang/rust/issues/29599)
    use concat_idents::concat_idents;

    use super::*;

    thread_local! {
        /// A [`TestTxEnv`] that can be used for tx host env functions calls
        /// that implements the WASM host environment in native environment.
        pub static ENV: RefCell<Option<Pin<Box<TestTxEnv>>>> =
            RefCell::new(None);
    }

    /// Initialize the tx host environment in [`ENV`]. This will be used in the
    /// host env function calls via macro `native_host_fn!`.
    pub fn init() {
        ENV.with(|env| {
            let test_env = TestTxEnv::default();
            *env.borrow_mut() = Some(Box::pin(test_env));
        });
    }

    /// Set the tx host environment in [`ENV`] from the given [`TestTxEnv`].
    /// This will be used in the host env function calls via
    /// macro `native_host_fn!`.
    pub fn set(test_env: TestTxEnv) {
        ENV.with(|env| {
            *env.borrow_mut() = Some(Box::pin(test_env));
        });
    }

    /// Mutably borrow the [`TestTxEnv`] from [`ENV`]. The [`ENV`] must be
    /// initialized.
    pub fn with<T>(f: impl Fn(&mut TestTxEnv) -> T) -> T {
        ENV.with(|env| {
            let mut env = env.borrow_mut();
            let mut env = env
                .as_mut()
                .expect(
                    "Did you forget to initialize the ENV? (e.g. call to \
                     `tx_host_env::init()`)",
                )
                .as_mut();
            f(&mut *env)
        })
    }

    /// Take the [`TestTxEnv`] out of [`ENV`]. The [`ENV`] must be initialized.
    pub fn take() -> TestTxEnv {
        ENV.with(|env| {
            let mut env = env.borrow_mut();
            let env = env.take().expect(
                "Did you forget to initialize the ENV? (e.g. call to \
                 `tx_host_env::init()`)",
            );
            let env = Pin::into_inner(env);
            *env
        })
    }

    pub fn commit_tx_and_block() {
        with(|env| env.commit_tx_and_block())
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
                        with(|TestTxEnv {
                                storage,
                                write_log,
                                iterators,
                                verifiers,
                                gas_meter,
                                result_buffer,
                                vp_wasm_cache,
                                vp_cache_dir: _,
                                tx_wasm_cache,
                                tx_cache_dir: _,
                                tx: _,
                            }: &mut TestTxEnv| {

                            let tx_env = vm::host_env::testing::tx_env(
                                storage,
                                write_log,
                                iterators,
                                verifiers,
                                gas_meter,
                                result_buffer,
                                vp_wasm_cache,
                                tx_wasm_cache,
                            );

                            // Call the `host_env` function and unwrap any
                            // runtime errors
                            $fn( &tx_env, $($arg),* ).unwrap()
                        })
                    }
                });
            };

            // non-unit return type
            ( $fn:ident ( $($arg:ident : $type:ty),* $(,)?) -> $ret:ty ) => {
                concat_idents!(extern_fn_name = anoma, _, $fn {
                    #[no_mangle]
                    extern "C" fn extern_fn_name( $($arg: $type),* ) -> $ret {
                        with(|TestTxEnv {
                                storage,
                                write_log,
                                iterators,
                                verifiers,
                                gas_meter,
                                result_buffer,
                                vp_wasm_cache,
                                vp_cache_dir: _,
                                tx_wasm_cache,
                                tx_cache_dir: _,
                                tx: _,
                            }: &mut TestTxEnv| {

                            let tx_env = vm::host_env::testing::tx_env(
                                storage,
                                write_log,
                                iterators,
                                verifiers,
                                gas_meter,
                                result_buffer,
                                vp_wasm_cache,
                                tx_wasm_cache,
                            );

                            // Call the `host_env` function and unwrap any
                            // runtime errors
                            $fn( &tx_env, $($arg),* ).unwrap()
                        })
                    }
                });
            }
        }

    // Implement all the exported functions from
    // [`anoma_vm_env::imports::tx`] `extern "C"` section.
    native_host_fn!(tx_read(key_ptr: u64, key_len: u64) -> i64);
    native_host_fn!(tx_result_buffer(result_ptr: u64));
    native_host_fn!(tx_has_key(key_ptr: u64, key_len: u64) -> i64);
    native_host_fn!(tx_write(
        key_ptr: u64,
        key_len: u64,
        val_ptr: u64,
        val_len: u64
    ));
    native_host_fn!(tx_write_temp(
        key_ptr: u64,
        key_len: u64,
        val_ptr: u64,
        val_len: u64
    ));
    native_host_fn!(tx_delete(key_ptr: u64, key_len: u64));
    native_host_fn!(tx_iter_prefix(prefix_ptr: u64, prefix_len: u64) -> u64);
    native_host_fn!(tx_iter_next(iter_id: u64) -> i64);
    native_host_fn!(tx_insert_verifier(addr_ptr: u64, addr_len: u64));
    native_host_fn!(tx_update_validity_predicate(
        addr_ptr: u64,
        addr_len: u64,
        code_ptr: u64,
        code_len: u64,
    ));
    native_host_fn!(tx_init_account(
        code_ptr: u64,
        code_len: u64,
        result_ptr: u64
    ));
    native_host_fn!(tx_emit_ibc_event(event_ptr: u64, event_len: u64));
    native_host_fn!(tx_get_chain_id(result_ptr: u64));
    native_host_fn!(tx_get_block_height() -> u64);
    native_host_fn!(tx_get_block_time() -> i64);
    native_host_fn!(tx_get_block_hash(result_ptr: u64));
    native_host_fn!(tx_get_block_epoch() -> u64);
    native_host_fn!(tx_log_string(str_ptr: u64, str_len: u64));
}
