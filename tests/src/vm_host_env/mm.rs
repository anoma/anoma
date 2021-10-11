use std::cell::RefCell;
use std::collections::HashSet;

use anoma::gossip::mm::MmHost;
use anoma::vm;

/// This module combines the native host function implementations from
/// `native_mm_host_env` with the functions exposed to the matchmaker wasm
/// that will call to the native functions, instead of interfacing via a
/// wasm runtime. It can be used for host environment integration tests.
pub mod mm_host_env {
    pub use anoma_vm_env::matchmaker_prelude::*;

    pub use super::native_mm_host_env::*;
}

/// Host environment structures required for transactions.
#[derive(Debug, Default)]
pub struct TestMmEnv {
    pub mm: TestMatchmaker,
}

#[derive(Debug, Default, Clone)]
pub struct TestMatchmaker {
    pub state: RefCell<Vec<u8>>,
    pub tx: RefCell<Vec<u8>>,
}

impl MmHost for TestMatchmaker {
    fn remove_intents(&self, _intents_id: HashSet<Vec<u8>>) {
        println!(
            "Matchmaker is trying to remove intents. This currently has no \
             effect in the tests.",
        );
    }

    fn inject_tx(&self, tx_data: Vec<u8>) {
        *self.tx.borrow_mut() = tx_data;
        println!(
            "Matchmaker is trying to inject a tx. The tx data can be obtained \
             via the `get_tx_data` function."
        );
    }

    fn update_state(&self, data: Vec<u8>) {
        *self.state.borrow_mut() = data;
        println!("Matchmaker is updating its state.");
    }
}

/// Initialize the host environment inside the [`mm_host_env`] module.
pub fn init_mm_env() {
    mm_host_env::ENV.with(|env| {
        *env.borrow_mut() =
            Some(vm::host_env::testing::mm_env(TestMatchmaker::default()))
    });
}

/// Get the last state of the matchmaker.
pub fn get_mm_state() -> Vec<u8> {
    mm_host_env::ENV
        .with(|env| env.borrow().as_ref().unwrap().mm.state.borrow().clone())
}

/// Get the last submitted transaction's data from the matchmaker.
pub fn get_tx_data() -> Vec<u8> {
    mm_host_env::ENV
        .with(|env| env.borrow().as_ref().unwrap().mm.tx.borrow().clone())
}

/// This module allows to test code with tx host environment functions.
/// It keeps a thread-local global `TxEnv`, which is passed to any of
/// invoked host environment functions and so it must be initialized
/// before the test.
mod native_mm_host_env {

    use std::cell::RefCell;

    use anoma::vm::host_env::*;
    use anoma::vm::memory::testing::NativeMemory;
    // TODO replace with `std::concat_idents` once stabilized (https://github.com/rust-lang/rust/issues/29599)
    use concat_idents::concat_idents;

    use super::*;

    thread_local! {
        pub static ENV: RefCell<Option<MatchmakerEnv<NativeMemory, TestMatchmaker>>> = RefCell::new(None);
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
                        ENV.with(|env| {
                            let env = env.borrow_mut();
                            let env = env.as_ref().expect("Did you forget to initialize the ENV?");

                            // Call the `host_env` function and unwrap any
                            // runtime errors
                            $fn( &env, $($arg),* )
                        })
                    }
                });
            };

            // non-unit return type
            ( $fn:ident ( $($arg:ident : $type:ty),* $(,)?) -> $ret:ty ) => {
                concat_idents!(extern_fn_name = anoma, _, $fn {
                    #[no_mangle]
                    extern "C" fn extern_fn_name( $($arg: $type),* ) -> $ret {
                        ENV.with(|env| {
                            let env = env.borrow_mut();
                            let env = env.as_ref().expect("Did you forget to initialize the ENV?");

                            // Call the `host_env` function and unwrap any
                            // runtime errors
                            $fn( &env, $($arg),* )
                        })
                    }
                });
            }
        }

    // Implement all the exported functions from
    // [`anoma_vm_env::imports::mm`] `extern "C"` section.
    native_host_fn!(mm_send_match(data_ptr: u64, data_len: u64));
    native_host_fn!(mm_update_state(state_ptr: u64, state_len: u64));
    native_host_fn!(mm_remove_intents(
        intents_id_ptr: u64,
        intents_id_len: u64
    ));
    native_host_fn!(mm_log_string(str_ptr: u64, str_len: u64));
}
