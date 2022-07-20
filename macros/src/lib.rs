//! Anoma macros for generating WASM binding code for transactions, validity
//! predicates and matchmaker.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, ItemFn};

/// Generate WASM binding for a transaction main entrypoint function.
///
/// This macro expects a function with signature:
///
/// ```compiler_fail
/// fn apply_tx(
///     ctx: &mut Ctx,
///     tx_data: Vec<u8>
/// ) -> TxResult
/// ```
#[proc_macro_attribute]
pub fn transaction(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ItemFn);
    let ident = &ast.sig.ident;
    let gen = quote! {
        // Use `wee_alloc` as the global allocator.
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

        #ast

        // The module entrypoint callable by wasm runtime
        #[no_mangle]
        extern "C" fn _apply_tx(tx_data_ptr: u64, tx_data_len: u64) {
            let slice = unsafe {
                core::slice::from_raw_parts(
                    tx_data_ptr as *const u8,
                    tx_data_len as _,
                )
            };
            let tx_data = slice.to_vec();

            // The context on WASM side is only provided by the VM once its
            // being executed (in here it's implicit). But because we want to
            // have interface consistent with the VP interface, in which the
            // context is explicit, in here we're just using an empty `Ctx`
            // to "fake" it.
            let mut ctx = unsafe { anoma_tx_prelude::Ctx::new() };

            if let Err(err) = #ident(&mut ctx, tx_data) {
                anoma_tx_prelude::debug_log!("Transaction error: {}", err);
                // crash the transaction to abort
                panic!();
            }
        }
    };
    TokenStream::from(gen)
}

/// Generate WASM binding for validity predicate main entrypoint function.
///
/// This macro expects a function with signature:
///
/// ```compiler_fail
/// fn validate_tx(
///     ctx: &Ctx,
///     tx_data: Vec<u8>,
///     addr: Address,
///     keys_changed: BTreeSet<storage::Key>,
///     verifiers: BTreeSet<Address>
/// ) -> VpResult
/// ```
#[proc_macro_attribute]
pub fn validity_predicate(
    _attr: TokenStream,
    input: TokenStream,
) -> TokenStream {
    let ast = parse_macro_input!(input as ItemFn);
    let ident = &ast.sig.ident;
    let gen = quote! {
        // Use `wee_alloc` as the global allocator.
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

        #ast

        // The module entrypoint callable by wasm runtime
        #[no_mangle]
        extern "C" fn _validate_tx(
            // VP's account's address
            addr_ptr: u64,
            addr_len: u64,
            tx_data_ptr: u64,
            tx_data_len: u64,
            keys_changed_ptr: u64,
            keys_changed_len: u64,
            verifiers_ptr: u64,
            verifiers_len: u64,
        ) -> u64 {
            let slice = unsafe {
                core::slice::from_raw_parts(addr_ptr as *const u8, addr_len as _)
            };
            let addr = Address::try_from_slice(slice).unwrap();

            let slice = unsafe {
                core::slice::from_raw_parts(
                    tx_data_ptr as *const u8,
                    tx_data_len as _,
                )
            };
            let tx_data = slice.to_vec();

            let slice = unsafe {
                core::slice::from_raw_parts(
                    keys_changed_ptr as *const u8,
                    keys_changed_len as _,
                )
            };
            let keys_changed: BTreeSet<storage::Key> = BTreeSet::try_from_slice(slice).unwrap();

            let slice = unsafe {
                core::slice::from_raw_parts(
                    verifiers_ptr as *const u8,
                    verifiers_len as _,
                )
            };
            let verifiers: BTreeSet<Address> = BTreeSet::try_from_slice(slice).unwrap();

            // The context on WASM side is only provided by the VM once its
            // being executed (in here it's implicit). But because we want to
            // have interface identical with the native VPs, in which the
            // context is explicit, in here we're just using an empty `Ctx`
            // to "fake" it.
            let ctx = unsafe { anoma_vp_prelude::Ctx::new() };

            // run validation with the concrete type(s)
            match #ident(&ctx, tx_data, addr, keys_changed, verifiers)
            {
                Ok(true) => 1,
                Ok(false) => 0,
                Err(err) => {
                    anoma_vp_prelude::debug_log!("Validity predicate error: {}", err);
                    0
                },
            }
        }
    };
    TokenStream::from(gen)
}

/// Derive dynamic library binding for a matchmaker implementation.
///
/// This macro requires that the data structure implements
/// [`std::default::Default`] that is used to instantiate the matchmaker and
/// `anoma::types::matchmaker::AddIntent` to implement a custom matchmaker
/// algorithm.
///
/// # Examples
///
/// ```compiler_fail
/// use anoma::types::matchmaker::AddIntent;
/// use anoma_macros::Matchmaker;
///
/// #[derive(Default, Matchmaker)]
/// struct Matchmaker;
///
/// impl AddIntent for Matchmaker {
/// }
/// ```
#[proc_macro_derive(Matchmaker)]
pub fn matchmaker(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = &ast.ident;
    // Print out the original AST and add add_intent implementation and binding
    let gen = quote! {

        /// Add the marker trait
        #[automatically_derived]
        impl anoma::types::matchmaker::Matchmaker for #ident {}

        /// Instantiate a new matchmaker and return a pointer to it. The caller is
        /// responsible for making sure that the memory of the pointer will be dropped,
        /// which can be done by calling the `_drop_matchmaker` function.
        #[no_mangle]
        #[automatically_derived]
        fn _new_matchmaker() -> *mut std::ffi::c_void {
            let state = Box::new(#ident::default());
            let state_ptr = Box::into_raw(state) as *mut std::ffi::c_void;
            state_ptr
        }

        /// Drop the matchmaker's state to reclaim its memory
        #[no_mangle]
        #[automatically_derived]
        fn _drop_matchmaker(state_ptr: *mut std::ffi::c_void) {
            // The state will be dropped on going out of scope
            let _state = unsafe { Box::from_raw(state_ptr as *mut #ident) };
        }

        /// Ask the matchmaker to process a new intent
        #[allow(clippy::ptr_arg)]
        #[no_mangle]
        #[automatically_derived]
        fn _add_intent(
            state_ptr: *mut std::ffi::c_void,
            intent_id: &Vec<u8>,
            intent_data: &Vec<u8>,
        ) -> anoma::types::matchmaker::AddIntentResult {
            let state_ptr = state_ptr as *mut #ident;
            let mut state: #ident = unsafe { std::ptr::read(state_ptr) };
            let result = state.add_intent(intent_id, intent_data);
            unsafe { std::ptr::write(state_ptr, state) };
            result
        }
    };
    TokenStream::from(gen)
}

/// Generate WASM binding for matchmaker filter main entrypoint function.
///
/// This macro expects a function with signature:
///
/// ```compiler_fail
/// fn validate_intent(intent: Vec<u8>) -> bool
/// ```
#[proc_macro_attribute]
pub fn filter(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ItemFn);
    let ident = &ast.sig.ident;
    let gen = quote! {
        // Use `wee_alloc` as the global allocator.
        #[global_allocator]
        static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

        #ast

        /// The module interface callable by wasm runtime
        #[no_mangle]
        extern "C" fn _validate_intent(
            intent_data_ptr: u64,
            intent_data_len: u64,
        ) -> u64 {
            let get_data = |ptr, len| {
                let slice = unsafe {
                    core::slice::from_raw_parts(ptr as *const u8, len as _)
                };
                slice.to_vec()
            };

            if #ident(
                get_data(intent_data_ptr, intent_data_len),
            ) {
                0
            } else {
                1
            }
        }
    };
    TokenStream::from(gen)
}
