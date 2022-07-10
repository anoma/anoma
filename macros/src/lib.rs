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
/// fn apply_tx(tx_data: Vec<u8>)
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
            #ident(tx_data);
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
///     tx_data: Vec<u8>,
///     addr: Address,
///     keys_changed: BTreeSet<storage::Key>,
///     verifiers: BTreeSet<Address>
/// ) -> bool
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
            // TODO Should the address be on demand (a call to host function?)
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

            // run validation with the concrete type(s)
            if #ident(tx_data, addr, keys_changed, verifiers) {
                1
            } else {
                0
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
