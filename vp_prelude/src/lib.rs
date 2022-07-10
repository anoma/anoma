//! This crate contains library code for validity predicate WASM. Most of the
//! code is re-exported from the `anoma_vm_env` crate.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

use core::convert::AsRef;

use anoma_vm_env::vp_prelude::hash::Hash;
pub use anoma_vm_env::vp_prelude::*;
pub use sha2::{Digest, Sha256, Sha384, Sha512};

pub fn sha256(bytes: &[u8]) -> Hash {
    let digest = Sha256::digest(bytes);
    Hash(*digest.as_ref())
}

pub fn is_tx_whitelisted() -> bool {
    let tx_hash = get_tx_code_hash();
    let key = parameters::storage::get_tx_whitelist_storage_key();
    let whitelist: Vec<String> = read_pre(&key.to_string()).unwrap_or_default();
    // if whitelist is empty, allow any transaction
    whitelist.is_empty() || whitelist.contains(&tx_hash.to_string())
}

pub fn is_vp_whitelisted(vp_bytes: &[u8]) -> bool {
    let vp_hash = sha256(vp_bytes);
    let key = parameters::storage::get_vp_whitelist_storage_key();
    let whitelist: Vec<String> = read_pre(&key.to_string()).unwrap_or_default();
    // if whitelist is empty, allow any transaction
    whitelist.is_empty() || whitelist.contains(&vp_hash.to_string())
}

/// Log a string in a debug build. The message will be printed at the
/// `tracing::Level::Info`. Any `debug_log!` statements are only enabled in
/// non optimized builds by default. An optimized build will not execute
/// `debug_log!` statements unless `-C debug-assertions` is passed to the
/// compiler.
#[macro_export]
macro_rules! debug_log {
    ($($arg:tt)*) => {{
        (if cfg!(debug_assertions) { log_string(format!($($arg)*)) })
    }}
}
