//! This crate contains library code for transaction WASM. Most of the code is
//! re-exported from the `anoma_vm_env` crate.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

pub use anoma_vm_env::tx_prelude::*;

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
