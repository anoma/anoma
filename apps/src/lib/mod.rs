//! Shared code for the node, client etc.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

pub mod cli;
pub mod client;
pub mod config;
pub mod logging;
pub mod node;
pub mod proto;
pub mod wallet;
pub mod wasm_loader;

#[cfg(not(feature = "ABCI"))]
pub extern crate tendermint;
#[cfg(not(feature = "ABCI"))]
pub extern crate tendermint_config;
#[cfg(feature = "ABCI")]
pub extern crate tendermint_config_abci;
#[cfg(not(feature = "ABCI"))]
pub extern crate tendermint_rpc;
#[cfg(feature = "ABCI")]
pub extern crate tendermint_rpc_abci;
#[cfg(feature = "ABCI")]
pub extern crate tendermint_stable;

// This is here only to include the std's docs in our docs.
// Taken from <https://github.com/rust-lang/rfcs/issues/2324#issuecomment-502437904>.
#[doc(inline)]
pub use std;
