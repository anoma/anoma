//! Shared code for the node, client etc.

#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

pub mod cli;
pub mod client;
pub mod config;
pub mod logging;
pub mod node;
pub mod proto;
pub mod types;
pub mod wallet;

// This is here only to include the std's docs in our docs.
// Taken from <https://github.com/rust-lang/rfcs/issues/2324#issuecomment-502437904>.
#[doc(inline)]
pub use std;
