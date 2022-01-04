//! The shared code for the Anoma ledger, gossip and wasms.

#![doc(html_favicon_url = "https://docs.anoma.network/favicon.png")]
#![doc(html_logo_url = "https://docs.anoma.network/rustdoc-logo.png")]
#![warn(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

pub mod bytes;
pub mod ledger;
pub mod proto;
pub mod types;
pub mod vm;

#[cfg(test)]
#[macro_use]
extern crate assert_matches;
