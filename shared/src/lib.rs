//! The shared code for the Anoma ledger, gossip and wasms.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![warn(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

#[cfg(not(feature = "ABCI"))]
pub use ibc;
#[cfg(feature = "ABCI")]
pub use ibc_abci as ibc;
#[cfg(not(feature = "ABCI"))]
pub use ibc_proto;
#[cfg(feature = "ABCI")]
pub use ibc_proto_abci as ibc_proto;
#[cfg(not(feature = "ABCI"))]
pub use tendermint;
#[cfg(not(feature = "ABCI"))]
pub use tendermint_proto;
#[cfg(feature = "ABCI")]
pub use tendermint_proto_abci as tendermint_proto;
#[cfg(feature = "ABCI")]
pub use tendermint_stable as tendermint;

pub mod bytes;
pub mod ledger;
pub mod proto;
pub mod types;
pub mod vm;

#[cfg(test)]
#[macro_use]
extern crate assert_matches;
