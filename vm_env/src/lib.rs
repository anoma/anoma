//! This crate contains library code for wasm. Some of the code is re-exported
//! from the `shared` crate.
//!
//! Note that all the modules in here depend on the `imports` module, which
//! currently cannot be compiled by tarpaulin (test coverage tool), hence all
//! the modules in here are not included in the tarpaulin report
//! (`#[cfg(not(tarpaulin))]`).

#![doc(html_favicon_url = "https://docs.anoma.network/favicon.png")]
#![doc(html_logo_url = "https://docs.anoma.network/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

#[cfg(all(feature = "ibc", not(tarpaulin)))]
pub mod ibc;
#[cfg(not(tarpaulin))]
pub mod imports;
#[cfg(not(tarpaulin))]
pub mod intent;
#[cfg(not(tarpaulin))]
pub mod key;
#[cfg(not(tarpaulin))]
pub mod proof_of_stake;
#[cfg(not(tarpaulin))]
pub mod token;

#[cfg(not(tarpaulin))]
mod vm_env {

    pub mod tx_prelude {
        pub use anoma::types::address::Address;
        pub use anoma::types::*;
        pub use anoma_macros::transaction;

        #[cfg(feature = "ibc")]
        pub use crate::ibc::{Ibc, IbcActions};
        pub use crate::imports::tx::*;
        pub use crate::intent::tx as intent;
        pub use crate::proof_of_stake::{self, PoS, PosRead, PosWrite};
        pub use crate::token::tx as token;
    }

    pub mod vp_prelude {
        // used in the VP input
        pub use std::collections::HashSet;

        pub use anoma::types::address::Address;
        pub use anoma::types::*;
        pub use anoma_macros::validity_predicate;

        pub use crate::imports::vp::*;
        pub use crate::intent::vp as intent;
        pub mod key {
            pub use crate::key::ed25519::vp as ed25519;
        }
        pub use anoma::ledger::pos as proof_of_stake;

        pub use crate::token::vp as token;
    }
}

pub use vm_env::*;
