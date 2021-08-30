//! This crate contains library code for wasm. Some of the code is re-exported
//! from the `shared` crate.

#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

pub mod imports;
pub mod intent;
pub mod key;
pub mod proof_of_stake;
pub mod token;

pub mod tx_prelude {
    pub use anoma::types::address::Address;
    pub use anoma::types::*;
    pub use anoma_vm_macro::transaction;

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
    pub use anoma_vm_macro::validity_predicate;

    pub use crate::imports::vp::*;
    pub use crate::intent::vp as intent;
    pub mod key {
        pub use crate::key::ed25519::vp as ed25519;
    }
    pub use anoma::ledger::pos as proof_of_stake;

    pub use crate::token::vp as token;
}

pub mod matchmaker_prelude {
    pub use anoma::types::address::Address;
    pub use anoma::types::*;
    pub use anoma_vm_macro::matchmaker;

    pub use crate::imports::matchmaker::*;
}

pub mod filter_prelude {
    pub use anoma::types::address::Address;
    pub use anoma::types::*;
    pub use anoma_vm_macro::filter;

    pub use super::imports::filter::*;
}
