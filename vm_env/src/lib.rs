//! This crate contains library code for wasm. Some of the code is re-exported
//! from the `shared` crate.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

pub mod governance;
pub mod ibc;
pub mod imports;
pub mod intent;
pub mod key;
pub mod nft;
pub mod proof_of_stake;
pub mod token;

pub mod tx_prelude {
    pub use anoma::ledger::governance::storage;
    pub use anoma::ledger::parameters::storage as parameters_storage;
    pub use anoma::ledger::storage::types::encode;
    pub use anoma::ledger::treasury::storage as treasury_storage;
    pub use anoma::proto::{Signed, SignedTxData};
    pub use anoma::types::address::Address;
    pub use anoma::types::storage::Key;
    pub use anoma::types::*;
    pub use anoma_macros::transaction;

    pub use crate::governance::tx as governance;
    pub use crate::ibc::{Ibc, IbcActions};
    pub use crate::imports::tx::*;
    pub use crate::intent::tx as intent;
    pub use crate::nft::tx as nft;
    pub use crate::proof_of_stake::{self, PoS, PosRead, PosWrite};
    pub use crate::token::tx as token;
}

pub mod vp_prelude {
    // used in the VP input
    pub use std::collections::{BTreeSet, HashSet};

    pub use anoma::ledger::governance::storage as gov_storage;
    pub use anoma::ledger::{parameters, pos as proof_of_stake};
    pub use anoma::proto::{Signed, SignedTxData};
    pub use anoma::types::address::Address;
    pub use anoma::types::storage::Key;
    pub use anoma::types::*;
    pub use anoma_macros::validity_predicate;

    pub use crate::imports::vp::*;
    pub use crate::intent::vp as intent;
    pub use crate::key::vp as key;
    pub use crate::nft::vp as nft;
    pub use crate::token::vp as token;
}
