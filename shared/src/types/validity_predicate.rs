//! Types that are used in validity predicates.
use std::collections::BTreeSet;
use std::ops::{Deref, DerefMut};

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};

use crate::types::address::{Address, InternalAddress};

/// A validity predicate with an input that is intended to be invoked via `eval`
/// host function.
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct EvalVp {
    /// The VP code to `eval`
    pub vp_code: Vec<u8>,
    /// The input for the `eval`ed VP
    pub input: Vec<u8>,
}

/// A set of validatiy predicates to be run
/// to verify a transaction
#[derive(Debug)]
pub struct Verifiers {
    /// The addresses of the owners of the validity predicates
    pub addresses: BTreeSet<Address>,
}

impl Default for Verifiers {
    fn default() -> Self {
        Self {
            addresses: BTreeSet::from([Address::Internal(
                InternalAddress::EthereumSentinel,
            )]),
        }
    }
}

impl Deref for Verifiers {
    type Target = BTreeSet<Address>;

    fn deref(&self) -> &Self::Target {
        &self.addresses
    }
}

impl DerefMut for Verifiers {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.addresses
    }
}

impl From<BTreeSet<Address>> for Verifiers {
    fn from(addresses: BTreeSet<Address>) -> Self {
        let mut addresses = addresses;
        addresses.insert(Address::Internal(InternalAddress::EthereumSentinel));
        Self { addresses }
    }
}
