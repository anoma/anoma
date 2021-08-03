//! Types used for PoS system interactions

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};

use super::address::Address;
use super::token;

/// A simple bilateral token transfer
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Hash,
    Eq,
    Serialize,
    Deserialize,
)]
pub struct Bond {
    /// Validator address
    pub validator: Address,
    /// The amount of tokens
    pub amount: token::Amount,
    /// Source address for delegations. For self-bonds, the validator is
    /// also the source.
    pub source: Option<Address>,
}
