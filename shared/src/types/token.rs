//! A basic fungible token

use std::fmt::Display;
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::types::address::Address;
use crate::types::storage::{DbKeySeg, Key, KeySeg};

/// Amount in micro units. For different granularity another representation
/// might be more appropriate.
#[derive(
    Clone,
    Copy,
    Default,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Debug,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct Amount {
    micro: u64,
}

const MAX_SCALE: u32 = 6;

/// A change in tokens amount
pub type Change = i128;

impl Amount {
    /// Get the amount as a [`Change`]
    pub fn change(&self) -> Change {
        self.micro as Change
    }

    /// Spend a given amount
    /// Panics when given `amount` > `self.micro` amount.
    pub fn spend(&mut self, amount: &Amount) {
        self.micro -= amount.micro
    }

    /// Receive a given amount
    pub fn receive(&mut self, amount: &Amount) {
        self.micro += amount.micro
    }

    /// Create a new amount from whole number of tokens
    pub fn whole(amount: u64) -> Self {
        Self {
            micro: amount * 1_000_000,
        }
    }
}

impl From<u64> for Amount {
    fn from(micro: u64) -> Self {
        Self { micro }
    }
}

impl Display for Amount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decimal =
            rust_decimal::Decimal::new(self.micro as i64, 6).normalize();
        write!(f, "{}", decimal)
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum AmountParseError {
    #[error("Error decoding token amount: {0}")]
    InvalidDecimal(rust_decimal::Error),
    #[error(
        "Error decoding token amount, scale too large: {0}. Maximum \
         {MAX_SCALE}"
    )]
    ScaleTooLarge(u32),
    #[error("Error decoding token amount, the value is within invalid range.")]
    InvalidRange,
}

impl FromStr for Amount {
    type Err = AmountParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match rust_decimal::Decimal::from_str(s) {
            Ok(decimal) => {
                let scale = decimal.scale();
                if scale > 6 {
                    return Err(AmountParseError::ScaleTooLarge(scale));
                }
                let whole = decimal * rust_decimal::Decimal::new(1_000_000, 0);
                let micro: u64 =
                    rust_decimal::prelude::ToPrimitive::to_u64(&whole)
                        .ok_or(AmountParseError::InvalidRange)?;
                Ok(Self { micro })
            }
            Err(err) => Err(AmountParseError::InvalidDecimal(err)),
        }
    }
}

const BALANCE_STORAGE_KEY: &str = "balance";

/// Obtain a storage key for user's balance.
pub fn balance_key(token_addr: &Address, owner: &Address) -> Key {
    Key::from(token_addr.to_db_key())
        .push(&BALANCE_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&owner.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Check if the given storage key is balance key for the given token. If it is,
/// returns the owner.
pub fn is_balance_key<'a>(
    token_addr: &Address,
    key: &'a Key,
) -> Option<&'a Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(key), DbKeySeg::AddressSeg(owner)]
            if key == BALANCE_STORAGE_KEY && addr == token_addr =>
        {
            Some(owner)
        }
        _ => None,
    }
}

/// Check if the given storage key is balance key for unspecified token. If it
/// is, returns the owner.
pub fn is_any_token_balance_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(_), DbKeySeg::StringSeg(key), DbKeySeg::AddressSeg(owner)]
            if key == BALANCE_STORAGE_KEY =>
        {
            Some(owner)
        }
        _ => None,
    }
}

/// A simple bilateral token transfer
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Hash,
    Eq,
    PartialOrd,
    Serialize,
    Deserialize,
)]
pub struct Transfer {
    /// Source address will spend the tokens
    pub source: Address,
    /// Target address will receive the tokens
    pub target: Address,
    /// Token's address
    pub token: Address,
    /// The amount of tokens
    pub amount: Amount,
}
