//! A basic fungible token

use std::convert::TryFrom;
use std::fmt::Display;
use std::ops::{Add, AddAssign, Mul, Sub, SubAssign};
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::types::address::{Address, Error as AddressError, InternalAddress};
use crate::types::ibc::data::FungibleTokenPacketData;
use crate::types::storage::{DbKeySeg, Key, KeySeg};

/// Amount in micro units. For different granularity another representation
/// might be more appropriate.
#[derive(
    Clone,
    Copy,
    Default,
    BorshSerialize,
    BorshDeserialize,
    BorshSchema,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Debug,
    Hash,
)]
pub struct Amount {
    micro: u64,
}

/// Maximum decimal places in a token [`Amount`] and [`Change`].
pub const MAX_DECIMAL_PLACES: u32 = 6;
/// Decimal scale of token [`Amount`] and [`Change`].
pub const SCALE: u64 = 1_000_000;
const SCALE_F64: f64 = SCALE as f64;

/// A change in tokens amount
pub type Change = i128;

impl Amount {
    /// Get the amount as a [`Change`]
    pub fn change(&self) -> Change {
        self.micro as Change
    }

    /// Spend a given amount.
    /// Panics when given `amount` > `self.micro` amount.
    pub fn spend(&mut self, amount: &Amount) {
        self.micro = self.micro.checked_sub(amount.micro).unwrap();
    }

    /// Receive a given amount.
    /// Panics on overflow.
    pub fn receive(&mut self, amount: &Amount) {
        self.micro = self.micro.checked_add(amount.micro).unwrap();
    }

    /// Create a new amount from whole number of tokens
    pub const fn whole(amount: u64) -> Self {
        Self {
            micro: amount * SCALE,
        }
    }

    /// Create a new amount with the maximum value
    pub fn max() -> Self {
        Self { micro: u64::MAX }
    }

    /// Create amount from Change
    ///
    /// # Panics
    ///
    /// Panics if the change is negative or overflows `u64`.
    pub fn from_change(change: Change) -> Self {
        Self {
            micro: change as u64,
        }
    }
}

impl serde::Serialize for Amount {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let amount_string = self.to_string();
        serde::Serialize::serialize(&amount_string, serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Amount {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let amount_string: String =
            serde::Deserialize::deserialize(deserializer)?;
        Self::from_str(&amount_string).map_err(D::Error::custom)
    }
}

impl From<Amount> for f64 {
    /// Warning: `f64` loses precision and it should not be used when exact
    /// values are required.
    fn from(amount: Amount) -> Self {
        amount.micro as f64 / SCALE_F64
    }
}

impl From<f64> for Amount {
    /// Warning: `f64` loses precision and it should not be used when exact
    /// values are required.
    fn from(micro: f64) -> Self {
        Self {
            micro: (micro * SCALE_F64).round() as u64,
        }
    }
}

impl From<u64> for Amount {
    fn from(micro: u64) -> Self {
        Self { micro }
    }
}

impl From<Amount> for u64 {
    fn from(amount: Amount) -> Self {
        amount.micro
    }
}

impl Add for Amount {
    type Output = Amount;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.micro += rhs.micro;
        self
    }
}

impl Mul<Amount> for u64 {
    type Output = Amount;

    fn mul(mut self, rhs: Amount) -> Self::Output {
        self *= rhs.micro;
        Self::Output::from(self)
    }
}

impl AddAssign for Amount {
    fn add_assign(&mut self, rhs: Self) {
        self.micro += rhs.micro
    }
}

impl Sub for Amount {
    type Output = Amount;

    fn sub(mut self, rhs: Self) -> Self::Output {
        self.micro -= rhs.micro;
        self
    }
}

impl SubAssign for Amount {
    fn sub_assign(&mut self, rhs: Self) {
        self.micro -= rhs.micro
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum AmountParseError {
    #[error("Error decoding token amount: {0}")]
    InvalidDecimal(rust_decimal::Error),
    #[error(
        "Error decoding token amount, too many decimal places: {0}. Maximum \
         {MAX_DECIMAL_PLACES}"
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
                let whole =
                    decimal * rust_decimal::Decimal::new(SCALE as i64, 0);
                let micro: u64 =
                    rust_decimal::prelude::ToPrimitive::to_u64(&whole)
                        .ok_or(AmountParseError::InvalidRange)?;
                Ok(Self { micro })
            }
            Err(err) => Err(AmountParseError::InvalidDecimal(err)),
        }
    }
}

impl Display for Amount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decimal = rust_decimal::Decimal::from_i128_with_scale(
            self.micro as i128,
            MAX_DECIMAL_PLACES,
        )
        .normalize();
        write!(f, "{}", decimal)
    }
}

impl From<Amount> for Change {
    fn from(amount: Amount) -> Self {
        amount.micro as i128
    }
}

/// Key segment for a balance key
pub const BALANCE_STORAGE_KEY: &str = "balance";

/// Obtain a storage key for user's balance.
pub fn balance_key(token_addr: &Address, owner: &Address) -> Key {
    Key::from(token_addr.to_db_key())
        .push(&BALANCE_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&owner.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Obtain a storage key prefix for all users' balances.
pub fn balance_prefix(token_addr: &Address) -> Key {
    Key::from(token_addr.to_db_key())
        .push(&BALANCE_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Check if the given storage key is balance key for the given token. If it is,
/// returns the owner.
pub fn is_balance_key<'a>(
    token_addr: &Address,
    key: &'a Key,
) -> Option<&'a Address> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(key),
            DbKeySeg::AddressSeg(owner),
        ] if key == BALANCE_STORAGE_KEY && addr == token_addr => Some(owner),
        _ => None,
    }
}

/// Check if the given storage key is balance key for unspecified token. If it
/// is, returns the owner.
pub fn is_any_token_balance_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(_),
            DbKeySeg::StringSeg(key),
            DbKeySeg::AddressSeg(owner),
        ] if key == BALANCE_STORAGE_KEY => Some(owner),
        _ => None,
    }
}

/// Check if the given storage key is non-owner's balance key. If it is, returns
/// the address.
pub fn is_non_owner_balance_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(_),
            DbKeySeg::StringSeg(key),
            DbKeySeg::AddressSeg(owner),
        ] if key == BALANCE_STORAGE_KEY => match owner {
            Address::Internal(InternalAddress::IbcEscrow(_))
            | Address::Internal(InternalAddress::IbcBurn)
            | Address::Internal(InternalAddress::IbcMint) => Some(owner),
            _ => None,
        },
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
    BorshSchema,
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

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum TransferError {
    #[error("Invalid address is specified: {0}")]
    Address(AddressError),
    #[error("Invalid amount: {0}")]
    Amount(AmountParseError),
    #[error("No token is specified")]
    NoToken,
}

impl TryFrom<FungibleTokenPacketData> for Transfer {
    type Error = TransferError;

    fn try_from(data: FungibleTokenPacketData) -> Result<Self, Self::Error> {
        let source =
            Address::decode(&data.sender).map_err(TransferError::Address)?;
        let target =
            Address::decode(&data.receiver).map_err(TransferError::Address)?;
        let token_str =
            data.denom.split('/').last().ok_or(TransferError::NoToken)?;
        let token =
            Address::decode(token_str).map_err(TransferError::Address)?;
        let amount =
            Amount::from_str(&data.amount).map_err(TransferError::Amount)?;
        Ok(Self {
            source,
            target,
            token,
            amount,
        })
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    proptest! {
            /// The upper limit is set to `2^51`, because then the float is
            /// starting to lose precision.
            #[test]
            fn test_token_amount_f64_conversion(raw_amount in 0..2_u64.pow(51)) {
                let amount = Amount::from(raw_amount);
                // A round-trip conversion to and from f64 should be an identity
                let float = f64::from(amount);
                let identity = Amount::from(float);
                assert_eq!(amount, identity);
        }
    }

    #[test]
    fn test_token_display() {
        let max = Amount::from(u64::MAX);
        assert_eq!("18446744073709.551615", max.to_string());

        let whole = Amount::from(u64::MAX / SCALE * SCALE);
        assert_eq!("18446744073709", whole.to_string());

        let trailing_zeroes = Amount::from(123000);
        assert_eq!("0.123", trailing_zeroes.to_string());

        let zero = Amount::from(0);
        assert_eq!("0", zero.to_string());
    }
}
