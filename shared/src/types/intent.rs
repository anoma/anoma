//! Intent data definitions and transaction and validity-predicate helpers.

use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::io::ErrorKind;

use borsh::{BorshDeserialize, BorshSerialize};
use rust_decimal::prelude::*;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::types::address::Address;
use crate::types::key::ed25519::Signed;
use crate::types::storage::{DbKeySeg, Key, KeySeg};
use crate::types::token;

/// A simple intent for fungible token trade
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
    Eq,
)]
pub struct FungibleTokenIntent {
    /// List of exchange definitions
    pub exchange: HashSet<Signed<Exchange>>,
}

#[derive(
    Debug,
    Clone,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
    Eq,
    PartialEq,
    Hash,
    PartialOrd,
)]
/// The definition of an intent exchange
pub struct Exchange {
    /// The source address
    pub addr: Address,
    /// The token to be sold
    pub token_sell: Address,
    /// The minimum rate
    pub rate_min: DecimalWrapper,
    /// The maximum amount of token to be sold
    pub max_sell: token::Amount,
    /// The token to be bought
    pub token_buy: Address,
    /// The amount of token to be bought
    pub min_buy: token::Amount,
}

/// These are transfers crafted from matched [`Exchange`]s.
#[derive(
    Debug, Clone, BorshSerialize, BorshDeserialize, Serialize, Deserialize,
)]
pub struct IntentTransfers {
    /// Transfers crafted from the matched intents
    pub transfers: HashSet<token::Transfer>,
    // TODO benchmark between an map or a set, see which is less costly
    /// The exchanges that were matched
    pub exchanges: HashMap<Address, Signed<Exchange>>,
    /// The intents
    // TODO: refactor this without duplicating stuff. The exchanges in the
    // `exchanges` hashmap are already contained in the FungibleTokenIntents
    // belows
    pub intents: HashMap<Address, Signed<FungibleTokenIntent>>,
}

/// Struct holding a safe rapresentation of a float
#[derive(
    Debug,
    Clone,
    Eq,
    PartialEq,
    Hash,
    PartialOrd,
    Serialize,
    Deserialize,
    Default,
)]
pub struct DecimalWrapper(pub Decimal);

impl From<Decimal> for DecimalWrapper {
    fn from(decimal: Decimal) -> Self {
        DecimalWrapper(decimal)
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Error parsing as decimal: {0}.")]
    DecimalParseError(String),
}

impl TryFrom<token::Amount> for DecimalWrapper {
    type Error = Error;

    fn try_from(amount: token::Amount) -> Result<Self, Self::Error> {
        let decimal = Decimal::from_i128(amount.change());

        match decimal {
            Some(d) => Ok(DecimalWrapper::from(d)),
            None => Err(Error::DecimalParseError(amount.change().to_string())),
        }
    }
}

impl FromStr for DecimalWrapper {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let decimal = Decimal::from_str(s)
            .map_err(|e| Self::Err::DecimalParseError(e.to_string()));

        match decimal {
            Ok(d) => Ok(DecimalWrapper::from(d)),
            Err(e) => Err(e),
        }
    }
}

impl BorshSerialize for DecimalWrapper {
    fn serialize<W: std::io::Write>(
        &self,
        writer: &mut W,
    ) -> std::io::Result<()> {
        let vec = self.0.to_string().as_bytes().to_vec();
        let bytes = vec
            .try_to_vec()
            .expect("DecimalWrapper bytes encoding shouldn't fail");
        writer.write_all(&bytes)
    }
}

impl BorshDeserialize for DecimalWrapper {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        // deserialize the bytes first
        let bytes: Vec<u8> =
            BorshDeserialize::deserialize(buf).map_err(|e| {
                std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Error decoding DecimalWrapper: {}", e),
                )
            })?;
        let decimal_str: &str =
            std::str::from_utf8(bytes.as_slice()).map_err(|e| {
                std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Error decoding decimal: {}", e),
                )
            })?;
        let decimal = Decimal::from_str(decimal_str).map_err(|e| {
            std::io::Error::new(
                ErrorKind::InvalidInput,
                format!("Error decoding decimal: {}", e),
            )
        })?;
        Ok(DecimalWrapper(decimal))
    }
}

impl IntentTransfers {
    /// Create an empty [`IntentTransfers`].
    pub fn empty() -> Self {
        Self {
            transfers: HashSet::new(),
            exchanges: HashMap::new(),
            intents: HashMap::new(),
        }
    }
}

const INVALID_INTENT_STORAGE_KEY: &str = "invalid_intent";

/// Obtain a storage key for user's invalid intent set.
pub fn invalid_intent_key(owner: &Address) -> Key {
    Key::from(owner.to_db_key())
        .push(&INVALID_INTENT_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Check if the given storage key is a key for a set of intent sig. If it is,
/// returns the owner.
pub fn is_invalid_intent_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(owner), DbKeySeg::StringSeg(key)]
            if key == INVALID_INTENT_STORAGE_KEY =>
        {
            Some(owner)
        }
        _ => None,
    }
}
