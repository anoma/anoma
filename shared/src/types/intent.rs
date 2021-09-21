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
    /// The vp code
    pub vp: Option<Vec<u8>>,
}

/// These are transfers crafted from matched [`Exchange`]s.
#[derive(
    Debug,
    Clone,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
    PartialEq,
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

#[cfg(test)]
mod tests {
    use std::array::IntoIter;
    use std::env;
    use std::iter::FromIterator;

    use constants::*;

    use super::*;
    use crate::ledger::storage::types::{decode, encode};
    use crate::types::key::ed25519;

    #[test]
    fn test_encode_decode_intent_transfer_without_vp() {
        let bertha_addr = Address::from_str(BERTHA).unwrap();
        let albert_addr = Address::from_str(ALBERT).unwrap();

        let bertha_keypair = ed25519::testing::keypair_1();
        let albert_keypair = ed25519::testing::keypair_2();

        let exchange_one = Exchange {
            addr: Address::from_str(BERTHA).unwrap(),
            token_buy: Address::from_str(XAN).unwrap(),
            token_sell: Address::from_str(BTC).unwrap(),
            max_sell: token::Amount::from(100),
            min_buy: token::Amount::from(1),
            rate_min: DecimalWrapper::from_str("0.1").unwrap(),
            vp: None,
        };
        let exchange_two = Exchange {
            addr: Address::from_str(ALBERT).unwrap(),
            token_buy: Address::from_str(BTC).unwrap(),
            token_sell: Address::from_str(XAN).unwrap(),
            max_sell: token::Amount::from(1),
            min_buy: token::Amount::from(100),
            rate_min: DecimalWrapper::from_str("10").unwrap(),
            vp: None,
        };

        let signed_exchange_one = Signed::new(&bertha_keypair, exchange_one);
        let signed_exchange_two = Signed::new(&bertha_keypair, exchange_two);

        let mut it = IntentTransfers::empty();
        it.exchanges = HashMap::<_, _>::from_iter(IntoIter::new([
            (bertha_addr.clone(), signed_exchange_one.clone()),
            (albert_addr.clone(), signed_exchange_two.clone()),
        ]));

        it.intents = HashMap::<_, _>::from_iter(IntoIter::new([
            (
                bertha_addr.clone(),
                Signed::new(
                    &bertha_keypair,
                    FungibleTokenIntent {
                        exchange: HashSet::from_iter(vec![signed_exchange_one]),
                    },
                ),
            ),
            (
                albert_addr.clone(),
                Signed::new(
                    &albert_keypair,
                    FungibleTokenIntent {
                        exchange: HashSet::from_iter(vec![signed_exchange_two]),
                    },
                ),
            ),
        ]));

        it.transfers = HashSet::<_>::from_iter(IntoIter::new([
            token::Transfer {
                source: bertha_addr.clone(),
                target: albert_addr.clone(),
                token: Address::from_str(BTC).unwrap(),
                amount: token::Amount::from(100),
            },
            token::Transfer {
                source: albert_addr,
                target: bertha_addr,
                token: Address::from_str(XAN).unwrap(),
                amount: token::Amount::from(1),
            },
        ]));

        let encoded_intent_transfer = encode(&it);
        let decoded_intent_transfer: IntentTransfers =
            decode(encoded_intent_transfer).unwrap();

        assert!(decoded_intent_transfer == it);
    }

    #[test]
    fn test_encode_decode_intent_transfer_with_vp() {
        let bertha_addr = Address::from_str(BERTHA).unwrap();
        let albert_addr = Address::from_str(ALBERT).unwrap();

        let bertha_keypair = ed25519::testing::keypair_1();
        let albert_keypair = ed25519::testing::keypair_2();

        let working_dir = env::current_dir().unwrap();

        let exchange_one = Exchange {
            addr: Address::from_str(BERTHA).unwrap(),
            token_buy: Address::from_str(XAN).unwrap(),
            token_sell: Address::from_str(BTC).unwrap(),
            max_sell: token::Amount::from(100),
            min_buy: token::Amount::from(1),
            rate_min: DecimalWrapper::from_str("0.1").unwrap(),
            vp: Some(
                std::fs::read(format!(
                    "{}/../{}",
                    working_dir.to_string_lossy(),
                    VP_ALWAYS_FALSE_WASM
                ))
                .unwrap(),
            ),
        };
        let exchange_two = Exchange {
            addr: Address::from_str(ALBERT).unwrap(),
            token_buy: Address::from_str(BTC).unwrap(),
            token_sell: Address::from_str(XAN).unwrap(),
            max_sell: token::Amount::from(1),
            min_buy: token::Amount::from(100),
            rate_min: DecimalWrapper::from_str("10").unwrap(),
            vp: Some(
                std::fs::read(format!(
                    "{}/../{}",
                    working_dir.to_string_lossy(),
                    VP_ALWAYS_TRUE_WASM
                ))
                .unwrap(),
            ),
        };

        let signed_exchange_one = Signed::new(&bertha_keypair, exchange_one);
        let signed_exchange_two = Signed::new(&bertha_keypair, exchange_two);

        let mut it = IntentTransfers::empty();
        it.exchanges = HashMap::<_, _>::from_iter(IntoIter::new([
            (bertha_addr.clone(), signed_exchange_one.clone()),
            (albert_addr.clone(), signed_exchange_two.clone()),
        ]));

        it.intents = HashMap::<_, _>::from_iter(IntoIter::new([
            (
                bertha_addr.clone(),
                Signed::new(
                    &bertha_keypair,
                    FungibleTokenIntent {
                        exchange: HashSet::from_iter(vec![signed_exchange_one]),
                    },
                ),
            ),
            (
                albert_addr.clone(),
                Signed::new(
                    &albert_keypair,
                    FungibleTokenIntent {
                        exchange: HashSet::from_iter(vec![signed_exchange_two]),
                    },
                ),
            ),
        ]));

        it.transfers = HashSet::<_>::from_iter(IntoIter::new([
            token::Transfer {
                source: bertha_addr.clone(),
                target: albert_addr.clone(),
                token: Address::from_str(BTC).unwrap(),
                amount: token::Amount::from(100),
            },
            token::Transfer {
                source: albert_addr,
                target: bertha_addr,
                token: Address::from_str(XAN).unwrap(),
                amount: token::Amount::from(1),
            },
        ]));

        let encoded_intent_transfer = encode(&it);
        let decoded_intent_transfer: IntentTransfers =
            decode(encoded_intent_transfer).unwrap();

        assert!(decoded_intent_transfer == it);
    }

    #[cfg(test)]
    #[allow(dead_code)]
    mod constants {

        // User addresses
        pub const ALBERT: &str = "a1qq5qqqqqg4znssfsgcurjsfhgfpy2vjyxy6yg3z98pp5zvp5xgersvfjxvcnx3f4xycrzdfkak0xhx";
        pub const BERTHA: &str = "a1qq5qqqqqxv6yydz9xc6ry33589q5x33eggcnjs2xx9znydj9xuens3phxppnwvzpg4rrqdpswve4n9";
        pub const CHRISTEL: &str = "a1qq5qqqqqxsuygd2x8pq5yw2ygdryxs6xgsmrsdzx8pryxv34gfrrssfjgccyg3zpxezrqd2y2s3g5s";

        // Fungible token addresses
        pub const XAN: &str = "a1qq5qqqqqxuc5gvz9gycryv3sgye5v3j9gvurjv34g9prsd6x8qu5xs2ygdzrzsf38q6rss33xf42f3";
        pub const BTC: &str = "a1qq5qqqqq8q6yy3p4xyurys3n8qerz3zxxeryyv6rg4pnxdf3x3pyv32rx3zrgwzpxu6ny32r3laduc";
        pub const ETH: &str = "a1qq5qqqqqx3z5xd3ngdqnzwzrgfpnxd3hgsuyx3phgfry2s3kxsc5xves8qe5x33sgdprzvjptzfry9";
        pub const DOT: &str = "a1qq5qqqqqxq652v3sxap523fs8pznjse5g3pyydf3xqurws6ygvc5gdfcxyuy2deeggenjsjrjrl2ph";

        // Bite-sized tokens
        pub const SCHNITZEL: &str = "a1qq5qqqqq8prrzv6xxcury3p4xucygdp5gfprzdfex9prz3jyg56rxv69gvenvsj9g5enswpcl8npyz";
        pub const APFEL: &str = "a1qq5qqqqqgfp52de4x56nqd3ex56y2wph8pznssjzx5ersw2pxfznsd3jxeqnjd3cxapnqsjz2fyt3j";
        pub const KARTOFFEL: &str = "a1qq5qqqqqxs6yvsekxuuyy3pjxsmrgd2rxuungdzpgsmyydjrxsenjdp5xaqn233sgccnjs3eak5wwh";

        // Paths to the WASMs used for tests
        pub const TX_TRANSFER_WASM: &str = "wasm/tx_transfer.wasm";
        pub const VP_USER_WASM: &str = "wasm/vp_user.wasm";
        pub const TX_NO_OP_WASM: &str = "wasm_for_tests/tx_no_op.wasm";
        pub const VP_ALWAYS_TRUE_WASM: &str =
            "wasm_for_tests/vp_always_true.wasm";
        pub const VP_ALWAYS_FALSE_WASM: &str =
            "wasm_for_tests/vp_always_false.wasm";
    }
}
