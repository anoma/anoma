//! Implements transparent addresses as described in [Accounts
//! Addresses](docs/src/explore/design/ledger/accounts.md#addresses).

use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::str::FromStr;
use std::string;

use bech32::{self, FromBase32, ToBase32, Variant};
use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use crate::types::key;

/// The length of [`Address`] encoded with Borsh.
pub const RAW_ADDRESS_LEN: usize = 45;
/// The length of [`Address`] encoded with Bech32m.
pub const ADDRESS_LEN: usize = 80;

/// human-readable part of Bech32m encoded address
const ADDRESS_HRP: &str = "a";
const ADDRESS_BECH32_VARIANT: bech32::Variant = Variant::Bech32m;
pub(crate) const HASH_LEN: usize = 40;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Error decoding address from Bech32m: {0}")]
    DecodeBech32(bech32::Error),
    #[error("Error decoding address from base32: {0}")]
    DecodeBase32(bech32::Error),
    #[error(
        "Unexpected Bech32m human-readable part {0}, expected {ADDRESS_HRP}"
    )]
    UnexpectedBech32Prefix(String),
    #[error(
        "Unexpected Bech32m variant {0:?}, expected {ADDRESS_BECH32_VARIANT:?}"
    )]
    UnexpectedBech32Variant(bech32::Variant),
    #[error("Address must be encoded with utf-8")]
    NonUtf8Address(string::FromUtf8Error),
    #[error("Invalid address encoding")]
    InvalidAddressEncoding(std::io::Error),
    #[error("Unexpected address hash length {0}, expected {HASH_LEN}")]
    UnexpectedHashLength(usize),
}

/// Result of a function that may fail
pub type Result<T> = std::result::Result<T, Error>;

/// An account's address
#[derive(
    Clone,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
)]
pub enum Address {
    /// An established address is generated on-chain
    Established(EstablishedAddress),
    /// An implicit address is derived from a cryptographic key
    Implicit(ImplicitAddress),
    /// An internal address represents a module with a native VP
    Internal(InternalAddress),
}

impl Address {
    /// Encode an address with Bech32m encoding
    pub fn encode(&self) -> String {
        let bytes = self
            .try_to_vec()
            .expect("Encoding an address shouldn't fail");
        bech32::encode(ADDRESS_HRP, bytes.to_base32(), ADDRESS_BECH32_VARIANT)
            .unwrap_or_else(|_| {
                panic!(
                    "The human-readable part {} should never cause a failure",
                    ADDRESS_HRP
                )
            })
    }

    /// Decode an address from Bech32m encoding
    pub fn decode(string: impl AsRef<str>) -> Result<Self> {
        let (prefix, hash_base32, variant) =
            bech32::decode(string.as_ref()).map_err(Error::DecodeBech32)?;
        if prefix != ADDRESS_HRP {
            return Err(Error::UnexpectedBech32Prefix(prefix));
        }
        match variant {
            ADDRESS_BECH32_VARIANT => {}
            _ => return Err(Error::UnexpectedBech32Variant(variant)),
        }
        let bytes: Vec<u8> = FromBase32::from_base32(&hash_base32)
            .map_err(Error::DecodeBase32)?;
        let address = BorshDeserialize::try_from_slice(&bytes[..])
            .map_err(Error::InvalidAddressEncoding)?;
        match &address {
            Address::Established(established) => {
                if established.hash.len() != HASH_LEN {
                    return Err(Error::UnexpectedHashLength(
                        established.hash.len(),
                    ));
                }
            }
            Address::Implicit(ImplicitAddress::Ed25519(pkh)) => {
                if pkh.0.len() != HASH_LEN {
                    return Err(Error::UnexpectedHashLength(pkh.0.len()));
                }
            }
            Address::Internal(_) => {}
        }
        Ok(address)
    }

    fn pretty_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Address::Established(_) => {
                write!(f, "Established: {}", self.encode(),)
            }
            Address::Implicit(_) => {
                write!(f, "Implicit: {}", self.encode(),)
            }
            Address::Internal(kind) => {
                write!(f, "Internal {}: {}", kind, self.encode())
            }
        }
    }
}

impl serde::Serialize for Address {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let encoded = self.encode();
        serde::Serialize::serialize(&encoded, serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Address {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let encoded: String = serde::Deserialize::deserialize(deserializer)?;
        Self::decode(encoded).map_err(D::Error::custom)
    }
}

impl Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

impl Debug for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

impl FromStr for Address {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        Address::decode(s)
    }
}

/// An established address is generated on-chain
#[derive(
    Debug,
    Clone,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct EstablishedAddress {
    hash: String,
}

/// A generator of established addresses
#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
pub struct EstablishedAddressGen {
    last_hash: String,
}

impl EstablishedAddressGen {
    /// Initialize a new address generator with a given randomness seed.
    pub fn new(seed: impl AsRef<str>) -> Self {
        Self {
            last_hash: seed.as_ref().to_owned(),
        }
    }

    /// Generate a new established address. Requires a source of randomness as
    /// arbitrary bytes. In the ledger, this could be some unpredictable value,
    /// such as hash of the transaction that has initialized the new address.
    pub fn generate_address(
        &mut self,
        rng_source: impl AsRef<[u8]>,
    ) -> Address {
        let gen_bytes = self
            .try_to_vec()
            .expect("Encoding established addresses generator shouldn't fail");
        let mut hasher = Sha256::new();
        let bytes = [&gen_bytes, rng_source.as_ref()].concat();
        hasher.update(bytes);
        // hex of the first 40 chars of the hash
        let hash = format!("{:.width$X}", hasher.finalize(), width = HASH_LEN);
        self.last_hash = hash.clone();
        Address::Established(EstablishedAddress { hash })
    }
}

/// An implicit address is derived from a cryptographic key
#[derive(
    Debug,
    Clone,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum ImplicitAddress {
    /// Address derived from [`key::ed25519::PublicKeyHash`]
    Ed25519(key::ed25519::PublicKeyHash),
}

/// An internal address represents a module with a native VP
#[derive(
    Debug,
    Clone,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum InternalAddress {
    /// Proof-of-stake
    PoS,
    /// Inter-blockchain communication
    Ibc,
    /// Protocol parameters
    Parameters,
}

impl Display for InternalAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::PoS => "PoS",
                Self::Ibc => "IBC",
                Self::Parameters => "Parameters",
            }
        )
    }
}

/// Temporary helper for testing
pub fn xan() -> Address {
    Address::decode("a1qq5qqqqqxuc5gvz9gycryv3sgye5v3j9gvurjv34g9prsd6x8qu5xs2ygdzrzsf38q6rss33xf42f3").expect("The token address decoding shouldn't fail")
}

/// Temporary helper for testing
pub fn btc() -> Address {
    Address::decode("a1qq5qqqqq8q6yy3p4xyurys3n8qerz3zxxeryyv6rg4pnxdf3x3pyv32rx3zrgwzpxu6ny32r3laduc").expect("The token address decoding shouldn't fail")
}

/// Temporary helper for testing
pub fn eth() -> Address {
    Address::decode("a1qq5qqqqqx3z5xd3ngdqnzwzrgfpnxd3hgsuyx3phgfry2s3kxsc5xves8qe5x33sgdprzvjptzfry9").expect("The token address decoding shouldn't fail")
}

/// Temporary helper for testing
pub fn dot() -> Address {
    Address::decode("a1qq5qqqqqxq652v3sxap523fs8pznjse5g3pyydf3xqurws6ygvc5gdfcxyuy2deeggenjsjrjrl2ph").expect("The token address decoding shouldn't fail")
}

/// Temporary helper for testing
pub fn schnitzel() -> Address {
    Address::decode("a1qq5qqqqq8prrzv6xxcury3p4xucygdp5gfprzdfex9prz3jyg56rxv69gvenvsj9g5enswpcl8npyz").expect("The token address decoding shouldn't fail")
}

/// Temporary helper for testing
pub fn apfel() -> Address {
    Address::decode("a1qq5qqqqqgfp52de4x56nqd3ex56y2wph8pznssjzx5ersw2pxfznsd3jxeqnjd3cxapnqsjz2fyt3j").expect("The token address decoding shouldn't fail")
}

/// Temporary helper for testing
pub fn kartoffel() -> Address {
    Address::decode("a1qq5qqqqqxs6yvsekxuuyy3pjxsmrgd2rxuungdzpgsmyydjrxsenjdp5xaqn233sgccnjs3eak5wwh").expect("The token address decoding shouldn't fail")
}

/// Temporary helper for testing, a hash map of tokens addresses with their
/// informal currency codes.
pub fn tokens() -> HashMap<Address, &'static str> {
    vec![
        (xan(), "XAN"),
        (btc(), "BTC"),
        (eth(), "ETH"),
        (dot(), "DOT"),
        (schnitzel(), "Schnitzel"),
        (apfel(), "Apfel"),
        (kartoffel(), "Kartoffel"),
    ]
    .into_iter()
    .collect()
}

/// Temporary helper for testing
pub fn matchmaker() -> Address {
    Address::decode("a1qq5qqqqqxu6rvdzpxymnqwfkxfznvsjxggunyd3jg5erg3p3geqnvv35gep5yvzxx5m5x3fsfje8td").expect("The token address decoding shouldn't fail")
}

#[cfg(test)]
pub mod tests {
    use rand::prelude::ThreadRng;
    use rand::{thread_rng, RngCore};

    use super::*;

    /// Run `cargo test gen_established_address -- --nocapture` to generate a
    /// new established address.
    #[test]
    fn gen_established_address() {
        let seed = "such randomness, much wow";
        let mut key_gen = EstablishedAddressGen::new(seed);

        let mut rng: ThreadRng = thread_rng();
        let mut rng_bytes = vec![0u8; 32];
        rng.fill_bytes(&mut rng_bytes[..]);
        let rng_source = rng_bytes
            .iter()
            .map(|b| format!("{:02X}", b))
            .collect::<Vec<String>>()
            .join("");
        let address = key_gen.generate_address(rng_source);
        println!("address {}", address);
    }

    #[test]
    fn test_address_len() {
        let addr = testing::established_address_1();
        let bytes = addr.try_to_vec().unwrap();
        assert_eq!(bytes.len(), RAW_ADDRESS_LEN);
        assert_eq!(addr.encode().len(), ADDRESS_LEN);
    }

    #[test]
    fn test_address_serde_serialize() {
        let original_address = Address::decode("a1qq5qqqqqgcmyxd35xguy2wp5xsu5vs6pxqcy232pgvm5zs6yggunssfs89znv33h8q6rjde4cjc3dr").unwrap();
        let expect =
            "\"a1qq5qqqqqgcmyxd35xguy2wp5xsu5vs6pxqcy232pgvm5zs6yggunssfs89znv33h8q6rjde4cjc3dr\"";
        let decoded_address: Address =
            serde_json::from_str(expect).expect("could not read JSON");
        assert_eq!(original_address, decoded_address);

        let encoded_address = serde_json::to_string(&original_address).unwrap();
        assert_eq!(encoded_address, expect);
    }
}

/// Helpers for testing with addresses.
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use proptest::prelude::*;

    use super::*;
    use crate::types::key::ed25519;

    /// A sampled established address for tests
    pub fn established_address_1() -> Address {
        Address::decode("a1qq5qqqqqgcmyxd35xguy2wp5xsu5vs6pxqcy232pgvm5zs6yggunssfs89znv33h8q6rjde4cjc3dr").expect("The token address decoding shouldn't fail")
    }

    /// A sampled established address for tests
    pub fn established_address_2() -> Address {
        Address::decode("a1qq5qqqqqgcuyxv2pxgcrzdecx4prq3pexccr2vj9xse5gvf3gvmnv3f3xqcyyvjyxv6yvv34e393x7").expect("The token address decoding shouldn't fail")
    }

    /// Generate an arbitrary [`Address`].
    pub fn arb_address() -> impl Strategy<Value = Address> {
        prop_oneof![
            arb_established_address().prop_map(Address::Established),
            arb_implicit_address().prop_map(Address::Implicit),
        ]
    }

    /// Generate an arbitrary [`EstablishedAddress`].
    pub fn arb_established_address() -> impl Strategy<Value = EstablishedAddress>
    {
        any::<Vec<u8>>().prop_map(|rng_source| {
            let mut key_gen = EstablishedAddressGen::new("seed");
            match key_gen.generate_address(rng_source) {
                Address::Established(addr) => addr,
                _ => {
                    panic!(
                        "Assuming key gen to only generated established \
                         addresses"
                    )
                }
            }
        })
    }

    /// Generate an arbitrary [`ImplicitAddress`].
    pub fn arb_implicit_address() -> impl Strategy<Value = ImplicitAddress> {
        ed25519::testing::arb_keypair().prop_map(|keypair| {
            let pk = ed25519::PublicKey::from(keypair.public);
            let pkh = ed25519::PublicKeyHash::from(pk);
            ImplicitAddress::Ed25519(pkh)
        })
    }
}
