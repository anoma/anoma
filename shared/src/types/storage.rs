//! Storage types
use std::convert::{TryFrom, TryInto};
use std::fmt::Display;
use std::ops::Add;

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::bytes::ByteBuf;
use crate::types::address::{self, Address, InternalAddress};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("TEMPORARY error: {error}")]
    Temporary { error: String },
    #[error("Error parsing address: {0}")]
    ParseAddress(address::Error),
    #[error("Error parsing address from a storage key")]
    ParseAddressFromKey,
    #[error("Reserved prefix or string is specified: {0}")]
    InvalidKeySeg(String),
}

/// Result for functions that may fail
pub type Result<T> = std::result::Result<T, Error>;

/// The length of chain ID string
// TODO adjust once chain ID scheme is chosen, add `Default` impl that allocates
// this
pub const CHAIN_ID_LENGTH: usize = 20;
/// The length of the block's hash string
pub const BLOCK_HASH_LENGTH: usize = 32;

/// The separator of storage key segments
pub const KEY_SEGMENT_SEPARATOR: char = '/';
/// The reserved storage key prefix for addresses
pub const RESERVED_ADDRESS_PREFIX: char = '#';
/// The reserved storage key prefix for validity predicates
pub const VP_KEY_PREFIX: char = '?';
/// The reserved storage key for validity predicates
pub const RESERVED_VP_KEY: &str = "?";

/// Height of a block, i.e. the level.
#[derive(
    Default,
    Clone,
    Copy,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Debug,
    Serialize,
    Deserialize,
)]
pub struct BlockHeight(pub u64);

impl Display for BlockHeight {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add<u64> for BlockHeight {
    type Output = BlockHeight;

    fn add(self, rhs: u64) -> Self::Output {
        Self(self.0 + rhs)
    }
}

/// Hash of a block as fixed-size byte array
#[derive(
    Clone,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
)]
pub struct BlockHash(pub [u8; BLOCK_HASH_LENGTH]);

impl TryFrom<i64> for BlockHeight {
    type Error = String;

    fn try_from(value: i64) -> std::result::Result<Self, Self::Error> {
        value
            .try_into()
            .map(BlockHeight)
            .map_err(|e| format!("Unexpected height value {}, {}", value, e))
    }
}
impl BlockHeight {
    /// Get the height of the next block
    pub fn next_height(&self) -> BlockHeight {
        BlockHeight(self.0 + 1)
    }
}

impl Default for BlockHash {
    fn default() -> Self {
        Self([0; 32])
    }
}
impl TryFrom<&[u8]> for BlockHash {
    type Error = self::Error;

    fn try_from(value: &[u8]) -> Result<Self> {
        if value.len() != BLOCK_HASH_LENGTH {
            return Err(Error::Temporary {
                error: format!(
                    "Unexpected block hash length {}, expected {}",
                    value.len(),
                    BLOCK_HASH_LENGTH
                ),
            });
        }
        let mut hash = [0; 32];
        hash.copy_from_slice(value);
        Ok(BlockHash(hash))
    }
}
impl TryFrom<Vec<u8>> for BlockHash {
    type Error = self::Error;

    fn try_from(value: Vec<u8>) -> Result<Self> {
        if value.len() != BLOCK_HASH_LENGTH {
            return Err(Error::Temporary {
                error: format!(
                    "Unexpected block hash length {}, expected {}",
                    value.len(),
                    BLOCK_HASH_LENGTH
                ),
            });
        }
        let mut hash = [0; 32];
        hash.copy_from_slice(&value);
        Ok(BlockHash(hash))
    }
}
impl core::fmt::Debug for BlockHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let hash = format!("{}", ByteBuf(&self.0));
        f.debug_tuple("BlockHash").field(&hash).finish()
    }
}

/// A storage key is made of storage key segments [`DbKeySeg`], separated by
/// [`KEY_SEGMENT_SEPARATOR`].
#[derive(
    Clone,
    BorshSerialize,
    BorshDeserialize,
    Debug,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct Key {
    /// The segments of the key in the original (left-to-right) order.
    pub segments: Vec<DbKeySeg>,
}

impl From<DbKeySeg> for Key {
    fn from(seg: DbKeySeg) -> Self {
        Self {
            segments: vec![seg],
        }
    }
}

impl Key {
    /// Parses string and returns a key
    pub fn parse(string: impl AsRef<str>) -> Result<Self> {
        let mut segments = Vec::new();
        for s in string.as_ref().split(KEY_SEGMENT_SEPARATOR) {
            segments.push(DbKeySeg::parse(s.to_owned())?);
        }
        Ok(Key { segments })
    }

    /// Returns a new key with segments of `Self` and the given segment
    pub fn push<T: KeySeg>(&self, other: &T) -> Result<Self> {
        let mut segments = self.segments.clone();
        segments.push(DbKeySeg::parse(other.raw())?);
        Ok(Key { segments })
    }

    /// Returns a new key with segments of `Self` and the given key
    pub fn join(&self, other: &Key) -> Self {
        let mut segments = self.segments.clone();
        let mut added = other.segments.clone();
        segments.append(&mut added);
        Key { segments }
    }

    /// Returns the addresses from the key segments
    pub fn find_addresses(&self) -> Vec<Address> {
        let mut addresses = Vec::new();
        for s in &self.segments {
            match s {
                DbKeySeg::AddressSeg(addr) => addresses.push(addr.clone()),
                _ => continue,
            }
        }
        addresses
    }

    /// Returns the length
    pub fn len(&self) -> usize {
        self.to_string().len()
    }

    /// Returns `true` if the key is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a key of the validity predicate of the given address
    /// Only this function can push "?" segment for validity predicate
    pub fn validity_predicate(addr: &Address) -> Self {
        let mut segments = Self::from(addr.to_db_key()).segments;
        segments.push(DbKeySeg::StringSeg(RESERVED_VP_KEY.to_owned()));
        Key { segments }
    }

    /// Check if the given key is a key to a validity predicate.
    pub fn is_validity_predicate(&self) -> bool {
        match &self.segments[..] {
            [DbKeySeg::AddressSeg(_), DbKeySeg::StringSeg(sub_key)]
                if sub_key == RESERVED_VP_KEY =>
            {
                true
            }
            _ => false,
        }
    }

    /// Check if the given key is a key to IBC-related data
    pub fn is_ibc_key(&self) -> bool {
        match self.segments.get(0) {
            Some(seg) => {
                *seg == DbKeySeg::AddressSeg(Address::Internal(
                    InternalAddress::Ibc,
                ))
            }
            _ => false,
        }
    }

    /// Check if the given key is a key of the connection counter
    pub fn is_ibc_connection_counter(&self) -> bool {
        *self == Self::ibc_connection_counter()
    }

    /// Returns a key of the IBC-related data
    /// Only this function can push `InternalAddress::Ibc` segment
    pub fn ibc_key(path: impl AsRef<str>) -> Result<Self> {
        let path = Self::parse(path)?;
        let addr = Address::Internal(InternalAddress::Ibc);
        let key = Self::from(addr.to_db_key());
        Ok(key.join(&path))
    }

    /// Returns a key of the IBC connection counter
    pub fn ibc_connection_counter() -> Self {
        let path = "connections/counter".to_owned();
        Key::ibc_key(path)
            .expect("Creating a key for the connection counter shouldn't fail")
    }

    /// Returns a key from the given DB key path that has the height and
    /// the space type
    pub fn parse_db_key(db_key: &str) -> Result<Self> {
        let mut segments: Vec<&str> =
            db_key.split(KEY_SEGMENT_SEPARATOR).collect();
        let key = match segments.get(2) {
            Some(seg)
                if *seg == Address::Internal(InternalAddress::Ibc).raw() =>
            {
                // the path of IBC-related data should start with
                // height/subspace/#IBC
                Self::ibc_key(
                    segments
                        .split_off(3)
                        .join(&KEY_SEGMENT_SEPARATOR.to_string()),
                )
                .map_err(|e| Error::Temporary {
                    error: format!(
                        "Cannot parse key segments {}: {}",
                        db_key, e
                    ),
                })?
            }
            _ => match segments.get(3) {
                Some(seg) if *seg == RESERVED_VP_KEY => {
                    // the path of a validity predicate should be
                    // height/subspace/{address}/?
                    let mut addr_str =
                        (*segments.get(2).expect("the address not found"))
                            .to_owned();
                    let _ = addr_str.remove(0);
                    let addr = Address::decode(&addr_str)
                        .expect("cannot decode the address");
                    Self::validity_predicate(&addr)
                }
                _ => Self::parse(
                    segments
                        .split_off(2)
                        .join(&KEY_SEGMENT_SEPARATOR.to_string()),
                )
                .map_err(|e| Error::Temporary {
                    error: format!(
                        "Cannot parse key segments {}: {}",
                        db_key, e
                    ),
                })?,
            },
        };
        Ok(key)
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let key = self
            .segments
            .iter()
            .map(|s| DbKeySeg::raw(s))
            .collect::<Vec<String>>()
            .join(&KEY_SEGMENT_SEPARATOR.to_string());
        f.write_str(&key)
    }
}

// TODO use std::convert::{TryFrom, Into}?
/// Represents a segment in a path that may be used as a database key
pub trait KeySeg {
    /// Reverse of `into_string`. Convert key segment to `Self`.
    fn parse(string: String) -> Result<Self>
    where
        Self: Sized;

    /// Convert `Self` to a string.
    fn raw(&self) -> String;

    /// Convert `Self` to a key segment. This mapping should preserve the
    /// ordering of `Self`
    fn to_db_key(&self) -> DbKeySeg;
}

/// A storage key segment
#[derive(
    Clone,
    BorshSerialize,
    BorshDeserialize,
    Debug,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum DbKeySeg {
    /// A segment made of an address
    AddressSeg(Address),
    /// Any other key segment
    StringSeg(String),
}

impl KeySeg for DbKeySeg {
    fn parse(mut string: String) -> Result<Self> {
        // a separator should not included
        if string.contains(KEY_SEGMENT_SEPARATOR) {
            return Err(Error::InvalidKeySeg(string));
        }
        if string == Address::Internal(InternalAddress::Ibc).raw() {
            return Err(Error::InvalidKeySeg(string));
        }
        match string.chars().next() {
            // address hashes are prefixed with `'#'`
            Some(c) if c == RESERVED_ADDRESS_PREFIX => {
                let _ = string.remove(0);
                Address::decode(&string)
                    .map_err(Error::ParseAddress)
                    .map(DbKeySeg::AddressSeg)
            }
            // reserved for a validity predicate
            Some(c) if c == VP_KEY_PREFIX && string == RESERVED_VP_KEY => {
                Err(Error::InvalidKeySeg(string))
            }
            _ => Ok(DbKeySeg::StringSeg(string)),
        }
    }

    fn raw(&self) -> String {
        match self {
            DbKeySeg::AddressSeg(addr) => {
                format!("{}{}", RESERVED_ADDRESS_PREFIX, addr.encode())
            }
            DbKeySeg::StringSeg(seg) => seg.to_owned(),
        }
    }

    fn to_db_key(&self) -> DbKeySeg {
        self.clone()
    }
}

impl KeySeg for String {
    fn parse(string: String) -> Result<Self> {
        Ok(string)
    }

    fn raw(&self) -> String {
        self.to_owned()
    }

    fn to_db_key(&self) -> DbKeySeg {
        DbKeySeg::StringSeg(self.clone())
    }
}

impl KeySeg for BlockHeight {
    fn parse(string: String) -> Result<Self> {
        let h = string.parse::<u64>().map_err(|e| Error::Temporary {
            error: format!("Unexpected height value {}, {}", string, e),
        })?;
        Ok(BlockHeight(h))
    }

    fn raw(&self) -> String {
        format!("{}", self.0)
    }

    fn to_db_key(&self) -> DbKeySeg {
        DbKeySeg::StringSeg(self.raw())
    }
}

impl KeySeg for Address {
    fn parse(mut seg: String) -> Result<Self> {
        match seg.chars().next() {
            Some(c) if c == RESERVED_ADDRESS_PREFIX => {
                let _ = seg.remove(0);
                Address::decode(seg).map_err(Error::ParseAddress)
            }
            _ => Err(Error::ParseAddressFromKey),
        }
    }

    fn raw(&self) -> String {
        format!("{}{}", RESERVED_ADDRESS_PREFIX, self.encode())
    }

    fn to_db_key(&self) -> DbKeySeg {
        DbKeySeg::AddressSeg(self.clone())
    }
}

/// Epoch identifier. Epochs are identified by consecutive numbers.
#[derive(
    Clone,
    Copy,
    Default,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshSerialize,
    BorshDeserialize,
)]
pub struct Epoch(pub u64);

impl Display for Epoch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Epoch {
    /// Change to the next epoch
    pub fn next(&self) -> Self {
        Self(self.0 + 1)
    }
}

impl Add<u64> for Epoch {
    type Output = Epoch;

    fn add(self, rhs: u64) -> Self::Output {
        Self(self.0 + rhs)
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    proptest! {
        /// Tests that any key that doesn't contain reserved prefixes is valid.
        /// This test excludes key segments starting with `#` or `?`
        /// because they are reserved for `Address` or a validity predicate.
        #[test]
        fn test_key_parse(s in "[^#?/][^/]*/[^#?/][^/]*/[^#?/][^/]*") {
            let key = Key::parse(s.clone()).expect("cannnot parse the string");
            assert_eq!(key.to_string(), s);
        }

        /// Tests that any key that doesn't contain reserved prefixes and
        /// separators is valid. This test excludes key segments including `/`
        /// or starting with `#` or `?` because they are reserved for separator,
        /// `Address` or validity predicate.
        #[test]
        fn test_key_push(s in "[^#?/][^/]*") {
            let addr = address::testing::established_address_1();
            let key = Key::from(addr.to_db_key()).push(&s).expect("cannnot push the segment");
            assert_eq!(key.segments[1].raw(), s);
        }
    }

    #[test]
    fn test_key_parse_valid() {
        let addr = address::testing::established_address_1();
        let target = format!("{}/test", KeySeg::raw(&addr));
        let key = Key::parse(target.clone()).expect("cannot parse the string");
        assert_eq!(key.to_string(), target);

        let target = "?test/test@".to_owned();
        let key = Key::parse(target.clone()).expect("cannot parse the string");
        assert_eq!(key.to_string(), target);
    }

    #[test]
    fn test_key_parse_invalid() {
        let target = "?/test".to_owned();
        match Key::parse(target).expect_err("unexpectedly succeeded") {
            Error::InvalidKeySeg(s) => assert_eq!(s, "?"),
            _ => panic!("unexpected error happens"),
        }

        let encoded_ibc_addr = Address::Internal(InternalAddress::Ibc).raw();
        let target = format!("{}/test", encoded_ibc_addr);
        match Key::parse(target).expect_err("unexpectedly succeeded") {
            Error::InvalidKeySeg(s) => assert_eq!(s, encoded_ibc_addr),
            _ => panic!("unexpected error happens"),
        }
    }

    #[test]
    fn test_key_push_valid() {
        let addr = address::testing::established_address_1();
        let other = address::testing::established_address_2();
        let target = KeySeg::raw(&other);
        let key = Key::from(addr.to_db_key())
            .push(&target)
            .expect("cannnot push the segment");
        assert_eq!(key.segments[1].raw(), target);

        let target = "?test".to_owned();
        let key = Key::from(addr.to_db_key())
            .push(&target)
            .expect("cannnot push the segment");
        assert_eq!(key.segments[1].raw(), target);
    }

    #[test]
    fn test_key_push_invalid() {
        let addr = address::testing::established_address_1();
        let target = "/".to_owned();
        match Key::from(addr.to_db_key())
            .push(&target)
            .expect_err("unexpectedly succeeded")
        {
            Error::InvalidKeySeg(s) => assert_eq!(s, "/"),
            _ => panic!("unexpected error happens"),
        }

        let target = "?".to_owned();
        match Key::from(addr.to_db_key())
            .push(&target)
            .expect_err("unexpectedly succeeded")
        {
            Error::InvalidKeySeg(s) => assert_eq!(s, "?"),
            _ => panic!("unexpected error happens"),
        }
    }
}

/// Helpers for testing with storage types.
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use proptest::collection;
    use proptest::prelude::*;

    use super::*;
    use crate::types::address::testing::arb_address;

    /// Generate an arbitrary [`Key`].
    pub fn arb_key() -> impl Strategy<Value = Key> {
        prop_oneof![
            // a key for a validity predicate
            arb_address().prop_map(|addr| Key::validity_predicate(&addr)),
            // a key from key segments
            collection::vec(arb_key_seg(), 1..5)
                .prop_map(|segments| { Key { segments } }),
        ]
    }

    /// Generate an arbitrary [`DbKeySeg`].
    pub fn arb_key_seg() -> impl Strategy<Value = DbKeySeg> {
        prop_oneof![
            // the string segment is 5 time more likely to be generated
            5 => "[a-zA-Z0-9_]{1,100}".prop_map(DbKeySeg::StringSeg),
            1 => arb_address().prop_map(DbKeySeg::AddressSeg),
        ]
    }
}
