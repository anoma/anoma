//! Storage types
use std::convert::{TryFrom, TryInto};
use std::fmt::Display;
use std::num::ParseIntError;
use std::ops::Add;
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[cfg(feature = "ferveo-tpke")]
use super::transaction::WrapperTx;
use crate::bytes::ByteBuf;
use crate::types::address::{self, Address, InternalAddress};
use crate::types::token::BALANCE_STORAGE_KEY;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("TEMPORARY error: {error}")]
    Temporary { error: String },
    #[error("Error parsing address: {0}")]
    ParseAddress(address::DecodeError),
    #[error("Error parsing address from a storage key")]
    ParseAddressFromKey,
    #[error("Reserved prefix or string is specified: {0}")]
    InvalidKeySeg(String),
}

/// Result for functions that may fail
pub type Result<T> = std::result::Result<T, Error>;

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

/// Transaction index within block.
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
    Hash,
    Debug,
    Serialize,
    Deserialize,
)]
pub struct TxIndex(pub u32);

impl Display for TxIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add<u32> for TxIndex {
    type Output = TxIndex;

    fn add(self, rhs: u32) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl From<TxIndex> for u32 {
    fn from(index: TxIndex) -> Self {
        index.0
    }
}

/// Represents the accepted transactions in a block
#[derive(
    Clone,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Debug,
    Serialize,
    Deserialize,
)]
pub struct BlockResults(pub Vec<u8>);

impl BlockResults {
    /// Accept the tx at the given position
    pub fn accept(&mut self, idx: usize) {
        // Sufficiently extend range to contain index
        for i in self.0.len()..((idx/7) + 1) {
            // Update previous MSB to indicate new byte
            self.0[i-1] |= 0x80u8;
            // New bytes reject and have nothing following
            self.0.push(0x7Fu8);
        }
        // Now indicate that the given transaction is accepted
        self.0[idx / 7] &= !(0x01u8 << (idx % 7));
    }
    /// Reject the tx at the given position
    pub fn reject(&mut self, idx: usize) {
        // If index is out of range then it was already off
        if idx/7 >= self.0.len() { return }
        // Otherwise turn off the relevant bit
        self.0[idx / 7] |= 0x01u8 << (idx % 7);
        // Now maintain canonicity by popping rejected bytes
        while self.0.len() > 1 && *self.0.last().unwrap() == 0x7Fu8 {
            // Delete redundant last byte
            self.0.pop();
            // Turn off MSB of previous byte
            *self.0.last_mut().unwrap() &= 0x7Fu8;
        }
    }
    /// Check if the tx at the given position is accepted
    pub fn is_accepted(&self, idx: usize) -> bool {
        // Index beyond stored range implies rejection, otherwise check relevant
        // bit vector entry
        (idx/7 < self.0.len()) &&
            (self.0[idx / 7] & (0x01u8 << (idx % 7))) == 0
    }
}

impl Default for BlockResults {
    fn default() -> Self {
        // Results object always has at least one byte
        Self(vec![0x7Fu8])
    }
}

impl Display for BlockResults {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for byte in &self.0 {
            for i in 0..7 {
                if byte & (0x01u8 << i) == 0 {
                    // Current index is accepted
                    write!(f, "0")?
                } else {
                    // Current index is rejected
                    write!(f, "1")?
                }
            }
        }
        // Everything else is rejected
        write!(f, "1+")
    }
}

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
    Hash,
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

impl From<BlockHeight> for u64 {
    fn from(height: BlockHeight) -> Self {
        height.0
    }
}

/// Hash of a block as fixed-size byte array
#[derive(
    Clone,
    Default,
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
    BorshSchema,
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

    /// Check if the given key is a key to a validity predicate. If it is,
    /// returns the address of the account.
    pub fn is_validity_predicate(&self) -> Option<&Address> {
        match &self.segments[..] {
            [DbKeySeg::AddressSeg(address), DbKeySeg::StringSeg(sub_key)]
                if sub_key == RESERVED_VP_KEY =>
            {
                Some(address)
            }
            _ => None,
        }
    }

    /// Check if the given key can be updated
    pub fn is_updatable(&self) -> bool {
        !matches!(&self.segments[..], [
            DbKeySeg::AddressSeg(_),
            DbKeySeg::StringSeg(key),
            DbKeySeg::AddressSeg(Address::Internal(
                InternalAddress::IbcBurn | InternalAddress::IbcMint)),
        ] if key == BALANCE_STORAGE_KEY)
    }

    /// Returns a key from the given DB key path that has the height and
    /// the space type
    pub fn parse_db_key(db_key: &str) -> Result<Self> {
        let mut segments: Vec<&str> =
            db_key.split(KEY_SEGMENT_SEPARATOR).collect();
        let key = match segments.get(3) {
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
                error: format!("Cannot parse key segments {}: {}", db_key, e),
            })?,
        };
        Ok(key)
    }

    /// Returns a sub key without the first segment
    pub fn sub_key(&self) -> Result<Self> {
        match self.segments.split_first() {
            Some((_, rest)) => {
                if rest.is_empty() {
                    Err(Error::Temporary {
                        error: format!(
                            "The key doesn't have the sub segments: {}",
                            self
                        ),
                    })
                } else {
                    Ok(Self {
                        segments: rest.to_vec(),
                    })
                }
            }
            None => Err(Error::Temporary {
                error: "The key is empty".to_owned(),
            }),
        }
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let key = self
            .segments
            .iter()
            .map(DbKeySeg::raw)
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
    BorshSchema,
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
        match string.chars().next() {
            // address hashes are prefixed with `'#'`
            Some(c) if c == RESERVED_ADDRESS_PREFIX => {
                let _ = string.remove(0);
                Address::decode(&string)
                    .map_err(Error::ParseAddress)
                    .map(DbKeySeg::AddressSeg)
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
    BorshSchema,
    Serialize,
    Deserialize,
)]
pub struct Epoch(pub u64);

impl Display for Epoch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Epoch {
    type Err = ParseIntError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let raw: u64 = u64::from_str(s)?;
        Ok(Self(raw))
    }
}

impl Epoch {
    /// Change to the next epoch
    pub fn next(&self) -> Self {
        Self(self.0 + 1)
    }

    /// Change to the previous epoch
    pub fn prev(&self) -> Self {
        Self(self.0 - 1)
    }
}

impl Add<u64> for Epoch {
    type Output = Epoch;

    fn add(self, rhs: u64) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl From<Epoch> for u64 {
    fn from(epoch: Epoch) -> Self {
        epoch.0
    }
}

/// Predecessor block epochs
#[derive(
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshSerialize,
    BorshDeserialize,
)]
pub struct Epochs {
    /// The oldest epoch we can look-up.
    first_known_epoch: Epoch,
    /// The block heights of the first block of each known epoch.
    /// Invariant: the values must be sorted in ascending order.
    first_block_heights: Vec<BlockHeight>,
}

impl Default for Epochs {
    /// Initialize predecessor epochs, assuming starting on the epoch 0 and
    /// block height 0.
    fn default() -> Self {
        Self {
            first_known_epoch: Epoch::default(),
            first_block_heights: vec![BlockHeight::default()],
        }
    }
}

impl Epochs {
    /// Record start of a new epoch at the given block height and trim any
    /// epochs that ended more than `max_age_num_blocks` ago.
    pub fn new_epoch(
        &mut self,
        block_height: BlockHeight,
        max_age_num_blocks: u64,
    ) {
        let min_block_height_to_keep = (block_height.0 + 1)
            .checked_sub(max_age_num_blocks)
            .unwrap_or_default();
        // trim off any epochs whose last block is before the limit
        while let Some((_first_known_epoch_height, rest)) =
            self.first_block_heights.split_first()
        {
            if let Some(second_known_epoch_height) = rest.first() {
                if second_known_epoch_height.0 < min_block_height_to_keep {
                    self.first_known_epoch = self.first_known_epoch.next();
                    self.first_block_heights = rest.to_vec();
                    continue;
                }
            }
            break;
        }
        self.first_block_heights.push(block_height);
    }

    /// Look-up the epoch of a given block height.
    pub fn get_epoch(&self, block_height: BlockHeight) -> Option<Epoch> {
        if let Some((first_known_epoch_height, rest)) =
            self.first_block_heights.split_first()
        {
            if block_height < *first_known_epoch_height {
                return None;
            }
            let mut epoch = self.first_known_epoch;
            for next_block_height in rest {
                if block_height < *next_block_height {
                    return Some(epoch);
                } else {
                    epoch = epoch.next();
                }
            }
            return Some(epoch);
        }
        None
    }
}

#[cfg(feature = "ferveo-tpke")]
#[derive(Default, Debug, Clone, BorshDeserialize, BorshSerialize)]
/// Wrapper txs to be decrypted in the next block proposal
pub struct TxQueue {
    /// Index of next wrapper_tx to fetch from storage
    next_wrapper: usize,
    /// The actual wrappers
    queue: std::collections::VecDeque<WrapperTx>,
}

#[cfg(feature = "ferveo-tpke")]
impl TxQueue {
    /// Add a new wrapper at the back of the queue
    pub fn push(&mut self, wrapper: WrapperTx) {
        self.queue.push_back(wrapper);
    }

    /// Remove the wrapper at the head of the queue
    pub fn pop(&mut self) -> Option<WrapperTx> {
        self.queue.pop_front()
    }

    /// Iterate lazily over the queue. Finds the next value and advances the
    /// lazy iterator.
    #[allow(dead_code)]
    pub fn lazy_next(&mut self) -> Option<&WrapperTx> {
        let next = self.queue.get(self.next_wrapper);
        if self.next_wrapper < self.queue.len() {
            self.next_wrapper += 1;
        }
        next
    }

    /// Reset the iterator to the head of the queue
    pub fn rewind(&mut self) {
        self.next_wrapper = 0;
    }

    /// Get an iterator over the queue
    #[allow(dead_code)]
    pub fn iter(&self) -> impl std::iter::Iterator<Item = &WrapperTx> {
        self.queue.iter()
    }

    /// Check if there are any txs in the queue
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}

/// A value of a storage prefix iterator.
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize, BorshSchema)]
pub struct PrefixValue {
    /// Storage key
    pub key: Key,
    /// Raw value bytes
    pub value: Vec<u8>,
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

        let target = "?/test".to_owned();
        let key = Key::parse(target.clone()).expect("cannot parse the string");
        assert_eq!(key.to_string(), target);
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

        let target = "?".to_owned();
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
    }

    #[test]
    fn test_predecessor_epochs() {
        let mut epochs = Epochs::default();
        assert_eq!(epochs.get_epoch(BlockHeight(0)), Some(Epoch(0)));
        let mut max_age_num_blocks = 100;

        // epoch 1
        epochs.new_epoch(BlockHeight(10), max_age_num_blocks);
        println!("epochs {:#?}", epochs);
        assert_eq!(epochs.get_epoch(BlockHeight(0)), Some(Epoch(0)));
        assert_eq!(epochs.get_epoch(BlockHeight(9)), Some(Epoch(0)));
        assert_eq!(epochs.get_epoch(BlockHeight(10)), Some(Epoch(1)));
        assert_eq!(epochs.get_epoch(BlockHeight(11)), Some(Epoch(1)));
        assert_eq!(epochs.get_epoch(BlockHeight(100)), Some(Epoch(1)));

        // epoch 2
        epochs.new_epoch(BlockHeight(20), max_age_num_blocks);
        println!("epochs {:#?}", epochs);
        assert_eq!(epochs.get_epoch(BlockHeight(0)), Some(Epoch(0)));
        assert_eq!(epochs.get_epoch(BlockHeight(9)), Some(Epoch(0)));
        assert_eq!(epochs.get_epoch(BlockHeight(10)), Some(Epoch(1)));
        assert_eq!(epochs.get_epoch(BlockHeight(11)), Some(Epoch(1)));
        assert_eq!(epochs.get_epoch(BlockHeight(20)), Some(Epoch(2)));
        assert_eq!(epochs.get_epoch(BlockHeight(100)), Some(Epoch(2)));

        // epoch 3, epoch 0 and 1 should be trimmed
        epochs.new_epoch(BlockHeight(200), max_age_num_blocks);
        println!("epochs {:#?}", epochs);
        assert_eq!(epochs.get_epoch(BlockHeight(0)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(9)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(10)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(11)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(20)), Some(Epoch(2)));
        assert_eq!(epochs.get_epoch(BlockHeight(100)), Some(Epoch(2)));
        assert_eq!(epochs.get_epoch(BlockHeight(200)), Some(Epoch(3)));

        // increase the limit
        max_age_num_blocks = 200;

        // epoch 4
        epochs.new_epoch(BlockHeight(300), max_age_num_blocks);
        println!("epochs {:#?}", epochs);
        assert_eq!(epochs.get_epoch(BlockHeight(20)), Some(Epoch(2)));
        assert_eq!(epochs.get_epoch(BlockHeight(100)), Some(Epoch(2)));
        assert_eq!(epochs.get_epoch(BlockHeight(200)), Some(Epoch(3)));
        assert_eq!(epochs.get_epoch(BlockHeight(300)), Some(Epoch(4)));

        // epoch 5, epoch 2 should be trimmed
        epochs.new_epoch(BlockHeight(499), max_age_num_blocks);
        println!("epochs {:#?}", epochs);
        assert_eq!(epochs.get_epoch(BlockHeight(20)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(100)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(200)), Some(Epoch(3)));
        assert_eq!(epochs.get_epoch(BlockHeight(300)), Some(Epoch(4)));
        assert_eq!(epochs.get_epoch(BlockHeight(499)), Some(Epoch(5)));

        // epoch 6, epoch 3 should be trimmed
        epochs.new_epoch(BlockHeight(500), max_age_num_blocks);
        println!("epochs {:#?}", epochs);
        assert_eq!(epochs.get_epoch(BlockHeight(200)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(300)), Some(Epoch(4)));
        assert_eq!(epochs.get_epoch(BlockHeight(499)), Some(Epoch(5)));
        assert_eq!(epochs.get_epoch(BlockHeight(500)), Some(Epoch(6)));

        // decrease the limit
        max_age_num_blocks = 50;

        // epoch 7, epoch 4 and 5 should be trimmed
        epochs.new_epoch(BlockHeight(550), max_age_num_blocks);
        println!("epochs {:#?}", epochs);
        assert_eq!(epochs.get_epoch(BlockHeight(300)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(499)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(500)), Some(Epoch(6)));
        assert_eq!(epochs.get_epoch(BlockHeight(550)), Some(Epoch(7)));

        // epoch 8, epoch 6 should be trimmed
        epochs.new_epoch(BlockHeight(600), max_age_num_blocks);
        println!("epochs {:#?}", epochs);
        assert_eq!(epochs.get_epoch(BlockHeight(500)), None);
        assert_eq!(epochs.get_epoch(BlockHeight(550)), Some(Epoch(7)));
        assert_eq!(epochs.get_epoch(BlockHeight(600)), Some(Epoch(8)));
    }
}

/// Helpers for testing with storage types.
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use proptest::collection;
    use proptest::prelude::*;

    use super::*;
    use crate::types::address::testing::{
        arb_address, arb_non_internal_address,
    };

    /// Generate an arbitrary [`Key`].
    pub fn arb_key() -> impl Strategy<Value = Key> {
        prop_oneof![
            // a key for a validity predicate
            arb_non_internal_address()
                .prop_map(|addr| Key::validity_predicate(&addr)),
            // a key from key segments
            arb_key_no_vp(),
        ]
    }

    /// Generate an arbitrary [`Key`] other than a validity predicate key.
    pub fn arb_key_no_vp() -> impl Strategy<Value = Key> {
        // a key from key segments
        collection::vec(arb_key_seg(), 1..5)
            .prop_map(|segments| Key { segments })
    }

    /// Generate an arbitrary [`Key`] for a given address storage sub-space.
    pub fn arb_account_storage_key(
        address: Address,
    ) -> impl Strategy<Value = Key> {
        prop_oneof![
            // a key for a validity predicate
            Just(Key::validity_predicate(&address)),
            // a key from key segments
            arb_account_storage_key_no_vp(address),
        ]
    }

    /// Generate an arbitrary [`Key`] other than a validity predicate key for a
    /// given address storage sub-space.
    pub fn arb_account_storage_key_no_vp(
        address: Address,
    ) -> impl Strategy<Value = Key> {
        collection::vec(arb_key_seg(), 1..5).prop_map(move |arb_segments| {
            let mut segments = vec![address.to_db_key()];
            segments.extend(arb_segments);
            Key { segments }
        })
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
