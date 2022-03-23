//! Nft types
use std::fmt;

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};

use super::address::Address;
use super::storage::{DbKeySeg, Key, KeySeg};

const NFT_KEY: &str = "nft";
const TAG_KEY: &str = "tag";
const CREATOR_KEY: &str = "creator";
const KEYS: &str = "keys";
const OPTIONAL_KEYS: &str = "optional_keys";
const METADATA_KEY: &str = "metadata";
const APPROVALS_KEY: &str = "approvals";
const BURNT_KEY: &str = "burnt";
const IDS_KEY: &str = "ids";
const CURRENT_OWNER_KEY: &str = "current_owner";
const PAST_OWNERS_KEY: &str = "past_owners";
const VALUE_KEY: &str = "value";
const OPTIONAL_VALUE: &str = "optional_value";

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
/// Nft Version tag
pub enum NftTag {
    /// Tag v1
    V1,
}

impl Default for NftTag {
    fn default() -> Self {
        Self::V1
    }
}

impl fmt::Display for NftTag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NftTag::V1 => write!(f, "v1"),
        }
    }
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
/// The definition of an NFT
pub struct Nft {
    #[serde(default)]
    /// Nft version
    pub tag: NftTag,
    /// The source address
    pub creator: Address,
    /// The path to a file containing the validity predicate associated with
    /// the NFT
    pub vp_path: Option<String>,
    /// Mandatory NFT fields
    pub keys: Vec<String>,
    #[serde(default = "default_opt_keys")]
    /// Optional NFT fields
    pub opt_keys: Vec<String>,
    /// The list of tokens
    pub tokens: Vec<NftToken>,
}

impl fmt::Display for Nft {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "tag: {}, creator: {}, tokens: {:?}",
            self.tag, self.creator, self.tokens
        )
    }
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
/// The definition of an NFT token
pub struct NftToken {
    /// The token id
    pub id: u64,
    /// The URI containing metadata
    pub metadata: String,
    /// Current owner
    pub current_owner: Option<Address>,
    /// Past owners
    #[serde(default = "default_past_owners")]
    pub past_owners: Vec<Address>,
    /// Approved addresses
    pub approvals: Vec<Address>,
    /// Mandatory fields values
    pub values: Vec<String>,
    #[serde(default = "default_opt_values")]
    /// Optionals fields values
    pub opt_values: Vec<String>,
    #[serde(default = "default_burnt")]
    /// Is token burnt
    pub burnt: bool,
}

impl fmt::Display for NftToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "id: {}, metadata: {}, values: {:?}",
            self.id, self.metadata, self.values
        )
    }
}

fn default_opt_keys() -> Vec<String> {
    Vec::new()
}

fn default_past_owners() -> Vec<Address> {
    Vec::new()
}

fn default_opt_values() -> Vec<String> {
    Vec::new()
}

fn default_burnt() -> bool {
    false
}

/// Get the nft prefix
pub fn _nft_prefix(address: &Address) -> Key {
    Key::from(address.to_db_key())
        .push(&NFT_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft token prefix
pub fn _nft_token_prefix(address: &Address, token_id: &str) -> Key {
    _nft_prefix(address)
        .push(&IDS_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&token_id.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft owner storage key
pub fn get_tag_key(address: &Address) -> Key {
    _nft_prefix(address)
        .push(&TAG_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft owner storage key
pub fn get_creator_key(address: &Address) -> Key {
    _nft_prefix(address)
        .push(&CREATOR_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft keys storage key
pub fn get_keys_key(address: &Address) -> Key {
    _nft_prefix(address)
        .push(&KEYS.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft keys storage key
pub fn get_optional_keys_key(address: &Address) -> Key {
    _nft_prefix(address)
        .push(&OPTIONAL_KEYS.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft metadata storage key
pub fn get_token_metadata_key(address: &Address, nft_id: &str) -> Key {
    _nft_token_prefix(address, nft_id)
        .push(&METADATA_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft current_owner storage key
pub fn get_token_current_owner_key(address: &Address, nft_id: &str) -> Key {
    _nft_token_prefix(address, nft_id)
        .push(&CURRENT_OWNER_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft current_owner storage key
pub fn get_token_past_owners_key(address: &Address, nft_id: &str) -> Key {
    _nft_token_prefix(address, nft_id)
        .push(&PAST_OWNERS_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft value storage key
pub fn get_token_value_key(address: &Address, nft_id: &str) -> Key {
    _nft_token_prefix(address, nft_id)
        .push(&VALUE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft optional value storage key
pub fn get_token_optional_value_key(address: &Address, nft_id: &str) -> Key {
    _nft_token_prefix(address, nft_id)
        .push(&OPTIONAL_VALUE.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft burnt storage key
pub fn get_token_burnt_key(address: &Address, nft_id: &str) -> Key {
    _nft_token_prefix(address, nft_id)
        .push(&BURNT_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft approval storage key
pub fn get_token_approval_key(address: &Address, nft_id: &str) -> Key {
    _nft_token_prefix(address, nft_id)
        .push(&APPROVALS_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Check that nft is created by a specific creator address
pub fn is_nft_creator_key(key: &Key, address: &Address) -> Option<Address> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(nft_addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(creator_key),
        ] if nft_addr == address
            && prefix == NFT_KEY
            && creator_key == CREATOR_KEY =>
        {
            Some(nft_addr.to_owned())
        }
        _ => None,
    }
}

/// Check that a particular key is a approval storage key
pub fn is_nft_approval_key(
    key: &Key,
    address: &Address,
) -> Option<(Address, String)> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(nft_addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(ids_key),
            DbKeySeg::StringSeg(token_id_key),
            DbKeySeg::StringSeg(approval_key),
        ] if nft_addr == address
            && prefix == NFT_KEY
            && ids_key == IDS_KEY
            && approval_key == APPROVALS_KEY =>
        {
            Some((nft_addr.to_owned(), token_id_key.to_owned()))
        }
        _ => None,
    }
}

/// Check that a particular key is a metadata storage key
pub fn is_nft_metadata_key(
    key: &Key,
    address: &Address,
) -> Option<(Address, String)> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(nft_addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(ids_key),
            DbKeySeg::StringSeg(token_id_key),
            DbKeySeg::StringSeg(metadata_key),
        ] if nft_addr == address
            && prefix == NFT_KEY
            && ids_key == IDS_KEY
            && metadata_key == METADATA_KEY =>
        {
            Some((nft_addr.to_owned(), token_id_key.to_owned()))
        }
        _ => None,
    }
}

/// Check that a particular key is a current_owner storage key
pub fn is_nft_current_owner_key(
    key: &Key,
    address: &Address,
) -> Option<(Address, String)> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(nft_addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(ids_key),
            DbKeySeg::StringSeg(token_id_key),
            DbKeySeg::StringSeg(current_owner_key),
        ] if nft_addr == address
            && prefix == NFT_KEY
            && ids_key == IDS_KEY
            && current_owner_key == CURRENT_OWNER_KEY =>
        {
            Some((nft_addr.to_owned(), token_id_key.to_owned()))
        }
        _ => None,
    }
}

/// Check that a particular key is a past_owners storage key
pub fn is_nft_past_owners_key(
    key: &Key,
    address: &Address,
) -> Option<(Address, String)> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(nft_addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(ids_key),
            DbKeySeg::StringSeg(token_id_key),
            DbKeySeg::StringSeg(past_owners_key),
        ] if nft_addr == address
            && prefix == NFT_KEY
            && ids_key == IDS_KEY
            && past_owners_key == PAST_OWNERS_KEY =>
        {
            Some((nft_addr.to_owned(), token_id_key.to_owned()))
        }
        _ => None,
    }
}

/// Check that a key points to a nft storage key
pub fn is_nft_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(nft_addr),
            DbKeySeg::StringSeg(prefix),
            ..,
        ] if prefix == NFT_KEY => Some(nft_addr),
        _ => None,
    }
}
