//! Types that are used in nft transactions.

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};

use super::address::Address;
use super::storage::{DbKeySeg, Key, KeySeg};

const NFT_KEY: &str = "nft";
const CREATOR_KEY: &str = "creator";
const METADATA_KEY: &str = "metadata";
const APPROVALS_KEY: &str = "approvals";
const BURNT_KEY: &str = "burnt";
const IDS_KEY: &str = "ids";
const CURRENT_OWNER_KEY: &str = "current_owner";
const PAST_OWNERS_KEY: &str = "past_owners";

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
    /// The source address
    pub owner: Address,
    /// The validity predicate associated with the NFT
    pub vp: Vec<u8>,
    /// The list of tokens
    pub tokens: Vec<NftToken>,
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
    /// The token Id
    pub id: u64,
    /// The URI containing metadata
    pub metadata: String,
    /// Approved addresses
    pub approvals: Vec<Address>,
}

/// Get the nft prefix
pub fn _nft_prefix(address: &Address) -> Key {
    Key::from(address.to_db_key())
        .push(&NFT_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft token prefix
pub fn nft_token_prefix(address: &Address, token_id: &str) -> Key {
    _nft_prefix(address)
        .push(&IDS_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&token_id.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft owner storage key
pub fn get_creator_key(address: &Address, creator_address: &Address) -> Key {
    _nft_prefix(address)
        .push(&CREATOR_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&creator_address.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft metadata storage key
pub fn get_token_metadata_key(
    address: &Address,
    nft_id: &str,
    metadata: &str,
) -> Key {
    nft_token_prefix(address, nft_id)
        .push(&METADATA_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&metadata.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft current_owner storage key
pub fn get_token_current_owner_key(
    address: &Address,
    nft_id: &str,
    current_owner_address: &Address,
) -> Key {
    nft_token_prefix(address, nft_id)
        .push(&CURRENT_OWNER_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&current_owner_address.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft current_owner storage key
pub fn get_token_past_owners_key(address: &Address, nft_id: &str) -> Key {
    nft_token_prefix(address, nft_id)
        .push(&PAST_OWNERS_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft burnt storage key
pub fn get_token_burnt_key(address: &Address, nft_id: &str) -> Key {
    nft_token_prefix(address, nft_id)
        .push(&BURNT_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the nft approval storage key
pub fn get_token_approval_key(
    address: &Address,
    nft_id: &str,
    approval: &Address,
) -> Key {
    nft_token_prefix(address, nft_id)
        .push(&APPROVALS_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&approval.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Check that a particular nft is created by a particular creator address
pub fn is_nft_creator_key(key: &Key, address: &Address) -> Option<Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(nft_addr), DbKeySeg::StringSeg(prefix), DbKeySeg::StringSeg(creator_key), DbKeySeg::AddressSeg(creator_addr)]
            if nft_addr == address
                && prefix == NFT_KEY
                && creator_key == CREATOR_KEY =>
                // && creator_addr == creator =>
        {
            Some(creator_addr.to_owned())
        }
        _ => None,
    }
}

/// Check that a particular key is a approval storage key
pub fn is_nft_approval_key(key: &Key, address: &Address) -> Option<String> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(nft_addr), DbKeySeg::StringSeg(prefix), DbKeySeg::StringSeg(ids_key), DbKeySeg::StringSeg(token_id_key), DbKeySeg::StringSeg(approval_key), DbKeySeg::AddressSeg(_approval_address_key)]
            if nft_addr == address
                && prefix == NFT_KEY
                && ids_key == IDS_KEY
                && approval_key == APPROVALS_KEY =>
        {
            Some(token_id_key.to_owned())
        }
        _ => None,
    }
}

/// Check that a particular key is a metadata storage key
pub fn is_nft_metadata_key(key: &Key, address: &Address) -> Option<String> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(nft_addr), DbKeySeg::StringSeg(prefix), DbKeySeg::StringSeg(ids_key), DbKeySeg::StringSeg(token_id_key), DbKeySeg::StringSeg(metadata_key)]
            if nft_addr == address
                && prefix == NFT_KEY
                && ids_key == IDS_KEY
                && metadata_key == METADATA_KEY =>
        {
            Some(token_id_key.to_owned())
        }
        _ => None,
    }
}

/// Check that a particular key is a current_owner storage key
pub fn is_nft_current_owner_key(
    key: &Key,
    address: &Address,
) -> Option<String> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(nft_addr), DbKeySeg::StringSeg(prefix), DbKeySeg::StringSeg(ids_key), DbKeySeg::StringSeg(token_id_key), DbKeySeg::StringSeg(current_owner_key), DbKeySeg::AddressSeg(_)]
            if nft_addr == address
                && prefix == NFT_KEY
                && ids_key == IDS_KEY
                && current_owner_key == CURRENT_OWNER_KEY =>
        {
            Some(token_id_key.to_owned())
        }
        _ => None,
    }
}

/// Check that a particular key is a past_owners storage key
pub fn is_nft_past_owners_key(
    key: &Key,
    address: &Address,
    token_id: &str,
) -> bool {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(nft_addr), DbKeySeg::StringSeg(prefix), DbKeySeg::StringSeg(ids_key), DbKeySeg::StringSeg(token_id_key), DbKeySeg::StringSeg(past_owners_key), DbKeySeg::AddressSeg(_)]
            if nft_addr == address
                && prefix == NFT_KEY
                && ids_key == IDS_KEY
                && token_id_key == token_id
                && past_owners_key == PAST_OWNERS_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Check that a key points to a nft storage key
pub fn is_nft_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(nft_addr), DbKeySeg::StringSeg(prefix), ..]
            if prefix == NFT_KEY =>
        {
            Some(nft_addr)
        }
        _ => None,
    }
}
// loop using iter_prefix ->
// /Users/fraccaman/Heliax/anoma-prototype/shared/src/ledger/ibc/channel.rs
