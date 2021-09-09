//! Types that are used in transactions.

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};

use super::{key::ed25519::PublicKey, nft::NftToken};
use crate::types::address::Address;

/// A tx data type to update an account's validity predicate
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct UpdateVp {
    /// An address of the account
    pub addr: Address,
    /// The new VP code
    pub vp_code: Vec<u8>,
}

/// A tx data type to initialize a new established account
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct InitAccount {
    /// Public key to be written into the account's storage. This can be used
    /// for signature verification of transactions for the newly created
    /// account.
    pub public_key: PublicKey,
    /// The VP code
    pub vp_code: Vec<u8>,
}

/// A tx data type to create a new NFT
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct CreateNft {
    /// The source address
    pub owner: Address,
    /// The token to be sold
    pub vp_code: Vec<u8>,
    /// The nft tokens
    pub tokens: Vec<NftToken>,
}

/// A tx data type to mint new nft tokens
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct MintNft {
    /// The nft owner address
    pub owner: Address,
    /// The nft address
    pub address: Address,
    /// The nft tokens
    pub tokens: Vec<NftToken>,
}
