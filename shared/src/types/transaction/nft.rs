use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};

use crate::types::address::Address;
use crate::types::nft::NftToken;

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
    /// Nft version
    pub tag: String,
    /// The creator address
    pub creator: Address,
    /// The nft vp code
    pub vp_code: Vec<u8>,
    /// The nft keys
    pub keys: Vec<String>,
    /// The nft optional keys
    pub opt_keys: Vec<String>,
    /// The nft tokens descriptions
    pub tokens: Vec<NftToken>,
}

/// A tx data type to mint nft tokens
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
    /// The nft address
    pub address: Address,
    /// The creator address
    pub creator: Address,
    /// The nft tokens
    pub tokens: Vec<NftToken>,
}
