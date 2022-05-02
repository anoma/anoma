//! Trivial transaction for now

use anoma_tx_prelude::*;
use borsh::{BorshDeserialize, BorshSerialize};

#[derive(BorshDeserialize, BorshSerialize)]
pub struct AddressedBlob {
    address: Address,
    blob: Vec<u8>,
}

#[transaction]
fn apply_tx(_tx_data: Vec<u8>) {}
