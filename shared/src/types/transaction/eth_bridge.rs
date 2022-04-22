use borsh::{BorshDeserialize, BorshSerialize};

use crate::types::address::Address;
use crate::types::token::Amount;

// TODO: custom type for queue - Vec<TransferFromEthereum>

#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
pub struct TransferFromEthereum {
    /// the address on Namada receiving the tokens
    pub receiver: Address,
    /// the amount of ETH token to mint
    pub amount: Amount,
    /// minimum number of confirmations needed for mints
    pub min_confirmations: u8,
    /// height of the block at which the message appeared
    pub height: u64,
    // the hash & height of the last descendant block marked as `seen`
    // latest_descendant: ([u8; 32], u64)  // TODO
}

/// Contains information to update the queue
#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
pub struct UpdateQueue {
    /// New transfers to add to the queue
    pub enqueue: Vec<TransferFromEthereum>,
}
