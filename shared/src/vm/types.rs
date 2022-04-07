//! VM types can be passed between the host and guest via wasm linear
//! memory.
//!
//! These are either:
//! 1. Module call types
//!    The module call inputs are passed host-to-guest.
//!
//! 2. Execution environment types
//!    The environment inputs are passed guest-to-host and outputs back from
//!    host-to-guest.

use std::collections::{BTreeSet, HashSet};

use borsh::{BorshDeserialize, BorshSerialize};

use crate::types::address::Address;
use crate::types::ethereum_headers::{EpochPower, EthereumHeader};
use crate::types::storage::Key;

/// Input for validity predicate wasm module call
pub struct VpInput<'a> {
    /// The address of the validity predicate's owning account
    pub addr: &'a Address,
    /// The input data as arbitrary bytes
    pub data: &'a [u8],
    /// The storage changed keys from the write log of storage updates
    /// performed by the transaction for the account associated with the VP
    pub keys_changed: &'a BTreeSet<Key>,
    /// The verifiers to trigger VPs
    pub verifiers: &'a BTreeSet<Address>,
}

/// Input for matchmaker wasm module call
pub type MatchmakerInput = Vec<u8>;

/// Key-value pair represents data from account's subspace
#[derive(Clone, Debug, BorshSerialize, BorshDeserialize)]
pub struct KeyVal {
    /// The storage key
    pub key: String,
    /// The value as arbitrary bytes
    pub val: Vec<u8>,
}

/// The struct updating an Ethereum header from the bridge.
#[derive(BorshSerialize, BorshDeserialize)]
pub struct EthereumHeaderUpdate {
    /// The Ethereum header
    pub header: EthereumHeader,
    /// Set of validators who have seen this header
    /// and their voting power at a particular block height
    pub seen_by: HashSet<EpochPower>,
    /// The percentage of voting power that has seen this header
    pub voting_power: (u64, u64),
    /// Indicates if more than 2/3 of the staking validators have seen this
    /// header
    pub seen: bool,
}
