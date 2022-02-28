use crate::types::{address::Address, storage::Epoch};
use borsh::{BorshSerialize, BorshDeserialize};
use serde::{Deserialize, Serialize};

/// A tx data type to hold proposal data
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct InitProposalData {
    /// The proposal id
    pub id: Option<u64>,
    /// The proposal content
    pub content: String,
    /// The proposal author address
    pub author: Address,
    /// The epoch from which voting is allowed
    pub voting_start_epoch: Epoch,
    /// The epoch from which voting is stopped
    pub voting_end_epoch: Epoch,
    /// The epoch from which this changes are executed
    pub grace_epoch: Epoch,
    /// The code containing the storage changes
    pub proposal_code: Option<Vec<u8>>
}