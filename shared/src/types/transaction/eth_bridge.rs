use borsh::{BorshDeserialize, BorshSerialize};

/// Contains information to update the queue
#[derive(Debug, Clone, PartialEq, BorshSerialize, BorshDeserialize)]
pub struct UpdateQueue {
    /// The newval to set queue to
    pub newval: u64,
}
