//! Serializable structs that may be useful to use in transactions and validity
//! predicates
use borsh::{BorshDeserialize, BorshSerialize};

/// Represents a write to a storage key
#[derive(BorshDeserialize, BorshSerialize, Debug)]
pub struct WriteOp {
    /// The storage key to write to
    pub key: String,
    /// The new value to be written to the storage key
    pub value: Vec<u8>,
}
