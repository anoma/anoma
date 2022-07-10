//! Storage helpers for testing

use std::rc::Rc;

use anoma::types::storage;
use derivative::Derivative;

/// A list of changes, which must be applied in the same order to get to the
/// current state.
pub type Changes = Vec<Change>;

/// Storage modification
#[derive(Clone, Debug)]
pub struct Change {
    pub key: storage::Key,
    pub value: ValueChange,
}

/// Storage value modification
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub enum ValueChange {
    Write(
        #[derivative(Debug = "ignore")] Rc<dyn Fn(Option<Vec<u8>>) -> Vec<u8>>,
    ),
    Delete,
}
