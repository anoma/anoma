//! Matchmaker types and traits

use std::collections::HashSet;

/// The matchmaker's host, used to communicate back from the VM
pub trait MmHost: Clone {
    /// Remove intents with the given IDs from the mempool
    fn remove_intents(&self, intents_id: HashSet<Vec<u8>>);
    /// Inject a transaction with the given data
    fn inject_tx(&self, tx_data: Vec<u8>);
    /// Update the state of the matchmaker
    fn update_state(&self, data: Vec<u8>);
}
