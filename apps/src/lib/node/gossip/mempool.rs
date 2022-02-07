use std::collections::HashMap;

use anoma::proto::{Intent, IntentId};

/// In-memory intent mempool
#[derive(Clone, Debug, Default)]
pub struct IntentMempool(HashMap<IntentId, Intent>);

impl IntentMempool {
    /// Insert a new intent. If the mempool didn't have this intent present,
    /// returns `true`.
    pub fn insert(&mut self, intent: Intent) -> bool {
        self.0.insert(intent.id(), intent).is_none()
    }

    /// Remove an intent from mempool. If the mempool didn't have this intent
    /// present, returns `true`. in the mempool.
    pub fn remove(&mut self, intent_id: &IntentId) -> bool {
        self.0.remove(intent_id).is_some()
    }

    /// Returns `true` if the map contains intent with specified ID.
    pub fn contains(&self, intent_id: &IntentId) -> bool {
        self.0.contains_key(intent_id)
    }
}
