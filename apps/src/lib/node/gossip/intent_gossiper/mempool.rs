use std::collections::HashMap;

use anoma::proto::{Intent, IntentId};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct IntentMempool(HashMap<IntentId, Intent>);

impl IntentMempool {
    pub fn new() -> Self {
        Self(HashMap::default())
    }

    pub fn put(&mut self, intent: Intent) -> Result<bool> {
        Ok(self.0.insert(intent.id(), intent).is_none())
    }

    pub fn remove(&mut self, intent_id: &IntentId) -> bool {
        self.0.remove(intent_id).is_some()
    }
}
