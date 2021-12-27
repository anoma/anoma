//! Matchmaker types

use std::collections::HashSet;

/// The result of calling matchmaker's `add_intent` function
#[derive(Clone, Debug, Default)]
pub struct AddIntentResult {
    /// A transaction matched from the intent, if any
    pub tx: Option<Vec<u8>>,
    /// The intent IDs that were matched into the tx, if any
    pub matched_intents: Option<HashSet<Vec<u8>>>,
    /// Matchmaker's state
    pub state: Vec<u8>,
}
