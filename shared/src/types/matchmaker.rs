//! Matchmaker types

use std::collections::HashSet;

/// A matchmaker marker trait. This should not be implemented manually. Instead,
/// it is added by the derive `Matchmaker` macro, which also adds necessary
/// binding code for matchmaker dylib runner.
pub trait Matchmaker: AddIntent {}

/// A matchmaker must implement this trait
pub trait AddIntent: Default {
    // TODO: For some reason, using `&[u8]` causes the `decode_intent_data` to
    // fail decoding
    /// Add a new intent to matchmaker's state
    #[allow(clippy::ptr_arg)]
    fn add_intent(
        &mut self,
        intent_id: &Vec<u8>,
        intent_data: &Vec<u8>,
    ) -> AddIntentResult;
}

/// The result of calling matchmaker's `add_intent` function
#[derive(Clone, Debug, Default)]
pub struct AddIntentResult {
    /// A transaction matched from the intent, if any
    pub tx: Option<Vec<u8>>,
    /// The intent IDs that were matched into the tx, if any
    pub matched_intents: Option<HashSet<Vec<u8>>>,
}
