use anoma::types::matchmaker::{AddIntent, AddIntentResult};
use anoma_macros::Matchmaker;

#[derive(Default, Matchmaker)]
struct MyMatchmaker;

impl AddIntent for MyMatchmaker {
    fn add_intent(
        &mut self,
        _intent_id: &Vec<u8>,
        _intent_data: &Vec<u8>,
    ) -> AddIntentResult {
        AddIntentResult::default()
    }
}
