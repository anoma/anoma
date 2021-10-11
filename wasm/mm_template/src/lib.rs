use anoma_vm_env::matchmaker_prelude::*;

#[matchmaker]
fn add_intent(
    _last_state: Vec<u8>,
    _intent_id: Vec<u8>,
    _intent_data: Vec<u8>,
) -> bool {
    true
}
