#[allow(clippy::ptr_arg)]
#[no_mangle]
fn add_intent(
    _last_state: &Vec<u8>,
    _intent_id: &Vec<u8>,
    _intent_data: &Vec<u8>,
) -> bool {
    // Was the new intent matched into a transaction?
    false
}
