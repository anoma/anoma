use anoma_vm_env::filter_prelude::*;

#[filter]
fn validate_intent(_intent: Vec<u8>) -> bool {
    true
}
