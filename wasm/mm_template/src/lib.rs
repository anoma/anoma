use anoma_vm_env::matchmaker_prelude::*;

#[matchmaker]
fn add_intent(_graph_bytes: Vec<u8>, _id: Vec<u8>, _data: Vec<u8>) -> bool {
    true
}
