use anoma_vp_prelude::*;

#[validity_predicate]
fn validate_tx(
    tx_data: Vec<u8>,
    addr: Address,
    keys_changed: BTreeSet<storage::Key>,
    verifiers: BTreeSet<Address>,
) -> bool {
    log_string(format!(
        "validate_tx called with addr: {}, key_changed: {:#?}, tx_data: \
         {:#?}, verifiers: {:?}",
        addr, keys_changed, tx_data, verifiers
    ));

    for key in keys_changed.iter() {
        let key = key.to_string();
        let pre: Option<u64> = read_pre(&key);
        let post: Option<u64> = read_post(&key);
        log_string(format!(
            "validate_tx key: {}, pre: {:#?}, post: {:#?}",
            key, pre, post,
        ));
    }
    true
}
