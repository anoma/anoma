use anoma_vm_env::tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    log_string(format!("apply_tx called with data: {:#?}", tx_data));
}

#[cfg(test)]
mod tests {
    use anoma_tests::tx::*;

    use super::*;

    /// An example test, checking that this transaction performs no storage
    /// modifications.
    #[test]
    fn test_no_op_transaction() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        let tx_data = vec![];
        apply_tx(tx_data);

        assert!(env.all_touched_storage_keys().is_empty());
    }
}
