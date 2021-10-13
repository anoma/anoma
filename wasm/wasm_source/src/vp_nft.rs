use anoma_vm_env::vp_prelude::*;

#[validity_predicate]
fn validate_tx(
    tx_data: Vec<u8>,
    addr: Address,
    keys_changed: HashSet<storage::Key>,
    verifiers: HashSet<Address>,
) -> bool {
    log_string(format!(
        "validate_tx called with token addr: {}, key_changed: {:#?}, \
            verifiers: {:?}",
        addr, keys_changed, verifiers
    ));

    nft::vp(tx_data, &addr, &keys_changed, &verifiers)
}

#[cfg(test)]
mod tests {
    use address::testing::arb_non_internal_address;
    // Use this as `#[test]` annotation to enable logging
    use anoma::types::nft::NftToken;
    use anoma::types::transaction::{CreateNft, MintNft};
    use anoma_tests::log::test;
    use anoma_tests::tx::{self, tx_host_env, TestTxEnv};
    use anoma_tests::vp::vp_host_env::storage::Key;
    use anoma_tests::vp::*;
    use proptest::prelude::*;
    use storage::testing::arb_account_storage_key_no_vp;

    use super::*;

    const VP_ALWAYS_TRUE_WASM: &str =
        "../../wasm_for_tests/vp_always_true.wasm";

    /// Test that no-op transaction (i.e. no storage modifications) accepted.
    // #[test]
    fn test_no_op_transaction() {
        let mut tx_env = TestTxEnv::default();
        tx::init_tx_env(&mut tx_env);

        let nft_owner = address::testing::established_address_2();
        tx_env.spawn_accounts([&nft_owner]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        let nft_address = tx_host_env::nft::init_nft(CreateNft {
            owner: nft_owner.clone(),
            vp_code: vp_code.clone(),
            tokens: vec![],
        });

        tx_env.write_log.commit_tx();

        let vp_env =
            init_vp_env_from_tx(nft_address.clone(), tx_env, |address| {
                // Apply transfer in a transaction
                tx_host_env::insert_verifier(address)
            });

        let tx_data: Vec<u8> = vec![];
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = vp_env.get_verifiers();
        assert!(validate_tx(tx_data, nft_address, keys_changed, verifiers));
    }

    // #[test]
    fn test_mint_no_tokens() {
        let mut tx_env = TestTxEnv::default();
        tx::init_tx_env(&mut tx_env);

        let nft_owner = address::testing::established_address_2();
        tx_env.spawn_accounts([&nft_owner]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        let nft_address = tx_host_env::nft::init_nft(CreateNft {
            owner: nft_owner.clone(),
            vp_code: vp_code.clone(),
            tokens: vec![],
        });

        tx_env.write_log.commit_tx();

        let vp_env =
            init_vp_env_from_tx(nft_address.clone(), tx_env, |address| {
                // Apply transfer in a transaction
                tx_host_env::nft::mint_tokens(MintNft {
                    address: nft_address.clone(),
                    owner: nft_owner.clone(),
                    tokens: vec![],
                });
                tx_host_env::insert_verifier(address)
            });

        let tx_data: Vec<u8> = vec![];
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = vp_env.get_verifiers();
        assert!(validate_tx(tx_data, nft_address, keys_changed, verifiers));
    }

    #[test]
    fn test_mint_tokens() {
        let mut tx_env = TestTxEnv::default();
        tx::init_tx_env(&mut tx_env);

        let nft_owner = address::testing::established_address_2();
        let nft_token_owner = address::testing::established_address_1();
        tx_env.spawn_accounts([&nft_owner, &nft_token_owner]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        let nft_address = tx_host_env::nft::init_nft(CreateNft {
            owner: nft_owner.clone(),
            vp_code: vp_code.clone(),
            tokens: vec![],
        });

        tx_env.write_log.commit_tx();
        tx_env.write_log.commit_block(&mut tx_env.storage);

        let vp_env =
            init_vp_env_from_tx(nft_address.clone(), tx_env, |address| {
                // Apply transfer in a transaction
                tx_host_env::nft::mint_tokens(MintNft {
                    address: nft_address.clone(),
                    owner: nft_token_owner.clone(),
                    tokens: vec![NftToken {
                        id: 1,
                        metadata: "".to_string(),
                        approvals: vec![],
                    }],
                });
                tx_host_env::insert_verifier(&nft_owner)
            });

        let tx_data: Vec<u8> = vec![];
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = vp_env.get_verifiers();

        assert!(validate_tx(tx_data, nft_address, keys_changed, verifiers));
    }
}
