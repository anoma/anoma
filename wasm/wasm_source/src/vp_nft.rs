//! A VP for a nft.

use anoma_vp_prelude::*;

#[validity_predicate]
fn validate_tx(
    ctx: &Ctx,
    tx_data: Vec<u8>,
    addr: Address,
    keys_changed: BTreeSet<storage::Key>,
    verifiers: BTreeSet<Address>,
) -> VpResult {
    log_string(format!(
        "validate_tx called with token addr: {}, key_changed: {:#?}, \
         verifiers: {:?}",
        addr, keys_changed, verifiers
    ));

    if !is_tx_whitelisted(ctx)? {
        return reject();
    }

    let vp_check = keys_changed.iter().all(|key| {
        if key.is_validity_predicate().is_some() {
            match ctx.read_bytes_post(key) {
                Ok(Some(vp)) => {
                    matches!(is_vp_whitelisted(ctx, &vp), Ok(true))
                }
                _ => false,
            }
        } else {
            true
        }
    });

    Ok(vp_check && nft::vp(ctx, tx_data, &addr, &keys_changed, &verifiers)?)
}

#[cfg(test)]
mod tests {
    use anoma::types::nft::{self, NftToken};
    use anoma::types::transaction::nft::{CreateNft, MintNft};
    use anoma_tests::log::test;
    use anoma_tests::tx::{self, tx_host_env, TestTxEnv};
    use anoma_tests::vp::*;
    use anoma_tx_prelude::TxEnv;

    use super::*;

    const VP_ALWAYS_TRUE_WASM: &str =
        "../../wasm_for_tests/vp_always_true.wasm";

    /// Test that no-op transaction (i.e. no storage modifications) accepted.
    #[test]
    fn test_no_op_transaction() {
        let mut tx_env = TestTxEnv::default();

        let nft_creator = address::testing::established_address_2();
        tx_env.spawn_accounts([&nft_creator]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        tx_host_env::set(tx_env);
        let nft_address = tx_host_env::nft::init_nft(
            tx::ctx(),
            CreateNft {
                tag: "v1".to_string(),
                creator: nft_creator.clone(),
                vp_code,
                keys: vec![],
                opt_keys: vec![],
                tokens: vec![],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.write_log.commit_tx();

        vp_host_env::init_from_tx(nft_address.clone(), tx_env, |address| {
            // Apply transfer in a transaction
            tx::ctx().insert_verifier(address).unwrap()
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = vp_env.get_verifiers();
        vp_host_env::set(vp_env);
        assert!(
            validate_tx(&CTX, tx_data, nft_address, keys_changed, verifiers)
                .unwrap()
        );
    }

    /// Test that you can create an nft without tokens
    #[test]
    fn test_mint_no_tokens() {
        let mut tx_env = TestTxEnv::default();

        let nft_creator = address::testing::established_address_2();
        tx_env.spawn_accounts([&nft_creator]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        tx_host_env::set(tx_env);
        let nft_address = tx_host_env::nft::init_nft(
            tx::ctx(),
            CreateNft {
                tag: "v1".to_string(),
                creator: nft_creator.clone(),
                vp_code,
                keys: vec![],
                opt_keys: vec![],
                tokens: vec![],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.write_log.commit_tx();

        vp_host_env::init_from_tx(nft_address.clone(), tx_env, |address| {
            // Apply transfer in a transaction
            tx_host_env::nft::mint_tokens(
                tx::ctx(),
                MintNft {
                    address: nft_address.clone(),
                    tokens: vec![],
                    creator: nft_creator.clone(),
                },
            )
            .unwrap();
            tx::ctx().insert_verifier(address).unwrap()
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = vp_env.get_verifiers();
        vp_host_env::set(vp_env);

        assert!(
            validate_tx(&CTX, tx_data, nft_address, keys_changed, verifiers)
                .unwrap()
        );
    }

    /// Test that you can create an nft with tokens
    #[test]
    fn test_mint_tokens() {
        let mut tx_env = TestTxEnv::default();

        let nft_creator = address::testing::established_address_2();
        let nft_token_owner = address::testing::established_address_1();
        tx_env.spawn_accounts([&nft_creator, &nft_token_owner]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        tx_host_env::set(tx_env);
        let nft_address = tx_host_env::nft::init_nft(
            tx::ctx(),
            CreateNft {
                tag: "v1".to_string(),
                creator: nft_creator.clone(),
                vp_code,
                keys: vec![],
                opt_keys: vec![],
                tokens: vec![],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.commit_tx_and_block();

        vp_host_env::init_from_tx(nft_address.clone(), tx_env, |_| {
            // Apply transfer in a transaction
            tx_host_env::nft::mint_tokens(
                tx::ctx(),
                MintNft {
                    address: nft_address.clone(),
                    creator: nft_creator.clone(),
                    tokens: vec![NftToken {
                        id: 1,
                        values: vec![],
                        opt_values: vec![],
                        metadata: "".to_string(),
                        approvals: vec![],
                        current_owner: Some(nft_token_owner.clone()),
                        past_owners: vec![],
                        burnt: false,
                    }],
                },
            )
            .unwrap();
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = vp_env.get_verifiers();
        vp_host_env::set(vp_env);

        assert!(
            validate_tx(&CTX, tx_data, nft_address, keys_changed, verifiers)
                .unwrap()
        );
    }

    /// Test that only owner can mint new tokens
    #[test]
    fn test_mint_tokens_wrong_owner() {
        let mut tx_env = TestTxEnv::default();

        let nft_creator = address::testing::established_address_2();
        let nft_token_owner = address::testing::established_address_1();
        tx_env.spawn_accounts([&nft_creator, &nft_token_owner]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        tx_host_env::set(tx_env);
        let nft_address = tx_host_env::nft::init_nft(
            tx::ctx(),
            CreateNft {
                tag: "v1".to_string(),
                creator: nft_creator.clone(),
                vp_code,
                keys: vec![],
                opt_keys: vec![],
                tokens: vec![],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.commit_tx_and_block();

        vp_host_env::init_from_tx(nft_address.clone(), tx_env, |_| {
            // Apply transfer in a transaction
            tx_host_env::nft::mint_tokens(
                tx::ctx(),
                MintNft {
                    address: nft_address.clone(),
                    creator: nft_token_owner.clone(),
                    tokens: vec![NftToken {
                        id: 1,
                        values: vec![],
                        opt_values: vec![],
                        metadata: "".to_string(),
                        approvals: vec![],
                        current_owner: Some(nft_token_owner.clone()),
                        past_owners: vec![],
                        burnt: false,
                    }],
                },
            )
            .unwrap();
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = vp_env.get_verifiers();
        vp_host_env::set(vp_env);

        assert!(
            !validate_tx(&CTX, tx_data, nft_address, keys_changed, verifiers)
                .unwrap()
        );
    }

    /// Test that an approval can add another approval
    #[test]
    fn test_mint_tokens_with_approvals_authorized() {
        let mut tx_env = TestTxEnv::default();

        let nft_creator = address::testing::established_address_2();
        let nft_token_owner = address::testing::established_address_1();
        let nft_token_approval = address::testing::established_address_3();
        let nft_token_approval_2 = address::testing::established_address_4();
        tx_env.spawn_accounts([
            &nft_creator,
            &nft_token_owner,
            &nft_token_approval,
            &nft_token_approval_2,
        ]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        tx_host_env::set(tx_env);
        let nft_address = tx_host_env::nft::init_nft(
            tx::ctx(),
            CreateNft {
                tag: "v1".to_string(),
                creator: nft_creator.clone(),
                vp_code,
                keys: vec![],
                opt_keys: vec![],
                tokens: vec![],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.commit_tx_and_block();

        tx_host_env::set(tx_env);
        tx_host_env::nft::mint_tokens(
            tx::ctx(),
            MintNft {
                address: nft_address.clone(),
                creator: nft_creator.clone(),
                tokens: vec![NftToken {
                    id: 1,
                    values: vec![],
                    opt_values: vec![],
                    metadata: "".to_string(),
                    approvals: vec![nft_token_approval.clone()],
                    current_owner: None,
                    past_owners: vec![],
                    burnt: false,
                }],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.commit_tx_and_block();

        vp_host_env::init_from_tx(nft_address.clone(), tx_env, |_| {
            let approval_key = nft::get_token_approval_key(&nft_address, "1");
            tx::ctx()
                .write(
                    &approval_key,
                    [&nft_token_approval_2, &nft_token_approval],
                )
                .unwrap();
            tx::ctx().insert_verifier(&nft_token_approval).unwrap();
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = vp_env.get_verifiers();
        vp_host_env::set(vp_env);

        assert!(
            validate_tx(&CTX, tx_data, nft_address, keys_changed, verifiers)
                .unwrap()
        );
    }

    /// Test that an approval can add another approval
    #[test]
    fn test_mint_tokens_with_approvals_not_authorized() {
        let mut tx_env = TestTxEnv::default();

        let nft_creator = address::testing::established_address_2();
        let nft_token_owner = address::testing::established_address_1();
        let nft_token_approval = address::testing::established_address_3();
        let nft_token_approval_2 = address::testing::established_address_4();
        tx_env.spawn_accounts([
            &nft_creator,
            &nft_token_owner,
            &nft_token_approval,
            &nft_token_approval_2,
        ]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        tx_host_env::set(tx_env);
        let nft_address = tx_host_env::nft::init_nft(
            tx::ctx(),
            CreateNft {
                tag: "v1".to_string(),
                creator: nft_creator.clone(),
                vp_code,
                keys: vec![],
                opt_keys: vec![],
                tokens: vec![],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.commit_tx_and_block();

        tx_host_env::set(tx_env);
        tx_host_env::nft::mint_tokens(
            tx::ctx(),
            MintNft {
                address: nft_address.clone(),
                creator: nft_creator.clone(),
                tokens: vec![NftToken {
                    id: 1,
                    values: vec![],
                    opt_values: vec![],
                    metadata: "".to_string(),
                    approvals: vec![nft_token_approval.clone()],
                    current_owner: None,
                    past_owners: vec![],
                    burnt: false,
                }],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.commit_tx_and_block();

        vp_host_env::init_from_tx(nft_address.clone(), tx_env, |_| {
            let approval_key = nft::get_token_approval_key(&nft_address, "1");
            tx::ctx()
                .write(
                    &approval_key,
                    [&nft_token_approval_2, &nft_token_approval],
                )
                .unwrap();
            tx::ctx().insert_verifier(&nft_token_approval_2).unwrap();
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = vp_env.get_verifiers();
        vp_host_env::set(vp_env);

        assert!(
            !validate_tx(&CTX, tx_data, nft_address, keys_changed, verifiers)
                .unwrap()
        );
    }

    /// Test nft address cannot be changed
    #[test]
    fn test_cant_change_owner() {
        let mut tx_env = TestTxEnv::default();

        let nft_owner = address::testing::established_address_2();
        let another_address = address::testing::established_address_1();
        tx_env.spawn_accounts([&nft_owner, &another_address]);

        // just a dummy vp, its not used during testing
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        tx_host_env::set(tx_env);
        let nft_address = tx_host_env::nft::init_nft(
            tx::ctx(),
            CreateNft {
                tag: "v1".to_string(),
                creator: nft_owner.clone(),
                vp_code,
                keys: vec![],
                opt_keys: vec![],
                tokens: vec![],
            },
        )
        .unwrap();

        let mut tx_env = tx_host_env::take();
        tx_env.commit_tx_and_block();

        vp_host_env::init_from_tx(nft_address.clone(), tx_env, |_| {
            let creator_key = nft::get_creator_key(&nft_address);
            tx::ctx().write(&creator_key, &another_address).unwrap();
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = vp_env.get_verifiers();
        vp_host_env::set(vp_env);

        assert!(
            !validate_tx(&CTX, tx_data, nft_address, keys_changed, verifiers)
                .unwrap()
        );
    }
}
