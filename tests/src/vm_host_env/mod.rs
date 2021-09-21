//! VM host environment integration tests.
//!
//! You can enable logging with the `RUST_LOG` environment variable, e.g.:
//!
//! `RUST_LOG=debug cargo test`.
//!
//! Because the test runner captures the standard output from the test, this
//! will only print logging from failed tests. To avoid that, use the
//! `--nocapture` argument. Also, because by default the tests run in parallel,
//! it is better to select a single test, e.g.:
//!
//! `RUST_LOG=debug cargo test test_tx_read_write -- --nocapture`
pub mod tx;
pub mod vp;

#[cfg(test)]
mod tests {
    use anoma::proto::Tx;
    use anoma::types::key::ed25519::SignedTxData;
    use anoma::types::storage::{Key, KeySeg};
    use anoma::types::{address, key};
    use anoma_vm_env::tx_prelude::{
        BorshDeserialize, BorshSerialize, KeyValIterator,
    };
    use anoma_vm_env::vp_prelude::{PostKeyValIterator, PreKeyValIterator};
    use itertools::Itertools;
    use test_env_log::test;

    use super::tx::*;
    use super::vp::*;

    // paths to the WASMs used for tests
    const VP_ALWAYS_TRUE_WASM: &str = "../wasm_for_tests/vp_always_true.wasm";
    const VP_ALWAYS_FALSE_WASM: &str = "../wasm_for_tests/vp_always_false.wasm";

    #[test]
    fn test_tx_read_write() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        let key = "key";
        let read_value: Option<String> = tx_host_env::read(key);
        assert_eq!(
            None, read_value,
            "Trying to read a key that doesn't exists shouldn't find any value"
        );

        // Write some value
        let value = "test".repeat(4);
        tx_host_env::write(key, value.clone());

        let read_value: Option<String> = tx_host_env::read(key);
        assert_eq!(
            Some(value),
            read_value,
            "After a value has been written, we should get back the same \
             value when we read it"
        );

        let value = vec![1_u8; 1000];
        tx_host_env::write(key, value.clone());
        let read_value: Option<Vec<u8>> = tx_host_env::read(key);
        assert_eq!(
            Some(value),
            read_value,
            "Writing to an existing key should override the previous value"
        );
    }

    #[test]
    fn test_tx_has_key() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        let key = "key";
        assert!(
            !tx_host_env::has_key(key),
            "Before a key-value is written, its key shouldn't be found"
        );

        // Write some value
        let value = "test".to_string();
        tx_host_env::write(key, value);

        assert!(
            tx_host_env::has_key(key),
            "After a key-value has been written, its key should be found"
        );
    }

    #[test]
    fn test_tx_delete() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        // Trying to delete a key that doesn't exists should be a no-op
        let key = "key";
        tx_host_env::delete(key);

        let value = "test".to_string();
        tx_host_env::write(key, value);
        assert!(
            tx_host_env::has_key(key),
            "After a key-value has been written, its key should be found"
        );

        // Then delete it
        tx_host_env::delete(key);

        assert!(
            !tx_host_env::has_key(key),
            "After a key has been deleted, its key shouldn't be found"
        );
    }

    #[test]
    fn test_tx_iter_prefix() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        let iter: KeyValIterator<Vec<u8>> = tx_host_env::iter_prefix("empty");
        assert_eq!(
            iter.count(),
            0,
            "Trying to iter a prefix that doesn't have any matching keys \
             should yield an empty iterator."
        );

        // Write some values directly into the storage first
        let prefix = Key::parse("prefix").unwrap();
        for i in 0..10_i32 {
            let key = prefix.join(&Key::parse(i.to_string()).unwrap());
            let value = i.try_to_vec().unwrap();
            env.storage.write(&key, value).unwrap();
        }
        env.storage.commit().unwrap();

        // Then try to iterate over their prefix
        let iter: KeyValIterator<i32> =
            tx_host_env::iter_prefix(prefix.to_string());
        let expected = (0..10).map(|i| (format!("{}/{}", prefix, i), i));
        itertools::assert_equal(iter.sorted(), expected.sorted());
    }

    #[test]
    fn test_tx_insert_verifier() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        assert!(env.verifiers.is_empty(), "pre-condition");
        let verifier = address::testing::established_address_1();
        tx_host_env::insert_verifier(&verifier);
        assert!(
            env.verifiers.contains(&verifier),
            "The verifier should have been inserted"
        );
        assert_eq!(
            env.verifiers.len(),
            1,
            "There should be only one verifier inserted"
        );
    }

    #[test]
    #[should_panic]
    fn test_tx_init_account_with_invalid_vp() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        let code = vec![];
        tx_host_env::init_account(code);
    }

    #[test]
    fn test_tx_init_account_with_valid_vp() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        let code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");
        tx_host_env::init_account(code);
    }

    #[test]
    fn test_tx_get_metadata() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        init_tx_env(&mut env);

        assert_eq!(tx_host_env::get_chain_id(), env.storage.get_chain_id().0);
        assert_eq!(
            tx_host_env::get_block_height(),
            env.storage.get_block_height().0
        );
        assert_eq!(
            tx_host_env::get_block_hash(),
            env.storage.get_block_hash().0
        );
        assert_eq!(
            tx_host_env::get_block_epoch(),
            env.storage.get_current_epoch().0
        );
    }

    /// An example how to write a VP host environment integration test
    #[test]
    fn test_vp_host_env() {
        // The environment must be initialized first
        let mut env = TestVpEnv::default();
        init_vp_env(&mut env);

        // We can add some data to the environment
        let key_raw = "key";
        let key = Key::parse(key_raw.to_string()).unwrap();
        let value = "test".to_string();
        let value_raw = value.try_to_vec().unwrap();
        env.write_log.write(&key, value_raw).unwrap();

        let read_pre_value: Option<String> = vp_host_env::read_pre(key_raw);
        assert_eq!(None, read_pre_value);
        let read_post_value: Option<String> = vp_host_env::read_post(key_raw);
        assert_eq!(Some(value), read_post_value);
    }

    #[test]
    fn test_vp_read_and_has_key() {
        let mut tx_env = TestTxEnv::default();

        let addr = address::testing::established_address_1();
        let addr_key = Key::from(addr.to_db_key());

        // Write some value to storage
        let existing_key =
            addr_key.join(&Key::parse("existing_key_raw").unwrap());
        let existing_key_raw = existing_key.to_string();
        let existing_value = vec![2_u8; 1000];
        // Values written to storage have to be encoded with Borsh
        let existing_value_encoded = existing_value.try_to_vec().unwrap();
        tx_env
            .storage
            .write(&existing_key, existing_value_encoded)
            .unwrap();

        // In a transaction, write override the existing key's value and add
        // another key-value
        let override_value = "override".to_string();
        let new_key =
            addr_key.join(&Key::parse("new_key").unwrap()).to_string();
        let new_value = "vp".repeat(4);

        // Initialize the VP environment via a transaction
        // The `_vp_env` MUST NOT be dropped until the end of the test
        let _vp_env = init_vp_env_from_tx(addr, tx_env, |_addr| {
            // Override the existing key
            tx_host_env::write(&existing_key_raw, &override_value);

            // Write the new key-value
            tx_host_env::write(&new_key, new_value.clone());
        });

        assert!(
            vp_host_env::has_key_pre(&existing_key_raw),
            "The existing key before transaction should be found"
        );
        let pre_existing_value: Option<Vec<u8>> =
            vp_host_env::read_pre(&existing_key_raw);
        assert_eq!(
            Some(existing_value),
            pre_existing_value,
            "The existing value read from state before transaction should be \
             unchanged"
        );

        assert!(
            !vp_host_env::has_key_pre(&new_key),
            "The new key before transaction shouldn't be found"
        );
        let pre_new_value: Option<Vec<u8>> = vp_host_env::read_pre(&new_key);
        assert_eq!(
            None, pre_new_value,
            "The new value read from state before transaction shouldn't yet \
             exist"
        );

        assert!(
            vp_host_env::has_key_post(&existing_key_raw),
            "The existing key after transaction should still be found"
        );
        let post_existing_value: Option<String> =
            vp_host_env::read_post(&existing_key_raw);
        assert_eq!(
            Some(override_value),
            post_existing_value,
            "The existing value read from state after transaction should be \
             overridden"
        );

        assert!(
            vp_host_env::has_key_post(&new_key),
            "The new key after transaction should be found"
        );
        let post_new_value: Option<String> = vp_host_env::read_post(&new_key);
        assert_eq!(
            Some(new_value),
            post_new_value,
            "The new value read from state after transaction should have a \
             value equal to the one written in the transaction"
        );
    }

    #[test]
    fn test_vp_iter_prefix() {
        let mut tx_env = TestTxEnv::default();

        let addr = address::testing::established_address_1();
        let addr_key = Key::from(addr.to_db_key());

        // Write some value to storage
        let prefix = addr_key.join(&Key::parse("prefix").unwrap());
        for i in 0..10_i32 {
            let key = prefix.join(&Key::parse(i.to_string()).unwrap());
            let value = i.try_to_vec().unwrap();
            tx_env.storage.write(&key, value).unwrap();
        }
        tx_env.storage.commit().unwrap();

        // In a transaction, write override the existing key's value and add
        // another key-value
        let existing_key = prefix.join(&Key::parse(5.to_string()).unwrap());
        let existing_key_raw = existing_key.to_string();
        let new_key = prefix.join(&Key::parse(11.to_string()).unwrap());
        let new_key_raw = new_key.to_string();

        // Initialize the VP environment via a transaction
        // The `_vp_env` MUST NOT be dropped until the end of the test
        let _vp_env = init_vp_env_from_tx(addr, tx_env, |_addr| {
            // Override one of the existing keys
            tx_host_env::write(&existing_key_raw, 100_i32);

            // Write the new key-value under the same prefix
            tx_host_env::write(&new_key_raw, 11.try_to_vec().unwrap());
        });

        let iter_pre: PreKeyValIterator<i32> =
            vp_host_env::iter_prefix_pre(prefix.to_string());
        let expected_pre = (0..10).map(|i| (format!("{}/{}", prefix, i), i));
        itertools::assert_equal(iter_pre.sorted(), expected_pre.sorted());

        let iter_post: PostKeyValIterator<i32> =
            vp_host_env::iter_prefix_post(prefix.to_string());
        let expected_post = (0..10).map(|i| {
            let val = if i == 5 { 100 } else { i };
            (format!("{}/{}", prefix, i), val)
        });
        itertools::assert_equal(iter_post.sorted(), expected_post.sorted());
    }

    #[test]
    fn test_vp_verify_tx_signature() {
        let mut env = TestVpEnv::default();

        let addr = address::testing::established_address_1();

        // Write the public key to storage
        let pk_key = key::ed25519::pk_key(&addr);
        let keypair = key::ed25519::testing::keypair_1();
        let pk = keypair.public.clone();
        env.storage
            .write(&pk_key, pk.try_to_vec().unwrap())
            .unwrap();

        // Use some arbitrary bytes for tx code
        let code = vec![4, 3, 2, 1, 0];
        for data in &[
            // Tx with some arbitrary data
            Some(vec![1, 2, 3, 4].repeat(10)),
            // Tx without any data
            None,
        ] {
            env.tx = Tx::new(code.clone(), data.clone()).sign(&keypair);
            // Initialize the environment
            init_vp_env(&mut env);

            let tx_data = env.tx.data.expect("data should exist");
            let signed_tx_data =
                match SignedTxData::try_from_slice(&tx_data[..]) {
                    Ok(data) => data,
                    _ => panic!("decoding failed"),
                };
            assert_eq!(&signed_tx_data.data, data);
            assert!(vp_host_env::verify_tx_signature(&pk, &signed_tx_data.sig));

            let other_keypair = key::ed25519::testing::keypair_2();
            assert!(!vp_host_env::verify_tx_signature(
                &other_keypair.public,
                &signed_tx_data.sig
            ));
        }
    }

    #[test]
    fn test_vp_get_metadata() {
        // The environment must be initialized first
        let mut env = TestVpEnv::default();
        init_vp_env(&mut env);

        assert_eq!(vp_host_env::get_chain_id(), env.storage.get_chain_id().0);
        assert_eq!(
            vp_host_env::get_block_height(),
            env.storage.get_block_height().0
        );
        assert_eq!(
            vp_host_env::get_block_hash(),
            env.storage.get_block_hash().0
        );
        assert_eq!(
            vp_host_env::get_block_epoch(),
            env.storage.get_current_epoch().0
        );
    }

    #[test]
    fn test_vp_eval() {
        // The environment must be initialized first
        let mut env = TestVpEnv::default();
        init_vp_env(&mut env);

        // evaluating without any code should fail
        let empty_code = vec![];
        let input_data = vec![];
        let result = vp_host_env::eval(empty_code, input_data);
        assert!(!result);

        // evaluating the VP template which always returns `true` should pass
        let code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");
        let input_data = vec![];
        let result = vp_host_env::eval(code, input_data);
        assert!(result);

        // evaluating the VP template which always returns `false` shouldn't
        // pass
        let code =
            std::fs::read(VP_ALWAYS_FALSE_WASM).expect("cannot load wasm");
        let input_data = vec![];
        let result = vp_host_env::eval(code, input_data);
        assert!(!result);
    }
}
