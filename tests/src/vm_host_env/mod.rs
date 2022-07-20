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

pub mod ibc;
pub mod tx;
pub mod vp;

#[cfg(test)]
mod tests {

    use std::panic;

    use anoma::ibc::tx_msg::Msg;
    use anoma::ledger::ibc::handler::IbcActions;
    use anoma::ledger::ibc::vp::{
        get_dummy_header as tm_dummy_header, Error as IbcError,
    };
    use anoma::ledger::tx_env::TxEnv;
    use anoma::proto::{SignedTxData, Tx};
    use anoma::tendermint_proto::Protobuf;
    use anoma::types::key::*;
    use anoma::types::storage::{self, BlockHash, BlockHeight, Key, KeySeg};
    use anoma::types::time::DateTimeUtc;
    use anoma::types::token::{self, Amount};
    use anoma::types::{address, key};
    use anoma_tx_prelude::{BorshDeserialize, BorshSerialize};
    use anoma_vp_prelude::VpEnv;
    use itertools::Itertools;
    use prost::Message;
    use test_log::test;

    use super::{ibc, tx, vp};
    use crate::tx::{tx_host_env, TestTxEnv};
    use crate::vp::{vp_host_env, TestVpEnv};

    // paths to the WASMs used for tests
    const VP_ALWAYS_TRUE_WASM: &str = "../wasm_for_tests/vp_always_true.wasm";
    const VP_ALWAYS_FALSE_WASM: &str = "../wasm_for_tests/vp_always_false.wasm";

    #[test]
    fn test_tx_read_write() {
        // The environment must be initialized first
        tx_host_env::init();

        let key = storage::Key::parse("key").unwrap();
        let read_value: Option<String> = tx::ctx().read(&key).unwrap();
        assert_eq!(
            None, read_value,
            "Trying to read a key that doesn't exists shouldn't find any value"
        );

        // Write some value
        let value = "test".repeat(4);
        tx::ctx().write(&key, value.clone()).unwrap();

        let read_value: Option<String> = tx::ctx().read(&key).unwrap();
        assert_eq!(
            Some(value),
            read_value,
            "After a value has been written, we should get back the same \
             value when we read it"
        );

        let value = vec![1_u8; 1000];
        tx::ctx().write(&key, value.clone()).unwrap();
        let read_value: Option<Vec<u8>> = tx::ctx().read(&key).unwrap();
        assert_eq!(
            Some(value),
            read_value,
            "Writing to an existing key should override the previous value"
        );
    }

    #[test]
    fn test_tx_has_key() {
        // The environment must be initialized first
        tx_host_env::init();

        let key = storage::Key::parse("key").unwrap();
        assert!(
            !tx::ctx().has_key(&key).unwrap(),
            "Before a key-value is written, its key shouldn't be found"
        );

        // Write some value
        let value = "test".to_string();
        tx::ctx().write(&key, value).unwrap();

        assert!(
            tx::ctx().has_key(&key).unwrap(),
            "After a key-value has been written, its key should be found"
        );
    }

    #[test]
    fn test_tx_delete() {
        // The environment must be initialized first
        let mut env = TestTxEnv::default();
        let test_account = address::testing::established_address_1();
        env.spawn_accounts([&test_account]);
        tx_host_env::set(env);

        // Trying to delete a key that doesn't exists should be a no-op
        let key = storage::Key::parse("key").unwrap();
        tx::ctx().delete(&key).unwrap();

        let value = "test".to_string();
        tx::ctx().write(&key, value).unwrap();
        assert!(
            tx::ctx().has_key(&key).unwrap(),
            "After a key-value has been written, its key should be found"
        );

        // Then delete it
        tx::ctx().delete(&key).unwrap();

        assert!(
            !tx::ctx().has_key(&key).unwrap(),
            "After a key has been deleted, its key shouldn't be found"
        );

        // Trying to delete a validity predicate should fail
        let key = storage::Key::validity_predicate(&test_account);
        assert!(
            panic::catch_unwind(|| { tx::ctx().delete(&key).unwrap() })
                .err()
                .map(|a| a.downcast_ref::<String>().cloned().unwrap())
                .unwrap()
                .contains("CannotDeleteVp")
        );
    }

    #[test]
    fn test_tx_iter_prefix() {
        // The environment must be initialized first
        tx_host_env::init();

        let empty_key = storage::Key::parse("empty").unwrap();
        let mut iter = tx::ctx().iter_prefix(&empty_key).unwrap();
        assert!(
            tx::ctx().iter_next(&mut iter).unwrap().is_none(),
            "Trying to iter a prefix that doesn't have any matching keys \
             should yield an empty iterator."
        );

        // Write some values directly into the storage first
        let prefix = Key::parse("prefix").unwrap();
        tx_host_env::with(|env| {
            for i in 0..10_i32 {
                let key = prefix.join(&Key::parse(i.to_string()).unwrap());
                let value = i.try_to_vec().unwrap();
                env.storage.write(&key, value).unwrap();
            }
            env.storage.commit().unwrap();
        });

        // Then try to iterate over their prefix
        let iter = tx::ctx().iter_prefix(&prefix).unwrap();
        let iter = itertools::unfold(iter, |iter| {
            if let Ok(Some((key, value))) = tx::ctx().iter_next(iter) {
                let decoded_value = i32::try_from_slice(&value[..]).unwrap();
                return Some((key, decoded_value));
            }
            None
        });
        let expected = (0..10).map(|i| (format!("{}/{}", prefix, i), i));
        itertools::assert_equal(iter.sorted(), expected.sorted());
    }

    #[test]
    fn test_tx_insert_verifier() {
        // The environment must be initialized first
        tx_host_env::init();

        assert!(
            tx_host_env::with(|env| env.verifiers.is_empty()),
            "pre-condition"
        );
        let verifier = address::testing::established_address_1();
        tx::ctx().insert_verifier(&verifier).unwrap();
        assert!(
            tx_host_env::with(|env| env.verifiers.contains(&verifier)),
            "The verifier should have been inserted"
        );
        assert_eq!(
            tx_host_env::with(|env| env.verifiers.len()),
            1,
            "There should be only one verifier inserted"
        );
    }

    #[test]
    #[should_panic]
    fn test_tx_init_account_with_invalid_vp() {
        // The environment must be initialized first
        tx_host_env::init();

        let code = vec![];
        tx::ctx().init_account(code).unwrap();
    }

    #[test]
    fn test_tx_init_account_with_valid_vp() {
        // The environment must be initialized first
        tx_host_env::init();

        let code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");
        tx::ctx().init_account(code).unwrap();
    }

    #[test]
    fn test_tx_get_metadata() {
        // The environment must be initialized first
        tx_host_env::init();

        assert_eq!(
            tx::ctx().get_chain_id().unwrap(),
            tx_host_env::with(|env| env.storage.get_chain_id().0)
        );
        assert_eq!(
            tx::ctx().get_block_height().unwrap(),
            tx_host_env::with(|env| env.storage.get_block_height().0)
        );
        assert_eq!(
            tx::ctx().get_block_hash().unwrap(),
            tx_host_env::with(|env| env.storage.get_block_hash().0)
        );
        assert_eq!(
            tx::ctx().get_block_epoch().unwrap(),
            tx_host_env::with(|env| env.storage.get_current_epoch().0)
        );
    }

    /// An example how to write a VP host environment integration test
    #[test]
    fn test_vp_host_env() {
        // The environment must be initialized first
        vp_host_env::init();

        // We can add some data to the environment
        let key_raw = "key";
        let key = Key::parse(key_raw).unwrap();
        let value = "test".to_string();
        let value_raw = value.try_to_vec().unwrap();
        vp_host_env::with(|env| {
            env.write_log.write(&key, value_raw.clone()).unwrap()
        });

        let read_pre_value: Option<String> = vp::CTX.read_pre(&key).unwrap();
        assert_eq!(None, read_pre_value);
        let read_post_value: Option<String> = vp::CTX.read_post(&key).unwrap();
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
        let new_key = addr_key.join(&Key::parse("new_key").unwrap());
        let new_value = "vp".repeat(4);

        // Initialize the VP environment via a transaction
        vp_host_env::init_from_tx(addr, tx_env, |_addr| {
            // Override the existing key
            tx::ctx().write(&existing_key, &override_value).unwrap();

            // Write the new key-value
            tx::ctx().write(&new_key, new_value.clone()).unwrap();
        });

        assert!(
            vp::CTX.has_key_pre(&existing_key).unwrap(),
            "The existing key before transaction should be found"
        );
        let pre_existing_value: Option<Vec<u8>> =
            vp::CTX.read_pre(&existing_key).unwrap();
        assert_eq!(
            Some(existing_value),
            pre_existing_value,
            "The existing value read from state before transaction should be \
             unchanged"
        );

        assert!(
            !vp::CTX.has_key_pre(&new_key).unwrap(),
            "The new key before transaction shouldn't be found"
        );
        let pre_new_value: Option<Vec<u8>> =
            vp::CTX.read_pre(&new_key).unwrap();
        assert_eq!(
            None, pre_new_value,
            "The new value read from state before transaction shouldn't yet \
             exist"
        );

        assert!(
            vp::CTX.has_key_post(&existing_key).unwrap(),
            "The existing key after transaction should still be found"
        );
        let post_existing_value: Option<String> =
            vp::CTX.read_post(&existing_key).unwrap();
        assert_eq!(
            Some(override_value),
            post_existing_value,
            "The existing value read from state after transaction should be \
             overridden"
        );

        assert!(
            vp::CTX.has_key_post(&new_key).unwrap(),
            "The new key after transaction should be found"
        );
        let post_new_value: Option<String> =
            vp::CTX.read_post(&new_key).unwrap();
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
        let new_key = prefix.join(&Key::parse(11.to_string()).unwrap());

        // Initialize the VP environment via a transaction
        vp_host_env::init_from_tx(addr, tx_env, |_addr| {
            // Override one of the existing keys
            tx::ctx().write(&existing_key, 100_i32).unwrap();

            // Write the new key-value under the same prefix
            tx::ctx().write(&new_key, 11_i32).unwrap();
        });

        let iter_pre = vp::CTX.iter_prefix(&prefix).unwrap();
        let iter_pre = itertools::unfold(iter_pre, |iter| {
            if let Ok(Some((key, value))) = vp::CTX.iter_pre_next(iter) {
                if let Ok(decoded_value) = i32::try_from_slice(&value[..]) {
                    return Some((key, decoded_value));
                }
            }
            None
        });
        let expected_pre = (0..10).map(|i| (format!("{}/{}", prefix, i), i));
        itertools::assert_equal(iter_pre.sorted(), expected_pre.sorted());

        let iter_post = vp::CTX.iter_prefix(&prefix).unwrap();
        let iter_post = itertools::unfold(iter_post, |iter| {
            if let Ok(Some((key, value))) = vp::CTX.iter_post_next(iter) {
                let decoded_value = i32::try_from_slice(&value[..]).unwrap();
                return Some((key, decoded_value));
            }
            None
        });
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
        let pk_key = key::pk_key(&addr);
        let keypair = key::testing::keypair_1();
        let pk = keypair.ref_to();
        env.storage
            .write(&pk_key, pk.try_to_vec().unwrap())
            .unwrap();
        // Initialize the environment
        vp_host_env::set(env);

        // Use some arbitrary bytes for tx code
        let code = vec![4, 3, 2, 1, 0];
        for data in &[
            // Tx with some arbitrary data
            Some(vec![1, 2, 3, 4].repeat(10)),
            // Tx without any data
            None,
        ] {
            let signed_tx_data = vp_host_env::with(|env| {
                env.tx = Tx::new(code.clone(), data.clone()).sign(&keypair);
                let tx_data = env.tx.data.as_ref().expect("data should exist");

                SignedTxData::try_from_slice(&tx_data[..])
                    .expect("decoding signed data we just signed")
            });
            assert_eq!(&signed_tx_data.data, data);
            assert!(
                vp::CTX
                    .verify_tx_signature(&pk, &signed_tx_data.sig)
                    .unwrap()
            );

            let other_keypair = key::testing::keypair_2();
            assert!(
                !vp::CTX
                    .verify_tx_signature(
                        &other_keypair.ref_to(),
                        &signed_tx_data.sig
                    )
                    .unwrap()
            );
        }
    }

    #[test]
    fn test_vp_get_metadata() {
        // The environment must be initialized first
        vp_host_env::init();

        assert_eq!(
            vp::CTX.get_chain_id().unwrap(),
            vp_host_env::with(|env| env.storage.get_chain_id().0)
        );
        assert_eq!(
            vp::CTX.get_block_height().unwrap(),
            vp_host_env::with(|env| env.storage.get_block_height().0)
        );
        assert_eq!(
            vp::CTX.get_block_hash().unwrap(),
            vp_host_env::with(|env| env.storage.get_block_hash().0)
        );
        assert_eq!(
            vp::CTX.get_block_epoch().unwrap(),
            vp_host_env::with(|env| env.storage.get_current_epoch().0)
        );
    }

    #[test]
    fn test_vp_eval() {
        // The environment must be initialized first
        vp_host_env::init();

        // evaluating without any code should fail
        let empty_code = vec![];
        let input_data = vec![];
        let result = vp::CTX.eval(empty_code, input_data).unwrap();
        assert!(!result);

        // evaluating the VP template which always returns `true` should pass
        let code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");
        let input_data = vec![];
        let result = vp::CTX.eval(code, input_data).unwrap();
        assert!(result);

        // evaluating the VP template which always returns `false` shouldn't
        // pass
        let code =
            std::fs::read(VP_ALWAYS_FALSE_WASM).expect("cannot load wasm");
        let input_data = vec![];
        let result = vp::CTX.eval(code, input_data).unwrap();
        assert!(!result);
    }

    #[test]
    fn test_ibc_client() {
        // The environment must be initialized first
        tx_host_env::init();

        ibc::init_storage();

        // Start an invalid transaction
        let msg = ibc::msg_create_client();
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // get and increment the connection counter
        let counter_key = ibc::client_counter_key();
        let counter = tx::ctx()
            .get_and_inc_counter(&counter_key)
            .expect("getting the counter failed");
        let client_id = ibc::client_id(msg.client_state.client_type(), counter)
            .expect("invalid client ID");
        // only insert a client type
        let client_type_key = ibc::client_type_key(&client_id);
        tx::ctx()
            .write(
                &client_type_key,
                msg.client_state.client_type().as_str().as_bytes(),
            )
            .unwrap();

        // Check should fail due to no client state
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(matches!(
            result.expect_err("validation succeeded unexpectedly"),
            IbcError::ClientError(_),
        ));
        // drop the transaction
        env.write_log.drop_tx();

        // Start a transaction to create a new client
        tx_host_env::set(env);
        let msg = ibc::msg_create_client();
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());

        // create a client with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("creating a client failed");

        // Check
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));

        // Commit
        env.commit_tx_and_block();
        // update the block height for the following client update
        env.storage
            .begin_block(BlockHash::default(), BlockHeight(2))
            .unwrap();
        env.storage.set_header(tm_dummy_header()).unwrap();

        // Start an invalid transaction
        tx_host_env::set(env);
        let msg = ibc::msg_update_client(client_id);
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // get and update the client without a header
        let client_id = msg.client_id.clone();
        // update the client with the same state
        let old_data = ibc::msg_create_client();
        let same_client_state = old_data.client_state.clone();
        let height = same_client_state.latest_height();
        let same_consensus_state = old_data.consensus_state;
        let client_state_key = ibc::client_state_key(&client_id);
        tx::ctx()
            .write_bytes(
                &client_state_key,
                same_client_state.encode_vec().unwrap(),
            )
            .unwrap();
        let consensus_state_key = ibc::consensus_state_key(&client_id, height);
        tx::ctx()
            .write(
                &consensus_state_key,
                same_consensus_state.encode_vec().unwrap(),
            )
            .unwrap();
        let event = ibc::make_update_client_event(&client_id, &msg);
        TxEnv::emit_ibc_event(tx::ctx(), &event.try_into().unwrap()).unwrap();

        // Check should fail due to the invalid updating
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(matches!(
            result.expect_err("validation succeeded unexpectedly"),
            IbcError::ClientError(_),
        ));
        // drop the transaction
        env.write_log.drop_tx();

        // Start a transaction to update the client
        tx_host_env::set(env);
        let msg = ibc::msg_update_client(client_id.clone());
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // update the client with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("updating the client failed");

        // Check
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));

        // Commit
        env.commit_tx_and_block();
        // update the block height for the following client update
        env.storage
            .begin_block(BlockHash::default(), BlockHeight(3))
            .unwrap();
        env.storage.set_header(tm_dummy_header()).unwrap();

        // Start a transaction to upgrade the client
        tx_host_env::set(env);
        let msg = ibc::msg_upgrade_client(client_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // upgrade the client with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("upgrading the client failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_connection_init_and_open() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        ibc::init_storage();
        let (client_id, client_state, writes) = ibc::prepare_client();
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // Start an invalid transaction
        let msg = ibc::msg_connection_open_init(client_id.clone());
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // get and increment the connection counter
        let counter_key = ibc::connection_counter_key();
        let counter = tx::ctx()
            .get_and_inc_counter(&counter_key)
            .expect("getting the counter failed");
        // insert a new opened connection
        let conn_id = ibc::connection_id(counter);
        let conn_key = ibc::connection_key(&conn_id);
        let mut connection = ibc::init_connection(&msg);
        ibc::open_connection(&mut connection);
        tx::ctx()
            .write_bytes(&conn_key, connection.encode_vec().unwrap())
            .unwrap();
        let event = ibc::make_open_init_connection_event(&conn_id, &msg);
        TxEnv::emit_ibc_event(tx::ctx(), &event.try_into().unwrap()).unwrap();

        // Check should fail due to directly opening a connection
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(matches!(
            result.expect_err("validation succeeded unexpectedly"),
            IbcError::ConnectionError(_),
        ));
        // drop the transaction
        env.write_log.drop_tx();

        // Start a transaction for ConnectionOpenInit
        tx_host_env::set(env);
        let msg = ibc::msg_connection_open_init(client_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // init a connection with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("creating a connection failed");

        // Check
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));

        // Commit
        env.commit_tx_and_block();
        // set a block header again
        env.storage.set_header(tm_dummy_header()).unwrap();

        // Start the next transaction for ConnectionOpenAck
        tx_host_env::set(env);
        let msg = ibc::msg_connection_open_ack(conn_id, client_state);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // open the connection with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("opening the connection failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_connection_try_and_open() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        ibc::init_storage();

        let mut env = tx_host_env::take();
        let (client_id, client_state, writes) = ibc::prepare_client();
        writes.into_iter().for_each(|(key, val)| {
            env.storage.write(&key, &val).expect("write error");
        });

        // Start a transaction for ConnectionOpenTry
        tx_host_env::set(env);
        let msg = ibc::msg_connection_open_try(client_id, client_state);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // open try a connection with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("creating a connection failed");

        // Check
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));

        // Commit
        env.commit_tx_and_block();
        // set a block header again
        env.storage.set_header(tm_dummy_header()).unwrap();

        // Start the next transaction for ConnectionOpenConfirm
        tx_host_env::set(env);
        let conn_id = ibc::connection_id(0);
        let msg = ibc::msg_connection_open_confirm(conn_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // open the connection with the mssage
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("opening the connection failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_channel_init_and_open() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // Start an invalid transaction
        let port_id = ibc::port_id("test_port").expect("invalid port ID");
        let msg = ibc::msg_channel_open_init(port_id.clone(), conn_id.clone());
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // not bind a port
        // get and increment the channel counter
        let counter_key = ibc::channel_counter_key();
        let counter = tx::ctx()
            .get_and_inc_counter(&counter_key)
            .expect("getting the counter failed");
        // channel
        let channel_id = ibc::channel_id(counter);
        let port_channel_id = ibc::port_channel_id(port_id, channel_id.clone());
        let channel_key = ibc::channel_key(&port_channel_id);
        tx::ctx()
            .write_bytes(&channel_key, msg.channel.encode_vec().unwrap())
            .unwrap();
        let event = ibc::make_open_init_channel_event(&channel_id, &msg);
        TxEnv::emit_ibc_event(tx::ctx(), &event.try_into().unwrap()).unwrap();

        // Check should fail due to no port binding
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(matches!(
            result.expect_err("validation succeeded unexpectedly"),
            IbcError::ChannelError(_),
        ));
        // drop the transaction
        env.write_log.drop_tx();

        // Start an invalid transaction
        tx_host_env::set(env);
        let port_id = ibc::port_id("test_port").expect("invalid port ID");
        let msg = ibc::msg_channel_open_init(port_id.clone(), conn_id.clone());
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // bind a port
        tx::ctx()
            .bind_port(&port_id)
            .expect("binding the port failed");
        // get and increment the channel counter
        let counter_key = ibc::channel_counter_key();
        let counter = tx::ctx()
            .get_and_inc_counter(&counter_key)
            .expect("getting the counter failed");
        // insert a opened channel
        let channel_id = ibc::channel_id(counter);
        let port_channel_id = ibc::port_channel_id(port_id, channel_id.clone());
        let channel_key = ibc::channel_key(&port_channel_id);
        let mut channel = msg.channel.clone();
        ibc::open_channel(&mut channel);
        tx::ctx()
            .write_bytes(&channel_key, channel.encode_vec().unwrap())
            .unwrap();
        let event = ibc::make_open_init_channel_event(&channel_id, &msg);
        TxEnv::emit_ibc_event(tx::ctx(), &event.try_into().unwrap()).unwrap();

        // Check should fail due to directly opening a channel

        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(matches!(
            result.expect_err("validation succeeded unexpectedly"),
            IbcError::ChannelError(_),
        ));
        // drop the transaction
        env.write_log.drop_tx();

        // Start a transaction for ChannelOpenInit
        tx_host_env::set(env);
        let port_id = ibc::port_id("test_port").expect("invalid port ID");
        let msg = ibc::msg_channel_open_init(port_id.clone(), conn_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // init a channel with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("creating a channel failed");

        // Check
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));

        // Commit
        env.commit_tx_and_block();
        tx_host_env::set(env);

        // Start the next transaction for ChannelOpenAck
        let msg = ibc::msg_channel_open_ack(port_id, channel_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // open the channle with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("opening the channel failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_channel_try_and_open() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // Start a transaction for ChannelOpenTry
        let port_id = ibc::port_id("test_port").expect("invalid port ID");
        let msg = ibc::msg_channel_open_try(port_id.clone(), conn_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // try open a channel with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("creating a channel failed");

        // Check
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));

        // Commit
        env.commit_tx_and_block();

        // Start the next transaction for ChannelOpenConfirm
        tx_host_env::set(env);
        let channel_id = ibc::channel_id(0);
        let msg = ibc::msg_channel_open_confirm(port_id, channel_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // open a channel with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("opening the channel failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_channel_close_init() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, true);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // Start a transaction to close the channel
        let msg = ibc::msg_channel_close_init(port_id, channel_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // close the channel with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("closing the channel failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_channel_close_confirm() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, true);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // Start a transaction to close the channel
        let msg = ibc::msg_channel_close_confirm(port_id, channel_id);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());

        // close the channel with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("closing the channel failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_send_token() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        let (token, sender) = ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, false);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // Start a transaction to send a packet
        let msg =
            ibc::msg_transfer(port_id, channel_id, token.to_string(), &sender);
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // send the token and a packet with the data
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("sending a packet failed");

        // Check
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
        // Check if the token was escrowed
        let escrow = address::Address::Internal(
            address::InternalAddress::ibc_escrow_address(
                msg.source_port.to_string(),
                msg.source_channel.to_string(),
            ),
        );
        let token_vp_result =
            ibc::validate_token_vp_from_tx(&env, &tx, &escrow);
        assert!(token_vp_result.expect("token validation failed unexpectedly"));

        // Commit
        env.commit_tx_and_block();

        // Start the next transaction for receiving an ack
        tx_host_env::set(env);
        let counterparty = ibc::dummy_channel_counterparty();
        let packet =
            ibc::packet_from_message(&msg, ibc::sequence(1), &counterparty);
        let msg = ibc::msg_packet_ack(packet);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // ack the packet with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("the packet ack failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_burn_token() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        let (token, sender) = ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, false);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // Start a transaction to send a packet
        // Set this chain is the sink zone
        let token = format!("{}/{}/{}", port_id, channel_id, token);
        let msg = ibc::msg_transfer(port_id, channel_id, token, &sender);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // send the token and a packet with the data
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("sending a packet failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
        // Check if the token was burned
        let burn =
            address::Address::Internal(address::InternalAddress::IbcBurn);
        let result = ibc::validate_token_vp_from_tx(&env, &tx, &burn);
        assert!(result.expect("token validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_receive_token() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        let (token, receiver) = ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, false);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // packet
        let packet = ibc::received_packet(
            port_id,
            channel_id,
            ibc::sequence(1),
            token.to_string(),
            &receiver,
        );

        // Start a transaction to receive a packet
        let msg = ibc::msg_packet_recv(packet);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // receive a packet with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("receiving a packet failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
        // Check if the token was minted
        let mint =
            address::Address::Internal(address::InternalAddress::IbcMint);
        let result = ibc::validate_token_vp_from_tx(&env, &tx, &mint);
        assert!(result.expect("token validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_unescrow_token() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        let (token, receiver) = ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, false);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });
        // escrow in advance
        let counterparty = ibc::dummy_channel_counterparty();
        let escrow = address::Address::Internal(
            address::InternalAddress::ibc_escrow_address(
                port_id.to_string(),
                channel_id.to_string(),
            ),
        );
        let key = token::balance_key(&token, &escrow);
        let val = Amount::from(1_000_000_000u64).try_to_vec().unwrap();
        tx_host_env::with(|env| {
            env.storage.write(&key, &val).expect("write error");
        });

        // Set this chain as the source zone
        let token = format!(
            "{}/{}/{}",
            counterparty.port_id().clone(),
            counterparty.channel_id().unwrap().clone(),
            token
        );
        // packet
        let packet = ibc::received_packet(
            port_id,
            channel_id,
            ibc::sequence(1),
            token,
            &receiver,
        );

        // Start a transaction to receive a packet
        let msg = ibc::msg_packet_recv(packet);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // receive a packet with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("receiving a packet failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
        // Check if the token was unescrowed
        let result = ibc::validate_token_vp_from_tx(&env, &tx, &escrow);
        assert!(result.expect("token validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_send_packet_unordered() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        let (token, sender) = ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, false);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // Start a transaction to send a packet
        let msg =
            ibc::msg_transfer(port_id, channel_id, token.to_string(), &sender);
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // send a packet with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("sending a packet failed");

        // the transaction does something before senging a packet

        // Check
        let mut env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));

        // Commit
        env.commit_tx_and_block();

        // Start the next transaction for receiving an ack
        tx_host_env::set(env);
        let counterparty = ibc::dummy_channel_counterparty();
        let packet =
            ibc::packet_from_message(&msg, ibc::sequence(1), &counterparty);
        let msg = ibc::msg_packet_ack(packet);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // ack the packet with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("the packet ack failed");

        // the transaction does something after the ack

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_receive_packet_unordered() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        let (token, receiver) = ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, false);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            });
        });

        // packet (sequence number isn't checked for the unordered channel)
        let packet = ibc::received_packet(
            port_id,
            channel_id,
            ibc::sequence(100),
            token.to_string(),
            &receiver,
        );

        // Start a transaction to receive a packet
        let msg = ibc::msg_packet_recv(packet);
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());
        // receive a packet with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("receiving a packet failed");

        // the transaction does something according to the packet

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_packet_timeout() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        let (token, sender) = ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, true);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            })
        });

        // Start a transaction to send a packet
        let mut msg =
            ibc::msg_transfer(port_id, channel_id, token.to_string(), &sender);
        ibc::set_timeout_height(&mut msg);
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        // send a packet with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("sending apacket failed");

        // Commit
        tx_host_env::commit_tx_and_block();

        // Start a transaction to notify the timeout
        let counterparty = ibc::dummy_channel_counterparty();
        let packet =
            ibc::packet_from_message(&msg, ibc::sequence(1), &counterparty);
        let msg = ibc::msg_timeout(packet.clone(), ibc::sequence(1));
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());

        // close the channel with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("closing the channel failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
        // Check if the token was refunded
        let escrow = address::Address::Internal(
            address::InternalAddress::ibc_escrow_address(
                packet.source_port.to_string(),
                packet.source_channel.to_string(),
            ),
        );
        let result = ibc::validate_token_vp_from_tx(&env, &tx, &escrow);
        assert!(result.expect("token validation failed unexpectedly"));
    }

    #[test]
    fn test_ibc_timeout_on_close() {
        // The environment must be initialized first
        tx_host_env::init();

        // Set the initial state before starting transactions
        let (token, sender) = ibc::init_storage();
        let (client_id, _client_state, mut writes) = ibc::prepare_client();
        let (conn_id, conn_writes) = ibc::prepare_opened_connection(&client_id);
        writes.extend(conn_writes);
        let (port_id, channel_id, channel_writes) =
            ibc::prepare_opened_channel(&conn_id, true);
        writes.extend(channel_writes);
        writes.into_iter().for_each(|(key, val)| {
            tx_host_env::with(|env| {
                env.storage.write(&key, &val).expect("write error");
            })
        });

        // Start a transaction to send a packet
        let msg =
            ibc::msg_transfer(port_id, channel_id, token.to_string(), &sender);
        let mut tx_data = vec![];
        msg.clone()
            .to_any()
            .encode(&mut tx_data)
            .expect("encoding failed");
        // send a packet with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("sending a packet failed");

        // Commit
        tx_host_env::commit_tx_and_block();

        // Start a transaction to notify the timing-out on closed
        let counterparty = ibc::dummy_channel_counterparty();
        let packet =
            ibc::packet_from_message(&msg, ibc::sequence(1), &counterparty);
        let msg = ibc::msg_timeout_on_close(packet.clone(), ibc::sequence(1));
        let mut tx_data = vec![];
        msg.to_any().encode(&mut tx_data).expect("encoding failed");
        let tx = Tx {
            code: vec![],
            data: Some(tx_data.clone()),
            timestamp: DateTimeUtc::now(),
        }
        .sign(&key::testing::keypair_1());

        // close the channel with the message
        tx::ctx()
            .dispatch_ibc_action(&tx_data)
            .expect("closing the channel failed");

        // Check
        let env = tx_host_env::take();
        let result = ibc::validate_ibc_vp_from_tx(&env, &tx);
        assert!(result.expect("validation failed unexpectedly"));
        // Check if the token was refunded
        let escrow = address::Address::Internal(
            address::InternalAddress::ibc_escrow_address(
                packet.source_port.to_string(),
                packet.source_channel.to_string(),
            ),
        );
        let result = ibc::validate_token_vp_from_tx(&env, &tx, &escrow);
        assert!(result.expect("token validation failed unexpectedly"));
    }
}
