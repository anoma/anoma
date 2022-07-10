//! A "faucet" account for testnet.
//!
//! This VP allows anyone to withdraw up to [`MAX_FREE_DEBIT`] tokens without
//! the faucet's signature.
//!
//! Any other storage key changes are allowed only with a valid signature.

use anoma_vp_prelude::{SignedTxData, *};
use once_cell::unsync::Lazy;

/// Allows anyone to withdraw up to 1_000 tokens in a single tx
pub const MAX_FREE_DEBIT: i128 = 1_000_000_000; // in micro units

#[validity_predicate]
fn validate_tx(
    tx_data: Vec<u8>,
    addr: Address,
    keys_changed: BTreeSet<storage::Key>,
    verifiers: BTreeSet<Address>,
) -> bool {
    debug_log!(
        "vp_testnet_faucet called with user addr: {}, key_changed: {:?}, \
         verifiers: {:?}",
        addr,
        keys_changed,
        verifiers
    );

    let signed_tx_data =
        Lazy::new(|| SignedTxData::try_from_slice(&tx_data[..]));

    let valid_sig = Lazy::new(|| match &*signed_tx_data {
        Ok(signed_tx_data) => {
            let pk = key::get(&addr);
            match pk {
                Some(pk) => verify_tx_signature(&pk, &signed_tx_data.sig),
                None => false,
            }
        }
        _ => false,
    });

    if !is_tx_whitelisted() {
        return false;
    }

    for key in keys_changed.iter() {
        let is_valid = if let Some(owner) = token::is_any_token_balance_key(key)
        {
            if owner == &addr {
                let key = key.to_string();
                let pre: token::Amount = read_pre(&key).unwrap_or_default();
                let post: token::Amount = read_post(&key).unwrap_or_default();
                let change = post.change() - pre.change();
                // Debit over `MAX_FREE_DEBIT` has to signed, credit doesn't
                change >= -MAX_FREE_DEBIT || change >= 0 || *valid_sig
            } else {
                // If this is not the owner, allow any change
                true
            }
        } else if let Some(owner) = key.is_validity_predicate() {
            let key = key.to_string();
            let has_post: bool = has_key_post(&key);
            if owner == &addr {
                if has_post {
                    let vp: Vec<u8> = read_bytes_post(&key).unwrap();
                    return *valid_sig && is_vp_whitelisted(&vp);
                } else {
                    return false;
                }
            } else {
                let vp: Vec<u8> = read_bytes_post(&key).unwrap();
                return is_vp_whitelisted(&vp);
            }
        } else {
            // Allow any other key change if authorized by a signature
            *valid_sig
        };
        if !is_valid {
            debug_log!("key {} modification failed vp", key);
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use address::testing::arb_non_internal_address;
    // Use this as `#[test]` annotation to enable logging
    use anoma_tests::log::test;
    use anoma_tests::tx::{tx_host_env, TestTxEnv};
    use anoma_tests::vp::vp_host_env::storage::Key;
    use anoma_tests::vp::*;
    use anoma_vp_prelude::key::RefTo;
    use proptest::prelude::*;
    use storage::testing::arb_account_storage_key_no_vp;

    use super::*;

    const VP_ALWAYS_TRUE_WASM: &str =
        "../../wasm_for_tests/vp_always_true.wasm";

    /// Test that no-op transaction (i.e. no storage modifications) accepted.
    #[test]
    fn test_no_op_transaction() {
        let tx_data: Vec<u8> = vec![];
        let addr: Address = address::testing::established_address_1();
        let keys_changed: BTreeSet<storage::Key> = BTreeSet::default();
        let verifiers: BTreeSet<Address> = BTreeSet::default();

        // The VP env must be initialized before calling `validate_tx`
        vp_host_env::init();

        assert!(validate_tx(tx_data, addr, keys_changed, verifiers));
    }

    /// Test that a credit transfer is accepted.
    #[test]
    fn test_credit_transfer_accepted() {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let source = address::testing::established_address_2();
        let token = address::xan();
        let amount = token::Amount::from(10_098_123);

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner, &source, &token]);

        // Credit the tokens to the source before running the transaction to be
        // able to transfer from it
        tx_env.credit_tokens(&source, &token, amount);

        // Initialize VP environment from a transaction
        vp_host_env::init_from_tx(vp_owner.clone(), tx_env, |address| {
            // Apply transfer in a transaction
            tx_host_env::token::transfer(
                &source, address, &token, amount, &None, &None,
            );
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = BTreeSet::default();
        vp_host_env::set(vp_env);
        assert!(validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

    /// Test that a validity predicate update without a valid signature is
    /// rejected.
    #[test]
    fn test_unsigned_vp_update_rejected() {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner]);

        // Initialize VP environment from a transaction
        vp_host_env::init_from_tx(vp_owner.clone(), tx_env, |address| {
            // Update VP in a transaction
            tx_host_env::update_validity_predicate(address, &vp_code);
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = BTreeSet::default();
        vp_host_env::set(vp_env);
        assert!(!validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

    /// Test that a validity predicate update with a valid signature is
    /// accepted.
    #[test]
    fn test_signed_vp_update_accepted() {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let keypair = key::testing::keypair_1();
        let public_key = &keypair.ref_to();
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner]);

        tx_env.write_public_key(&vp_owner, public_key);

        // Initialize VP environment from a transaction
        vp_host_env::init_from_tx(vp_owner.clone(), tx_env, |address| {
            // Update VP in a transaction
            tx_host_env::update_validity_predicate(address, &vp_code);
        });

        let mut vp_env = vp_host_env::take();
        let tx = vp_env.tx.clone();
        let signed_tx = tx.sign(&keypair);
        let tx_data: Vec<u8> = signed_tx.data.as_ref().cloned().unwrap();
        vp_env.tx = signed_tx;
        let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = BTreeSet::default();
        vp_host_env::set(vp_env);
        assert!(validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

    prop_compose! {
        /// Generates an account address and a storage key inside its storage.
        fn arb_account_storage_subspace_key()
            // Generate an address
            (address in arb_non_internal_address())
            // Generate a storage key other than its VP key (VP cannot be
            // modified directly via `write`, it has to be modified via
            // `tx::update_validity_predicate`.
            (storage_key in arb_account_storage_key_no_vp(address.clone()),
            // Use the generated address too
            address in Just(address))
        -> (Address, Key) {
            (address, storage_key)
        }
    }

    proptest! {
    /// Test that a debit of more than [`MAX_FREE_DEBIT`] tokens without a valid signature is rejected.
    #[test]
    fn test_unsigned_debit_over_limit_rejected(amount in (MAX_FREE_DEBIT as u64 + 1..)) {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let target = address::testing::established_address_2();
        let token = address::xan();
        let amount = token::Amount::from(amount);

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner, &target, &token]);

        // Credit the tokens to the VP owner before running the transaction to
        // be able to transfer from it
        tx_env.credit_tokens(&vp_owner, &token, amount);

        // Initialize VP environment from a transaction
        vp_host_env::init_from_tx(vp_owner.clone(), tx_env, |address| {
        // Apply transfer in a transaction
        tx_host_env::token::transfer(address, &target, &token, amount, &None, &None);
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
        vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = BTreeSet::default();
        vp_host_env::set(vp_env);
        assert!(!validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

    /// Test that a debit of less than or equal to [`MAX_FREE_DEBIT`] tokens without a valid signature is accepted.
    #[test]
    fn test_unsigned_debit_under_limit_accepted(amount in (..MAX_FREE_DEBIT as u64 + 1)) {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let target = address::testing::established_address_2();
        let token = address::xan();
        let amount = token::Amount::from(amount);

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner, &target, &token]);

        // Credit the tokens to the VP owner before running the transaction to
        // be able to transfer from it
        tx_env.credit_tokens(&vp_owner, &token, amount);

        // Initialize VP environment from a transaction
        vp_host_env::init_from_tx(vp_owner.clone(), tx_env, |address| {
        // Apply transfer in a transaction
        tx_host_env::token::transfer(address, &target, &token, amount, &None, &None);
        });

        let vp_env = vp_host_env::take();
        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> =
        vp_env.all_touched_storage_keys();
        let verifiers: BTreeSet<Address> = BTreeSet::default();
        vp_host_env::set(vp_env);
        assert!(validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

        /// Test that a signed tx that performs arbitrary storage writes or
        /// deletes to the account is accepted.
        #[test]
        fn test_signed_arb_storage_write(
            (vp_owner, storage_key) in arb_account_storage_subspace_key(),
            // Generate bytes to write. If `None`, delete from the key instead
            storage_value in any::<Option<Vec<u8>>>(),
        ) {
            // Initialize a tx environment
            let mut tx_env = TestTxEnv::default();

            let keypair = key::testing::keypair_1();
            let public_key = &keypair.ref_to();

            // Spawn all the accounts in the storage key to be able to modify
            // their storage
            let storage_key_addresses = storage_key.find_addresses();
            tx_env.spawn_accounts(storage_key_addresses);

            tx_env.write_public_key(&vp_owner, public_key);

            // Initialize VP environment from a transaction
            vp_host_env::init_from_tx(vp_owner.clone(), tx_env, |_address| {
                // Write or delete some data in the transaction
                if let Some(value) = &storage_value {
                    tx_host_env::write(storage_key.to_string(), value);
                } else {
                    tx_host_env::delete(storage_key.to_string());
                }
            });

            let mut vp_env = vp_host_env::take();
            let tx = vp_env.tx.clone();
            let signed_tx = tx.sign(&keypair);
            let tx_data: Vec<u8> = signed_tx.data.as_ref().cloned().unwrap();
            vp_env.tx = signed_tx;
            let keys_changed: BTreeSet<storage::Key> =
            vp_env.all_touched_storage_keys();
            let verifiers: BTreeSet<Address> = BTreeSet::default();
            vp_host_env::set(vp_env);
            assert!(validate_tx(tx_data, vp_owner, keys_changed, verifiers));
        }
    }
}
