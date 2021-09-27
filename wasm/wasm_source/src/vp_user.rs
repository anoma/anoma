//! A basic user VP.
//!
//! This VP currently provides a signature verification against a public key for
//! sending tokens (receiving tokens is permissive).
//!
//! It allows to bond, unbond and withdraw tokens to and from PoS system with a
//! valid signature.
//!
//! It allows to fulfil intents that were signed by this account's key if they
//! haven't already been fulfilled (fulfilled intents are added to the owner's
//! invalid intent set).
//!
//! Any other storage key changes are allowed only with a valid signature.

use anoma_vm_env::vp_prelude::intent::{
    Exchange, FungibleTokenIntent, IntentTransfers,
};
use anoma_vm_env::vp_prelude::key::ed25519::{Signed, SignedTxData};
use anoma_vm_env::vp_prelude::*;
use once_cell::unsync::Lazy;
use rust_decimal::prelude::*;

#[validity_predicate]
fn validate_tx(
    tx_data: Vec<u8>,
    addr: Address,
    keys_changed: HashSet<storage::Key>,
    verifiers: HashSet<Address>,
) -> bool {
    log_string(format!(
        "vp_user called with user addr: {}, key_changed: {:?}, verifiers: {:?}",
        addr, keys_changed, verifiers
    ));

    let signed_tx_data =
        Lazy::new(|| SignedTxData::try_from_slice(&tx_data[..]));

    let valid_sig = Lazy::new(|| match &*signed_tx_data {
        Ok(signed_tx_data) => {
            let pk = key::ed25519::get(&addr);
            match pk {
                Some(pk) => verify_tx_signature(&pk, &signed_tx_data.sig),
                None => false,
            }
        }
        _ => false,
    });

    let valid_intent = Lazy::new(|| match &*signed_tx_data {
        Ok(signed_tx_data) => check_intent_transfers(&addr, signed_tx_data),
        _ => false,
    });

    for key in keys_changed.iter() {
        let is_valid = if let Some(owner) = token::is_any_token_balance_key(key)
        {
            if owner == &addr {
                let key = key.to_string();
                let pre: token::Amount = read_pre(&key).unwrap_or_default();
                let post: token::Amount = read_post(&key).unwrap_or_default();
                let change = post.change() - pre.change();
                // debit has to signed, credit doesn't
                let valid = change >= 0 || *valid_sig || *valid_intent;
                log_string(format!(
                    "token key: {}, change: {}, valid_sig: {}, valid_intent: \
                     {}, valid modification: {}",
                    key, change, *valid_sig, *valid_intent, valid
                ));
                valid
            } else {
                log_string(format!(
                    "This address ({}) is not of owner ({}) of token key: {}",
                    addr, owner, key
                ));
                // If this is not the owner, allow any change
                true
            }
        } else if proof_of_stake::is_pos_key(key) {
            // Allow the account to be used in PoS
            let bond_id = proof_of_stake::is_bond_key(key)
                .or_else(|| proof_of_stake::is_unbond_key(key));
            let valid = match bond_id {
                Some(bond_id) => {
                    // Bonds and unbonds changes for this address
                    // must be signed
                    bond_id.source != addr || *valid_sig
                }
                None => {
                    // Any other PoS changes are allowed without signature
                    true
                }
            };
            log_string(format!(
                "PoS key {} {}",
                key,
                if valid { "accepted" } else { "rejected" }
            ));
            valid
        } else if let Some(owner) = intent::is_invalid_intent_key(key) {
            if owner == &addr {
                let key = key.to_string();
                let pre: Vec<Vec<u8>> = read_pre(&key).unwrap_or_default();
                let post: Vec<Vec<u8>> = read_post(&key).unwrap_or_default();
                // A new invalid intent must have been added
                pre.len() + 1 == post.len()
            } else {
                log_string(format!(
                    "This address ({}) is not of owner ({}) of \
                     InvalidIntentSet key: {}",
                    addr, owner, key
                ));
                // If this is not the owner, allow any change
                true
            }
        } else {
            log_string(format!(
                "Unknown key modified, valid sig {}",
                *valid_sig
            ));
            // Allow any other key change if authorized by a signature
            *valid_sig
        };
        if !is_valid {
            log_string(format!("key {} modification failed vp", key));
            return false;
        }
    }
    true
}

fn check_intent_transfers(
    addr: &Address,
    signed_tx_data: &SignedTxData,
) -> bool {
    if let Some((raw_intent_transfers, exchange, intent)) =
        try_decode_intent(addr, signed_tx_data)
    {
        log_string("check intent".to_string());
        return check_intent(addr, exchange, intent, raw_intent_transfers);
    }
    false
}

fn try_decode_intent(
    addr: &Address,
    signed_tx_data: &SignedTxData,
) -> Option<(Vec<u8>, Signed<Exchange>, Signed<FungibleTokenIntent>)> {
    let raw_intent_transfers = signed_tx_data.data.as_ref().cloned()?;
    let mut tx_data =
        IntentTransfers::try_from_slice(&raw_intent_transfers[..]).ok()?;
    log_string(format!(
        "tx_data.matches.exchanges: {:?}, {}",
        tx_data.matches.exchanges, &addr
    ));
    if let (Some(exchange), Some(intent)) = (
        tx_data.matches.exchanges.remove(addr),
        tx_data.matches.intents.remove(addr),
    ) {
        return Some((raw_intent_transfers, exchange, intent));
    } else {
        log_string("no intent with a matching address".to_string());
    }
    None
}

fn check_intent(
    addr: &Address,
    exchange: Signed<Exchange>,
    intent: Signed<FungibleTokenIntent>,
    raw_intent_transfers: Vec<u8>,
) -> bool {
    // verify signature
    let pk = key::ed25519::get(addr);
    if let Some(pk) = pk {
        if intent.verify(&pk).is_err() {
            log_string("invalid sig".to_string());
            return false;
        }
    } else {
        return false;
    }

    // verify the intent have not been already used
    if !intent::vp_exchange(&exchange) {
        return false;
    }

    // verify the intent is fulfilled
    let Exchange {
        addr,
        token_sell,
        rate_min,
        token_buy,
        min_buy,
        max_sell,
        vp,
    } = &exchange.data;

    log_string(format!("vp is: {}", vp.is_some()));

    if let Some(code) = vp {
        let eval_result = eval(code.to_vec(), raw_intent_transfers);
        log_string(format!("eval result: {}", eval_result));
        if !eval_result {
            return false;
        }
    }

    log_string(format!(
        "exchange description: {}, {}, {}, {}, {}",
        token_sell,
        token_buy,
        max_sell.change(),
        min_buy.change(),
        rate_min.0
    ));

    let token_sell_key = token::balance_key(token_sell, addr).to_string();
    let mut sell_difference: token::Amount =
        read_pre(&token_sell_key).unwrap_or_default();
    let sell_post: token::Amount =
        read_post(token_sell_key).unwrap_or_default();

    sell_difference.spend(&sell_post);

    let token_buy_key = token::balance_key(token_buy, addr).to_string();
    let buy_pre: token::Amount = read_pre(&token_buy_key).unwrap_or_default();
    let mut buy_difference: token::Amount =
        read_post(token_buy_key).unwrap_or_default();

    buy_difference.spend(&buy_pre);

    let sell_diff: Decimal = sell_difference.change().into(); // -> how many token I sold
    let buy_diff: Decimal = buy_difference.change().into(); // -> how many token I got

    log_string(format!(
        "buy_diff > 0: {}, rate check: {}, max_sell > sell_diff: {}, buy_diff \
         > min_buy: {}",
        buy_difference.change() > 0,
        buy_diff / sell_diff >= rate_min.0,
        max_sell.change() >= sell_difference.change(),
        buy_diff >= min_buy.change().into()
    ));

    if !(buy_difference.change() > 0
        && (buy_diff / sell_diff >= rate_min.0)
        && max_sell.change() >= sell_difference.change()
        && buy_diff >= min_buy.change().into())
    {
        log_string(format!(
            "invalid exchange, {} / {}, sell diff: {}, buy diff: {}, \
             max_sell: {}, rate_min: {}, min_buy: {}, buy_diff / sell_diff: {}",
            token_sell,
            token_buy,
            sell_difference.change(),
            buy_difference.change(),
            max_sell.change(),
            rate_min.0,
            min_buy.change(),
            buy_diff / sell_diff
        ));
        false
    } else {
        true
    }
}

#[cfg(test)]
mod tests {
    use address::testing::arb_non_internal_address;
    // Use this as `#[test]` annotation to enable logging
    use anoma_tests::log::test;
    use anoma_tests::tx::{tx_host_env, TestTxEnv};
    use anoma_tests::vp::vp_host_env::storage::Key;
    use anoma_tests::vp::*;
    use proptest::prelude::*;
    use storage::testing::arb_account_storage_key_no_vp;

    use super::*;

    const VP_ALWAYS_TRUE_WASM: &str =
        "../../wasm_for_tests/vp_always_true.wasm";

    /// Test that no-op transaction (i.e. no storage modifications) accepted.
    #[test]
    fn test_no_op_transaction() {
        let mut env = TestVpEnv::default();
        init_vp_env(&mut env);

        let tx_data: Vec<u8> = vec![];
        let addr: Address = env.addr;
        let keys_changed: HashSet<storage::Key> = HashSet::default();
        let verifiers: HashSet<Address> = HashSet::default();

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
        let vp_env = init_vp_env_from_tx(vp_owner.clone(), tx_env, |address| {
            // Apply transfer in a transaction
            tx_host_env::token::transfer(&source, address, &token, amount);
        });

        let tx_data: Vec<u8> = vec![];
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = HashSet::default();
        assert!(validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

    /// Test that a debit transfer without a valid signature is rejected.
    #[test]
    fn test_unsigned_debit_transfer_rejected() {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let target = address::testing::established_address_2();
        let token = address::xan();
        let amount = token::Amount::from(10_098_123);

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner, &target, &token]);

        // Credit the tokens to the VP owner before running the transaction to
        // be able to transfer from it
        tx_env.credit_tokens(&vp_owner, &token, amount);

        // Initialize VP environment from a transaction
        let vp_env = init_vp_env_from_tx(vp_owner.clone(), tx_env, |address| {
            // Apply transfer in a transaction
            tx_host_env::token::transfer(address, &target, &token, amount);
        });

        let tx_data: Vec<u8> = vec![];
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = HashSet::default();
        assert!(!validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

    /// Test that a debit transfer with a valid signature is accepted.
    #[test]
    fn test_signed_debit_transfer_accepted() {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let keypair = key::ed25519::testing::keypair_1();
        let public_key = &keypair.public;
        let target = address::testing::established_address_2();
        let token = address::xan();
        let amount = token::Amount::from(10_098_123);

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner, &target, &token]);

        // Credit the tokens to the VP owner before running the transaction to
        // be able to transfer from it
        tx_env.credit_tokens(&vp_owner, &token, amount);

        tx_env.write_public_key(&vp_owner, public_key);

        // Initialize VP environment from a transaction
        let mut vp_env =
            init_vp_env_from_tx(vp_owner.clone(), tx_env, |address| {
                // Apply transfer in a transaction
                tx_host_env::token::transfer(address, &target, &token, amount);
            });

        let tx = vp_env.tx.clone();
        let signed_tx = key::ed25519::sign_tx(&keypair, tx);
        let tx_data: Vec<u8> = signed_tx.data.as_ref().cloned().unwrap();
        vp_env.tx = signed_tx;
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = HashSet::default();
        assert!(validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

    /// Test that a transfer on with accounts other than self is accepted.
    #[test]
    fn test_transfer_between_other_parties_accepted() {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let source = address::testing::established_address_2();
        let target = address::testing::established_address_3();
        let token = address::xan();
        let amount = token::Amount::from(10_098_123);

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner, &source, &target, &token]);

        // Credit the tokens to the VP owner before running the transaction to
        // be able to transfer from it
        tx_env.credit_tokens(&source, &token, amount);

        // Initialize VP environment from a transaction
        let vp_env = init_vp_env_from_tx(vp_owner.clone(), tx_env, |address| {
            tx_host_env::insert_verifier(address);
            // Apply transfer in a transaction
            tx_host_env::token::transfer(&source, &target, &token, amount);
        });

        let tx_data: Vec<u8> = vec![];
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = HashSet::default();
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
        /// Test that an unsigned tx that performs arbitrary storage writes or
        /// deletes to  the account is rejected.
        #[test]
        fn test_unsigned_arb_storage_write_rejected(
            (vp_owner, storage_key) in arb_account_storage_subspace_key(),
            // Generate bytes to write. If `None`, delete from the key instead
            storage_value in any::<Option<Vec<u8>>>(),
        ) {
            // Initialize a tx environment
            let mut tx_env = TestTxEnv::default();

            // Spawn all the accounts in the storage key to be able to modify
            // their storage
            let storage_key_addresses = storage_key.find_addresses();
            tx_env.spawn_accounts(storage_key_addresses);

            // Initialize VP environment from a transaction
            let vp_env =
                init_vp_env_from_tx(vp_owner.clone(), tx_env, |_address| {
                    // Write or delete some data in the transaction
                    if let Some(value) = &storage_value {
                        tx_host_env::write(storage_key.to_string(), value);
                    } else {
                        tx_host_env::delete(storage_key.to_string());
                    }
                });

            let tx_data: Vec<u8> = vec![];
            let keys_changed: HashSet<storage::Key> =
                vp_env.all_touched_storage_keys();
            let verifiers: HashSet<Address> = HashSet::default();
            assert!(!validate_tx(tx_data, vp_owner, keys_changed, verifiers));
        }
    }

    proptest! {
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

            let keypair = key::ed25519::testing::keypair_1();
            let public_key = &keypair.public;

            // Spawn all the accounts in the storage key to be able to modify
            // their storage
            let storage_key_addresses = storage_key.find_addresses();
            tx_env.spawn_accounts(storage_key_addresses);

            tx_env.write_public_key(&vp_owner, public_key);

            // Initialize VP environment from a transaction
            let mut vp_env =
                init_vp_env_from_tx(vp_owner.clone(), tx_env, |_address| {
                    // Write or delete some data in the transaction
                    if let Some(value) = &storage_value {
                        tx_host_env::write(storage_key.to_string(), value);
                    } else {
                        tx_host_env::delete(storage_key.to_string());
                    }
                });

            let tx = vp_env.tx.clone();
            let signed_tx = key::ed25519::sign_tx(&keypair, tx);
            let tx_data: Vec<u8> = signed_tx.data.as_ref().cloned().unwrap();
            vp_env.tx = signed_tx;
            let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
            let verifiers: HashSet<Address> = HashSet::default();
            assert!(validate_tx(tx_data, vp_owner, keys_changed, verifiers));
        }
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
        let vp_env = init_vp_env_from_tx(vp_owner.clone(), tx_env, |address| {
            // Update VP in a transaction
            tx_host_env::update_validity_predicate(address, &vp_code);
        });

        let tx_data: Vec<u8> = vec![];
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = HashSet::default();
        assert!(!validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }

    /// Test that a validity predicate update with a valid signature is
    /// accepted.
    #[test]
    fn test_signed_vp_update_accepted() {
        // Initialize a tx environment
        let mut tx_env = TestTxEnv::default();

        let vp_owner = address::testing::established_address_1();
        let keypair = key::ed25519::testing::keypair_1();
        let public_key = &keypair.public;
        let vp_code =
            std::fs::read(VP_ALWAYS_TRUE_WASM).expect("cannot load wasm");

        // Spawn the accounts to be able to modify their storage
        tx_env.spawn_accounts([&vp_owner]);

        tx_env.write_public_key(&vp_owner, public_key);

        // Initialize VP environment from a transaction
        let mut vp_env =
            init_vp_env_from_tx(vp_owner.clone(), tx_env, |address| {
                // Update VP in a transaction
                tx_host_env::update_validity_predicate(address, &vp_code);
            });

        let tx = vp_env.tx.clone();
        let signed_tx = key::ed25519::sign_tx(&keypair, tx);
        let tx_data: Vec<u8> = signed_tx.data.as_ref().cloned().unwrap();
        vp_env.tx = signed_tx;
        let keys_changed: HashSet<storage::Key> =
            vp_env.all_touched_storage_keys();
        let verifiers: HashSet<Address> = HashSet::default();
        assert!(validate_tx(tx_data, vp_owner, keys_changed, verifiers));
    }
}
