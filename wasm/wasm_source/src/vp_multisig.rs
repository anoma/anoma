use anoma_vp_prelude::{SignedTxData, *};
use once_cell::unsync::Lazy;

#[validity_predicate]
fn validate_tx(
    tx_data: Vec<u8>,
    addr: Address,
    keys_changed: BTreeSet<storage::Key>,
    verifiers: BTreeSet<Address>,
) -> bool {
    debug_log!(
        "vp_multisig called with user addr: {}, key_changed: {:?}, verifiers: {:?}",
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

    *valid_sig
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

    /// Test that no-op transaction (i.e. no storage modifications) accepted.
    #[test]
    fn test_no_op_transaction() {
        let mut env = TestVpEnv::default();
        init_vp_env(&mut env);

        let tx_data: Vec<u8> = vec![];
        let addr: Address = env.addr;
        let keys_changed: BTreeSet<storage::Key> = BTreeSet::default();
        let verifiers: BTreeSet<Address> = BTreeSet::default();

        assert!(validate_tx(tx_data, addr, keys_changed, verifiers));
    }
}