//! Multi-asset shielded pool VP.

use anoma_vp_prelude::*;

#[validity_predicate]
pub fn validate_tx(
    tx_data: Vec<u8>,
    addr: Address,
    keys_changed: BTreeSet<storage::Key>,
    verifiers: BTreeSet<Address>,
) -> bool {
    debug_log!(
        "vp_masp called with {} bytes data, address {}, keys_changed {:?}, \
         verifiers {:?}",
        tx_data.len(),
        addr,
        keys_changed,
        verifiers,
    );

    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let data = signed.data.as_ref().unwrap().clone();
    let _transfer =
        token::Transfer::try_from_slice(&signed.data.unwrap()[..]).unwrap();

    // Call out to host environment for crypto verification
    verify_masp(data)
}

#[cfg(test)]
mod tests {
    use anoma_tests::log::test;
    use anoma_tests::vp::*;
    use anoma_vp_prelude::*;

    use super::*;

    #[test]
    fn test_good_tx() {
        let env = TestVpEnv::default();
        let addr: Address = env.addr.clone();
        vp_host_env::set(env);

        let tx_data: Vec<u8> = vec![];
        let keys_changed: BTreeSet<storage::Key> = Default::default();
        let verifiers: BTreeSet<Address> = Default::default();

        assert!(validate_tx(tx_data, addr, keys_changed, verifiers))
    }
}
