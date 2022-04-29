/// Multi-asset shielded pool VP.
#[cfg(feature = "vp_masp")]
pub mod vp_masp {
    use anoma_vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        tx_data: Vec<u8>,
        addr: Address,
        keys_changed: BTreeSet<storage::Key>,
        verifiers: BTreeSet<Address>,
    ) -> bool {
        debug_log!("vp_masp called with {} bytes data, address {}, keys_changed {:?}, verifiers {:?}",
          tx_data.len(),
          addr,
          keys_changed,
          verifiers,
        );

        let signed =
            SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let data = signed.data.as_ref().unwrap().clone();
        let _transfer =
            token::Transfer::try_from_slice(&signed.data.unwrap()[..]).unwrap();

        // Call out to host environment for crypto verification
        verify_masp(data)
    }
}

#[cfg(test)]
mod tests {
    use anoma_test::log::test;
    use anoma_vp_prelude::*;

    #[test]
    fn test_good_tx() {
        let mut env = TestVpEnv::default;
        init_vp_env(&mut env);

        let tx_data: Vec<u8> = vec![];
        let addr: Address = env.addr;
        let keys_changed: HashSet<storage::Key> = HashSet::default();
        let verifiers: HashSet<Address> = HashSet::default();

        assert!(validate_tx(tx_data, addr, keys_changed, verifiers))
    }
}
