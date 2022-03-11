/// Multi-asset shielded pool VP.
#[cfg(feature = "vp_masp")]
pub mod vp_masp {
    use anoma_vp_prelude::*;

    #[derive(BorshDeserialize,BorshSerialize)]
    pub struct AddressedBlob {
        address: Address,
        blob: Vec<u8>,
    }

    #[validity_predicate]
    fn validate_tx(
        tx_data: Vec<u8>,
        addr: Address,
        keys_changed: HashSet<storage::Key>,
        verifiers: HashSet<Address>,
    ) -> bool {
        debug_log!("vp_masp called with {} bytes data, address {}, keys_changed {:?}, verifiers {:?}",
          tx_data.len(),
          addr,
          keys_changed,
          verifiers,
        );

        if !keys_changed.is_empty() {
            // no actual balance changes for now
            return false;
        }

        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let addressed_blob =
            AddressedBlob::try_from_slice(&signed.data.unwrap()[..]).unwrap();

        // Call out to host environment for crypto verification
        verify_masp(addressed_blob.blob)
    }
}

#[cfg(test)]
mod tests {
    use anoma_test::log::test;

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
