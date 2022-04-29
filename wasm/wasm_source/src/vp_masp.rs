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
        let _transfer =
            token::Transfer::try_from_slice(&signed.data.unwrap()[..]).unwrap();

        return true;
    }
}
