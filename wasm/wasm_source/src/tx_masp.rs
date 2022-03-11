//! Trivial transaction which just passes on a tx_data blob to a
//! specified address

pub mod tx_masp {
    use anoma_tx_prelude::*;
    use borsh::{BorshDeserialize, BorshSerialize};

    #[derive(BorshDeserialize,BorshSerialize)]
    pub struct AddressedBlob {
        address: Address,
        blob: Vec<u8>,
    }

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let addressed_blob =
            AddressedBlob::try_from_slice(&signed.data.unwrap()[..]).unwrap();
        insert_verifier(&addressed_blob.address);
    }
}
