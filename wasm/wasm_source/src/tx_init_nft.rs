//! A tx to initialize a new NFT account.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let tx_data =
        transaction::nft::CreateNft::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();
    log_string("apply_tx called to create a new NFT");

    nft::init_nft(tx_data);
}
