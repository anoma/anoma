//! A tx to mint new NFT token(s).

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let tx_data =
        transaction::nft::MintNft::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();
    log_string("apply_tx called to mint a new NFT tokens");

    nft::mint_tokens(ctx, tx_data)
}
