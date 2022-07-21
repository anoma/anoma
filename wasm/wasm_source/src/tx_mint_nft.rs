//! A tx to mint new NFT token(s).

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let tx_data = transaction::nft::MintNft::try_from_slice(&data[..])
        .err_msg("failed to decode MintNft")?;
    log_string("apply_tx called to mint a new NFT tokens");

    nft::mint_tokens(ctx, tx_data)
}
