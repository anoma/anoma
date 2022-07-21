//! A tx to initialize a new NFT account.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let tx_data = transaction::nft::CreateNft::try_from_slice(&data[..])
        .err_msg("failed to decode CreateNft")?;
    log_string("apply_tx called to create a new NFT");

    let _address = nft::init_nft(ctx, tx_data)?;
    Ok(())
}
