//! A tx for token transfer.
//! This tx uses `token::Transfer` wrapped inside `SignedTxData`
//! as its input as declared in `shared` crate.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let transfer = token::Transfer::try_from_slice(&data[..])
        .err_msg("failed to decode token::Transfer")?;
    debug_log!("apply_tx called with transfer: {:#?}", transfer);
    let token::Transfer {
        source,
        target,
        token,
        amount,
    } = transfer;
    token::transfer(ctx, &source, &target, &token, amount)
}
