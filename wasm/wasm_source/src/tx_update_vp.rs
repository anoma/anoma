//! A tx for updating an account's validity predicate.
//! This tx wraps the validity predicate inside `SignedTxData` as
//! its input as declared in `shared` crate.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let update_vp = transaction::UpdateVp::try_from_slice(&data[..])
        .err_msg("failed to decode UpdateVp")?;

    debug_log!("update VP for: {:#?}", update_vp.addr);

    ctx.update_validity_predicate(&update_vp.addr, update_vp.vp_code)
}
