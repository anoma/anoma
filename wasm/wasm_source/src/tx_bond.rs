//! A tx for a PoS bond that stakes tokens via a self-bond or delegation.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let bond = transaction::pos::Bond::try_from_slice(&data[..])
        .err_msg("failed to decode Bond")?;

    ctx.bond_tokens(bond.source.as_ref(), &bond.validator, bond.amount)
}
