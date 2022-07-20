//! A tx for a PoS bond that stakes tokens via a self-bond or delegation.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let bond =
        transaction::pos::Bond::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();

    if let Err(err) =
        ctx.bond_tokens(bond.source.as_ref(), &bond.validator, bond.amount)
    {
        debug_log!("Unbonding failed with: {}", err);
        panic!()
    }
    Ok(())
}
