//! A tx to initialize a new established address with a given public key and
//! a validity predicate.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let tx_data = transaction::InitAccount::try_from_slice(&data[..])
        .err_msg("failed to decode InitAccount")?;
    debug_log!("apply_tx called to init a new established account");

    let address = ctx.init_account(&tx_data.vp_code)?;
    let pk_key = key::pk_key(&address);
    ctx.write(&pk_key, &tx_data.public_key)?;
    Ok(())
}
