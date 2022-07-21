//! A tx for a PoS unbond that removes staked tokens from a self-bond or a
//! delegation to be withdrawn in or after unbonding epoch.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let withdraw = transaction::pos::Withdraw::try_from_slice(&data[..])
        .err_msg("failed to decode Withdraw")?;

    let slashed =
        ctx.withdraw_tokens(withdraw.source.as_ref(), &withdraw.validator)?;
    if slashed != token::Amount::default() {
        debug_log!("Withdrawal slashed for {}", slashed);
    }
    Ok(())
}
