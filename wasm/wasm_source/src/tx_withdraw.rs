//! A tx for a PoS unbond that removes staked tokens from a self-bond or a
//! delegation to be withdrawn in or after unbonding epoch.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let withdraw =
        transaction::pos::Withdraw::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();

    match ctx.withdraw_tokens(withdraw.source.as_ref(), &withdraw.validator) {
        Ok(slashed) => {
            debug_log!("Withdrawal slashed for {}", slashed);
        }
        Err(err) => {
            debug_log!("Withdrawal failed with: {}", err);
            panic!()
        }
    }
    Ok(())
}
