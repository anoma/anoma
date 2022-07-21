//! A tx for a token transfer crafted by matchmaker from intents.
//! This tx uses `intent::IntentTransfers` wrapped inside
//! `SignedTxData` as its input as declared in `shared` crate.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let tx_data = intent::IntentTransfers::try_from_slice(&data[..])
        .err_msg("failed to decode IntentTransfers")?;

    // make sure that the matchmaker has to validate this tx
    ctx.insert_verifier(&tx_data.source)?;

    for token::Transfer {
        source,
        target,
        token,
        amount,
    } in tx_data.matches.transfers
    {
        token::transfer(ctx, &source, &target, &token, amount)?;
    }

    for intent in tx_data.matches.exchanges.values() {
        intent::invalidate_exchange(ctx, intent)?;
    }
    Ok(())
}
