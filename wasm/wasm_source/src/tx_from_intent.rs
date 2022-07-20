//! A tx for a token transfer crafted by matchmaker from intents.
//! This tx uses `intent::IntentTransfers` wrapped inside
//! `SignedTxData` as its input as declared in `shared` crate.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();

    let tx_data =
        intent::IntentTransfers::try_from_slice(&signed.data.unwrap()[..]);

    let tx_data = tx_data.unwrap();

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
