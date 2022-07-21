//! A tx to vote on a proposal

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let tx_data =
        transaction::governance::VoteProposalData::try_from_slice(&data[..])
            .err_msg("failed to decode VoteProposalData")?;

    debug_log!("apply_tx called to vote a governance proposal");

    governance::vote_proposal(ctx, tx_data)
}
