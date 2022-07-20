//! A tx to vote on a proposal

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let tx_data = transaction::governance::VoteProposalData::try_from_slice(
        &signed.data.unwrap()[..],
    )
    .unwrap();
    log_string("apply_tx called to vote a governance proposal");

    governance::vote_proposal(ctx, tx_data)
}
