//! A tx to create a governance proposal.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let tx_data =
        transaction::governance::InitProposalData::try_from_slice(&data[..])
            .err_msg("failed to decode InitProposalData")?;
    log_string("apply_tx called to create a new governance proposal");

    governance::init_proposal(ctx, tx_data)
}
