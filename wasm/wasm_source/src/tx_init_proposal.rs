//! A tx to create a governance proposal.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let tx_data = transaction::governance::InitProposalData::try_from_slice(
        &signed.data.unwrap()[..],
    )
    .unwrap();
    log_string("apply_tx called to create a new governance proposal");

    governance::init_proposal(tx_data);
}
