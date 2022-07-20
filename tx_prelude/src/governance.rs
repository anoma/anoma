//! Governance

use anoma::ledger::governance::storage;
use anoma::ledger::governance::vp::ADDRESS as governance_address;
use anoma::types::address::xan as m1t;
use anoma::types::token::Amount;
use anoma::types::transaction::governance::{
    InitProposalData, VoteProposalData,
};

use super::*;
use crate::token::transfer;

/// A proposal creation transaction.
pub fn init_proposal(ctx: &mut Ctx, data: InitProposalData) -> TxResult {
    let counter_key = storage::get_counter_key();
    let proposal_id = if let Some(id) = data.id {
        id
    } else {
        ctx.read(&counter_key)?.unwrap()
    };

    let content_key = storage::get_content_key(proposal_id);
    ctx.write_bytes(&content_key, data.content)?;

    let author_key = storage::get_author_key(proposal_id);
    ctx.write(&author_key, data.author.clone())?;

    let voting_start_epoch_key =
        storage::get_voting_start_epoch_key(proposal_id);
    ctx.write(&voting_start_epoch_key, data.voting_start_epoch)?;

    let voting_end_epoch_key = storage::get_voting_end_epoch_key(proposal_id);
    ctx.write(&voting_end_epoch_key, data.voting_end_epoch)?;

    let grace_epoch_key = storage::get_grace_epoch_key(proposal_id);
    ctx.write(&grace_epoch_key, data.grace_epoch)?;

    if let Some(proposal_code) = data.proposal_code {
        let proposal_code_key = storage::get_proposal_code_key(proposal_id);
        ctx.write_bytes(&proposal_code_key, proposal_code)?;
    }

    ctx.write(&counter_key, proposal_id + 1)?;

    let min_proposal_funds_key = storage::get_min_proposal_fund_key();
    let min_proposal_funds: Amount =
        ctx.read(&min_proposal_funds_key)?.unwrap();

    let funds_key = storage::get_funds_key(proposal_id);
    ctx.write(&funds_key, min_proposal_funds)?;

    // this key must always be written for each proposal
    let committing_proposals_key =
        storage::get_committing_proposals_key(proposal_id, data.grace_epoch.0);
    ctx.write(&committing_proposals_key, ())?;

    transfer(
        ctx,
        &data.author,
        &governance_address,
        &m1t(),
        min_proposal_funds,
    )
}

/// A proposal vote transaction.
pub fn vote_proposal(ctx: &mut Ctx, data: VoteProposalData) -> TxResult {
    for delegation in data.delegations {
        let vote_key = storage::get_vote_proposal_key(
            data.id,
            data.voter.clone(),
            delegation,
        );
        ctx.write(&vote_key, data.vote.clone())?;
    }
    Ok(())
}
