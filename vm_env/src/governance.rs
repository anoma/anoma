/// Tx imports and functions.
pub mod tx {

    use anoma::ledger::governance::{storage, ADDRESS as governance_address};
    use anoma::types::address::xan as m1t;
    use anoma::types::token::Amount;
    use anoma::types::transaction::governance::{
        InitProposalData, VoteProposalData,
    };

    use crate::imports::tx;
    use crate::token::tx::transfer;

    /// A proposal creation transaction.
    pub fn init_proposal(data: InitProposalData) {
        let counter_key = storage::get_counter_key();
        let proposal_id = if let Some(id) = data.id {
            id
        } else {
            tx::read(&counter_key.to_string()).unwrap()
        };

        let content_key = storage::get_content_key(proposal_id);
        tx::write_bytes(&content_key.to_string(), data.content);

        let author_key = storage::get_author_key(proposal_id);
        tx::write(&author_key.to_string(), data.author.clone());

        let voting_start_epoch_key =
            storage::get_voting_start_epoch_key(proposal_id);
        tx::write(&voting_start_epoch_key.to_string(), data.voting_start_epoch);

        let voting_end_epoch_key =
            storage::get_voting_end_epoch_key(proposal_id);
        tx::write(&voting_end_epoch_key.to_string(), data.voting_end_epoch);

        let grace_epoch_key = storage::get_grace_epoch_key(proposal_id);
        tx::write(&grace_epoch_key.to_string(), data.grace_epoch);

        if data.proposal_code.is_some() {
            let proposal_code_key = storage::get_proposal_code_key(proposal_id);
            tx::write(&proposal_code_key.to_string(), data.proposal_code);
        }

        tx::write(&counter_key.to_string(), proposal_id + 1);

        let min_proposal_funds_key = storage::get_min_proposal_fund_key();
        let min_proposal_funds: Amount =
            tx::read(&min_proposal_funds_key.to_string()).unwrap();

        let funds_key = storage::get_funds_key(proposal_id);
        tx::write(&funds_key.to_string(), min_proposal_funds);

        // this key must always be written for each proposal
        let committing_proposals_key = storage::get_committing_proposals_key(
            proposal_id,
            data.grace_epoch.0,
        );
        tx::write(&committing_proposals_key.to_string(), ());

        transfer(
            &data.author,
            &governance_address,
            &m1t(),
            min_proposal_funds,
        );
    }

    /// A proposal vote transaction.
    pub fn vote_proposal(data: VoteProposalData) {
        for delegation in data.delegations {
            let vote_key = storage::get_vote_proposal_key(
                data.id,
                data.voter.clone(),
                delegation,
            );
            tx::write(&vote_key.to_string(), data.vote.clone());
        }
    }
}
