/// Tx imports and functions.
pub mod tx {

    use anoma::types::token::Amount;
    use anoma::types::transaction::governance::InitProposalData;
    use anoma::ledger::governance::storage;
    use anoma::ledger::governance::ADDRESS as governance_address;
    use anoma::types::address::xan as m1t;

    use crate::imports::tx;
    use super::super::token::tx::transfer;

    /// A proposal creation transaction.
    pub fn init_proposal(data: InitProposalData) {
        let proposal_id = if let Some(id) = data.id {
            id
        } else {
            let counter_key = storage::get_counter_key();
            tx::read(&counter_key.to_string()).unwrap()
        };

        let content_key = storage::get_content_key(proposal_id);
        tx::write(&content_key.to_string(), data.content);

        let author_key = storage::get_author_key(proposal_id);
        tx::write(&author_key.to_string(), data.author.clone());

        let voting_start_epoch_key = storage::get_voting_start_epoch_key(proposal_id);
        tx::write(&voting_start_epoch_key.to_string(), data.voting_start_epoch);

        let voting_end_epoch_key = storage::get_voting_end_epoch_key(proposal_id);
        tx::write(&voting_end_epoch_key.to_string(), data.voting_end_epoch);

        let voting_grace_epoch_key = storage::get_voting_grace_epoch_key(proposal_id);
        tx::write(&voting_grace_epoch_key.to_string(), data.grace_epoch);

        if data.proposal_code.is_some() {
            let proposal_code_key = storage::get_proposal_code_key(proposal_id);
            tx::write(&proposal_code_key.to_string(), data.proposal_code);
        }

        let counter_key = storage::get_counter_key();
        let current_counter: u64 = tx::read(&counter_key.to_string()).unwrap();
        tx::write(&counter_key.to_string(), current_counter + 1);

        let min_proposal_funds_key = storage::get_min_proposal_fund_key();
        let min_proposal_funds: u64 = tx::read(&min_proposal_funds_key.to_string()).unwrap();
        transfer(&data.author, &governance_address, &m1t(), Amount::whole(min_proposal_funds));
    }
}