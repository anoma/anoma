use borsh::{BorshDeserialize, BorshSerialize};

use crate::ledger::storage::{self, Storage, StorageHasher, types::encode};

use super::storage as gov_storage;

#[derive(
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshSerialize,
    BorshDeserialize,
)]
pub struct GovParams {
    /// Minimum amount of locked funds
    pub min_proposal_fund: u64,
    /// Maximum kilobyte length for proposal code
    pub max_proposal_code_size: u64,
    /// Minimum proposal voting period in epochs
    pub min_proposal_period: u64,
    /// Maximimum number of characters for proposal content
    pub max_proposal_content: u64
}

impl Default for GovParams {
    fn default() -> Self {
        Self { min_proposal_fund: 500, max_proposal_code_size: 500, min_proposal_period: 3, max_proposal_content: 10000 }
    }
}


impl GovParams {
    /// Initialize governance parameters into storage
    pub fn init_storage<DB, H>(
        &self,
        storage: &mut Storage<DB, H>,
    ) where
        DB: storage::DB + for<'iter> storage::DBIter<'iter>,
        H: storage::StorageHasher,
    {
        // TODO: handle errors
        let min_proposal_fund_key = gov_storage::get_min_proposal_fund_key();
        storage.write(&min_proposal_fund_key, encode(&self.min_proposal_fund));

        let max_proposal_code_size_key = gov_storage::get_max_proposal_code_size_key();
        storage.write(&max_proposal_code_size_key, encode(&self.max_proposal_code_size));

        let min_proposal_period_key = gov_storage::get_min_proposal_period_key();
        storage.write(&min_proposal_period_key, encode(&self.min_proposal_period));

        let max_proposal_content_key = gov_storage::get_max_proposal_content_key();
        storage.write(&max_proposal_content_key, encode(&self.max_proposal_content));

        let counter_key = gov_storage::get_counter_key();
        storage.write(&counter_key, encode(&u64::MIN));
    }
}