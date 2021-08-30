//! Proof-of-Stake system parameters

use borsh::{BorshDeserialize, BorshSerialize};

use crate::types::BasisPoints;

/// Proof-of-Stake system parameters
#[derive(Debug, Clone, BorshDeserialize, BorshSerialize)]
pub struct PosParams {
    /// A maximum number of active validators
    pub max_validator_slots: u64,
    /// Any change applied during an epoch `n` will become active at the
    /// beginning of epoch `n + pipeline_len`.
    pub pipeline_len: u64,
    /// How many epochs after a committed fault a validator can be slashed.
    /// If a fault is detected in epoch `n`, it can slashed up until the end of
    /// `n + slashable_period_len` epoch.
    pub unbonding_len: u64,
    /// Used in validators' voting power calculation. Given in basis points
    /// (voting power per ten thousand tokens).
    pub votes_per_token: BasisPoints,
    /// Amount of tokens rewarded to a validator for proposing a block
    pub block_proposer_reward: u64,
    /// Amount of tokens rewarded to each validator that voted on a block
    /// proposal
    pub block_vote_reward: u64,
    /// Portion of validator's stake that should be slashed on a duplicate
    /// vote. Given in basis points (slashed amount per ten thousand tokens).
    pub duplicate_vote_slash_rate: BasisPoints,
    /// Portion of validator's stake that should be slashed on a light client
    /// attack. Given in basis points (slashed amount per ten thousand tokens).
    pub light_client_attack_slash_rate: BasisPoints,
}

impl Default for PosParams {
    fn default() -> Self {
        Self {
            max_validator_slots: 128,
            pipeline_len: 2,
            unbonding_len: 6,
            // 1 voting power per 1000 tokens
            votes_per_token: BasisPoints::new(10),
            block_proposer_reward: 100,
            block_vote_reward: 1,
            // slash 5%
            duplicate_vote_slash_rate: BasisPoints::new(500),
            // slash 5%
            light_client_attack_slash_rate: BasisPoints::new(500),
        }
    }
}
