//! PoS system parameters

use std::ops::Mul;

use borsh::{BorshDeserialize, BorshSerialize};

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
}

/// ‱ (Parts per then thousand). This can be multiplied by any type that
/// implements [`Into<u64>`].
#[derive(Debug, Clone, Copy, BorshDeserialize, BorshSerialize)]
pub struct BasisPoints(u64);

impl BasisPoints {
    pub fn new(value: u64) -> Self {
        Self(value)
    }
}

impl Mul<u64> for BasisPoints {
    type Output = u64;

    fn mul(self, rhs: u64) -> Self::Output {
        // TODO checked arithmetics
        rhs * 10_000 / self.0
    }
}

impl Mul<i128> for BasisPoints {
    type Output = i128;

    fn mul(self, rhs: i128) -> Self::Output {
        // TODO checked arithmetics
        rhs * 10_000 / self.0 as i128
    }
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
        }
    }
}
