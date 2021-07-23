//! PoS system parameters

#[derive(Debug, Clone)]
pub struct PosParams {
    /// A maximum number of [`ValidatorState::Active`] validators
    pub max_validator_slots: u64,
    /// Any change applied during an epoch `n` will become active at the
    /// beginning of epoch `n + pipeline_len`.
    pub pipeline_len: u64,
    /// How many epochs after a committed fault a validator can be slashed.
    /// If a fault is detected in epoch `n`, it can slashed up until the end of
    /// `n + slashable_period_len` epoch.
    pub unbonding_len: u64,
    /// Used in validators' voting power calculation
    pub votes_per_token: u64,
    /// Amount of tokens rewarded to a validator for proposing a block
    pub block_proposer_reward: u64,
    /// Amount of tokens rewarded to each validator that voted on a block
    /// proposal
    pub block_vote_reward: u64,
}

impl Default for PosParams {
    fn default() -> Self {
        Self {
            max_validator_slots: 128,
            pipeline_len: 2,
            unbonding_len: 6,
            votes_per_token: 1000,
            block_proposer_reward: 100,
            block_vote_reward: 1,
        }
    }
}
