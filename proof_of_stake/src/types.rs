use core::fmt::Debug;
use core::ops;
use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;

use crate::epoched::{Epoched, OffsetPipelineLen, OffsetUnboundingLen};

/// Epoch identifier. Epochs are identified by consecutive natural numbers.
///
/// In the API functions, this type is wrapped in [`Into`]. When using this
/// library, to replace [`Epoch`] with a custom type, simply implement [`From`]
/// to and from the types here.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Epoch(u64);

/// Voting power is calculated from staked tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VotingPower(u64);

#[derive(Debug, Clone)]
pub struct GenesisValidator<ADDR, TOKEN, PK> {
    pub address: ADDR,
    pub token_amount: TOKEN,
    pub consensus_key: PK,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BondId<ADDR>
where
    ADDR: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    source: ADDR,
    validator: ADDR,
}

/// Validator's address with its voting power.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WeightedValidator<ADDR>
where
    ADDR: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    /// The `voting_power` field must be on top, because lexicographic ordering
    /// is based on the top-to-bottom declaration order and in the
    /// `ValidatorSet` the `WeighedValidator`s these need to be sorted by
    /// the `voting_power`.
    voting_power: VotingPower,
    address: ADDR,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ValidatorSet<ADDR>
where
    ADDR: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    /// Active validator set with maximum size equal to `max_active_validators`
    active: BTreeSet<WeightedValidator<ADDR>>,
    /// All the other validators that are not active
    inactive: BTreeSet<WeightedValidator<ADDR>>,
}

pub type ValidatorSets<ADDR> = Epoched<ValidatorSet<ADDR>, OffsetUnboundingLen>;

/// The sum of all active and inactive validators' voting power
pub type TotalVotingPower = Epoched<VotingPower, OffsetUnboundingLen>;

#[derive(Debug, Clone, Copy)]
pub enum ValidatorState {
    Inactive,
    Pending,
    Candidate,
    // TODO consider adding `Jailed`
}

pub type Bonds<ADDR, TOKEN> =
    HashMap<BondId<ADDR>, Epoched<Bond<TOKEN>, OffsetPipelineLen>>;

pub type Unbonds<ADDR, TOKEN> =
    HashMap<BondId<ADDR>, Epoched<Unbond<TOKEN>, OffsetUnboundingLen>>;

#[derive(Debug)]
pub struct Bond<TOKEN> {
    /// A key is a the epoch set for the bond. This is used in unbonding, where
    // it's needed for slash epoch range check.
    delta: HashMap<Epoch, TOKEN>,
}

pub struct Unbond<TOKEN> {
    /// A key is a pair of the epoch of the bond from which a unbond was
    /// created the epoch of unboding. This is needed for slash epoch range
    /// check.
    deltas: HashMap<(Epoch, Epoch), TOKEN>,
}

impl From<u64> for Epoch {
    fn from(epoch: u64) -> Self {
        Epoch(epoch)
    }
}

impl From<Epoch> for u64 {
    fn from(epoch: Epoch) -> Self {
        epoch.0
    }
}

impl From<Epoch> for usize {
    fn from(epoch: Epoch) -> Self {
        epoch.0 as usize
    }
}

impl ops::Add<u64> for Epoch {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        Epoch(self.0 + rhs)
    }
}

impl ops::Add<usize> for Epoch {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Epoch(self.0 + rhs as u64)
    }
}

impl ops::Sub<u64> for Epoch {
    type Output = Epoch;

    fn sub(self, rhs: u64) -> Self::Output {
        Epoch(self.0 - rhs)
    }
}

impl ops::Sub<Epoch> for Epoch {
    type Output = Self;

    fn sub(self, rhs: Epoch) -> Self::Output {
        Epoch(self.0 - rhs.0)
    }
}

#[cfg(test)]
pub mod tests {

    use proptest::prelude::*;

    use super::*;

    /// Generate arbitrary epoch in given range
    pub fn arb_epoch(range: ops::Range<u64>) -> impl Strategy<Value = Epoch> {
        range.prop_map(Epoch)
    }
}
