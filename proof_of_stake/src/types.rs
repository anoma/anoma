use core::fmt::Debug;
use core::ops;
use std::collections::{BTreeSet, HashMap};
use std::convert::TryFrom;
use std::fmt::Display;
use std::hash::Hash;
use std::num::TryFromIntError;
use std::ops::AddAssign;

use crate::epoched::{Epoched, OffsetPipelineLen, OffsetUnboundingLen};
use crate::parameters::PosParams;

/// Epoch identifier. Epochs are identified by consecutive natural numbers.
///
/// In the API functions, this type is wrapped in [`Into`]. When using this
/// library, to replace [`Epoch`] with a custom type, simply implement [`From`]
/// to and from the types here.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Epoch(u64);

/// Voting power is calculated from staked tokens.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VotingPower(u64);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VotingPowerDelta(i64);

#[derive(Debug, Clone)]
pub struct GenesisValidator<Address, Token, PK> {
    pub address: Address,
    /// An address to which any staking rewards will be credited, must be
    /// different from the `address`
    pub staking_reward_address: Address,
    /// Staked tokens
    pub tokens: Token,
    pub consensus_key: PK,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BondId<Address>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    pub source: Address,
    pub validator: Address,
}

/// Validator's address with its voting power.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WeightedValidator<Address>
where
    Address: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    /// The `voting_power` field must be on top, because lexicographic ordering
    /// is based on the top-to-bottom declaration order and in the
    /// `ValidatorSet` the `WeighedValidator`s these need to be sorted by
    /// the `voting_power`.
    pub voting_power: VotingPower,
    pub address: Address,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ValidatorSet<Address>
where
    Address: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    /// Active validator set with maximum size equal to `max_validator_slots`
    /// in [`PosParams`].
    pub active: BTreeSet<WeightedValidator<Address>>,
    /// All the other validators that are not active
    pub inactive: BTreeSet<WeightedValidator<Address>>,
}

#[derive(Debug, Clone, Copy)]
pub enum ValidatorState {
    Inactive,
    Pending,
    Candidate,
    // TODO consider adding `Jailed`
}

pub type Bonds<Address, Token> =
    HashMap<BondId<Address>, Epoched<Bond<Token>, OffsetPipelineLen>>;

pub type Unbonds<Address, Token> =
    HashMap<BondId<Address>, Epoched<Unbond<Token>, OffsetUnboundingLen>>;

#[derive(Debug, Clone, Default)]
pub struct Bond<Token: Default> {
    /// A key is a the epoch set for the bond. This is used in unbonding, where
    /// it's needed for slash epoch range check.
    ///
    /// TODO: For Bonds, there's unnecessary redundancy with this hash map.
    /// We only need to keep the start `Epoch` for the Epoched head element
    /// (i.e. the current epoch data), the rest of the array can be calculated
    /// from the offset from the head
    pub delta: HashMap<Epoch, Token>,
}

#[derive(Debug, Clone, Default)]
pub struct Unbond<Token: Default> {
    /// A key is a pair of the epoch of the bond from which a unbond was
    /// created the epoch of unbonding. This is needed for slash epoch range
    /// check.
    pub deltas: HashMap<(Epoch, Epoch), Token>,
}

impl VotingPower {
    pub fn from_tokens(tokens: impl Into<u64>, params: &PosParams) -> Self {
        Self(params.votes_per_token * tokens.into())
    }
}

impl VotingPowerDelta {
    pub fn from_token_change(
        change: impl Into<i128>,
        params: &PosParams,
    ) -> Result<Self, TryFromIntError> {
        let delta: i128 = params.votes_per_token * change.into();
        let delta: i64 = TryFrom::try_from(delta)?;
        Ok(Self(delta))
    }

    pub fn try_from_tokens(
        tokens: impl Into<u64>,
        params: &PosParams,
    ) -> Result<Self, TryFromIntError> {
        let delta: i64 =
            TryFrom::try_from(params.votes_per_token * tokens.into())?;
        Ok(Self(delta))
    }
}

impl Epoch {
    /// Iterate a range of consecutive epochs starting from `self` of a given
    /// length. Work-around for `Step` implementation pending on stabilization of <https://github.com/rust-lang/rust/issues/42168>.
    pub fn iter_range(self, len: u64) -> impl Iterator<Item = Epoch> + Clone {
        let start_ix: u64 = self.into();
        let end_ix: u64 = start_ix + len;
        (start_ix..end_ix).map(Epoch::from)
    }

    /// Checked epoch subtraction. Computes self - rhs, returning None if
    /// overflow occurred.
    #[must_use = "this returns the result of the operation, without modifying \
                  the original"]
    pub fn checked_sub(self, rhs: Epoch) -> Option<Self> {
        if rhs.0 > self.0 {
            None
        } else {
            Some(Self(self.0 - rhs.0))
        }
    }

    /// Checked epoch subtraction. Computes self - rhs, returning default
    /// `Epoch(0)` if overflow occurred.
    #[must_use = "this returns the result of the operation, without modifying \
                  the original"]
    pub fn sub_or_default(self, rhs: Epoch) -> Self {
        self.checked_sub(rhs).unwrap_or_default()
    }
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

impl<Address> Display for BondId<Address>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Bond source: {}, validator: {}",
            self.source, self.validator
        )
    }
}

impl<Token> Bond<Token>
where
    Token: Clone + Copy + ops::Add<Output = Token> + Default,
{
    /// Find the sum of all the bonds amounts.
    pub fn sum(&self) -> Token {
        self.delta
            .iter()
            .fold(Default::default(), |acc, (_epoch, amount)| acc + *amount)
    }
}

impl<Token> ops::Add for Bond<Token>
where
    Token: Clone + ops::Add<Output = Token> + Default,
{
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.delta.extend(rhs.delta);
        self
    }
}

impl<Token> Unbond<Token>
where
    Token: Clone + Copy + ops::Add<Output = Token> + Default,
{
    /// Find the sum of all the unbonds amounts.
    pub fn sum(&self) -> Token {
        self.deltas
            .iter()
            .fold(Default::default(), |acc, (_epoch, amount)| acc + *amount)
    }
}

impl<Token> ops::Add for Unbond<Token>
where
    Token: Clone + ops::Add<Output = Token> + Default,
{
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.deltas.extend(rhs.deltas);
        self
    }
}

impl From<u64> for VotingPower {
    fn from(voting_power: u64) -> Self {
        Self(voting_power)
    }
}

impl From<VotingPower> for u64 {
    fn from(vp: VotingPower) -> Self {
        vp.0
    }
}

impl ops::Add for VotingPower {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for VotingPower {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

impl From<i64> for VotingPowerDelta {
    fn from(delta: i64) -> Self {
        Self(delta)
    }
}

impl From<VotingPowerDelta> for i64 {
    fn from(vp: VotingPowerDelta) -> Self {
        vp.0
    }
}

impl ops::Add for VotingPowerDelta {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for VotingPowerDelta {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

impl ops::Sub for VotingPowerDelta {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl ops::Sub<i64> for VotingPowerDelta {
    type Output = Self;

    fn sub(self, rhs: i64) -> Self::Output {
        Self(self.0 - rhs)
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
