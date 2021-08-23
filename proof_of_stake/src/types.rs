use core::fmt::Debug;
use std::collections::{BTreeSet, HashMap};
use std::convert::TryFrom;
use std::fmt::Display;
use std::hash::Hash;
use std::num::TryFromIntError;
use std::ops::{Add, AddAssign, Sub};

use borsh::{BorshDeserialize, BorshSerialize};

use crate::epoched::{
    Epoched, EpochedDelta, OffsetPipelineLen, OffsetUnboundingLen,
};
use crate::parameters::PosParams;

pub type ValidatorConsensusKeys<PublicKey> =
    Epoched<PublicKey, OffsetPipelineLen>;
pub type ValidatorStates = Epoched<ValidatorState, OffsetPipelineLen>;
pub type ValidatorTotalDeltas<TokenChange> =
    EpochedDelta<TokenChange, OffsetUnboundingLen>;
pub type ValidatorVotingPowers =
    EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>;
pub type Bonds<TokenAmount> =
    EpochedDelta<Bond<TokenAmount>, OffsetPipelineLen>;
pub type Unbonds<TokenAmount> =
    EpochedDelta<Unbond<TokenAmount>, OffsetUnboundingLen>;
pub type ValidatorSets<Address> =
    Epoched<ValidatorSet<Address>, OffsetUnboundingLen>;
pub type TotalVotingPowers =
    EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>;

/// Epoch identifier. Epochs are identified by consecutive natural numbers.
///
/// In the API functions, this type is wrapped in [`Into`]. When using this
/// library, to replace [`Epoch`] with a custom type, simply implement [`From`]
/// to and from the types here.
#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshDeserialize,
    BorshSerialize,
)]
pub struct Epoch(u64);

/// Voting power is calculated from staked tokens.
#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshDeserialize,
    BorshSerialize,
)]
pub struct VotingPower(u64);

#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshDeserialize,
    BorshSerialize,
)]
pub struct VotingPowerDelta(i64);

#[derive(Debug, Clone)]
pub struct GenesisValidator<Address, Token, PK> {
    pub address: Address,
    /// An address to which any staking rewards will be credited, must be
    /// different from the `address`
    pub staking_reward_address: Address,
    /// Staked tokens are put into a self-bond
    pub tokens: Token,
    /// A public key used for signing validator's consensus actions
    pub consensus_key: PK,
    /// An public key associated with the staking reward address
    pub staking_reward_key: PK,
}

#[derive(Debug, Clone)]
pub enum ValidatorSetUpdate<PK> {
    /// A validator is active
    Active(ActiveValidator<PK>),
    /// A validator who was active in the last update and is now inactive
    Deactivated(PK),
}

#[derive(Debug, Clone)]
pub struct ActiveValidator<PK> {
    /// A public key used for signing validator's consensus actions
    pub consensus_key: PK,
    /// Voting power
    pub voting_power: VotingPower,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshDeserialize,
    BorshSerialize,
)]
pub struct BondId<Address>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    pub source: Address,
    pub validator: Address,
}

/// Validator's address with its voting power.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    BorshDeserialize,
    BorshSerialize,
)]
pub struct WeightedValidator<Address>
where
    Address: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize,
{
    /// The `voting_power` field must be on top, because lexicographic ordering
    /// is based on the top-to-bottom declaration order and in the
    /// `ValidatorSet` the `WeighedValidator`s these need to be sorted by
    /// the `voting_power`.
    pub voting_power: VotingPower,
    pub address: Address,
}

impl<Address> Display for WeightedValidator<Address>
where
    Address: Display
        + Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} with voting power {}",
            self.address, self.voting_power
        )
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    BorshDeserialize,
    BorshSerialize,
)]
pub struct ValidatorSet<Address>
where
    Address: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize,
{
    /// Active validator set with maximum size equal to `max_validator_slots`
    /// in [`PosParams`].
    pub active: BTreeSet<WeightedValidator<Address>>,
    /// All the other validators that are not active
    pub inactive: BTreeSet<WeightedValidator<Address>>,
}

#[derive(Debug, Clone, Copy, BorshDeserialize, BorshSerialize)]
pub enum ValidatorState {
    Inactive,
    Pending,
    Candidate,
    // TODO consider adding `Jailed`
}

#[derive(Debug, Clone, Default, BorshDeserialize, BorshSerialize)]
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

#[derive(Debug, Clone, Default, BorshDeserialize, BorshSerialize)]
pub struct Unbond<Token: Default> {
    /// A key is a pair of the epoch of the bond from which a unbond was
    /// created the epoch of unbonding. This is needed for slash epoch range
    /// check.
    pub deltas: HashMap<(Epoch, Epoch), Token>,
}

impl VotingPower {
    pub fn from_tokens(tokens: impl Into<u64>, params: &PosParams) -> Self {
        Self(params.votes_per_token * tokens.into() / 1_000_000)
    }
}

impl VotingPowerDelta {
    pub fn from_token_change(
        change: impl Into<i128>,
        params: &PosParams,
    ) -> Result<Self, TryFromIntError> {
        let delta: i128 = params.votes_per_token * change.into() / 1_000_000;
        let delta: i64 = TryFrom::try_from(delta)?;
        Ok(Self(delta))
    }

    pub fn try_from_tokens(
        tokens: impl Into<u64>,
        params: &PosParams,
    ) -> Result<Self, TryFromIntError> {
        let delta: i64 = TryFrom::try_from(
            params.votes_per_token * tokens.into() / 1_000_000,
        )?;
        Ok(Self(delta))
    }
}

impl TryFrom<VotingPower> for VotingPowerDelta {
    type Error = TryFromIntError;

    fn try_from(value: VotingPower) -> Result<Self, Self::Error> {
        let delta: i64 = TryFrom::try_from(value.0)?;
        Ok(Self(delta))
    }
}

impl Display for VotingPower {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for VotingPowerDelta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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

impl Add<u64> for Epoch {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        Epoch(self.0 + rhs)
    }
}

impl Add<usize> for Epoch {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Epoch(self.0 + rhs as u64)
    }
}

impl Sub<u64> for Epoch {
    type Output = Epoch;

    fn sub(self, rhs: u64) -> Self::Output {
        Epoch(self.0 - rhs)
    }
}

impl Sub<Epoch> for Epoch {
    type Output = Self;

    fn sub(self, rhs: Epoch) -> Self::Output {
        Epoch(self.0 - rhs.0)
    }
}

impl Display for Epoch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<Address> Display for BondId<Address>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{source: {}, validator: {}}}",
            self.source, self.validator
        )
    }
}

impl<Token> Bond<Token>
where
    Token: Clone + Copy + Add<Output = Token> + Default,
{
    /// Find the sum of all the bonds amounts.
    pub fn sum(&self) -> Token {
        self.delta
            .iter()
            .fold(Default::default(), |acc, (_epoch, amount)| acc + *amount)
    }
}

impl<Token> Add for Bond<Token>
where
    Token: Clone + AddAssign + Default,
{
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        // This is almost the same as `self.delta.extend(rhs.delta);`, except
        // that we add values where a key is present on both sides.
        let iter = rhs.delta.into_iter();
        let reserve = if self.delta.is_empty() {
            iter.size_hint().0
        } else {
            (iter.size_hint().0 + 1) / 2
        };
        self.delta.reserve(reserve);
        iter.for_each(|(k, v)| {
            // Add or insert
            match self.delta.get_mut(&k) {
                Some(value) => *value += v,
                None => {
                    self.delta.insert(k, v);
                }
            }
        });
        self
    }
}

impl<Token> Unbond<Token>
where
    Token: Clone + Copy + Add<Output = Token> + Default,
{
    /// Find the sum of all the unbonds amounts.
    pub fn sum(&self) -> Token {
        self.deltas
            .iter()
            .fold(Default::default(), |acc, (_epoch, amount)| acc + *amount)
    }
}

impl<Token> Add for Unbond<Token>
where
    Token: Clone + AddAssign + Default,
{
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        // This is almost the same as `self.deltas.extend(rhs.deltas);`, except
        // that we add values where a key is present on both sides.
        let iter = rhs.deltas.into_iter();
        let reserve = if self.deltas.is_empty() {
            iter.size_hint().0
        } else {
            (iter.size_hint().0 + 1) / 2
        };
        self.deltas.reserve(reserve);
        iter.for_each(|(k, v)| {
            // Add or insert
            match self.deltas.get_mut(&k) {
                Some(value) => *value += v,
                None => {
                    self.deltas.insert(k, v);
                }
            }
        });
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

impl Add for VotingPower {
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

impl Add for VotingPowerDelta {
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

impl Sub for VotingPowerDelta {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Sub<i64> for VotingPowerDelta {
    type Output = Self;

    fn sub(self, rhs: i64) -> Self::Output {
        Self(self.0 - rhs)
    }
}

impl<Address, Token, PK> GenesisValidator<Address, Token, PK>
where
    Token: Copy + Into<u64>,
{
    /// Calculate validator's voting power
    pub fn voting_power(&self, params: &PosParams) -> VotingPower {
        VotingPower::from_tokens(self.tokens, params)
    }
}

#[cfg(test)]
pub mod tests {

    use std::ops::Range;

    use proptest::prelude::*;

    use super::*;

    /// Generate arbitrary epoch in given range
    pub fn arb_epoch(range: Range<u64>) -> impl Strategy<Value = Epoch> {
        range.prop_map(Epoch)
    }
}
