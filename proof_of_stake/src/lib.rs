use std::collections::{BTreeSet, HashMap};
use std::convert::TryInto;
use std::{cmp, ops};

type Address = String;
type Validator = Address;
type TokenAmount = u64;
type TokenChange = i128;
struct BondId {
    source: Address,
    validator: Validator,
}
type Epoch = u64;
// TODO parameterize
type PublicKey = String;
type VotingPower = u64;

#[derive(Debug, Clone)]
struct Epoched<Data> {
    /// The epoch in which this data was last updated
    last_update: Epoch,
    data: Vec<Option<Data>>,
}

#[derive(Debug, Clone)]
struct EpochedDelta<Data>
where
    Data: Copy + ops::Add<Output = Data>,
{
    /// The epoch in which this data was last updated
    last_update: Epoch,
    data: Vec<Option<Data>>,
}

/// Which offset should be used to set data. The value is read from
/// [`PosParams`].
#[derive(Debug, Clone)]
pub enum EpochOffset {
    PipelineLen,
    UnbondingLen,
}

impl EpochOffset {
    /// Find the length of a given offset from PoS parameters.
    pub fn length(&self, params: &PosParams) -> u64 {
        match self {
            EpochOffset::PipelineLen => params.pipeline_len,
            EpochOffset::UnbondingLen => params.unbonding_len,
        }
    }
}

impl<Data> Epoched<Data> {
    /// Initialize new epoched data. Sets the head to the given value.
    /// This should only be used at genesis.
    pub fn init_at_genesis(value: Data, epoch: Epoch) -> Self {
        Self {
            last_update: epoch,
            data: vec![Some(value)],
        }
    }

    /// Initialize new data at the given epoch offset.
    pub fn init(
        value: Data,
        epoch: Epoch,
        offset: EpochOffset,
        params: &PosParams,
    ) -> Self {
        let offset_len = offset.length(params);
        let mut data = vec![];
        for ix in 0..offset_len {
            data.push(None);
        }
        data.push(Some(value));
        Self {
            last_update: epoch,
            data,
        }
    }

    /// Update the value at the given offset.
    pub fn update(
        &mut self,
        value: Data,
        epoch: Epoch,
        offset: EpochOffset,
        params: &PosParams,
    ) {
        let offset_len = offset.length(params);
        let offset = offset_len as usize;
        let last_update = self.last_update;
        let shift = cmp::min((epoch - last_update) as usize, offset);

        // Resize the data if needed
        if self.data.len() < offset {
            self.data.resize_with(offset, Default::default);
        }

        if shift != 0 {
            let mid_point = cmp::min(shift, self.data.len());
            let mut latest_value: Option<Data> = None;
            // Find the latest value in and clear all the elements before the
            // mid-point
            for i in 0..mid_point {
                if let Some(data) = self.data[i] {
                    latest_value = Some(data);
                }
                self.data[i] = None;
            }
            // Rotate left on the mid-point
            self.data.rotate_left(mid_point);
        }

        self.data[offset] = Some(value);
        self.last_update = epoch;
    }
}

impl<Data> EpochedDelta<Data>
where
    Data: Copy + ops::Add<Output = Data>,
{
    /// Initialize new epoched delta data. Sets the head to the given value.
    /// This should only be used at genesis.
    pub fn init_at_genesis(value: Data, epoch: Epoch) -> Self {
        Self {
            last_update: epoch,
            data: vec![Some(value)],
        }
    }

    /// Initialize new data at the given epoch offset.
    pub fn init(
        value: Data,
        epoch: Epoch,
        offset: EpochOffset,
        params: &PosParams,
    ) -> Self {
        let offset_len = offset.length(params);
        let mut data = vec![];
        for ix in 0..offset_len {
            data.push(None);
        }
        data.push(Some(value));
        Self {
            last_update: epoch,
            data,
        }
    }

    /// Update the value at the given offset.
    pub fn update(
        &mut self,
        value: Data,
        epoch: Epoch,
        offset: EpochOffset,
        params: &PosParams,
    ) {
        let offset_len = offset.length(params);
        let offset = offset_len as usize;
        let last_update = self.last_update;
        let shift = cmp::min((epoch - last_update) as usize, offset);

        // Resize the data if needed
        if self.data.len() < offset {
            self.data.resize_with(offset, Default::default);
        }

        if shift != 0 {
            let mid_point = cmp::min(shift, self.data.len());
            let mut sum: Option<Data> = None;
            // Sum and clear all the elements before the mid-point
            for i in 0..mid_point {
                if let Some(next) = self.data.get(i) {
                    // Add current to the sum, if any
                    sum = match (sum, next) {
                        (Some(sum), Some(next)) => Some(sum + *next),
                        (Some(sum), None) => Some(sum),
                        (None, Some(next)) => Some(*next),
                        _ => sum,
                    };
                    // Clear the field
                    self.data[i] = None;
                }
            }
            // Rotate left on the mid-point
            self.data.rotate_left(mid_point);
            // Update the head with the sum
            self.data[0] = sum;
        }

        self.data[offset] = self.data[shift]
            .map_or_else(|| Some(value), |last_delta| Some(last_delta + value));
        self.last_update = epoch;
    }
}

struct InitialPosStorage {
    validators: Vec<ValidatorData>,
    bonds: Bonds,
    validator_sets: ValidatorSets,
    total_voting_power: TotalVotingPower,
}

struct ValidatorData {
    pub consensus_key: Epoched<PublicKey>,
    pub state: Epoched<ValidatorState>,
    pub total_deltas: Epoched<TokenChange>,
    pub voting_power: Epoched<VotingPower>,
}

/// Validator's address with its voting power.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct WeightedValidator {
    /// The `voting_power` field must be on top, because lexicographic ordering
    /// is based on the top-to-bottom declaration order and in the
    /// `ValidatorSet` the `WeighedValidator`s these need to be sorted by
    /// the `voting_power`.
    voting_power: VotingPower,
    address: Address,
}

struct ValidatorSet {
    /// Active validator set with maximum size equal to `max_active_validators`
    active: BTreeSet<WeightedValidator>,
    /// All the other validators that are not active
    inactive: BTreeSet<WeightedValidator>,
}

type ValidatorSets = Epoched<ValidatorSet>;

/// The sum of all active and inactive validators' voting power
type TotalVotingPower = Epoched<VotingPower>;

trait Pos {
    // TODO consider paraterizing types like so:
    // type Address;

    fn read_validator_consensus_key(
        &self,
        validator: Address,
    ) -> Option<Epoched<PublicKey>>;

    fn write_validator_consensus_key(
        &mut self,
        validator: Address,
        key: Epoched<PublicKey>,
    );
    fn write_validator_state(
        &mut self,
        validator: Address,
        key: Epoched<ValidatorState>,
    );
    fn write_validator_total_deltas(
        &mut self,
        validator: Address,
        key: Epoched<i128>,
    );
    fn write_validator_voting_power(
        &mut self,
        validator: Address,
        key: Epoched<u64>,
    );
}

fn init_genesis(
    pos: &mut impl Pos,
    params: &PosParams,
    validators: Vec<(Validator, TokenAmount, PublicKey)>,
    epoch: Epoch,
) {
    for (addr, tokens, pk) in validators {
        let consensus_key = Epoched::init_at_genesis(pk, epoch);
        pos.write_validator_consensus_key(addr, consensus_key);
        let state = Epoched::init_at_genesis(ValidatorState::Candidate, epoch);
        pos.write_validator_state(addr, state);
        let total_deltas = Epoched::init_at_genesis(tokens as i128, epoch);
        pos.write_validator_total_deltas(addr, total_deltas);
        let voting_power = Epoched::init_at_genesis(
            tokens * 10_000 / params.votes_per_token,
            epoch,
        );
        pos.write_validator_voting_power(addr, voting_power);
    }

    let mut bonds = HashMap::default;
    for (addr, tokens, pk) in validators.iter() {
        let bond_id = BondId {
            source: addr.clone(),
            validator: addr.clone(),
        };
        let mut delta = HashMap::default();
        delta.insert(epoch, tokens);
        let bond = Epoched::init_at_genesis(Bond { delta }, epoch);
        bonds.insert(bond_id, bond);
    }

    let mut active_validators: BTreeSet<WeightedValidator> = validators
        .iter()
        .map(|(address, tokens, pk)| WeightedValidator {
            voting_power: tokens * 10_000 / params.votes_per_token,
            address,
        })
        .collect();
    let mut inactive_validators: BTreeSet<WeightedValidator> =
        BTreeSet::default();
    while active_validators.len() > params.max_validator_slots as usize {
        match active_validators.pop_first_shim() {
            Some(first) => {
                inactive_validators.insert(first);
            }
            None => break,
        }
    }
    let mut validator_set = ValidatorSet {
        active: active_validators,
        inactive: inactive_validators,
    };
    let validator_sets = Epoched::init_at_genesis(validator_set, epoch);

    let total_voting_power = todo!();
    let mut storage = InitialPosStorage {
        validators: (),
        bonds: (),
        validator_sets: (),
        total_voting_power: (),
    };
    let mut state = EpochState {
        bonds: HashMap::default(),
        validators: validators.map(|(validator, tokens)| {
            (validator, ValidatorState::Active, tokens)
        }),
    };
    for (validator, tokens) in validators {
        let bond = Bond {
            delegator: validator.clone(),
            validators: (),
            amount: (),
        };
    }
    for epoch_ix in 0..params.pipeline_len {
        let epoch = Epoch(epoch_ix);
    }
}

trait BTreeSetShims<T> {
    fn first_shim(&self) -> Option<&T>;
    fn last_shim(&self) -> Option<&T>;
    fn pop_first_shim(&mut self) -> Option<T>;
    fn pop_last_shim(&mut self) -> Option<T>;
}

impl<T: Ord> BTreeSetShims<T> for BTreeSet<T> {
    fn pop_last_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let last = iter.last().map(|last| *last);
        if let Some(last) = last {
            self.remove(&last);
        }
        last
    }

    fn first_shim(&self) -> Option<&T> {
        let iter = self.iter();
        iter.next()
    }

    fn last_shim(&self) -> Option<&T> {
        let iter = self.iter();
        iter.last()
    }

    fn pop_first_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let first = iter.last().map(|first| *first);
        if let Some(first) = first {
            self.remove(&first);
        }
        first
    }
}

struct EpochState {
    bonds: HashMap<BondId, Bond>,
    validators: Vec<(Validator, ValidatorState, Tokens)>,
}

struct EpochDelta {
    bond_changes: Vec<(BondId, Tokens)>,
    validators_changes: Vec<(Validator, ValidatorState, ShareChange)>,
}

enum ValidatorState {
    Inactive,
    Pending,
    Candidate,
    // TODO consider adding `Jailed`
}

type Bonds = HashMap<BondId, Epoched<Bond>>;
type Unbonds = HashMap<BondId, Epoched<Unbond>>;

#[derive(Debug)]
struct Bond {
    /// A key is a the epoch set for the bond. This is used in unbonding, where
    // it's needed for slash epoch range check.
    delta: HashMap<Epoch, TokenAmount>,
}

struct Unbond {
    /// A key is a pair of the epoch of the bond from which a unbond was
    /// created the epoch of unboding. This is needed for slash epoch range
    /// check.
    deltas: HashMap<(Epoch, Epoch), TokenAmount>,
}

struct PosParams {
    /// A maximum number of [`ValidatorState::Active`] validators
    max_validator_slots: u64,
    /// Any change applied during an epoch `n` will become active at the
    /// beginning of epoch `n + pipeline_len`.
    pipeline_len: u64,
    /// How many epochs after a committed fault a validator can be slashed.
    /// If a fault is detected in epoch `n`, it can slashed up until the end of
    /// `n + slashable_period_len` epoch.
    unbonding_len: u64,
    /// Used in validators' voting power calculation
    votes_per_token: u64,
    /// Amount of tokens rewarded to a validator for proposing a block
    block_proposer_reward: u64,
    /// Amount of tokens rewarded to each validator that voted on a block
    /// proposal
    block_vote_reward: u64,
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

impl Bond {
    fn current_validator(&self) -> Address {
        return self
            .validators
            .last()
            .expect("Error retrieving current validator.")
            .1
            .expect("Bond is in the process of unbonding.");
    }

    fn add_to_bond(self, tokens: Tokens) -> Bond {
        let updated_amount = self.amount + tokens;
        let updated_bond = Bond {
            amount: updated_amount,
            ..self
        };
        return updated_bond;
        // add ledger interaction
    }

    fn delegate(
        current_epoch: Epoch,
        delegator_address: Address,
        validator_address: Address,
        tokens: Tokens,
    ) -> () {
        // create transaction to lock tokens [still needed]
        // submit it for processing [ledger interaction]

        let delegation = Bond {
            delegator: delegator_address,
            validators: vec![(current_epoch, Some(validator_address))],
            amount: tokens,
        };

        // increment validator voting power
        let voting_change: Result<u64, i64> = tokens.try_into();

        // TO FIX: ValidatorState enum should be read from the EpochState, for
        // now set to Active
        let delta: EpochDelta =
            vec![(validator_address, ValidatorState::Active, voting_change)];
    }

    fn redelegate(current_epoch: Epoch, bond_id: BondId) -> () {}

    // fn undelegate
    // fn complete_undelegate
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
