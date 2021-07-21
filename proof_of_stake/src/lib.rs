use core::convert::TryInto;
use core::marker::PhantomData;
use core::{cmp, fmt, ops};
use std::collections::{BTreeSet, HashMap};

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
struct Epoched<Data, Offset>
where
    Data: Clone,
    Offset: EpochOffset,
{
    /// The epoch in which this data was last updated
    last_update: Epoch,
    data: Vec<Option<Data>>,
    offset: PhantomData<Offset>,
}

#[derive(Debug, Clone)]
struct EpochedDelta<Data, Offset>
where
    Data: Copy + ops::Add<Output = Data>,
    Offset: EpochOffset,
{
    /// The epoch in which this data was last updated
    last_update: Epoch,
    data: Vec<Option<Data>>,
    offset: PhantomData<Offset>,
}

/// Which offset should be used to set data. The value is read from
/// [`PosParams`].
trait EpochOffset: fmt::Debug + Clone {
    /// Find the value of a given offset from PoS parameters.
    fn value(params: &PosParams) -> u64;
}
#[derive(Debug, Clone)]
struct OffsetPipelineLen;
impl EpochOffset for OffsetPipelineLen {
    fn value(params: &PosParams) -> u64 {
        params.pipeline_len
    }
}
#[derive(Debug, Clone)]
struct OffsetUnboundingLen;
impl EpochOffset for OffsetUnboundingLen {
    fn value(params: &PosParams) -> u64 {
        params.unbonding_len
    }
}

impl<Data, Offset> Epoched<Data, Offset>
where
    Data: fmt::Debug + Clone,
    Offset: EpochOffset,
{
    /// Initialize new epoched data. Sets the head to the given value.
    /// This should only be used at genesis.
    pub fn init_at_genesis(value: Data, epoch: Epoch) -> Self {
        Self {
            last_update: epoch,
            data: vec![Some(value)],
            offset: PhantomData,
        }
    }

    /// Initialize new data at the data's epoch offset.
    pub fn init(value: Data, epoch: Epoch, params: &PosParams) -> Self {
        let offset = Offset::value(params);
        let mut data = vec![];
        for ix in 0..offset {
            data.push(None);
        }
        data.push(Some(value));
        Self {
            last_update: epoch,
            data,
            offset: PhantomData,
        }
    }

    /// Find the value for the given epoch.
    pub fn get(&self, epoch: Epoch) -> Option<&Data> {
        let offset = (epoch - self.last_update) as usize;
        let mut index = cmp::min(offset, self.data.len());
        loop {
            if let Some(result @ Some(_)) = self.data.get(index) {
                return result.as_ref();
            }
            if index == 0 {
                return None;
            } else {
                index -= 1;
            }
        }
    }

    /// Update the value at the data's epoch offset.
    pub fn update(&mut self, value: Data, epoch: Epoch, params: &PosParams) {
        debug_assert!(
            epoch >= self.last_update,
            "The current epoch must be greater than or equal to the last \
             update"
        );
        let offset = Offset::value(params) as usize;
        let last_update = self.last_update;
        let shift = cmp::min((epoch - last_update) as usize, offset);

        // Resize the data if needed
        if self.data.len() < offset + 1 {
            self.data.resize_with(offset + 1, Default::default);
        }

        if shift != 0 {
            let mid_point = cmp::min(shift, self.data.len());
            let mut latest_value: Option<Data> = None;
            // Find the latest value in and clear all the elements before the
            // mid-point
            for i in 0..mid_point {
                if let Some(Some(data)) = self.data.get(i) {
                    latest_value = Some(data.clone());
                }
                self.data[i] = None;
            }
            // Rotate left on the mid-point
            self.data.rotate_left(mid_point);
            // Update the head with the latest value
            self.data[0] = latest_value;
        }

        self.data[offset] = Some(value);
        self.last_update = epoch;
    }
}

impl<Data, Offset> EpochedDelta<Data, Offset>
where
    Data: Copy + ops::Add<Output = Data>,
    Offset: EpochOffset,
{
    /// Initialize new epoched delta data. Sets the head to the given value.
    /// This should only be used at genesis.
    pub fn init_at_genesis(value: Data, epoch: Epoch) -> Self {
        Self {
            last_update: epoch,
            data: vec![Some(value)],
            offset: PhantomData,
        }
    }

    /// Initialize new data at the data's epoch offset.
    pub fn init(value: Data, epoch: Epoch, params: &PosParams) -> Self {
        let offset = Offset::value(params) as usize;
        let mut data = vec![];
        for ix in 0..offset {
            data.push(None);
        }
        data.push(Some(value));
        Self {
            last_update: epoch,
            data,
            offset: PhantomData,
        }
    }

    /// Find the sum of delta values for the given epoch.
    pub fn get(&self, epoch: Epoch) -> Option<Data> {
        let offset = (epoch - self.last_update) as usize;
        let index = cmp::min(offset, self.data.len());
        let mut sum: Option<Data> = None;
        for i in 0..index + 1 {
            if let Some(next) = self.data.get(i) {
                // Add current to the sum, if any
                sum = match (sum, next) {
                    (Some(sum), Some(next)) => Some(sum + *next),
                    (Some(sum), None) => Some(sum),
                    (None, Some(next)) => Some(*next),
                    _ => sum,
                };
            }
        }
        sum
    }

    /// Update the value at the data's epoch offset.
    pub fn update(&mut self, value: Data, epoch: Epoch, params: &PosParams) {
        debug_assert!(
            epoch >= self.last_update,
            "The current epoch must be greater than or equal to the last \
             update"
        );
        let offset = Offset::value(params) as usize;
        let last_update = self.last_update;
        let shift = cmp::min((epoch - last_update) as usize, offset);

        // Resize the data if needed
        if self.data.len() < offset + 1 {
            self.data.resize_with(offset + 1, Default::default);
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

// struct InitialPosStorage {
//     validators: Vec<ValidatorData>,
//     bonds: Bonds,
//     validator_sets: ValidatorSets,
//     total_voting_power: TotalVotingPower,
// }

// struct ValidatorData {
//     pub consensus_key: Epoched<PublicKey>,
//     pub state: Epoched<ValidatorState>,
//     pub total_deltas: Epoched<TokenChange>,
//     pub voting_power: Epoched<VotingPower>,
// }

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

type ValidatorSets = Epoched<ValidatorSet, OffsetUnboundingLen>;

/// The sum of all active and inactive validators' voting power
type TotalVotingPower = Epoched<VotingPower, OffsetUnboundingLen>;

trait Pos {
    // TODO consider paraterizing types like so:
    // type Address;

    fn read_validator_consensus_key(
        &self,
        validator: Address,
    ) -> Option<Epoched<PublicKey, OffsetPipelineLen>>;

    fn write_validator_consensus_key(
        &mut self,
        validator: Address,
        key: Epoched<PublicKey, OffsetPipelineLen>,
    );
    fn write_validator_state(
        &mut self,
        validator: Address,
        key: Epoched<ValidatorState, OffsetPipelineLen>,
    );
    fn write_validator_total_deltas(
        &mut self,
        validator: Address,
        key: Epoched<i128, OffsetUnboundingLen>,
    );
    fn write_validator_voting_power(
        &mut self,
        validator: Address,
        key: Epoched<u64, OffsetUnboundingLen>,
    );
}

fn init_genesis(
    pos: &mut impl Pos,
    params: &PosParams,
    validators: Vec<(Validator, TokenAmount, PublicKey)>,
    epoch: Epoch,
) {
    todo!()
    // for (adDr, tokens, pk) in validators {
    //     let consensus_key = Epoched::init_at_genesis(pk, epoch);
    //     pos.write_validator_consensus_key(addr, consensus_key);
    //     let state = Epoched::init_at_genesis(ValidatorState::Candidate,
    // epoch);     pos.write_validator_state(addr, state);
    //     let total_deltas = Epoched::init_at_genesis(tokens as i128, epoch);
    //     pos.write_validator_total_deltas(addr, total_deltas);
    //     let voting_power = Epoched::init_at_genesis(
    //         tokens * 10_000 / params.votes_per_token,
    //         epoch,
    //     );
    //     pos.write_validator_voting_power(addr, voting_power);
    // }

    // let mut bonds = HashMap::default;
    // for (addr, tokens, pk) in validators.iter() {
    //     let bond_id = BondId {
    //         source: addr.clone(),
    //         validator: addr.clone(),
    //     };
    //     let mut delta = HashMap::default();
    //     delta.insert(epoch, tokens);
    //     let bond = Epoched::init_at_genesis(Bond { delta }, epoch);
    //     bonds.insert(bond_id, bond);
    // }

    // let mut active_validators: BTreeSet<WeightedValidator> = validators
    //     .iter()
    //     .map(|(address, tokens, pk)| WeightedValidator {
    //         voting_power: tokens * 10_000 / params.votes_per_token,
    //         address,
    //     })
    //     .collect();
    // let mut inactive_validators: BTreeSet<WeightedValidator> =
    //     BTreeSet::default();
    // while active_validators.len() > params.max_validator_slots as usize {
    //     match active_validators.pop_first_shim() {
    //         Some(first) => {
    //             inactive_validators.insert(first);
    //         }
    //         None => break,
    //     }
    // }
    // let mut validator_set = ValidatorSet {
    //     active: active_validators,
    //     inactive: inactive_validators,
    // };
    // let validator_sets = Epoched::init_at_genesis(validator_set, epoch);

    // let total_voting_power = todo!();
    // let mut storage = InitialPosStorage {
    //     validators: (),
    //     bonds: (),
    //     validator_sets: (),
    //     total_voting_power: (),
    // };
    // let mut state = EpochState {
    //     bonds: HashMap::default(),
    //     validators: validators.map(|(validator, tokens)| {
    //         (validator, ValidatorState::Active, tokens)
    //     }),
    // };
    // for (validator, tokens) in validators {
    //     let bond = Bond {
    //         delegator: validator.clone(),
    //         validators: (),
    //         amount: (),
    //     };
    // }
    // for epoch_ix in 0..params.pipeline_len {
    //     let epoch = Epoch(epoch_ix);
    // }
}

/// This trait add shims for BTreeSet methods that not yet stable. They have the
/// same behavior as their nightly counterparts, but additionally require
/// `Clone` bound on the element type (for `pop_first` and `pop_last`).
trait BTreeSetShims<T> {
    fn first_shim(&self) -> Option<&T>;
    fn last_shim(&self) -> Option<&T>;
    fn pop_first_shim(&mut self) -> Option<T>;
    fn pop_last_shim(&mut self) -> Option<T>;
}

impl<T: Ord + Clone> BTreeSetShims<T> for BTreeSet<T> {
    /// Returns a reference to the first value in the set, if any. This value is
    /// always the minimum of all values in the set.
    fn first_shim(&self) -> Option<&T> {
        let mut iter = self.iter();
        iter.next()
    }

    /// Returns a reference to the last value in the set, if any. This value is
    /// always the maximum of all values in the set.
    fn last_shim(&self) -> Option<&T> {
        let iter = self.iter();
        iter.last()
    }

    /// Removes the first value from the set and returns it, if any. The first
    /// value is always the minimum value in the set.
    fn pop_first_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let first = iter.last().cloned();
        if let Some(first) = first {
            return self.take(&first);
        }
        None
    }

    /// Removes the last value from the set and returns it, if any. The last
    /// value is always the maximum value in the set.
    fn pop_last_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let last = iter.last().cloned();
        if let Some(last) = last {
            return self.take(&last);
        }
        None
    }
}

#[derive(Debug, Clone, Copy)]
enum ValidatorState {
    Inactive,
    Pending,
    Candidate,
    // TODO consider adding `Jailed`
}

type Bonds = HashMap<BondId, Epoched<Bond, OffsetPipelineLen>>;
type Unbonds = HashMap<BondId, Epoched<Unbond, OffsetUnboundingLen>>;

#[derive(Debug)]
pub struct Bond {
    /// A key is a the epoch set for the bond. This is used in unbonding, where
    // it's needed for slash epoch range check.
    delta: HashMap<Epoch, TokenAmount>,
}

pub struct Unbond {
    /// A key is a pair of the epoch of the bond from which a unbond was
    /// created the epoch of unboding. This is needed for slash epoch range
    /// check.
    deltas: HashMap<(Epoch, Epoch), TokenAmount>,
}

#[derive(Debug, Clone)]
pub struct PosParams {
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

// TODO
// impl Bond {
//     fn current_validator(&self) -> Address {
//         return self
//             .validators
//             .last()
//             .expect("Error retrieving current validator.")
//             .1
//             .expect("Bond is in the process of unbonding.");
//     }

//     fn add_to_bond(self, tokens: Tokens) -> Bond {
//         let updated_amount = self.amount + tokens;
//         let updated_bond = Bond {
//             amount: updated_amount,
//             ..self
//         };
//         return updated_bond;
//         // add ledger interaction
//     }

//     fn delegate(
//         current_epoch: Epoch,
//         delegator_address: Address,
//         validator_address: Address,
//         tokens: Tokens,
//     ) -> () {
//         // create transaction to lock tokens [still needed]
//         // submit it for processing [ledger interaction]

//         let delegation = Bond {
//             delegator: delegator_address,
//             validators: vec![(current_epoch, Some(validator_address))],
//             amount: tokens,
//         };

//         // increment validator voting power
//         let voting_change: Result<u64, i64> = tokens.try_into();

//         // TO FIX: ValidatorState enum should be read from the EpochState,
// for         // now set to Active
//         let delta: EpochDelta =
//             vec![(validator_address, ValidatorState::Active, voting_change)];
//     }

//     fn redelegate(current_epoch: Epoch, bond_id: BondId) -> () {}

//     // fn undelegate
//     // fn complete_undelegate
// }

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    // use proptest::prop_state_machine;
    use proptest::state_machine::{AbstractStateMachine, StateMachineTest};

    use super::*;

    /// Abstract state machine model for [`Epoched`].
    #[derive(Clone, Debug)]
    struct AbstractEpoched<Data> {
        init_at_genesis: bool,
        params: PosParams,
        last_update: Epoch,
        data: HashMap<Epoch, Data>,
    }

    #[derive(Clone, Debug)]
    enum EpochedTransition<Data> {
        Get(Epoch),
        Update(Data, Epoch),
    }

    /// Abstract state machine implementation for [`Epoched`].
    struct EpochedStateMachine<Offset: EpochOffset> {
        phantom: PhantomData<Offset>,
    }
    impl<Offset> AbstractStateMachine for EpochedStateMachine<Offset>
    where
        Offset: EpochOffset,
    {
        type State = AbstractEpoched<u64>;
        type Transition = EpochedTransition<u64>;

        fn init_state() -> BoxedStrategy<Self::State> {
            prop_oneof![
                // Initialized at genesis
                (arb_pos_params(), 0_u64..1_000_000, any::<u64>()).prop_map(
                    |(params, epoch, initial)| {
                        let mut data = HashMap::default();
                        data.insert(epoch, initial);
                        AbstractEpoched {
                            init_at_genesis: true,
                            params,
                            last_update: epoch,
                            data,
                        }
                    }
                ),
                // Initialized after genesis
                (arb_pos_params(), 0_u64..1_000_000, any::<u64>()).prop_map(
                    |(params, epoch, initial)| {
                        let offset = Offset::value(&params);
                        let mut data = HashMap::default();
                        data.insert(epoch + offset, initial);
                        AbstractEpoched {
                            init_at_genesis: false,
                            params,
                            last_update: epoch,
                            data,
                        }
                    }
                ),
            ]
            .boxed()
        }

        fn transitions(state: &Self::State) -> BoxedStrategy<Self::Transition> {
            let offset = Offset::value(&state.params);
            prop_oneof![
                (state.last_update..state.last_update + 4 * offset)
                    .prop_map(EpochedTransition::Get),
                (
                    any::<u64>(),
                    // Update's epoch may not be lower than the last_update
                    state.last_update..state.last_update + 10,
                )
                    .prop_map(|(value, epoch)| {
                        EpochedTransition::Update(value, epoch)
                    })
            ]
            .boxed()
        }

        fn apply_abstract(
            mut state: Self::State,
            transition: &Self::Transition,
        ) -> Self::State {
            match transition {
                EpochedTransition::Get(_epoch) => {
                    // no side effects
                }
                EpochedTransition::Update(value, epoch) => {
                    let offset = Offset::value(&state.params);
                    state.last_update = *epoch;
                    state.data.insert(epoch + offset, *value);
                }
            }
            state
        }
    }

    impl<Offset> StateMachineTest for EpochedStateMachine<Offset>
    where
        Offset: EpochOffset,
    {
        type Abstract = Self;
        type ConcreteState = (PosParams, Epoched<u64, Offset>);

        fn init_test(
            initial_state: <Self::Abstract as AbstractStateMachine>::State,
        ) -> Self::ConcreteState {
            assert!(initial_state.data.len() == 1);
            let data = if initial_state.init_at_genesis {
                let genesis_epoch = initial_state.last_update;
                let value = initial_state.data.get(&genesis_epoch).unwrap();
                Epoched::init_at_genesis(*value, genesis_epoch)
            } else {
                let (key, value) = initial_state.data.iter().next().unwrap();
                let data = Epoched::init(
                    *value,
                    initial_state.last_update,
                    &initial_state.params,
                );
                assert_eq!(
                    Some(*value),
                    data.data[(key - initial_state.last_update) as usize]
                );
                data
            };
            (initial_state.params, data)
        }

        fn apply_concrete(
            (params, mut data): Self::ConcreteState,
            transition: &<Self as AbstractStateMachine>::Transition,
        ) -> Self::ConcreteState {
            let offset = Offset::value(&params);
            match transition {
                EpochedTransition::Get(epoch) => {
                    let value = data.get(*epoch);
                    // Post-conditions
                    let last_update = data.last_update;
                    match value {
                        Some(val) => {
                            // When a value found, it should be the last value
                            // before or on the upper bound
                            let upper_bound = cmp::min(
                                cmp::min(epoch - last_update, offset) as usize
                                    + 1,
                                data.data.len(),
                            );
                            for i in (0..upper_bound).rev() {
                                match data.data[i] {
                                    Some(ref stored_val) => {
                                        assert_eq!(val, stored_val);
                                        break;
                                    }
                                    None => {
                                        // The value must be found on or after 0
                                        // index
                                        assert_ne!(i, 0);
                                    }
                                }
                            }
                        }
                        None => {
                            // When no value found, there should be no values
                            // before the upper bound
                            let upper_bound = cmp::min(
                                cmp::min(epoch - last_update, offset) as usize
                                    + 1,
                                data.data.len(),
                            );
                            for i in 0..upper_bound {
                                assert_eq!(None, data.data[i]);
                            }
                        }
                    }
                }
                EpochedTransition::Update(value, epoch) => {
                    data.update(*value, *epoch, &params);
                    // TODO: Post-conditions
                    assert_eq!(data.last_update, *epoch);
                }
            }
            (params, data)
        }

        fn invariants((params, data): &Self::ConcreteState) {
            let offset = Offset::value(&params);
            assert!(data.data.len() <= (offset + 1) as usize);
        }
    }

    // TODO the macro doesn't work with the generic param on EpochedStateMachine
    // prop_state_machine! {
    //     #[test]
    //     fn run_epoched_state_machine_with_pipeline_offset(
    //         sequential EpochedStateMachine::<OffsetPipelineLen> 1..20)
    // }
    // prop_state_machine! {
    //     #[test]
    //     fn run_epoched_state_machine_with_unbounding_offset(
    //         sequential EpochedStateMachine::<OffsetUnboundingLen> 1..20)
    // }
    type EpochedPipeline = EpochedStateMachine<OffsetPipelineLen>;
    type EpochedUnbonding = EpochedStateMachine<OffsetUnboundingLen>;

    proptest! {

        #[test]
        fn run_epoched_state_machine_with_pipeline_offset(
            (initial_state, transitions) in EpochedPipeline::sequential_strategy(1..20)
        ) {
            EpochedPipeline::test_sequential(initial_state, transitions)
        }

        #[test]
        fn run_epoched_state_machine_with_unbounding_offset(
            (initial_state, transitions) in EpochedUnbonding::sequential_strategy(1..20)
        ) {
            EpochedUnbonding::test_sequential(initial_state, transitions)
        }
    }

    fn arb_pos_params() -> impl Strategy<Value = PosParams> {
        (
            10..500_u64,
            1..10_u64,
            1..10_000_u64,
            1..1_000_u64,
            1..1_000_u64,
        )
            .prop_flat_map(
                |(
                    max_validator_slots,
                    pipeline_len,
                    votes_per_token,
                    block_proposer_reward,
                    block_vote_reward,
                )| {
                    (pipeline_len + 1..pipeline_len + 10).prop_map(
                        move |unbonding_len| PosParams {
                            max_validator_slots,
                            pipeline_len,
                            unbonding_len,
                            votes_per_token,
                            block_proposer_reward,
                            block_vote_reward,
                        },
                    )
                },
            )
    }
}
