//! [`Epoched`] and [`EpochedDelta`] are structures for data that is set for
//! future epochs at a given [`EpochOffset`].

use core::marker::PhantomData;
use core::{cmp, fmt, ops};

use crate::types::Epoch;
use crate::PosParams;

#[derive(Debug, Clone)]
pub struct Epoched<Data, Offset>
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
pub struct EpochedDelta<Data, Offset>
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
pub trait EpochOffset: fmt::Debug + Clone {
    /// Find the value of a given offset from PoS parameters.
    fn value(params: &PosParams) -> u64;
}
#[derive(Debug, Clone)]
pub struct OffsetPipelineLen;
impl EpochOffset for OffsetPipelineLen {
    fn value(params: &PosParams) -> u64 {
        params.pipeline_len
    }
}
#[derive(Debug, Clone)]
pub struct OffsetUnboundingLen;
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
    pub fn init_at_genesis(value: Data, epoch: impl Into<Epoch>) -> Self {
        Self {
            last_update: epoch.into(),
            data: vec![Some(value)],
            offset: PhantomData,
        }
    }

    /// Initialize new data at the data's epoch offset.
    pub fn init(
        value: Data,
        epoch: impl Into<Epoch>,
        params: &PosParams,
    ) -> Self {
        let offset = Offset::value(params);
        let mut data = vec![];
        for _ in 0..offset {
            data.push(None);
        }
        data.push(Some(value));
        Self {
            last_update: epoch.into(),
            data,
            offset: PhantomData,
        }
    }

    /// Find the value for the given epoch.
    pub fn get(&self, epoch: impl Into<Epoch>) -> Option<&Data> {
        let offset: usize = (epoch.into() - self.last_update).into();
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
    pub fn update(
        &mut self,
        value: Data,
        epoch: impl Into<Epoch>,
        params: &PosParams,
    ) {
        let epoch = epoch.into();
        debug_assert!(
            epoch >= self.last_update,
            "The current epoch must be greater than or equal to the last \
             update"
        );
        let offset = Offset::value(params) as usize;
        let last_update = self.last_update;
        let shift: usize = cmp::min((epoch - last_update).into(), offset);

        // Resize the data if needed
        if self.data.len() < offset + 1 {
            self.data.resize_with(offset + 1, Default::default);
        }

        if shift != 0 {
            let mid_point = cmp::min(shift, self.data.len());
            let mut latest_value: Option<Data> = None;
            // Find the latest value in elements before the mid-point and clear
            // them
            for i in 0..mid_point + 1 {
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
    Data: fmt::Debug + Copy + ops::Add<Output = Data>,
    Offset: EpochOffset,
{
    /// Initialize new epoched delta data. Sets the head to the given value.
    /// This should only be used at genesis.
    pub fn init_at_genesis(value: Data, epoch: impl Into<Epoch>) -> Self {
        Self {
            last_update: epoch.into(),
            data: vec![Some(value)],
            offset: PhantomData,
        }
    }

    /// Initialize new data at the data's epoch offset.
    pub fn init(
        value: Data,
        epoch: impl Into<Epoch>,
        params: &PosParams,
    ) -> Self {
        let offset = Offset::value(params) as usize;
        let mut data = vec![];
        for _ in 0..offset {
            data.push(None);
        }
        data.push(Some(value));
        Self {
            last_update: epoch.into(),
            data,
            offset: PhantomData,
        }
    }

    /// Find the sum of delta values for the given epoch.
    pub fn get(&self, epoch: impl Into<Epoch>) -> Option<Data> {
        let epoch = epoch.into();
        let offset: usize = (epoch - self.last_update).into();
        let index = cmp::min(offset, self.data.len());
        let mut sum: Option<Data> = None;
        for i in 0..index + 1 {
            if let Some(next) = self.data.get(i) {
                // Add current to the sum, if any
                sum = match (sum, next) {
                    (Some(sum), Some(next)) => Some(sum + *next),
                    (None, Some(next)) => Some(*next),
                    _ => sum,
                };
            }
        }
        sum
    }

    /// Update the value at the data's epoch offset.
    pub fn update(
        &mut self,
        value: Data,
        epoch: impl Into<Epoch>,
        params: &PosParams,
    ) {
        let epoch = epoch.into();
        debug_assert!(
            epoch >= self.last_update,
            "The current epoch must be greater than or equal to the last \
             update"
        );
        let offset = Offset::value(params) as usize;
        let last_update = self.last_update;
        let shift: usize = cmp::min((epoch - last_update).into(), offset);

        // Resize the data if needed
        if self.data.len() < offset + 1 {
            self.data.resize_with(offset + 1, Default::default);
        }

        if shift != 0 {
            let mid_point = cmp::min(shift, self.data.len());
            let mut sum: Option<Data> = None;
            // Sum and clear all the elements before the mid-point
            for i in 0..mid_point + 1 {
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

        self.data[offset] = self.data[offset]
            .map_or_else(|| Some(value), |last_delta| Some(last_delta + value));
        self.last_update = epoch;
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use proptest::prelude::*;
    use proptest::prop_state_machine;
    use proptest::state_machine::{AbstractStateMachine, StateMachineTest};

    use super::*;
    use crate::types::tests::arb_epoch;

    prop_state_machine! {
        #[test]
        fn epoched_state_machine_with_pipeline_offset(
            sequential 1..20 => EpochedAbstractStateMachine<OffsetPipelineLen>);

        #[test]
        fn epoched_state_machine_with_unbounding_offset(
            sequential 1..20 => EpochedAbstractStateMachine<OffsetUnboundingLen>);

        #[test]
        fn epoched_delta_state_machine_with_pipeline_offset(
            sequential 1..20 => EpochedDeltaAbstractStateMachine<OffsetPipelineLen>);

        #[test]
        fn epoched_delta_state_machine_with_unbounding_offset(
            sequential 1..20 => EpochedDeltaAbstractStateMachine<OffsetUnboundingLen>);
    }

    /// Abstract representation of [`Epoched`].
    #[derive(Clone, Debug)]
    struct EpochedState<Data> {
        init_at_genesis: bool,
        params: PosParams,
        last_update: Epoch,
        data: HashMap<Epoch, Data>,
    }

    #[derive(Clone, Debug)]
    enum EpochedTransition<Data> {
        Get(Epoch),
        Update { value: Data, epoch: Epoch },
    }

    /// Abstract state machine implementation for [`Epoched`].
    struct EpochedAbstractStateMachine<Offset: EpochOffset> {
        phantom: PhantomData<Offset>,
    }
    impl<Offset> AbstractStateMachine for EpochedAbstractStateMachine<Offset>
    where
        Offset: EpochOffset,
    {
        type State = EpochedState<u64>;
        type Transition = EpochedTransition<u64>;

        fn init_state() -> BoxedStrategy<Self::State> {
            prop_oneof![
                // Initialized at genesis
                (arb_pos_params(), 0_u64..1_000_000, any::<u64>()).prop_map(
                    |(params, epoch, initial)| {
                        let mut data = HashMap::default();
                        data.insert(epoch.into(), initial);
                        EpochedState {
                            init_at_genesis: true,
                            params,
                            last_update: epoch.into(),
                            data,
                        }
                    }
                ),
                // Initialized after genesis
                (arb_pos_params(), 0_u64..1_000_000, any::<u64>()).prop_map(
                    |(params, epoch, initial)| {
                        let offset = Offset::value(&params);
                        let mut data = HashMap::default();
                        data.insert((epoch + offset).into(), initial);
                        EpochedState {
                            init_at_genesis: false,
                            params,
                            last_update: epoch.into(),
                            data,
                        }
                    }
                ),
            ]
            .boxed()
        }

        fn transitions(state: &Self::State) -> BoxedStrategy<Self::Transition> {
            let offset = Offset::value(&state.params);
            let last_update: u64 = state.last_update.into();
            prop_oneof![
                arb_epoch(last_update..last_update + 4 * offset)
                    .prop_map(EpochedTransition::Get),
                (
                    any::<u64>(),
                    // Update's epoch may not be lower than the last_update
                    arb_epoch(last_update..last_update + 10),
                )
                    .prop_map(|(value, epoch)| {
                        EpochedTransition::Update { value, epoch }
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
                EpochedTransition::Update { value, epoch } => {
                    let offset = Offset::value(&state.params);
                    state.last_update = *epoch;
                    state.data.insert(*epoch + offset, *value);
                }
            }
            state
        }
    }

    impl<Offset> StateMachineTest for EpochedAbstractStateMachine<Offset>
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
                    data.data[usize::from(*key - initial_state.last_update)]
                );
                data
            };
            (initial_state.params, data)
        }

        fn apply_concrete(
            (params, mut data): Self::ConcreteState,
            transition: &<Self::Abstract as AbstractStateMachine>::Transition,
        ) -> Self::ConcreteState {
            let offset = Offset::value(&params) as usize;
            match transition {
                EpochedTransition::Get(epoch) => {
                    let epoch = *epoch;
                    let value = data.get(epoch);
                    // Post-conditions
                    let last_update = data.last_update;
                    match value {
                        Some(val) => {
                            // When a value found, it should be the last value
                            // before or on the upper bound
                            let upper_bound = cmp::min(
                                cmp::min((epoch - last_update).into(), offset)
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
                                cmp::min((epoch - last_update).into(), offset)
                                    + 1,
                                data.data.len(),
                            );
                            for i in 0..upper_bound {
                                assert_eq!(None, data.data[i]);
                            }
                        }
                    }
                }
                EpochedTransition::Update { value, epoch } => {
                    let current_before_update = data.get(*epoch).copied();
                    data.update(*value, *epoch, &params);

                    // Post-conditions
                    assert_eq!(data.last_update, *epoch);
                    assert_eq!(
                        data.data[offset as usize],
                        Some(*value),
                        "The value at offset must be updated"
                    );
                    assert!(
                        data.data.len() > offset as usize,
                        "The length of the data must be greater than the \
                         offset"
                    );
                    assert_eq!(
                        data.get(*epoch),
                        current_before_update.as_ref(),
                        "The current value must not change"
                    );
                }
            }
            (params, data)
        }

        fn invariants((params, data): &Self::ConcreteState) {
            let offset = Offset::value(&params);
            assert!(data.data.len() <= (offset + 1) as usize);
        }
    }

    /// Abstract state machine implementation for [`EpochedDelta`].
    struct EpochedDeltaAbstractStateMachine<Offset: EpochOffset> {
        phantom: PhantomData<Offset>,
    }
    impl<Offset> AbstractStateMachine for EpochedDeltaAbstractStateMachine<Offset>
    where
        Offset: EpochOffset,
    {
        type State = EpochedState<u64>;
        type Transition = EpochedTransition<u64>;

        fn init_state() -> BoxedStrategy<Self::State> {
            prop_oneof![
                // Initialized at genesis
                (arb_pos_params(), arb_epoch(0..1_000_000), 1..10_000_000_u64)
                    .prop_map(|(params, epoch, initial)| {
                        let mut data = HashMap::default();
                        data.insert(epoch, initial);
                        EpochedState {
                            init_at_genesis: true,
                            params,
                            last_update: epoch,
                            data,
                        }
                    }),
                // Initialized after genesis
                (arb_pos_params(), arb_epoch(0..1_000_000), 1..10_000_000_u64)
                    .prop_map(|(params, epoch, initial)| {
                        let offset = Offset::value(&params);
                        let mut data = HashMap::default();
                        data.insert(epoch + offset, initial);
                        EpochedState {
                            init_at_genesis: false,
                            params,
                            last_update: epoch,
                            data,
                        }
                    }),
            ]
            .boxed()
        }

        fn transitions(state: &Self::State) -> BoxedStrategy<Self::Transition> {
            let offset = Offset::value(&state.params);
            let last_update: u64 = state.last_update.into();
            prop_oneof![
                (last_update..last_update + 4 * offset)
                    .prop_map(|epoch| EpochedTransition::Get(epoch.into())),
                (
                    1..10_000_000_u64,
                    // Update's epoch may not be lower than the last_update
                    last_update..last_update + 10,
                )
                    .prop_map(|(value, epoch)| {
                        EpochedTransition::Update {
                            value,
                            epoch: epoch.into(),
                        }
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
                EpochedTransition::Update {
                    value: change,
                    epoch,
                } => {
                    let epoch = *epoch;
                    let offset = Offset::value(&state.params);
                    state.last_update = epoch;
                    let current = state.data.entry(epoch + offset).or_insert(0);
                    *current += *change;
                }
            }
            state
        }
    }

    impl<Offset> StateMachineTest for EpochedDeltaAbstractStateMachine<Offset>
    where
        Offset: EpochOffset,
    {
        type Abstract = Self;
        type ConcreteState = (PosParams, EpochedDelta<u64, Offset>);

        fn init_test(
            initial_state: <Self::Abstract as AbstractStateMachine>::State,
        ) -> Self::ConcreteState {
            assert!(initial_state.data.len() == 1);
            let data = if initial_state.init_at_genesis {
                let genesis_epoch = initial_state.last_update;
                let value = initial_state.data.get(&genesis_epoch).unwrap();
                EpochedDelta::init_at_genesis(*value, genesis_epoch)
            } else {
                let (key, value) = initial_state.data.iter().next().unwrap();
                let data = EpochedDelta::init(
                    *value,
                    initial_state.last_update,
                    &initial_state.params,
                );
                assert_eq!(
                    Some(*value),
                    data.data[usize::from(*key - initial_state.last_update)]
                );
                data
            };
            (initial_state.params, data)
        }

        fn apply_concrete(
            (params, mut data): Self::ConcreteState,
            transition: &<Self::Abstract as AbstractStateMachine>::Transition,
        ) -> Self::ConcreteState {
            let offset = Offset::value(&params) as usize;
            match transition {
                EpochedTransition::Get(epoch) => {
                    let epoch = *epoch;
                    let value = data.get(epoch);
                    // Post-conditions
                    let last_update = data.last_update;
                    match value {
                        Some(val) => {
                            // When a value found, it should be equal to the sum
                            // of deltas before and on the upper bound
                            let upper_bound = cmp::min(
                                cmp::min((epoch - last_update).into(), offset)
                                    + 1,
                                data.data.len(),
                            );
                            let mut sum = 0;
                            for i in (0..upper_bound).rev() {
                                if let Some(stored_val) = data.data[i] {
                                    sum += stored_val;
                                }
                            }
                            assert_eq!(val, sum);
                        }
                        None => {
                            // When no value found, there should be no values
                            // before the upper bound
                            let upper_bound = cmp::min(
                                cmp::min((epoch - last_update).into(), offset)
                                    + 1,
                                data.data.len(),
                            );
                            for i in 0..upper_bound {
                                assert_eq!(None, data.data[i]);
                            }
                        }
                    }
                }
                EpochedTransition::Update {
                    value: change,
                    epoch,
                } => {
                    let current_value_before_update = data.get(*epoch);
                    let value_at_offset_before_update =
                        data.get(*epoch + offset);
                    data.update(*change, *epoch, &params);

                    // Post-conditions
                    assert_eq!(data.last_update, *epoch);
                    let value_at_offset_after_update =
                        data.get(*epoch + offset);
                    assert_eq!(
                        value_at_offset_after_update.unwrap_or_default(),
                        *change
                            + value_at_offset_before_update.unwrap_or_default(),
                        "The value at the offset must have increased by the \
                         change"
                    );
                    assert!(
                        data.data.len() > offset as usize,
                        "The length of the data must be greater than the \
                         offset"
                    );
                    assert_eq!(
                        data.get(*epoch),
                        current_value_before_update,
                        "The current value must not change"
                    );
                }
            }
            (params, data)
        }

        fn invariants((params, data): &Self::ConcreteState) {
            let offset = Offset::value(&params);
            assert!(data.data.len() <= (offset + 1) as usize);
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
