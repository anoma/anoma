//! Validation of updated PoS data

use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

use borsh::{BorshDeserialize, BorshSerialize};
use thiserror::Error;

use crate::epoched::{
    DynEpochOffset, EpochedDelta, OffsetPipelineLen, OffsetUnboundingLen,
};
use crate::parameters::PosParams;
use crate::types::{Bond, BondId, Epoch};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error<Address, TokenChange>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
    TokenChange: Debug + Display,
{
    #[error("Validator staking reward address is required for validator {0}")]
    StakingRewardAddressIsRequired(Address),
    #[error(
        "Staking reward address must be different from the validator's \
         address {0}"
    )]
    StakingRewardAddressEqValidator(Address),
    #[error("Unexpectedly missing total deltas value for validator {0}")]
    MissingValidatorTotalDeltas(Address),
    #[error("The sum of total deltas for validator {0} are negative")]
    NegativeValidatorTotalDeltasSum(Address),
    #[error("Unexpectedly missing balance value")]
    MissingBalance,
    #[error("Last update should be equal to the current epoch")]
    InvalidLastUpdate,
    #[error(
        "Invalid staking token balances. Balance delta {balance_delta}, \
         bonded {bond_delta}, unbonded {unbond_delta}, withdrawn \
         {withdraw_delta}."
    )]
    InvalidBalances {
        balance_delta: TokenChange,
        bond_delta: TokenChange,
        unbond_delta: TokenChange,
        withdraw_delta: TokenChange,
    },
    #[error(
        "Data must be set or updated in the correct epoch. Got {got}, \
         expected {expected}"
    )]
    EpochedDataWrongEpoch { got: u64, expected: u64 },
    #[error("Empty bond {0} must be deleted")]
    EmptyBond(BondId<Address>),
    #[error(
        "Bond ID {id} must start at the correct epoch. Got {got}, expected \
         {expected}"
    )]
    InvalidBondStartEpoch {
        id: BondId<Address>,
        got: u64,
        expected: u64,
    },
    #[error(
        "Bond ID {id} must be added at the correct epoch. Got {got}, expected \
         {expected}"
    )]
    InvalidNewBondEpoch {
        id: BondId<Address>,
        got: u64,
        expected: u64,
    },
    #[error(
        "Invalid validator {address} sum of total deltas. Total delta \
         {total_delta}, bonded {bond_delta}, unbonded {unbond_delta}."
    )]
    InvalidValidatorTotalDeltasSum {
        address: Address,
        total_delta: TokenChange,
        bond_delta: TokenChange,
        unbond_delta: TokenChange,
    },
}

#[derive(Clone, Debug)]
pub enum DataUpdate<Address, TokenAmount, TokenChange>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
    TokenAmount: Clone
        + Debug
        + Default
        + Eq
        + Sub
        + Add<Output = TokenAmount>
        + AddAssign
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub<Output = TokenChange>
        + From<TokenAmount>
        + Into<i128>
        + PartialEq
        + Eq
        + BorshDeserialize
        + BorshSerialize,
{
    Balance(Data<TokenAmount>),
    Bond {
        id: BondId<Address>,
        data: Data<EpochedDelta<Bond<TokenAmount>, OffsetPipelineLen>>,
    },
    Validator {
        address: Address,
        update: ValidatorUpdate<Address, TokenChange>,
    },
}

#[derive(Clone, Debug)]
pub enum ValidatorUpdate<Address, TokenChange>
where
    Address: Clone + Debug,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub<Output = TokenChange>
        + PartialEq
        + Eq
        + BorshDeserialize
        + BorshSerialize,
{
    StakingRewardAddress(Data<Address>),
    TotalDeltas(Data<EpochedDelta<TokenChange, OffsetUnboundingLen>>),
}

#[derive(Clone, Debug)]
pub struct Data<T>
where
    T: Clone + Debug,
{
    /// State before the update
    pub pre: Option<T>,
    /// State after the update
    pub post: Option<T>,
}

pub fn validate<Address, TokenAmount, TokenChange>(
    params: &PosParams,
    changes: Vec<DataUpdate<Address, TokenAmount, TokenChange>>,
    current_epoch: impl Into<Epoch>,
) -> Vec<Error<Address, TokenChange>>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
    TokenAmount: Display
        + Clone
        + Copy
        + Debug
        + Default
        + Eq
        + Sub
        + Add<Output = TokenAmount>
        + AddAssign
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub<Output = TokenChange>
        + Neg<Output = TokenChange>
        + SubAssign
        + AddAssign
        + From<TokenAmount>
        + Into<i128>
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + BorshDeserialize
        + BorshSerialize,
{
    let current_epoch = current_epoch.into();
    use DataUpdate::*;
    use ValidatorUpdate::*;

    let pipeline_offset = DynEpochOffset::PipelineLen.value(params);
    let unbonding_offset = DynEpochOffset::UnbondingLen.value(params);
    let pipeline_epoch = current_epoch + pipeline_offset;
    let unbonding_epoch = current_epoch + unbonding_offset;

    let mut errors = vec![];
    let mut balance_delta = TokenChange::default();
    // Changes of validators' bonds
    let mut bond_delta: HashMap<Address, TokenChange> = HashMap::default();
    let mut unbond_delta: HashMap<Address, TokenChange> = HashMap::default();
    let mut withdraw_delta = TokenChange::default();
    // Changes of validator total deltas
    let mut total_deltas: HashMap<Address, TokenChange> = HashMap::default();
    for change in changes {
        match change {
            Validator { address, update } => match update {
                StakingRewardAddress(data) => match (data.pre, data.post) {
                    (Some(_), Some(post)) => {
                        if post == address {
                            errors.push(
                                Error::StakingRewardAddressEqValidator(
                                    address.clone(),
                                ),
                            );
                        }
                    }
                    _ => errors.push(Error::StakingRewardAddressIsRequired(
                        address.clone(),
                    )),
                },
                TotalDeltas(data) => match (data.pre, data.post) {
                    (Some(pre), Some(post)) => {
                        if post.last_update() != current_epoch {
                            errors.push(Error::InvalidLastUpdate)
                        }
                        let mut deltas = TokenChange::default();
                        // Iter from the first epoch to the last epoch of `post`
                        for epoch in Epoch::iter_range(
                            post.last_update(),
                            unbonding_offset + 1,
                        ) {
                            let mut delta = TokenChange::default();
                            if let Some(change) = if epoch == post.last_update()
                            {
                                // On the first epoch, we have to get the sum of
                                // all deltas at and before that epoch as the
                                // `pre` could have been set in an older epoch
                                pre.get(epoch)
                            } else {
                                pre.get_delta_at_epoch(epoch).copied()
                            } {
                                delta -= change;
                            }
                            if let Some(change) = post.get_delta_at_epoch(epoch)
                            {
                                delta += *change;
                            }
                            deltas += delta;
                            // A total delta can only be increased at
                            // `pipeline_offset` from bonds and decreased at
                            // `unbonding_offset` from unbonding
                            if delta > TokenChange::default()
                                && epoch != pipeline_epoch
                            {
                                errors.push(Error::EpochedDataWrongEpoch {
                                    got: epoch.into(),
                                    expected: pipeline_epoch.into(),
                                })
                            }
                            if delta < TokenChange::default()
                                && epoch != unbonding_epoch
                            {
                                errors.push(Error::EpochedDataWrongEpoch {
                                    got: epoch.into(),
                                    expected: pipeline_epoch.into(),
                                })
                            }
                        }
                        if deltas < TokenChange::default() {
                            errors.push(Error::NegativeValidatorTotalDeltasSum(
                                address.clone(),
                            ))
                        }
                        total_deltas.insert(address.clone(), deltas);
                    }
                    (None, Some(post)) => {
                        if post.last_update() != current_epoch {
                            errors.push(Error::InvalidLastUpdate)
                        }
                        let mut delta = TokenChange::default();
                        for epoch in Epoch::iter_range(
                            current_epoch,
                            unbonding_offset + 1,
                        ) {
                            if let Some(change) = post.get_delta_at_epoch(epoch)
                            {
                                // A new total delta can only be initialized at
                                // `pipeline_offset` (from bonds) and
                                // `unbonding_offset` (from unbonding)
                                if epoch != pipeline_epoch
                                    && epoch != unbonding_epoch
                                {
                                    errors.push(Error::EpochedDataWrongEpoch {
                                        got: epoch.into(),
                                        expected: pipeline_epoch.into(),
                                    })
                                }
                                delta += *change;
                            }
                        }
                        if delta < TokenChange::default() {
                            errors.push(Error::NegativeValidatorTotalDeltasSum(
                                address.clone(),
                            ))
                        }
                        if delta != TokenChange::default() {
                            total_deltas.insert(address.clone(), delta);
                        }
                    }
                    (Some(_), None) => {
                        errors.push(Error::MissingValidatorTotalDeltas(address))
                    }
                    (None, None) => continue,
                },
            },
            Balance(data) => match (data.pre, data.post) {
                (None, Some(post)) => balance_delta += post.into(),
                (Some(pre), Some(post)) => {
                    balance_delta -= pre.into();
                    balance_delta += post.into();
                }
                (Some(_), None) => errors.push(Error::MissingBalance),
                (None, None) => continue,
            },
            Bond { id, data } => match (data.pre, data.post) {
                // Bond may be updated from newly bonded tokens and unbonding
                (Some(pre), Some(post)) => {
                    if post.last_update() != current_epoch {
                        errors.push(Error::InvalidLastUpdate)
                    }
                    let mut total_pre_delta = TokenChange::default();
                    let mut total_post_delta = TokenChange::default();
                    // Pre-bonds keyed by their `start_epoch`
                    let mut pre_bonds: HashMap<Epoch, TokenChange> =
                        HashMap::default();
                    // Iter from the first epoch of `pre` to the last epoch of
                    // `post`
                    let pre_offset: u64 =
                        (current_epoch - pre.last_update()).into();
                    for epoch in Epoch::iter_range(
                        pre.last_update(),
                        pre_offset + pipeline_offset + 1,
                    ) {
                        if let Some(bond) = pre.get_delta_at_epoch(epoch) {
                            for (start_epoch, delta) in bond.delta.iter() {
                                let delta: TokenChange = (*delta).into();
                                total_pre_delta += delta;
                                pre_bonds.insert(*start_epoch, delta);
                            }
                        }
                        if let Some(bond) = post.get_delta_at_epoch(epoch) {
                            for (start_epoch, delta) in bond.delta.iter() {
                                // On the current epoch, all bond's
                                // `start_epoch`s must be equal or lower than
                                // `current_epoch`. For all others, the
                                // `start_epoch` must be equal
                                // to the `epoch` at which it's set.
                                if (epoch == current_epoch
                                    && *start_epoch > current_epoch)
                                    || (epoch != current_epoch
                                        && *start_epoch != epoch)
                                {
                                    errors.push(Error::InvalidBondStartEpoch {
                                        id: id.clone(),
                                        got: (*start_epoch).into(),
                                        expected: epoch.into(),
                                    })
                                }
                                let delta: TokenChange = (*delta).into();
                                total_post_delta += delta;

                                // Anywhere other than at `pipeline_offset`
                                // where new bonds are added, check against the
                                // data in `pre_bonds` to ensure that no new
                                // bond has been added and that the deltas are
                                // equal or lower to `pre_bonds` deltas.
                                // Note that any bonds from any epoch can be
                                // unbonded, even if they are not yet active.
                                if epoch != pipeline_epoch {
                                    match pre_bonds.get(start_epoch) {
                                        Some(pre_delta) => {
                                            if &delta > pre_delta {
                                                errors.push(
                                                Error::InvalidNewBondEpoch {
                                                    id: id.clone(),
                                                    got: epoch.into(),
                                                    expected: pipeline_epoch
                                                        .into(),
                                                });
                                            }
                                        }
                                        None => {
                                            errors.push(
                                                Error::InvalidNewBondEpoch {
                                                    id: id.clone(),
                                                    got: epoch.into(),
                                                    expected: (current_epoch
                                                        + pipeline_offset)
                                                        .into(),
                                                },
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // An empty bond must be deleted
                    if total_post_delta == TokenChange::default() {
                        errors.push(Error::EmptyBond(id.clone()))
                    }
                    let total = total_post_delta - total_pre_delta;
                    if total != TokenChange::default() {
                        bond_delta.insert(id.validator, total);
                    }
                }
                // Bond may be created from newly bonded tokens only
                (None, Some(post)) => {
                    if post.last_update() != current_epoch {
                        errors.push(Error::InvalidLastUpdate)
                    }
                    let mut total_delta = TokenChange::default();
                    for epoch in
                        Epoch::iter_range(current_epoch, pipeline_offset + 1)
                    {
                        if let Some(bond) = post.get_delta_at_epoch(epoch) {
                            // A new bond must be initialized at
                            // `pipeline_offset`
                            if epoch != pipeline_epoch {
                                errors.push(Error::EpochedDataWrongEpoch {
                                    got: epoch.into(),
                                    expected: pipeline_epoch.into(),
                                })
                            }
                            for (start_epoch, delta) in bond.delta.iter() {
                                if *start_epoch != epoch {
                                    errors.push(Error::InvalidBondStartEpoch {
                                        id: id.clone(),
                                        got: (*start_epoch).into(),
                                        expected: epoch.into(),
                                    })
                                }
                                total_delta += (*delta).into();
                            }
                        }
                    }
                    // An empty bond must be deleted
                    if total_delta == TokenChange::default() {
                        errors.push(Error::EmptyBond(id.clone()))
                    }
                    bond_delta.insert(id.validator, total_delta);
                }
                // Bond may be deleted when all the tokens are unbonded
                (Some(pre), None) => {
                    for index in 0..pipeline_offset + 1 {
                        let index = index as usize;
                        let epoch = pre.last_update() + index;
                        if let Some(bond) = pre.get_delta_at_epoch(epoch) {
                            for delta in bond.delta.values() {
                                let delta: TokenChange = (*delta).into();
                                bond_delta.insert(id.validator.clone(), -delta);
                            }
                        }
                    }
                }
                _ => continue,
            },
        }
    }
    // Check total deltas against bonds and unbonds
    for (validator, total_delta) in total_deltas.iter() {
        let bond_delta =
            bond_delta.get(&validator).copied().unwrap_or_default();
        let unbond_delta =
            unbond_delta.get(&validator).copied().unwrap_or_default();
        let total_delta = *total_delta;
        if total_delta != bond_delta - unbond_delta {
            errors.push(Error::InvalidValidatorTotalDeltasSum {
                address: validator.clone(),
                total_delta,
                bond_delta,
                unbond_delta,
            })
        }
    }
    // Check that all bonds also have a total deltas update
    for validator in bond_delta.keys() {
        if !total_deltas.contains_key(validator) {
            errors.push(Error::MissingValidatorTotalDeltas(validator.clone()))
        }
    }
    // Check that all unbonds also have a total deltas update
    for validator in unbond_delta.keys() {
        if !total_deltas.contains_key(validator) {
            errors.push(Error::MissingValidatorTotalDeltas(validator.clone()))
        }
    }

    // Sum the bond totals
    let bond_delta = bond_delta
        .values()
        .into_iter()
        .fold(TokenChange::default(), |acc, delta| acc + (*delta));
    // Sum the unbond totals
    let unbond_delta = unbond_delta
        .values()
        .into_iter()
        .fold(TokenChange::default(), |acc, delta| acc + (*delta));

    if balance_delta != bond_delta - unbond_delta - withdraw_delta {
        errors.push(Error::InvalidBalances {
            balance_delta,
            bond_delta,
            unbond_delta,
            withdraw_delta,
        })
    }

    errors
}
