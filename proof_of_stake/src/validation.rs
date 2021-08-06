//! Validation of updated PoS data

use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{Add, AddAssign, Sub, SubAssign};

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
    StakingRewardAddressIsRequired(String),
    #[error(
        "Staking reward address must be different from the validator's \
         address {0}"
    )]
    StakingRewardAddressEqValidator(String),
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
        "Bond {id} must start at the correct epoch. Got {got}, expected \
         {expected}"
    )]
    InvalidBondStartEpoch {
        id: BondId<Address>,
        got: u64,
        expected: u64,
    },
    #[error(
        "Bond {id} must be added at the correct epoch. Got {got}, expected \
         {expected}"
    )]
    InvalidNewBondEpoch {
        id: BondId<Address>,
        got: u64,
        expected: u64,
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
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub<Output = TokenChange>
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
    let mut bond_delta = TokenChange::default();
    let mut unbond_delta = TokenChange::default();
    let mut withdraw_delta = TokenChange::default();
    for change in changes {
        match change {
            Validator { address, update } => match update {
                StakingRewardAddress(data) => match (data.pre, data.post) {
                    (Some(_), Some(post)) => {
                        if post == address {
                            errors.push(
                                Error::StakingRewardAddressEqValidator(
                                    address.to_string(),
                                ),
                            );
                        }
                    }
                    _ => errors.push(Error::StakingRewardAddressIsRequired(
                        address.to_string(),
                    )),
                },
                TotalDeltas(data) => {
                    // TODO
                }
            },
            Balance(data) => match (data.pre, data.post) {
                (None, Some(post)) => balance_delta += post.into(),
                (Some(pre), Some(post)) => {
                    balance_delta -= pre.into();
                    balance_delta += post.into();
                }
                (Some(_), None) => errors.push(Error::MissingBalance),
                _ => continue,
            },
            Bond { id, data } => match (data.pre, data.post) {
                // Bond may be updated from newly bonded tokens and unbonding
                (Some(pre), Some(post)) => {
                    if post.last_update() != current_epoch {
                        errors.push(Error::InvalidLastUpdate)
                    }
                    let mut total_pre_delta = TokenChange::default();
                    let mut total_post_delta = TokenChange::default();
                    for epoch in
                        Epoch::iter_range(current_epoch, pipeline_offset + 1)
                    {
                        let mut current_delta = TokenChange::default();
                        // Pre-bonds keyed by their `start_epoch`
                        let mut pre_bonds: HashMap<Epoch, TokenChange> =
                            HashMap::default();
                        if let Some(bond) = pre.get_delta_at_epoch(epoch) {
                            for (start_epoch, delta) in bond.delta.iter() {
                                let delta: TokenChange = (*delta).into();
                                total_pre_delta += delta;
                                current_delta -= delta;
                                pre_bonds.insert(*start_epoch, delta);
                            }
                        }
                        if let Some(bond) = post.get_delta_at_epoch(epoch) {
                            for (start_epoch, delta) in bond.delta.iter() {
                                // In the current epoch, all bond's
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
                                current_delta += delta;

                                // Anywhere other than at `pipeline_offset`
                                // where new bonds are added, check against the
                                // data in `pre_bonds` to ensure that no new
                                // bond has been added and that the deltas are
                                // equal or lower to `pre_bonds` delta.
                                // Note that any bonds from any epoch can be
                                // unbonded.
                                if epoch != current_epoch + pipeline_offset {
                                    match pre_bonds.get(&epoch) {
                                        Some(pre_delta) => {
                                            if &delta > pre_delta {
                                                errors.push(
                                                Error::InvalidNewBondEpoch {
                                                    id: id.clone(),
                                                    got: epoch.into(),
                                                    expected: (current_epoch
                                                        + pipeline_offset)
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
                        // An increase (newly bonded tokens) is only allowed at
                        // `pipeline_offset`. Decrease happens on unbonding,
                        // which can remove tokens from bond at any epoch.
                        if epoch != pipeline_epoch
                            && current_delta > TokenChange::default()
                        {
                            errors.push(Error::EpochedDataWrongEpoch {
                                got: epoch.into(),
                                expected: pipeline_epoch.into(),
                            })
                        }
                    }
                    // An empty bond must be deleted
                    if total_post_delta == TokenChange::default() {
                        errors.push(Error::EmptyBond(id))
                    }
                    bond_delta -= total_pre_delta;
                    bond_delta += total_post_delta;
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
                        errors.push(Error::EmptyBond(id))
                    }
                    bond_delta += total_delta;
                }
                // Bond may be deleted when all the tokens are unbonded
                (Some(pre), None) => {
                    for epoch in
                        Epoch::iter_range(current_epoch, pipeline_offset + 1)
                    {
                        if let Some(bond) = pre.get_delta_at_epoch(epoch) {
                            for delta in bond.delta.values() {
                                bond_delta -= (*delta).into();
                            }
                        }
                    }
                }
                _ => continue,
            },
        }
    }
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
