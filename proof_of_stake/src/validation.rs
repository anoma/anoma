//! Validation of updated PoS data

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{Add, Sub};

use borsh::{BorshDeserialize, BorshSerialize};
use thiserror::Error;

use crate::epoched::{EpochedDelta, OffsetPipelineLen, OffsetUnboundingLen};
use crate::types::{Bond, BondId, Epoch};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error<TokenChange>
where
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
         bonded {bonded}, unbonded {unbonded}, withdrawn {}."
    )]
    InvalidBalances {
        balance_delta: TokenChange,
        bonded: TokenChange,
        unbonded: TokenChange,
        withdrawn: TokenChange,
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
    changes: Vec<DataUpdate<Address, TokenAmount, TokenChange>>,
    current_epoch: impl Into<Epoch>,
) -> Vec<Error<TokenChange>>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
    TokenAmount: Display
        + Clone
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
    let current_epoch = current_epoch.into();
    use DataUpdate::*;
    use ValidatorUpdate::*;

    let mut errors = vec![];
    let mut balance_delta: Option<TokenChange> = None;
    let mut bond_delta: Option<TokenChange> = None;
    let mut unbond_delta: Option<TokenChange> = None;
    let mut withdraw_delta: Option<TokenChange> = None;
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
                TotalDeltas(data) => todo!(),
            },
            Balance(data) => match (data.pre, data.post) {
                (None, Some(post)) => balance_delta = Some(post.into()),
                (Some(pre), Some(post)) => {
                    let pre: TokenChange = pre.into();
                    let post: TokenChange = post.into();
                    balance_delta = Some(post - pre);
                }
                (Some(_), None) => errors.push(Error::MissingBalance),
                _ => continue,
            },
            Bond { id, data } => match (data.pre, data.post) {
                (None, Some(post)) => {
                    if post.last_update != current_epoch {
                        errors.push(Error::InvalidLastUpdate)
                    }
                }
                (Some(_), None) => todo!(),
                (Some(_), Some(_)) => todo!(),
                _ => continue,
            },
        }
    }
    // TODO check balance_delta against bonds & withdrawals
    let balance_delta = balance_delta.unwrap_or_default();
    let bonded = bond_delta.unwrap_or_default();
    let unbonded = unbond_delta.unwrap_or_default();
    let withdrawn = withdraw_delta.unwrap_or_default();
    if balance_delta != bonded - unbonded - withdrawn {
        errors.push(Error::InvalidBalances {
            balance_delta,
            bonded,
            unbonded,
            withdrawn,
        })
    }

    errors
}
