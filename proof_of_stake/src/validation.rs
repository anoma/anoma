//! Validation of updated PoS data

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{Add, Sub};

use borsh::{BorshDeserialize, BorshSerialize};
use thiserror::Error;

use crate::epoched::{EpochedDelta, OffsetPipelineLen};
use crate::types::{Bond, BondId, Epoch};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Validator staking reward address is required")]
    StakingRewardAddressIsRequired,
    #[error(
        "Staking reward address must be different from the validator's address"
    )]
    StakingRewardAddressEqValidator,
    #[error("Unexpectedly missing balance value")]
    MissingBalance,
    #[error("Last update should be equal to the current epoch")]
    InvalidLastUpdate,
}

#[derive(Clone, Debug)]
pub enum DataUpdate<Address, TokenAmount>
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
{
    Balance(Data<TokenAmount>),
    Bond {
        id: BondId<Address>,
        data: Data<EpochedDelta<Bond<TokenAmount>, OffsetPipelineLen>>,
    },
    Validator {
        address: Address,
        update: ValidatorUpdate<Address>,
    },
}

#[derive(Clone, Debug)]
pub enum ValidatorUpdate<Address>
where
    Address: Clone + Debug,
{
    StakingRewardAddress(Data<Address>),
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
    changes: Vec<DataUpdate<Address, TokenAmount>>,
    current_epoch: impl Into<Epoch>,
) -> Vec<Error>
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
    TokenChange: Debug
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub<Output = TokenChange>
        + From<TokenAmount>
        + Into<i128>,
{
    let current_epoch = current_epoch.into();
    use DataUpdate::*;
    use ValidatorUpdate::*;

    let mut errors = vec![];
    let mut balance_delta: Option<TokenChange> = None;
    let mut bond_delta: Option<TokenChange> = None;
    let mut withdraw_delta: Option<TokenChange> = None;
    for change in changes {
        match change {
            Validator {
                address,
                update: StakingRewardAddress(data),
            } => match (data.pre, data.post) {
                (Some(_), Some(post)) => {
                    if post == address {
                        errors.push(Error::StakingRewardAddressEqValidator);
                    }
                }
                _ => errors.push(Error::StakingRewardAddressIsRequired),
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
    if balance_delta != bond_delta - withdraw_delta {
        errors.push(todo!())
    }

    errors
}
