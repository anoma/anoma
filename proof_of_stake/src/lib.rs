//! Proof of Stake system.
//!
//! TODO: We might need to storage both active and total validator set voting
//! power. For consensus, we only consider active validator set voting power,
//! but for other activities in which inactive validators can participate (e.g.
//! voting on a protocol parameter changes, upgrades, default VP changes) we
//! should use the total validator set voting power.

mod btree_set;
pub mod epoched;
pub mod parameters;
pub mod types;
pub mod validation;

use core::fmt::Debug;
use std::collections::{BTreeSet, HashMap};
use std::convert::TryFrom;
use std::fmt::Display;
use std::hash::Hash;
use std::num::TryFromIntError;
use std::ops::{self, Add, Neg, Sub, SubAssign};

use borsh::{BorshDeserialize, BorshSerialize};
use epoched::{
    DynEpochOffset, EpochOffset, Epoched, EpochedDelta, OffsetPipelineLen,
};
use parameters::PosParams;
use thiserror::Error;
use types::{
    ActiveValidator, Bonds, Epoch, GenesisValidator, TotalVotingPowers, Unbond,
    Unbonds, ValidatorConsensusKeys, ValidatorSet, ValidatorSetUpdate,
    ValidatorSets, ValidatorState, ValidatorStates, ValidatorTotalDeltas,
    ValidatorVotingPowers, VotingPower, VotingPowerDelta,
};

use crate::btree_set::BTreeSetShims;
use crate::types::{Bond, BondId, WeightedValidator};

/// Read-only part of the PoS system
pub trait PoSReadOnly {
    type Address: Display
        + Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize;
    type TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = Self::TokenAmount>
        + Sub
        + PartialOrd
        + Into<u64>
        + From<u64>
        + Into<Self::TokenChange>
        + SubAssign
        + BorshDeserialize
        + BorshSerialize;
    type TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = Self::TokenChange>
        + Sub
        + From<Self::TokenAmount>
        + Into<i128>
        + Neg<Output = Self::TokenChange>
        + BorshDeserialize
        + BorshSerialize;
    type PublicKey: Debug + Clone + BorshDeserialize + BorshSerialize;

    /// Address of the PoS account
    const POS_ADDRESS: Self::Address;
    /// Address of the staking token
    /// TODO: this should be `const`, but in the ledger `address::xan` is not a
    /// `const fn`
    fn staking_token_address() -> Self::Address;

    fn read_params(&self) -> PosParams;
    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> Option<Self::Address>;
    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys<Self::PublicKey>>;
    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorStates>;
    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorTotalDeltas<Self::TokenChange>>;
    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorVotingPowers>;
    fn read_bond(
        &self,
        key: &BondId<Self::Address>,
    ) -> Option<Bonds<Self::TokenAmount>>;
    fn read_unbond(
        &self,
        key: &BondId<Self::Address>,
    ) -> Option<Unbonds<Self::TokenAmount>>;
    fn read_validator_set(&self) -> ValidatorSets<Self::Address>;
    fn read_total_voting_power(&self) -> TotalVotingPowers;
}

/// PoS system trait to be implemented in integration that can read and write
/// PoS data.
pub trait PoS: PoSReadOnly {
    fn write_params(&mut self, params: &PosParams);
    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: Self::Address,
    );
    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: ValidatorConsensusKeys<Self::PublicKey>,
    );
    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: ValidatorStates,
    );
    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: ValidatorTotalDeltas<Self::TokenChange>,
    );
    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: ValidatorVotingPowers,
    );
    fn write_bond(
        &mut self,
        key: &BondId<Self::Address>,
        value: Bonds<Self::TokenAmount>,
    );
    fn write_unbond(
        &mut self,
        key: &BondId<Self::Address>,
        value: Unbonds<Self::TokenAmount>,
    );
    fn write_validator_set(&mut self, value: ValidatorSets<Self::Address>);
    fn write_total_voting_power(&mut self, value: TotalVotingPowers);

    fn delete_bond(&mut self, key: &BondId<Self::Address>);
    fn delete_unbond(&mut self, key: &BondId<Self::Address>);

    fn transfer(
        &mut self,
        token: &Self::Address,
        amount: Self::TokenAmount,
        src: &Self::Address,
        dest: &Self::Address,
    );

    /// Attempt to update the given account to become a validator.
    fn become_validator(
        &mut self,
        address: &Self::Address,
        staking_reward_address: &Self::Address,
        consensus_key: &Self::PublicKey,
        current_epoch: impl Into<Epoch>,
    ) -> Result<(), BecomeValidatorError<Self::Address>> {
        let current_epoch = current_epoch.into();
        let params = self.read_params();
        let mut validator_set = self.read_validator_set();
        if self.is_validator(address).is_some() {
            return Err(BecomeValidatorError::AlreadyValidator(
                address.clone(),
            ));
        }
        let BecomeValidatorData {
            consensus_key,
            state,
        } = become_validator(
            &params,
            address,
            consensus_key,
            &mut validator_set,
            current_epoch,
        );
        self.write_validator_staking_reward_address(
            address,
            staking_reward_address.clone(),
        );
        self.write_validator_consensus_key(address, consensus_key);
        self.write_validator_state(address, state);
        self.write_validator_set(validator_set);
        Ok(())
    }

    /// Check if the given address is a validator by checking that it has some
    /// state.
    fn is_validator(
        &mut self,
        address: &Self::Address,
    ) -> Option<Epoched<ValidatorState, OffsetPipelineLen>> {
        self.read_validator_state(address)
    }

    /// Self-bond tokens to a validator.
    fn validator_self_bond(
        &mut self,
        address: &Self::Address,
        amount: Self::TokenAmount,
        current_epoch: impl Into<Epoch>,
    ) -> Result<(), BondError<Self::Address>> {
        let current_epoch = current_epoch.into();
        let params = self.read_params();
        let validator_state = self.is_validator(address);
        let bond_id = BondId {
            source: address.clone(),
            validator: address.clone(),
        };
        let bond = self.read_bond(&bond_id);
        let validator_total_deltas = self.read_validator_total_deltas(address);
        let validator_voting_power = self.read_validator_voting_power(address);
        let mut total_voting_power = self.read_total_voting_power();
        let mut validator_set = self.read_validator_set();

        let BondData {
            bond,
            validator_total_deltas,
            validator_voting_power,
        } = bond_tokens(
            &params,
            validator_state,
            &bond_id,
            bond,
            amount,
            validator_total_deltas,
            validator_voting_power,
            &mut total_voting_power,
            &mut validator_set,
            current_epoch,
        )?;

        self.write_bond(&bond_id, bond);
        self.write_validator_total_deltas(address, validator_total_deltas);
        self.write_validator_voting_power(address, validator_voting_power);
        self.write_total_voting_power(total_voting_power);
        self.write_validator_set(validator_set);

        // Transfer the bonded tokens from the validator to PoS
        self.transfer(
            &Self::staking_token_address(),
            amount,
            address,
            &Self::POS_ADDRESS,
        );

        Ok(())
    }

    /// Unbond self-bonded tokens from a validator.
    fn validator_unbond(
        &mut self,
        address: &Self::Address,
        amount: Self::TokenAmount,
        current_epoch: impl Into<Epoch>,
    ) -> Result<(), UnbondError<Self::Address, Self::TokenAmount>> {
        let current_epoch = current_epoch.into();
        let params = self.read_params();
        let bond_id = BondId {
            source: address.clone(),
            validator: address.clone(),
        };
        let mut bond =
            self.read_bond(&bond_id).ok_or(UnbondError::NoBondFound)?;
        let unbond = self.read_unbond(&bond_id);
        let mut validator_total_deltas = self
            .read_validator_total_deltas(address)
            .ok_or_else(|| UnbondError::ValidatorHasNoBonds(address.clone()))?;
        let mut validator_voting_power =
            self.read_validator_voting_power(address).ok_or_else(|| {
                UnbondError::ValidatorHasNoVotingPower(address.clone())
            })?;
        let mut total_voting_power = self.read_total_voting_power();
        let mut validator_set = self.read_validator_set();

        let UnbondData { unbond } = unbond_tokens(
            &params,
            &bond_id,
            &mut bond,
            unbond,
            amount,
            &mut validator_total_deltas,
            &mut validator_voting_power,
            &mut total_voting_power,
            &mut validator_set,
            current_epoch,
        )?;

        let total_bonds = bond.get_at_offset(
            current_epoch,
            DynEpochOffset::PipelineLen,
            &params,
        );
        match total_bonds {
            Some(total_bonds) if total_bonds.sum() != 0.into() => {
                self.write_bond(&bond_id, bond);
            }
            _ => {
                // If the bond is left empty, delete it
                self.delete_bond(&bond_id)
            }
        }
        self.write_unbond(&bond_id, unbond);
        self.write_validator_total_deltas(address, validator_total_deltas);
        self.write_validator_voting_power(address, validator_voting_power);
        self.write_total_voting_power(total_voting_power);
        self.write_validator_set(validator_set);

        Ok(())
    }

    /// Withdraw tokens from validator's unbonds of self-bonds back into its
    /// account address.
    fn validator_withdraw_unbonds(
        &mut self,
        address: &Self::Address,
        current_epoch: impl Into<Epoch>,
    ) -> Result<(), WithdrawError<Self::Address>> {
        let current_epoch = current_epoch.into();
        let params = self.read_params();
        let bond_id = BondId {
            source: address.clone(),
            validator: address.clone(),
        };

        let unbond = self.read_unbond(&bond_id);

        let WithdrawData {
            unbond,
            withdrawn_amount,
        } = withdraw_unbonds(&params, &bond_id, unbond, current_epoch)?;

        let total_unbonds = unbond.get_at_offset(
            current_epoch,
            DynEpochOffset::UnbondingLen,
            &params,
        );
        match total_unbonds {
            Some(total_unbonds) if total_unbonds.sum() != 0.into() => {
                self.write_unbond(&bond_id, unbond);
            }
            _ => {
                // If the unbond is left empty, delete it
                self.delete_unbond(&bond_id)
            }
        }

        // Transfer the tokens from PoS to the validator
        self.transfer(
            &Self::staking_token_address(),
            withdrawn_amount,
            &Self::POS_ADDRESS,
            address,
        );

        Ok(())
    }
}

/// PoS system base trait for system initialization on genesis block, updating
/// the validator on a new epoch and applying slashes.
pub trait PoSBase {
    type Address: 'static
        + Display
        + Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize;
    type TokenAmount: 'static
        + Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = Self::TokenAmount>
        + Sub
        + PartialOrd
        + Into<u64>
        + From<u64>
        + Into<Self::TokenChange>
        + SubAssign
        + BorshDeserialize
        + BorshSerialize;
    type TokenChange: 'static
        + Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = Self::TokenChange>
        + Sub
        + From<Self::TokenAmount>
        + Into<i128>
        + Neg<Output = Self::TokenChange>
        + BorshDeserialize
        + BorshSerialize;
    type PublicKey: 'static + Debug + Clone + BorshDeserialize + BorshSerialize;

    fn read_validator_set(&self) -> ValidatorSets<Self::Address>;
    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys<Self::PublicKey>>;

    fn write_params(&mut self, params: &PosParams);
    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: &Self::Address,
    );
    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: &ValidatorConsensusKeys<Self::PublicKey>,
    );
    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: &ValidatorStates,
    );
    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: &ValidatorTotalDeltas<Self::TokenChange>,
    );
    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: &ValidatorVotingPowers,
    );
    fn write_bond(
        &mut self,
        key: &BondId<Self::Address>,
        value: &Bonds<Self::TokenAmount>,
    );
    fn write_validator_set(&mut self, value: &ValidatorSets<Self::Address>);
    fn write_total_voting_power(&mut self, value: &TotalVotingPowers);
    fn init_staking_reward_account(
        &mut self,
        address: &Self::Address,
        pk: &Self::PublicKey,
    );

    /// Initialize the PoS system storage data in the genesis block for the
    /// given PoS parameters and initial validator set. The validators'
    /// tokens will be put into self-bonds. The given PoS parameters are written
    /// with the [`Pos::write_params`] method.
    fn init_genesis<'a>(
        &mut self,
        params: &'a PosParams,
        validators: impl Iterator<
            Item = &'a GenesisValidator<
                Self::Address,
                Self::TokenAmount,
                Self::PublicKey,
            >,
        > + Clone
        + 'a,
        current_epoch: impl Into<Epoch>,
    ) -> Result<(), GenesisError> {
        let current_epoch = current_epoch.into();
        self.write_params(params);

        let GenesisData {
            validators,
            validator_set,
            total_voting_power,
        } = init_genesis(params, validators, current_epoch)?;

        for res in validators {
            let GenesisValidatorData {
                ref address,
                staking_reward_address,
                consensus_key,
                staking_reward_key,
                state,
                total_deltas,
                voting_power,
                bond: (bond_id, bond),
            } = res?;
            self.write_validator_staking_reward_address(
                address,
                &staking_reward_address,
            );
            self.write_validator_consensus_key(address, &consensus_key);
            self.write_validator_state(address, &state);
            self.write_validator_total_deltas(address, &total_deltas);
            self.write_validator_voting_power(address, &voting_power);
            self.write_bond(&bond_id, &bond);
            self.init_staking_reward_account(
                &staking_reward_address,
                &staking_reward_key,
            );
        }
        self.write_validator_set(&validator_set);
        self.write_total_voting_power(&total_voting_power);
        Ok(())
    }

    /// Calls a closure on each validator update element.
    fn validator_set_update(
        &self,
        current_epoch: impl Into<Epoch>,
        f: impl FnMut(ValidatorSetUpdate<Self::PublicKey>),
    ) {
        let current_epoch = current_epoch.into();
        let validators = self.read_validator_set();
        let validators = validators.get(current_epoch).unwrap();
        let active_validators = validators.active.iter().map(
            |WeightedValidator {
                 voting_power,
                 address,
             }: &WeightedValidator<Self::Address>| {
                let consensus_key = self
                    .read_validator_consensus_key(address)
                    .unwrap()
                    .get(current_epoch)
                    .unwrap()
                    .clone();
                ValidatorSetUpdate::Active(ActiveValidator {
                    consensus_key,
                    voting_power: *voting_power,
                })
            },
        );
        // TODO we only need inactive validators that were active in the last
        // update
        let inactive_validators = validators.inactive.iter().map(
            |WeightedValidator {
                 voting_power: _,
                 address,
             }: &WeightedValidator<Self::Address>| {
                let consensus_key = self
                    .read_validator_consensus_key(address)
                    .unwrap()
                    .get(current_epoch)
                    .unwrap()
                    .clone();
                ValidatorSetUpdate::Deactivated(consensus_key)
            },
        );
        active_validators.chain(inactive_validators).for_each(f)
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum GenesisError {
    #[error("Voting power overflow: {0}")]
    VotingPowerOverflow(TryFromIntError),
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum BecomeValidatorError<Address: Display + Debug> {
    #[error("The given address {0} is already a validator")]
    AlreadyValidator(Address),
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum BondError<Address: Display + Debug> {
    #[error("The given address {0} is not a validator address")]
    NotAValidator(Address),
    #[error("The given validator address {0} is inactive")]
    InactiveValidator(Address),
    #[error("Voting power overflow: {0}")]
    VotingPowerOverflow(TryFromIntError),
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum UnbondError<Address: Display + Debug, TokenAmount: Display + Debug> {
    #[error("No bond could be found")]
    NoBondFound,
    #[error(
        "Trying to withdraw more tokens ({0}) than the amount bonded ({0})"
    )]
    UnbondAmountGreaterThanBond(TokenAmount, TokenAmount),
    #[error("No bonds found for the validator {0}")]
    ValidatorHasNoBonds(Address),
    #[error("Voting power not found for the validator {0}")]
    ValidatorHasNoVotingPower(Address),
    #[error("Voting power overflow: {0}")]
    VotingPowerOverflow(TryFromIntError),
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum WithdrawError<Address>
where
    Address: Display + Debug + Clone + PartialOrd + Ord + Hash,
{
    #[error("No unbond could be found for {0}")]
    NoUnbondFound(BondId<Address>),
    #[error("No unbond may be withdrawn yet for {0}")]
    NoWithdrawableUnbond(BondId<Address>),
}

struct GenesisData<Validators, Address, TokenAmount, TokenChange, PK>
where
    Validators: Iterator<
        Item = Result<
            GenesisValidatorData<Address, TokenAmount, TokenChange, PK>,
            GenesisError,
        >,
    >,
    Address: Display
        + Debug
        + Clone
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize,
    TokenAmount: Debug
        + Default
        + Clone
        + ops::Add<Output = TokenAmount>
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: Debug
        + Copy
        + ops::Add<Output = TokenChange>
        + BorshDeserialize
        + BorshSerialize,
    PK: Debug + Clone + BorshDeserialize + BorshSerialize,
{
    validators: Validators,
    /// Active and inactive validator sets
    validator_set: ValidatorSets<Address>,
    /// The sum of all active and inactive validators' voting power
    total_voting_power: TotalVotingPowers,
}
struct GenesisValidatorData<Address, TokenAmount, TokenChange, PK>
where
    Address: Display
        + Debug
        + Clone
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize,
    TokenAmount: Debug
        + Default
        + Clone
        + ops::Add<Output = TokenAmount>
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: Debug
        + Copy
        + ops::Add<Output = TokenChange>
        + BorshDeserialize
        + BorshSerialize,
    PK: Debug + Clone + BorshDeserialize + BorshSerialize,
{
    address: Address,
    staking_reward_address: Address,
    consensus_key: ValidatorConsensusKeys<PK>,
    staking_reward_key: PK,
    state: ValidatorStates,
    total_deltas: ValidatorTotalDeltas<TokenChange>,
    voting_power: ValidatorVotingPowers,
    bond: (BondId<Address>, Bonds<TokenAmount>),
}

/// A function that returns genesis data created from the initial validator set.
fn init_genesis<'a, Address, TokenAmount, TokenChange, PK>(
    params: &'a PosParams,
    validators: impl Iterator<Item = &'a GenesisValidator<Address, TokenAmount, PK>>
    + Clone
    + 'a,
    current_epoch: Epoch,
) -> Result<
    GenesisData<
        impl Iterator<
            Item = Result<
                GenesisValidatorData<Address, TokenAmount, TokenChange, PK>,
                GenesisError,
            >,
        > + 'a,
        Address,
        TokenAmount,
        TokenChange,
        PK,
    >,
    GenesisError,
>
where
    Address: 'a
        + Display
        + Debug
        + Clone
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize,
    TokenAmount: 'a
        + Debug
        + Default
        + Clone
        + ops::Add<Output = TokenAmount>
        + Into<u64>
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: 'a
        + Debug
        + Copy
        + ops::Add<Output = TokenChange>
        + From<TokenAmount>
        + BorshDeserialize
        + BorshSerialize,
    PK: 'a + Debug + Clone + BorshDeserialize + BorshSerialize,
{
    // Accumulate the validator set and total voting power
    let mut active: BTreeSet<WeightedValidator<Address>> = BTreeSet::default();
    let mut total_voting_power = VotingPowerDelta::default();
    for GenesisValidator {
        address, tokens, ..
    } in validators.clone()
    {
        let delta = VotingPowerDelta::try_from_tokens(tokens.clone(), params)
            .map_err(GenesisError::VotingPowerOverflow)?;
        total_voting_power += delta;
        let voting_power = VotingPower::from_tokens(tokens.clone(), params);
        active.insert(WeightedValidator {
            voting_power,
            address: address.clone(),
        });
    }
    // Pop the smallest validators from the active set until its size is under
    // the limit and insert them into the inactive set
    let mut inactive: BTreeSet<WeightedValidator<Address>> =
        BTreeSet::default();
    while active.len() > params.max_validator_slots as usize {
        match active.pop_first_shim() {
            Some(first) => {
                inactive.insert(first);
            }
            None => break,
        }
    }
    let validator_set = ValidatorSet { active, inactive };
    let validator_set = Epoched::init_at_genesis(validator_set, current_epoch);
    let total_voting_power =
        EpochedDelta::init_at_genesis(total_voting_power, current_epoch);

    // Adapt the genesis validators data to PoS data
    let validators = validators.map(
        move |GenesisValidator {
                  address,
                  staking_reward_address,

                  tokens,
                  consensus_key,
                  staking_reward_key,
              }| {
            let consensus_key =
                Epoched::init_at_genesis(consensus_key.clone(), current_epoch);
            let state = Epoched::init_at_genesis(
                ValidatorState::Candidate,
                current_epoch,
            );
            let token_delta = TokenChange::from(tokens.clone());
            let total_deltas =
                EpochedDelta::init_at_genesis(token_delta, current_epoch);
            let voting_power =
                VotingPowerDelta::try_from_tokens(tokens.clone(), params)
                    .map_err(GenesisError::VotingPowerOverflow)?;
            let voting_power =
                EpochedDelta::init_at_genesis(voting_power, current_epoch);
            let bond_id = BondId {
                source: address.clone(),
                validator: address.clone(),
            };
            let mut delta = HashMap::default();
            delta.insert(current_epoch, tokens.clone());
            let bond =
                EpochedDelta::init_at_genesis(Bond { delta }, current_epoch);
            Ok(GenesisValidatorData {
                address: address.clone(),
                staking_reward_address: staking_reward_address.clone(),
                consensus_key,
                staking_reward_key: staking_reward_key.clone(),
                state,
                total_deltas,
                voting_power,
                bond: (bond_id, bond),
            })
        },
    );

    Ok(GenesisData {
        validators,
        validator_set,
        total_voting_power,
    })
}

struct BecomeValidatorData<PK>
where
    PK: Debug + Clone + BorshDeserialize + BorshSerialize,
{
    consensus_key: ValidatorConsensusKeys<PK>,
    state: ValidatorStates,
}

/// A function that initialized data for a new validator.
fn become_validator<Address, PK>(
    params: &PosParams,
    address: &Address,
    consensus_key: &PK,
    validator_set: &mut ValidatorSets<Address>,
    current_epoch: Epoch,
) -> BecomeValidatorData<PK>
where
    Address: Debug + Clone + Ord + Hash + BorshDeserialize + BorshSerialize,
    PK: Debug + Clone + BorshDeserialize + BorshSerialize,
{
    let consensus_key =
        Epoched::init(consensus_key.clone(), current_epoch, params);
    let mut state =
        Epoched::init(ValidatorState::Pending, current_epoch, params);
    state.set(ValidatorState::Candidate, current_epoch, params);
    validator_set.update_from_offset(
        |validator_set, _epoch| {
            let validator = WeightedValidator {
                voting_power: VotingPower::default(),
                address: address.clone(),
            };
            if validator_set.active.len() < params.max_validator_slots as usize
            {
                validator_set.active.insert(validator);
            } else {
                validator_set.inactive.insert(validator);
            }
        },
        current_epoch,
        DynEpochOffset::PipelineLen,
        params,
    );

    BecomeValidatorData {
        consensus_key,
        state,
    }
}

struct BondData<TokenAmount, TokenChange>
where
    TokenAmount: Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenAmount>
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: Debug
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + BorshDeserialize
        + BorshSerialize,
{
    pub bond: Bonds<TokenAmount>,
    pub validator_total_deltas: ValidatorTotalDeltas<TokenChange>,
    pub validator_voting_power: ValidatorVotingPowers,
}

/// Bond tokens to a validator (self-bond or delegation).
#[allow(clippy::too_many_arguments)]
fn bond_tokens<Address, TokenAmount, TokenChange>(
    params: &PosParams,
    validator_state: Option<ValidatorStates>,
    bond_id: &BondId<Address>,
    current_bond: Option<Bonds<TokenAmount>>,
    amount: TokenAmount,
    validator_total_deltas: Option<ValidatorTotalDeltas<TokenChange>>,
    validator_voting_power: Option<ValidatorVotingPowers>,
    total_voting_power: &mut TotalVotingPowers,
    validator_set: &mut ValidatorSets<Address>,
    current_epoch: Epoch,
) -> Result<BondData<TokenAmount, TokenChange>, BondError<Address>>
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
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenAmount>
        + Into<u64>
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub
        + From<TokenAmount>
        + Into<i128>
        + BorshDeserialize
        + BorshSerialize,
{
    // Check the validator state
    match validator_state {
        None => {
            return Err(BondError::NotAValidator(bond_id.validator.clone()));
        }
        Some(validator_state) => {
            // Check that it's not inactive anywhere from the current epoch
            // to the pipeline offset
            for epoch in
                current_epoch.iter_range(OffsetPipelineLen::value(params))
            {
                if let Some(ValidatorState::Inactive) =
                    validator_state.get(epoch)
                {
                    return Err(BondError::InactiveValidator(
                        bond_id.validator.clone(),
                    ));
                }
            }
        }
    }

    let update_offset = DynEpochOffset::PipelineLen;

    // Update or create the bond
    let mut value = Bond {
        delta: HashMap::default(),
    };
    value
        .delta
        .insert(current_epoch + update_offset.value(params), amount);
    let bond = match current_bond {
        None => EpochedDelta::init(value, current_epoch, params),
        Some(mut bond) => {
            bond.add(value, current_epoch, params);
            bond
        }
    };

    // Update validator's total deltas
    let delta = TokenChange::from(amount);
    let validator_total_deltas = match validator_total_deltas {
        Some(mut validator_total_deltas) => {
            validator_total_deltas.add_at_offset(
                delta,
                current_epoch,
                update_offset,
                params,
            );
            validator_total_deltas
        }
        None => EpochedDelta::init_at_offset(
            delta,
            current_epoch,
            update_offset,
            params,
        ),
    };

    // Update validator set. This has to be done before we update the
    // `validator_voting_power`, because we need to look-up the validator with
    // its voting power before the change.
    let token_change = TokenChange::from(amount);
    update_validator_set(
        params,
        &bond_id.validator,
        token_change,
        update_offset,
        validator_set,
        &validator_total_deltas,
        current_epoch,
    );

    // Update the validator's and the total voting power.
    let mut validator_voting_power = match validator_voting_power {
        Some(voting_power) => voting_power,
        None => EpochedDelta::init_at_offset(
            VotingPowerDelta::default(),
            current_epoch,
            update_offset,
            params,
        ),
    };
    update_voting_powers(
        params,
        update_offset,
        &validator_total_deltas,
        &mut validator_voting_power,
        total_voting_power,
        current_epoch,
    )
    .map_err(BondError::VotingPowerOverflow)?;

    Ok(BondData {
        bond,
        validator_total_deltas,
        validator_voting_power,
    })
}

struct UnbondData<TokenAmount>
where
    TokenAmount: Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenAmount>
        + BorshDeserialize
        + BorshSerialize,
{
    pub unbond: Unbonds<TokenAmount>,
}

/// Unbond tokens from a validator's bond (self-bond or delegation).
#[allow(clippy::too_many_arguments)]
fn unbond_tokens<Address, TokenAmount, TokenChange>(
    params: &PosParams,
    bond_id: &BondId<Address>,
    bond: &mut Bonds<TokenAmount>,
    unbond: Option<Unbonds<TokenAmount>>,
    amount: TokenAmount,
    validator_total_deltas: &mut ValidatorTotalDeltas<TokenChange>,
    validator_voting_power: &mut ValidatorVotingPowers,
    total_voting_power: &mut TotalVotingPowers,
    validator_set: &mut ValidatorSets<Address>,
    current_epoch: Epoch,
) -> Result<UnbondData<TokenAmount>, UnbondError<Address, TokenAmount>>
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
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + PartialOrd
        + Add<Output = TokenAmount>
        + Into<u64>
        + From<u64>
        + SubAssign
        + BorshDeserialize
        + BorshSerialize,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub
        + From<TokenAmount>
        + Neg<Output = TokenChange>
        + Into<i128>
        + BorshDeserialize
        + BorshSerialize,
{
    // We can unbond tokens that are bonded for a future epoch (not yet
    // active), hence we check the total at the pipeline offset
    let unbondable_amount = bond
        .get_at_offset(current_epoch, DynEpochOffset::PipelineLen, &params)
        .unwrap_or_default()
        .sum();
    if amount > unbondable_amount {
        return Err(UnbondError::UnbondAmountGreaterThanBond(
            amount,
            unbondable_amount,
        ));
    }

    let mut unbond = match unbond {
        Some(unbond) => unbond,
        None => EpochedDelta::init(Unbond::default(), current_epoch, params),
    };

    let update_offset = DynEpochOffset::UnbondingLen;
    let mut to_unbond = amount;
    let to_unbond = &mut to_unbond;
    // Decrement the bond deltas starting from the rightmost value (a bond in a
    // future-most epoch) until whole amount is decremented
    bond.rev_update_while(
        |bonds, _epoch| {
            bonds.delta.retain(|bond_start, bond_amount| {
                let mut deltas = HashMap::default();
                let unbond_end =
                    current_epoch + update_offset.value(params) - 1;
                if to_unbond > bond_amount {
                    *to_unbond -= *bond_amount;
                    deltas.insert((*bond_start, unbond_end), *bond_amount);
                    *bond_amount = 0.into();
                } else {
                    *to_unbond = 0.into();
                    deltas.insert((*bond_start, unbond_end), *to_unbond);
                    *bond_amount -= *to_unbond;
                }
                // For each decremented bond value write a new unbond
                unbond.add(Unbond { deltas }, current_epoch, params);
                // Remove bonds with no tokens left
                *bond_amount != 0.into()
            });

            // Stop the update once all the tokens are unbonded
            *to_unbond != 0.into()
        },
        current_epoch,
        params,
    );

    // Update validator's total deltas
    let delta = -TokenChange::from(amount);
    validator_total_deltas.add(delta, current_epoch, params);

    // Update validator set. This has to be done before we update the
    // `validator_voting_power`, because we need to look-up the validator with
    // its voting power before the change.
    let token_change = TokenChange::from(amount);
    update_validator_set(
        params,
        &bond_id.validator,
        token_change,
        update_offset,
        validator_set,
        validator_total_deltas,
        current_epoch,
    );

    // Update the validator's and the total voting power.
    update_voting_powers(
        params,
        update_offset,
        validator_total_deltas,
        validator_voting_power,
        total_voting_power,
        current_epoch,
    )
    .map_err(UnbondError::VotingPowerOverflow)?;

    Ok(UnbondData { unbond })
}

/// Update validator set when a validator's receives a new bond and when its
/// bond is unbonded (self-bond or delegation).
fn update_validator_set<Address, TokenChange>(
    params: &PosParams,
    validator: &Address,
    token_change: TokenChange,
    change_offset: DynEpochOffset,
    validator_set: &mut ValidatorSets<Address>,
    validator_total_deltas: &ValidatorTotalDeltas<TokenChange>,
    current_epoch: Epoch,
) where
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
    TokenChange: Display
        + Default
        + Debug
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub
        + Into<i128>
        + BorshDeserialize
        + BorshSerialize,
{
    validator_set.update_from_offset(
        |validator_set, epoch| {
            // Find the validator's voting power at the epoch that's being
            // updated from its total deltas
            let tokens_at_epoch =
                validator_total_deltas.get(epoch).unwrap_or_default()
                    + token_change;
            let tokens_at_epoch: i128 = tokens_at_epoch.into();
            let tokens_at_epoch: u64 =
                TryFrom::try_from(tokens_at_epoch).unwrap();
            let voting_power_at_epoch =
                VotingPower::from_tokens(tokens_at_epoch, params);
            let validator_at_epoch = WeightedValidator {
                voting_power: voting_power_at_epoch,
                address: validator.clone(),
            };

            if validator_set.inactive.contains(&validator_at_epoch) {
                let min_active_validator = validator_set.active.first_shim();
                let min_voting_power = min_active_validator
                    .map(|v| v.voting_power)
                    .unwrap_or_default();
                if voting_power_at_epoch > min_voting_power {
                    let deactivate_min = validator_set.active.pop_first_shim();
                    let popped =
                        validator_set.inactive.remove(&validator_at_epoch);
                    debug_assert!(popped);
                    validator_set.active.insert(validator_at_epoch);
                    if let Some(deactivate_min) = deactivate_min {
                        validator_set.inactive.insert(deactivate_min);
                    }
                }
            } else {
                debug_assert!(
                    validator_set.active.contains(&validator_at_epoch)
                );
                let max_inactive_validator = validator_set.inactive.last_shim();
                let max_voting_power = max_inactive_validator
                    .map(|v| v.voting_power)
                    .unwrap_or_default();
                if voting_power_at_epoch < max_voting_power {
                    let activate_max = validator_set.inactive.pop_last_shim();
                    let popped =
                        validator_set.active.remove(&validator_at_epoch);
                    debug_assert!(popped);
                    validator_set.inactive.insert(validator_at_epoch);
                    if let Some(activate_max) = activate_max {
                        validator_set.active.insert(activate_max);
                    }
                }
            }
        },
        current_epoch,
        change_offset,
        params,
    )
}

/// Update the validator's voting power and the total voting power.
fn update_voting_powers<TokenChange>(
    params: &PosParams,
    change_offset: DynEpochOffset,
    validator_total_deltas: &ValidatorTotalDeltas<TokenChange>,
    validator_voting_power: &mut ValidatorVotingPowers,
    total_voting_power: &mut TotalVotingPowers,
    current_epoch: Epoch,
) -> Result<(), TryFromIntError>
where
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub
        + Into<i128>
        + BorshDeserialize
        + BorshSerialize,
{
    // Update validator's voting power. Recalculate it from validator's total
    // deltas.
    let total_deltas_at_pipeline = validator_total_deltas
        .get_at_offset(current_epoch, change_offset, params)
        .unwrap_or_default();
    let total_deltas_at_pipeline: i128 = total_deltas_at_pipeline.into();
    let total_deltas_at_pipeline: u64 =
        TryFrom::try_from(total_deltas_at_pipeline).unwrap();
    let voting_power_at_pipeline = validator_voting_power
        .get_at_offset(current_epoch, change_offset, params)
        .unwrap_or_default();
    let voting_power_delta =
        VotingPowerDelta::try_from_tokens(total_deltas_at_pipeline, params)?
            - voting_power_at_pipeline;
    validator_voting_power.add_at_offset(
        voting_power_delta,
        current_epoch,
        change_offset,
        params,
    );

    // Update total voting power
    total_voting_power.add_at_offset(
        voting_power_delta,
        current_epoch,
        change_offset,
        params,
    );
    Ok(())
}

struct WithdrawData<TokenAmount>
where
    TokenAmount: Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenAmount>
        + BorshDeserialize
        + BorshSerialize,
{
    pub unbond: Unbonds<TokenAmount>,
    pub withdrawn_amount: TokenAmount,
}

/// Withdraw tokens from unbonds of self-bonds or delegations.
fn withdraw_unbonds<Address, TokenAmount>(
    params: &PosParams,
    bond_id: &BondId<Address>,
    unbond: Option<Unbonds<TokenAmount>>,
    current_epoch: Epoch,
) -> Result<WithdrawData<TokenAmount>, WithdrawError<Address>>
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
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + PartialOrd
        + Add<Output = TokenAmount>
        + Into<u64>
        + From<u64>
        + SubAssign
        + BorshDeserialize
        + BorshSerialize,
{
    let mut unbond =
        unbond.ok_or_else(|| WithdrawError::NoUnbondFound(bond_id.clone()))?;
    let withdrawable_unbond = unbond
        .get(current_epoch)
        .ok_or_else(|| WithdrawError::NoWithdrawableUnbond(bond_id.clone()))?;
    let withdrawn_amount = withdrawable_unbond.deltas.iter().fold(
        TokenAmount::default(),
        |sum, ((_epoch_start, _epoch_end), amount)| {
            // TODO check slashes
            sum + *amount
        },
    );
    unbond.delete_current(current_epoch, &params);
    Ok(WithdrawData {
        unbond,
        withdrawn_amount,
    })
}
