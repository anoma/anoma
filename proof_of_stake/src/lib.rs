mod btree_set;
pub mod epoched;
pub mod parameters;
pub mod types;

use core::fmt::Debug;
use std::collections::{BTreeSet, HashMap};
use std::convert::TryFrom;
use std::fmt::Display;
use std::hash::Hash;
use std::num::TryFromIntError;
use std::ops::{self, Add, Neg, Sub, SubAssign};

use epoched::{
    DynEpochOffset, EpochOffset, Epoched, EpochedDelta, OffsetPipelineLen,
    OffsetUnboundingLen,
};
use parameters::PosParams;
use thiserror::Error;
use types::{
    Epoch, GenesisValidator, Unbond, ValidatorSet, ValidatorState, VotingPower,
    VotingPowerDelta,
};

use crate::btree_set::BTreeSetShims;
use crate::types::{Bond, BondId, WeightedValidator};

pub trait Pos {
    type Address: Display
        + Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash;
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
        + SubAssign;
    type TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = Self::TokenChange>
        + Sub
        + From<Self::TokenAmount>
        + Into<i128>
        + Neg<Output = Self::TokenChange>;
    type PublicKey: Debug + Clone;

    /// Address of the PoS account
    const POS_ADDRESS: Self::Address;
    /// Address of the staking token
    const STAKING_TOKEN_ADDRESS: Self::Address;

    // TODO it may be nicer to instead provide generic functions for storage
    // write/read and a way for implementors to assign storage keys and convert
    // data into/from storage values (e.g. our ledger storage key type and
    // borsh encoding for values)
    // e.g.
    // fn write(&mut self, key: &impl StorageKey, value: &impl StorageValue);
    // fn read(&self, key: &impl StorageKey) -> Option<impl StorageValue>;
    // fn params_key() -> impl StorageKey;
    // fn validator_staking_reward_address_key(address: &Self::Address)
    // -> impl StorageKey;

    fn write_params(&mut self, params: &PosParams);
    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: Self::Address,
    );
    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: Epoched<Self::PublicKey, OffsetPipelineLen>,
    );
    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: Epoched<ValidatorState, OffsetPipelineLen>,
    );
    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: EpochedDelta<Self::TokenChange, OffsetUnboundingLen>,
    );
    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>,
    );
    fn write_bond(
        &mut self,
        key: &BondId<Self::Address>,
        value: EpochedDelta<Bond<Self::TokenAmount>, OffsetPipelineLen>,
    );
    fn write_unbond(
        &mut self,
        key: &BondId<Self::Address>,
        value: EpochedDelta<Unbond<Self::TokenAmount>, OffsetUnboundingLen>,
    );
    fn write_validator_set(
        &mut self,
        value: Epoched<ValidatorSet<Self::Address>, OffsetUnboundingLen>,
    );
    fn write_total_voting_power(
        &mut self,
        value: EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>,
    );

    fn read_params(&self) -> PosParams;
    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> Option<Self::Address>;
    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<Epoched<Self::PublicKey, OffsetPipelineLen>>;
    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<Epoched<ValidatorState, OffsetPipelineLen>>;
    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<EpochedDelta<Self::TokenChange, OffsetUnboundingLen>>;
    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>>;
    fn read_bond(
        &mut self,
        key: &BondId<Self::Address>,
    ) -> Option<EpochedDelta<Bond<Self::TokenAmount>, OffsetPipelineLen>>;
    fn read_unbond(
        &mut self,
        key: &BondId<Self::Address>,
    ) -> Option<EpochedDelta<Unbond<Self::TokenAmount>, OffsetUnboundingLen>>;
    fn read_validator_set(
        &mut self,
    ) -> Epoched<ValidatorSet<Self::Address>, OffsetUnboundingLen>;
    fn read_total_voting_power(
        &mut self,
    ) -> EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>;

    fn delete_bond(&mut self, key: &BondId<Self::Address>);
    fn delete_unbond(&mut self, key: &BondId<Self::Address>);

    fn transfer(
        &mut self,
        token: &Self::Address,
        amount: Self::TokenAmount,
        source: &Self::Address,
        target: &Self::Address,
    );

    /// Initialize the PoS system storage data in the genesis block for the
    /// given PoS parameters and initial validator set. The validators'
    /// tokens will be put into self-bonds. The given PoS parameters are written
    /// with the [`Pos::write_params`] method.
    fn init_genesis(
        &mut self,
        params: &PosParams,
        validators: impl AsRef<
            [GenesisValidator<
                Self::Address,
                Self::TokenAmount,
                Self::PublicKey,
            >],
        >,
        current_epoch: impl Into<Epoch>,
    ) -> Result<(), GenesisError> {
        let current_epoch = current_epoch.into();
        self.write_params(params);

        let GenesisData {
            validators,
            validator_set,
            total_voting_power,
        } = init_genesis(params, validators.as_ref().iter(), current_epoch)?;

        for res in validators {
            let GenesisValidatorData {
                ref address,
                staking_reward_address,
                consensus_key,
                state,
                total_deltas,
                voting_power,
                bond: (bond_id, bond),
            } = res?;
            self.write_validator_staking_reward_address(
                address,
                staking_reward_address,
            );
            self.write_validator_consensus_key(address, consensus_key);
            self.write_validator_state(address, state);
            self.write_validator_total_deltas(address, total_deltas);
            self.write_validator_voting_power(address, voting_power);
            self.write_bond(&bond_id, bond);
        }
        self.write_validator_set(validator_set);
        self.write_total_voting_power(total_voting_power);
        Ok(())
    }

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
            &Self::STAKING_TOKEN_ADDRESS,
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
            &Self::STAKING_TOKEN_ADDRESS,
            withdrawn_amount,
            &Self::POS_ADDRESS,
            address,
        );

        Ok(())
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
    Address: Display + Debug + Clone + Ord + Hash,
    TokenAmount: Debug + Default + Clone + ops::Add<Output = TokenAmount>,
    TokenChange: Debug + Copy + ops::Add<Output = TokenChange>,
    PK: Debug + Clone,
{
    validators: Validators,
    /// Active and inactive validator sets
    validator_set: Epoched<ValidatorSet<Address>, OffsetUnboundingLen>,
    /// The sum of all active and inactive validators' voting power
    total_voting_power: EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>,
}
struct GenesisValidatorData<Address, TokenAmount, TokenChange, PK>
where
    Address: Display + Debug + Clone + Ord + Hash,
    TokenAmount: Debug + Default + Clone + ops::Add<Output = TokenAmount>,
    TokenChange: Debug + Copy + ops::Add<Output = TokenChange>,
    PK: Debug + Clone,
{
    address: Address,
    staking_reward_address: Address,
    consensus_key: Epoched<PK, OffsetPipelineLen>,
    state: Epoched<ValidatorState, OffsetPipelineLen>,
    total_deltas: EpochedDelta<TokenChange, OffsetUnboundingLen>,
    voting_power: EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>,
    bond: (
        BondId<Address>,
        EpochedDelta<Bond<TokenAmount>, OffsetPipelineLen>,
    ),
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
    Address: 'a + Display + Debug + Clone + Ord + Hash,
    TokenAmount: 'a
        + Debug
        + Default
        + Clone
        + ops::Add<Output = TokenAmount>
        + Into<u64>,
    TokenChange:
        'a + Debug + Copy + ops::Add<Output = TokenChange> + From<TokenAmount>,
    PK: 'a + Debug + Clone,
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
    PK: Debug + Clone,
{
    consensus_key: Epoched<PK, OffsetPipelineLen>,
    state: Epoched<ValidatorState, OffsetPipelineLen>,
}

/// A function that initialized data for a new validator.
fn become_validator<Address, PK>(
    params: &PosParams,
    address: &Address,
    consensus_key: &PK,
    validator_set: &mut Epoched<ValidatorSet<Address>, OffsetUnboundingLen>,
    current_epoch: Epoch,
) -> BecomeValidatorData<PK>
where
    Address: Debug + Clone + Ord + Hash,
    PK: Debug + Clone,
{
    let consensus_key =
        Epoched::init(consensus_key.clone(), current_epoch, params);
    let mut state =
        Epoched::init(ValidatorState::Pending, current_epoch, params);
    state.set(ValidatorState::Candidate, current_epoch, params);
    validator_set.update_from_offset(
        |validator_set, _epoch| {
            validator_set.inactive.insert(WeightedValidator {
                voting_power: VotingPower::default(),
                address: address.clone(),
            });
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
    TokenAmount: Debug + Default + Clone + Copy + Add<Output = TokenAmount>,
    TokenChange: Debug + Clone + Copy + Add<Output = TokenChange>,
{
    pub bond: EpochedDelta<Bond<TokenAmount>, OffsetPipelineLen>,
    pub validator_total_deltas: EpochedDelta<TokenChange, OffsetUnboundingLen>,
    pub validator_voting_power:
        EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>,
}

/// Bond tokens to a validator (self-bond or delegation).
#[allow(clippy::too_many_arguments)]
fn bond_tokens<Address, TokenAmount, TokenChange>(
    params: &PosParams,
    validator_state: Option<Epoched<ValidatorState, OffsetPipelineLen>>,
    bond_id: &BondId<Address>,
    current_bond: Option<EpochedDelta<Bond<TokenAmount>, OffsetPipelineLen>>,
    amount: TokenAmount,
    validator_total_deltas: Option<
        EpochedDelta<TokenChange, OffsetUnboundingLen>,
    >,
    validator_voting_power: Option<
        EpochedDelta<VotingPowerDelta, OffsetUnboundingLen>,
    >,
    total_voting_power: &mut EpochedDelta<
        VotingPowerDelta,
        OffsetUnboundingLen,
    >,
    validator_set: &mut Epoched<ValidatorSet<Address>, OffsetUnboundingLen>,
    current_epoch: Epoch,
) -> Result<BondData<TokenAmount, TokenChange>, BondError<Address>>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenAmount>
        + Into<u64>,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub
        + From<TokenAmount>
        + Into<i128>,
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
                current_epoch.iter_range(OffsetPipelineLen::value(&params))
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

    // Update or create the bond
    let mut value = Bond {
        delta: HashMap::default(),
    };
    value.delta.insert(
        current_epoch + DynEpochOffset::PipelineLen.value(params),
        amount,
    );
    let bond = match current_bond {
        None => EpochedDelta::init(value, current_epoch, &params),
        Some(mut bond) => {
            bond.add(value, current_epoch, &params);
            bond
        }
    };

    // Update validator's total deltas
    let delta = TokenChange::from(amount);
    let validator_total_deltas = match validator_total_deltas {
        Some(mut validator_total_deltas) => {
            validator_total_deltas.add(delta, current_epoch, params);
            validator_total_deltas
        }
        None => EpochedDelta::init_at_offset(
            delta,
            current_epoch,
            DynEpochOffset::PipelineLen,
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
        DynEpochOffset::PipelineLen,
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
            DynEpochOffset::PipelineLen,
            params,
        ),
    };
    update_voting_powers(
        params,
        DynEpochOffset::PipelineLen,
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
    TokenAmount: Debug + Default + Clone + Copy + Add<Output = TokenAmount>,
{
    pub unbond: EpochedDelta<Unbond<TokenAmount>, OffsetUnboundingLen>,
}

/// Unbond tokens from a validator's bond (self-bond or delegation).
#[allow(clippy::too_many_arguments)]
fn unbond_tokens<Address, TokenAmount, TokenChange>(
    params: &PosParams,
    bond_id: &BondId<Address>,
    bond: &mut EpochedDelta<Bond<TokenAmount>, OffsetPipelineLen>,
    unbond: Option<EpochedDelta<Unbond<TokenAmount>, OffsetUnboundingLen>>,
    amount: TokenAmount,
    validator_total_deltas: &mut EpochedDelta<TokenChange, OffsetUnboundingLen>,
    validator_voting_power: &mut EpochedDelta<
        VotingPowerDelta,
        OffsetUnboundingLen,
    >,
    total_voting_power: &mut EpochedDelta<
        VotingPowerDelta,
        OffsetUnboundingLen,
    >,
    validator_set: &mut Epoched<ValidatorSet<Address>, OffsetUnboundingLen>,
    current_epoch: Epoch,
) -> Result<UnbondData<TokenAmount>, UnbondError<Address, TokenAmount>>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + PartialOrd
        + Add<Output = TokenAmount>
        + Into<u64>
        + From<u64>
        + SubAssign,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub
        + From<TokenAmount>
        + Neg<Output = TokenChange>
        + Into<i128>,
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

    let mut to_unbond = amount;
    let to_unbond = &mut to_unbond;
    // Decrement the bond deltas starting from the rightmost value (a bond in a
    // future-most epoch) until whole amount is decremented
    bond.rev_update_while(
        |bonds, _epoch| {
            bonds.delta.retain(|bond_start, bond_amount| {
                let mut deltas = HashMap::default();
                let unbond_end = current_epoch
                    + DynEpochOffset::UnbondingLen.value(params)
                    - 1;
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
        DynEpochOffset::UnbondingLen,
        validator_set,
        &validator_total_deltas,
        current_epoch,
    );

    // Update the validator's and the total voting power.
    update_voting_powers(
        params,
        DynEpochOffset::UnbondingLen,
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
    validator_set: &mut Epoched<ValidatorSet<Address>, OffsetUnboundingLen>,
    validator_total_deltas: &EpochedDelta<TokenChange, OffsetUnboundingLen>,
    current_epoch: Epoch,
) where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
    TokenChange: Display
        + Default
        + Debug
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub
        + Into<i128>,
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
    validator_total_deltas: &EpochedDelta<TokenChange, OffsetUnboundingLen>,
    validator_voting_power: &mut EpochedDelta<
        VotingPowerDelta,
        OffsetUnboundingLen,
    >,
    total_voting_power: &mut EpochedDelta<
        VotingPowerDelta,
        OffsetUnboundingLen,
    >,
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
        + Into<i128>,
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
    let voting_power_delta = voting_power_at_pipeline
        + VotingPowerDelta::try_from_tokens(total_deltas_at_pipeline, params)?;
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
    TokenAmount: Debug + Default + Clone + Copy + Add<Output = TokenAmount>,
{
    pub unbond: EpochedDelta<Unbond<TokenAmount>, OffsetUnboundingLen>,
    pub withdrawn_amount: TokenAmount,
}

/// Withdraw tokens from unbonds of self-bonds or delegations.
fn withdraw_unbonds<Address, TokenAmount>(
    params: &PosParams,
    bond_id: &BondId<Address>,
    unbond: Option<EpochedDelta<Unbond<TokenAmount>, OffsetUnboundingLen>>,
    current_epoch: Epoch,
) -> Result<WithdrawData<TokenAmount>, WithdrawError<Address>>
where
    Address: Display + Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash,
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + PartialOrd
        + Add<Output = TokenAmount>
        + Into<u64>
        + From<u64>
        + SubAssign,
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
