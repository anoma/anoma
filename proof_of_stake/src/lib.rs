//! Proof of Stake system.
//!
//! TODO: We might need to storage both active and total validator set voting
//! power. For consensus, we only consider active validator set voting power,
//! but for other activities in which inactive validators can participate (e.g.
//! voting on a protocol parameter changes, upgrades, default VP changes) we
//! should use the total validator set voting power.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![warn(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

pub mod btree_set;
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
use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
use epoched::{
    DynEpochOffset, EpochOffset, Epoched, EpochedDelta, OffsetPipelineLen,
};
use parameters::PosParams;
use thiserror::Error;
use types::{
    ActiveValidator, Bonds, Epoch, GenesisValidator, Slash, SlashType, Slashes,
    TotalVotingPowers, Unbond, Unbonds, ValidatorConsensusKeys, ValidatorSet,
    ValidatorSetUpdate, ValidatorSets, ValidatorState, ValidatorStates,
    ValidatorTotalDeltas, ValidatorVotingPowers, VotingPower, VotingPowerDelta,
};

use crate::btree_set::BTreeSetShims;
use crate::types::{Bond, BondId, WeightedValidator};

/// Read-only part of the PoS system
pub trait PosReadOnly {
    /// Address type
    type Address: Display
        + Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema;
    /// Token amount type
    type TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = Self::TokenAmount>
        + AddAssign
        + Sub
        + PartialOrd
        + Into<u64>
        + From<u64>
        + Into<Self::TokenChange>
        + SubAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema;
    /// Token change type
    type TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = Self::TokenChange>
        + Sub<Output = Self::TokenChange>
        + From<Self::TokenAmount>
        + Into<i128>
        + Neg<Output = Self::TokenChange>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema;
    /// Cryptographic public key type
    type PublicKey: Debug
        + Clone
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema;

    /// Address of the PoS account
    const POS_ADDRESS: Self::Address;
    /// Address of the staking token
    /// TODO: this should be `const`, but in the ledger `address::xan` is not a
    /// `const fn`
    fn staking_token_address() -> Self::Address;

    /// Read PoS parameters.
    fn read_pos_params(&self) -> PosParams;
    /// Read PoS validator's staking reward address.
    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> Option<Self::Address>;
    /// Read PoS validator's consensus key (used for signing block votes).
    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys<Self::PublicKey>>;
    /// Read PoS validator's state.
    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorStates>;
    /// Read PoS validator's total deltas of their bonds (validator self-bonds
    /// and delegations).
    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorTotalDeltas<Self::TokenChange>>;
    /// Read PoS validator's voting power.
    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorVotingPowers>;
    /// Read PoS slashes applied to a validator.
    fn read_validator_slashes(&self, key: &Self::Address) -> Vec<Slash>;
    /// Read PoS bond (validator self-bond or a delegation).
    fn read_bond(
        &self,
        key: &BondId<Self::Address>,
    ) -> Option<Bonds<Self::TokenAmount>>;
    /// Read PoS unbond (unbonded tokens from validator self-bond or a
    /// delegation).
    fn read_unbond(
        &self,
        key: &BondId<Self::Address>,
    ) -> Option<Unbonds<Self::TokenAmount>>;
    /// Read PoS validator set (active and inactive).
    fn read_validator_set(&self) -> ValidatorSets<Self::Address>;
    /// Read PoS total voting power of all validators (active and inactive).
    fn read_total_voting_power(&self) -> TotalVotingPowers;
}

/// PoS system trait to be implemented in integration that can read and write
/// PoS data.
pub trait PosActions: PosReadOnly {
    /// Write PoS parameters.
    fn write_pos_params(&mut self, params: &PosParams);
    /// Write PoS validator's raw hash of its consensus key.
    fn write_validator_address_raw_hash(
        &mut self,
        address: &Self::Address,
        consensus_key: &Self::PublicKey,
    );
    /// Write PoS validator's staking reward address, into which staking rewards
    /// will be credited.
    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: Self::Address,
    );
    /// Write PoS validator's consensus key (used for signing block votes).
    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: ValidatorConsensusKeys<Self::PublicKey>,
    );
    /// Write PoS validator's state.
    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: ValidatorStates,
    );
    /// Write PoS validator's total deltas of their bonds (validator self-bonds
    /// and delegations).
    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: ValidatorTotalDeltas<Self::TokenChange>,
    );
    /// Write PoS validator's voting power.
    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: ValidatorVotingPowers,
    );
    /// Write PoS bond (validator self-bond or a delegation).
    fn write_bond(
        &mut self,
        key: &BondId<Self::Address>,
        value: Bonds<Self::TokenAmount>,
    );
    /// Write PoS unbond (unbonded tokens from validator self-bond or a
    /// delegation).
    fn write_unbond(
        &mut self,
        key: &BondId<Self::Address>,
        value: Unbonds<Self::TokenAmount>,
    );
    /// Write PoS validator set (active and inactive).
    fn write_validator_set(&mut self, value: ValidatorSets<Self::Address>);
    /// Write PoS total voting power of all validators (active and inactive).
    fn write_total_voting_power(&mut self, value: TotalVotingPowers);

    /// Delete an emptied PoS bond (validator self-bond or a delegation).
    fn delete_bond(&mut self, key: &BondId<Self::Address>);
    /// Delete an emptied PoS unbond (unbonded tokens from validator self-bond
    /// or a delegation).
    fn delete_unbond(&mut self, key: &BondId<Self::Address>);

    /// Transfer tokens from the `src` to the `dest`.
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
        let params = self.read_pos_params();
        let mut validator_set = self.read_validator_set();
        if self.is_validator(address) {
            return Err(BecomeValidatorError::AlreadyValidator(
                address.clone(),
            ));
        }
        if address == staking_reward_address {
            return Err(
                BecomeValidatorError::StakingRewardAddressEqValidatorAddress(
                    address.clone(),
                ),
            );
        }
        let consensus_key_clone = consensus_key.clone();
        let BecomeValidatorData {
            consensus_key,
            state,
            total_deltas,
            voting_power,
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
        self.write_validator_address_raw_hash(address, &consensus_key_clone);
        self.write_validator_total_deltas(address, total_deltas);
        self.write_validator_voting_power(address, voting_power);
        Ok(())
    }

    /// Check if the given address is a validator by checking that it has some
    /// state.
    fn is_validator(&self, address: &Self::Address) -> bool {
        self.read_validator_state(address).is_some()
    }

    /// Self-bond tokens to a validator when `source` is `None` or equal to
    /// the `validator` address, or delegate tokens from the `source` to the
    /// `validator`.
    fn bond_tokens(
        &mut self,
        source: Option<&Self::Address>,
        validator: &Self::Address,
        amount: Self::TokenAmount,
        current_epoch: impl Into<Epoch>,
    ) -> Result<(), BondError<Self::Address>> {
        let current_epoch = current_epoch.into();
        if let Some(source) = source {
            if source != validator && self.is_validator(source) {
                return Err(BondError::SourceMustNotBeAValidator(
                    source.clone(),
                ));
            }
        }
        let params = self.read_pos_params();
        let validator_state = self.read_validator_state(validator);
        let source = source.unwrap_or(validator);
        let bond_id = BondId {
            source: source.clone(),
            validator: validator.clone(),
        };
        let bond = self.read_bond(&bond_id);
        let validator_total_deltas =
            self.read_validator_total_deltas(validator);
        let validator_voting_power =
            self.read_validator_voting_power(validator);
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
        self.write_validator_total_deltas(validator, validator_total_deltas);
        self.write_validator_voting_power(validator, validator_voting_power);
        self.write_total_voting_power(total_voting_power);
        self.write_validator_set(validator_set);

        // Transfer the bonded tokens from the source to PoS
        self.transfer(
            &Self::staking_token_address(),
            amount,
            source,
            &Self::POS_ADDRESS,
        );

        Ok(())
    }

    /// Unbond self-bonded tokens from a validator when `source` is `None` or
    /// equal to the `validator` address, or unbond delegated tokens from
    /// the `source` to the `validator`.
    fn unbond_tokens(
        &mut self,
        source: Option<&Self::Address>,
        validator: &Self::Address,
        amount: Self::TokenAmount,
        current_epoch: impl Into<Epoch>,
    ) -> Result<(), UnbondError<Self::Address, Self::TokenAmount>> {
        let current_epoch = current_epoch.into();
        let params = self.read_pos_params();
        let source = source.unwrap_or(validator);
        let bond_id = BondId {
            source: source.clone(),
            validator: validator.clone(),
        };
        let mut bond =
            self.read_bond(&bond_id).ok_or(UnbondError::NoBondFound)?;
        let unbond = self.read_unbond(&bond_id);
        let mut validator_total_deltas =
            self.read_validator_total_deltas(validator).ok_or_else(|| {
                UnbondError::ValidatorHasNoBonds(validator.clone())
            })?;
        let mut validator_voting_power =
            self.read_validator_voting_power(validator).ok_or_else(|| {
                UnbondError::ValidatorHasNoVotingPower(validator.clone())
            })?;
        let slashes = self.read_validator_slashes(validator);
        let mut total_voting_power = self.read_total_voting_power();
        let mut validator_set = self.read_validator_set();

        let UnbondData { unbond } = unbond_tokens(
            &params,
            &bond_id,
            &mut bond,
            unbond,
            amount,
            slashes,
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
        self.write_validator_total_deltas(validator, validator_total_deltas);
        self.write_validator_voting_power(validator, validator_voting_power);
        self.write_total_voting_power(total_voting_power);
        self.write_validator_set(validator_set);

        Ok(())
    }

    /// Withdraw unbonded tokens from a self-bond to a validator when `source`
    /// is `None` or equal to the `validator` address, or withdraw unbonded
    /// tokens delegated to the `validator` to the `source`.
    fn withdraw_tokens(
        &mut self,
        source: Option<&Self::Address>,
        validator: &Self::Address,
        current_epoch: impl Into<Epoch>,
    ) -> Result<Self::TokenAmount, WithdrawError<Self::Address>> {
        let current_epoch = current_epoch.into();
        let params = self.read_pos_params();
        let source = source.unwrap_or(validator);
        let bond_id = BondId {
            source: source.clone(),
            validator: validator.clone(),
        };

        let unbond = self.read_unbond(&bond_id);
        let slashes = self.read_validator_slashes(&bond_id.validator);

        let WithdrawData {
            unbond,
            withdrawn,
            slashed,
        } = withdraw_unbonds(
            &params,
            &bond_id,
            unbond,
            slashes,
            current_epoch,
        )?;

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

        // Transfer the tokens from PoS back to the source
        self.transfer(
            &Self::staking_token_address(),
            withdrawn,
            &Self::POS_ADDRESS,
            source,
        );

        Ok(slashed)
    }
}

/// PoS system base trait for system initialization on genesis block, updating
/// the validator on a new epoch and applying slashes.
pub trait PosBase {
    /// Address type
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
        + BorshSerialize
        + BorshSchema;
    /// Token amount type
    type TokenAmount: 'static
        + Display
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = Self::TokenAmount>
        + AddAssign
        + Sub
        + PartialOrd
        + Into<u64>
        + From<u64>
        + Into<Self::TokenChange>
        + SubAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema;
    /// Token change type
    type TokenChange: 'static
        + Display
        + Debug
        + Default
        + Clone
        + Copy
        + PartialOrd
        + Add<Output = Self::TokenChange>
        + Sub<Output = Self::TokenChange>
        + From<Self::TokenAmount>
        + From<i128>
        + Into<i128>
        + Neg<Output = Self::TokenChange>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema;
    /// Cryptographic public key type
    type PublicKey: 'static
        + Debug
        + Clone
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema;

    /// Address of the PoS account
    const POS_ADDRESS: Self::Address;
    /// Address of the staking token
    /// TODO: this should be `const`, but in the ledger `address::xan` is not a
    /// `const fn`
    fn staking_token_address() -> Self::Address;
    /// Address of the slash pool, into which slashed tokens are transferred.
    const POS_SLASH_POOL_ADDRESS: Self::Address;

    /// Read PoS parameters.
    fn read_pos_params(&self) -> PosParams;
    /// Read PoS raw hash of validator's consensus key.
    fn read_validator_address_raw_hash(
        &self,
        raw_hash: impl AsRef<str>,
    ) -> Option<Self::Address>;
    /// Read PoS validator's consensus key (used for signing block votes).
    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys<Self::PublicKey>>;
    /// Read PoS validator's state.
    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorStates>;
    /// Read PoS validator's total deltas of their bonds (validator self-bonds
    /// and delegations).
    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorTotalDeltas<Self::TokenChange>>;
    /// Read PoS validator's voting power.
    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorVotingPowers>;
    /// Read PoS slashes applied to a validator.
    fn read_validator_slashes(&self, key: &Self::Address) -> Slashes;
    /// Read PoS validator set (active and inactive).
    fn read_validator_set(&self) -> ValidatorSets<Self::Address>;
    /// Read PoS total voting power of all validators (active and inactive).
    fn read_total_voting_power(&self) -> TotalVotingPowers;

    /// Write PoS parameters.
    fn write_pos_params(&mut self, params: &PosParams);
    /// Write PoS validator's raw hash of its consensus key.
    fn write_validator_address_raw_hash(
        &mut self,
        address: &Self::Address,
        consensus_key: &Self::PublicKey,
    );
    /// Write PoS validator's staking reward address, into which staking rewards
    /// will be credited.
    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: &Self::Address,
    );
    /// Write PoS validator's consensus key (used for signing block votes).
    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: &ValidatorConsensusKeys<Self::PublicKey>,
    );
    /// Write PoS validator's state.
    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: &ValidatorStates,
    );
    /// Write PoS validator's total deltas of their bonds (validator self-bonds
    /// and delegations).
    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: &ValidatorTotalDeltas<Self::TokenChange>,
    );
    /// Write PoS validator's voting power.
    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: &ValidatorVotingPowers,
    );
    /// Write (append) PoS slash applied to a validator.
    fn write_validator_slash(
        &mut self,
        validator: &Self::Address,
        value: Slash,
    );
    /// Write PoS bond (validator self-bond or a delegation).
    fn write_bond(
        &mut self,
        key: &BondId<Self::Address>,
        value: &Bonds<Self::TokenAmount>,
    );
    /// Write PoS validator set (active and inactive).
    fn write_validator_set(&mut self, value: &ValidatorSets<Self::Address>);
    /// Read PoS total voting power of all validators (active and inactive).
    fn write_total_voting_power(&mut self, value: &TotalVotingPowers);
    /// Initialize staking reward account with the given public key.
    fn init_staking_reward_account(
        &mut self,
        address: &Self::Address,
        pk: &Self::PublicKey,
    );
    /// Credit tokens to the `target` account. This should only be used at
    /// genesis.
    fn credit_tokens(
        &mut self,
        token: &Self::Address,
        target: &Self::Address,
        amount: Self::TokenAmount,
    );
    /// Transfer tokens from the `src` to the `dest`.
    fn transfer(
        &mut self,
        token: &Self::Address,
        amount: Self::TokenAmount,
        src: &Self::Address,
        dest: &Self::Address,
    );

    /// Initialize the PoS system storage data in the genesis block for the
    /// given PoS parameters and initial validator set. The validators'
    /// tokens will be put into self-bonds. The given PoS parameters are written
    /// with the [`PosBase::write_pos_params`] method.
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
        self.write_pos_params(params);

        let GenesisData {
            validators,
            validator_set,
            total_voting_power,
            total_bonded_balance,
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
            self.write_validator_address_raw_hash(
                address,
                consensus_key
                    .get(current_epoch)
                    .expect("Consensus key must be set"),
            );
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
        // Credit the bonded tokens to the PoS account
        self.credit_tokens(
            &Self::staking_token_address(),
            &Self::POS_ADDRESS,
            total_bonded_balance,
        );
        Ok(())
    }

    /// Calls a closure on each validator update element.
    fn validator_set_update(
        &self,
        current_epoch: impl Into<Epoch>,
        f: impl FnMut(ValidatorSetUpdate<Self::PublicKey>),
    ) {
        let current_epoch: Epoch = current_epoch.into();
        let current_epoch_u64: u64 = current_epoch.into();
        // INVARIANT: We can only access the previous epochs data, because
        // this function is called on a beginning of a new block, before
        // anything else could be updated (in epoched data updates, the old
        // epochs data are merged with the current one).
        let previous_epoch: Option<Epoch> = if current_epoch_u64 == 0 {
            None
        } else {
            Some(Epoch::from(current_epoch_u64 - 1))
        };
        let validators = self.read_validator_set();
        let cur_validators = validators.get(current_epoch).unwrap();
        let prev_validators =
            previous_epoch.and_then(|epoch| validators.get(epoch));

        // If the validator never been active before and it doesn't have more
        // than 0 voting power, we should not tell Tendermint to update it until
        // it does. Tendermint uses 0 voting power as a way to signal
        // that a validator has been removed from the validator set, but
        // fails if we attempt to give it a new validator with 0 voting
        // power.
        // For active validators, this would only ever happen until all the
        // validator slots are filled with non-0 voting power validators, but we
        // still need to guard against it.
        let active_validators = cur_validators.active.iter().filter_map(
            |validator: &WeightedValidator<_>| {
                // If the validators set from previous epoch contains the same
                // validator, it means its voting power hasn't changed and hence
                // doesn't need to updated.
                if let (Some(prev_epoch), Some(prev_validators)) =
                    (previous_epoch, prev_validators)
                {
                    if prev_validators.active.contains(validator) {
                        println!(
                            "skipping validator update, still the same {}",
                            validator.address
                        );
                        return None;
                    }
                    if validator.voting_power == 0.into() {
                        // If the validator was `Pending` in the previous epoch,
                        // it means that it just was just added to validator
                        // set. We have to skip it, because it's 0.
                        if let Some(state) =
                            self.read_validator_state(&validator.address)
                        {
                            if let Some(ValidatorState::Pending) =
                                state.get(prev_epoch)
                            {
                                println!(
                                    "skipping validator update, it's new {}",
                                    validator.address
                                );
                                return None;
                            }
                        }
                    }
                }
                let consensus_key = self
                    .read_validator_consensus_key(&validator.address)
                    .unwrap()
                    .get(current_epoch)
                    .unwrap()
                    .clone();
                Some(ValidatorSetUpdate::Active(ActiveValidator {
                    consensus_key,
                    voting_power: validator.voting_power,
                }))
            },
        );
        let inactive_validators = cur_validators.inactive.iter().filter_map(
            |validator: &WeightedValidator<Self::Address>| {
                // If the validators set from previous epoch contains the same
                // validator, it means its voting power hasn't changed and hence
                // doesn't need to updated.
                if let (Some(prev_epoch), Some(prev_validators)) =
                    (previous_epoch, prev_validators)
                {
                    if prev_validators.inactive.contains(validator) {
                        return None;
                    }
                    if validator.voting_power == 0.into() {
                        // If the validator was `Pending` in the previous epoch,
                        // it means that it just was just added to validator
                        // set. We have to skip it, because it's 0.
                        if let Some(state) =
                            self.read_validator_state(&validator.address)
                        {
                            if let Some(ValidatorState::Pending) =
                                state.get(prev_epoch)
                            {
                                return None;
                            }
                        }
                    }
                }
                let consensus_key = self
                    .read_validator_consensus_key(&validator.address)
                    .unwrap()
                    .get(current_epoch)
                    .unwrap()
                    .clone();
                Some(ValidatorSetUpdate::Deactivated(consensus_key))
            },
        );
        active_validators.chain(inactive_validators).for_each(f)
    }

    /// Apply a slash to a byzantine validator for the given evidence.
    fn slash(
        &mut self,
        params: &PosParams,
        current_epoch: impl Into<Epoch>,
        evidence_epoch: impl Into<Epoch>,
        evidence_block_height: impl Into<u64>,
        slash_type: SlashType,
        validator: &Self::Address,
    ) -> Result<(), SlashError<Self::Address>> {
        let current_epoch = current_epoch.into();
        let evidence_epoch = evidence_epoch.into();
        let rate = slash_type.get_slash_rate(params);
        let validator_slash = Slash {
            epoch: evidence_epoch,
            r#type: slash_type,
            rate,
            block_height: evidence_block_height.into(),
        };

        let mut total_deltas =
            self.read_validator_total_deltas(validator).ok_or_else(|| {
                SlashError::ValidatorHasNoTotalDeltas(validator.clone())
            })?;
        let mut voting_power =
            self.read_validator_voting_power(validator).ok_or_else(|| {
                SlashError::ValidatorHasNoVotingPower(validator.clone())
            })?;
        let mut validator_set = self.read_validator_set();
        let mut total_voting_power = self.read_total_voting_power();

        let slashed_change = slash(
            params,
            current_epoch,
            validator,
            &validator_slash,
            &mut total_deltas,
            &mut voting_power,
            &mut validator_set,
            &mut total_voting_power,
        )?;
        let slashed_change: i128 = slashed_change.into();
        let slashed_amount = u64::try_from(slashed_change)
            .map_err(|_err| SlashError::InvalidSlashChange(slashed_change))?;
        let slashed_amount = Self::TokenAmount::from(slashed_amount);

        self.write_validator_total_deltas(validator, &total_deltas);
        self.write_validator_voting_power(validator, &voting_power);
        self.write_validator_slash(validator, validator_slash);
        self.write_validator_set(&validator_set);
        self.write_total_voting_power(&total_voting_power);
        // Transfer the slashed tokens to the PoS slash pool
        self.transfer(
            &Self::staking_token_address(),
            slashed_amount,
            &Self::POS_ADDRESS,
            &Self::POS_SLASH_POOL_ADDRESS,
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
    #[error(
        "The staking reward address must be different from the validator's \
         address {0}"
    )]
    StakingRewardAddressEqValidatorAddress(Address),
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum BondError<Address: Display + Debug> {
    #[error("The given address {0} is not a validator address")]
    NotAValidator(Address),
    #[error(
        "The given source address {0} is a validator address. Validators may \
         not delegate."
    )]
    SourceMustNotBeAValidator(Address),
    #[error("The given validator address {0} is inactive")]
    InactiveValidator(Address),
    #[error("Voting power overflow: {0}")]
    VotingPowerOverflow(TryFromIntError),
    #[error("Given zero amount to unbond")]
    ZeroAmount,
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
    #[error("Given zero amount to unbond")]
    ZeroAmount,
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum WithdrawError<Address>
where
    Address: Display
        + Debug
        + Clone
        + PartialOrd
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
{
    #[error("No unbond could be found for {0}")]
    NoUnbondFound(BondId<Address>),
    #[error("No unbond may be withdrawn yet for {0}")]
    NoWithdrawableUnbond(BondId<Address>),
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum SlashError<Address>
where
    Address: Display + Debug + Clone + PartialOrd + Ord + Hash,
{
    #[error("The validator {0} has no total deltas value")]
    ValidatorHasNoTotalDeltas(Address),
    #[error("The validator {0} has no voting power")]
    ValidatorHasNoVotingPower(Address),
    #[error("Unexpected slash token change")]
    InvalidSlashChange(i128),
    #[error("Voting power overflow: {0}")]
    VotingPowerOverflow(TryFromIntError),
    #[error("Unexpected negative stake {0} for validator {1}")]
    NegativeStake(i128, Address),
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
        + BorshSerialize
        + BorshSchema,
    TokenAmount: Debug
        + Default
        + Clone
        + Add<Output = TokenAmount>
        + AddAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    TokenChange: Debug
        + Copy
        + Add<Output = TokenChange>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    PK: Debug + Clone + BorshDeserialize + BorshSerialize + BorshSchema,
{
    validators: Validators,
    /// Active and inactive validator sets
    validator_set: ValidatorSets<Address>,
    /// The sum of all active and inactive validators' voting power
    total_voting_power: TotalVotingPowers,
    /// The sum of all active and inactive validators' bonded tokens
    total_bonded_balance: TokenAmount,
}
struct GenesisValidatorData<Address, TokenAmount, TokenChange, PK>
where
    Address: Display
        + Debug
        + Clone
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    TokenAmount: Debug
        + Default
        + Clone
        + Add<Output = TokenAmount>
        + AddAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    TokenChange: Debug
        + Copy
        + Add<Output = TokenChange>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    PK: Debug + Clone + BorshDeserialize + BorshSerialize + BorshSchema,
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
        + BorshSerialize
        + BorshSchema,
    TokenAmount: 'a
        + Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenAmount>
        + AddAssign
        + Into<u64>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    TokenChange: 'a
        + Debug
        + Copy
        + Add<Output = TokenChange>
        + From<TokenAmount>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    PK: 'a + Debug + Clone + BorshDeserialize + BorshSerialize + BorshSchema,
{
    // Accumulate the validator set and total voting power
    let mut active: BTreeSet<WeightedValidator<Address>> = BTreeSet::default();
    let mut total_voting_power = VotingPowerDelta::default();
    let mut total_bonded_balance = TokenAmount::default();
    for GenesisValidator {
        address, tokens, ..
    } in validators.clone()
    {
        total_bonded_balance += *tokens;
        let delta = VotingPowerDelta::try_from_tokens(*tokens, params)
            .map_err(GenesisError::VotingPowerOverflow)?;
        total_voting_power += delta;
        let voting_power = VotingPower::from_tokens(*tokens, params);
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
            let token_delta = TokenChange::from(*tokens);
            let total_deltas =
                EpochedDelta::init_at_genesis(token_delta, current_epoch);
            let voting_power =
                VotingPowerDelta::try_from_tokens(*tokens, params)
                    .map_err(GenesisError::VotingPowerOverflow)?;
            let voting_power =
                EpochedDelta::init_at_genesis(voting_power, current_epoch);
            let bond_id = BondId {
                source: address.clone(),
                validator: address.clone(),
            };
            let mut deltas = HashMap::default();
            deltas.insert(current_epoch, *tokens);
            let bond =
                EpochedDelta::init_at_genesis(Bond { deltas }, current_epoch);
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
        total_bonded_balance,
    })
}

/// A function to apply a slash to byzantine validator.
#[allow(clippy::too_many_arguments)]
fn slash<Address, TokenChange>(
    params: &PosParams,
    current_epoch: Epoch,
    validator: &Address,
    slash: &Slash,
    total_deltas: &mut ValidatorTotalDeltas<TokenChange>,
    voting_power: &mut ValidatorVotingPowers,
    validator_set: &mut ValidatorSets<Address>,
    total_voting_power: &mut TotalVotingPowers,
) -> Result<TokenChange, SlashError<Address>>
where
    Address: Display
        + Debug
        + Clone
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    TokenChange: Display
        + Debug
        + Copy
        + Default
        + Neg<Output = TokenChange>
        + Add<Output = TokenChange>
        + Sub<Output = TokenChange>
        + From<i128>
        + Into<i128>
        + PartialOrd
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
{
    let current_stake: TokenChange =
        total_deltas.get(current_epoch).unwrap_or_default();
    if current_stake < TokenChange::default() {
        return Err(SlashError::NegativeStake(
            current_stake.into(),
            validator.clone(),
        ));
    }
    let raw_current_stake: i128 = current_stake.into();
    let slashed_amount: TokenChange = (slash.rate * raw_current_stake).into();
    let token_change = -slashed_amount;

    // Apply slash at pipeline offset
    let update_offset = DynEpochOffset::PipelineLen;

    // Update validator set. This has to be done before we update the
    // `validator_total_deltas`, because we need to look-up the validator with
    // its voting power before the change.
    update_validator_set(
        params,
        validator,
        token_change,
        update_offset,
        validator_set,
        Some(total_deltas),
        current_epoch,
    );

    // Update validator's total deltas
    total_deltas.add_at_offset(
        token_change,
        current_epoch,
        update_offset,
        params,
    );

    // Update the validator's and the total voting power.
    update_voting_powers(
        params,
        update_offset,
        total_deltas,
        voting_power,
        total_voting_power,
        current_epoch,
    )
    .map_err(SlashError::VotingPowerOverflow)?;

    Ok(slashed_amount)
}

struct BecomeValidatorData<PK, TokenChange>
where
    PK: Debug + Clone + BorshDeserialize + BorshSerialize + BorshSchema,
    TokenChange: Default
        + Debug
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
{
    consensus_key: ValidatorConsensusKeys<PK>,
    state: ValidatorStates,
    total_deltas: ValidatorTotalDeltas<TokenChange>,
    voting_power: ValidatorVotingPowers,
}

/// A function that initialized data for a new validator.
fn become_validator<Address, PK, TokenChange>(
    params: &PosParams,
    address: &Address,
    consensus_key: &PK,
    validator_set: &mut ValidatorSets<Address>,
    current_epoch: Epoch,
) -> BecomeValidatorData<PK, TokenChange>
where
    Address: Debug
        + Clone
        + Ord
        + Hash
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    PK: Debug + Clone + BorshDeserialize + BorshSerialize + BorshSchema,
    TokenChange: Default
        + Debug
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
{
    let consensus_key =
        Epoched::init(consensus_key.clone(), current_epoch, params);

    let mut state =
        Epoched::init_at_genesis(ValidatorState::Pending, current_epoch);
    state.set(ValidatorState::Candidate, current_epoch, params);

    let total_deltas = EpochedDelta::init_at_offset(
        Default::default(),
        current_epoch,
        DynEpochOffset::PipelineLen,
        params,
    );
    let voting_power = EpochedDelta::init_at_offset(
        Default::default(),
        current_epoch,
        DynEpochOffset::PipelineLen,
        params,
    );

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
        total_deltas,
        voting_power,
    }
}

struct BondData<TokenAmount, TokenChange>
where
    TokenAmount: Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenAmount>
        + AddAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    TokenChange: Debug
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
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
        + BorshSerialize
        + BorshSchema,
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + PartialEq
        + Add<Output = TokenAmount>
        + AddAssign
        + Into<u64>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
    TokenChange: Display
        + Debug
        + Default
        + Clone
        + Copy
        + Neg
        + Add<Output = TokenChange>
        + Sub
        + From<TokenAmount>
        + Into<i128>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
{
    if amount == TokenAmount::default() {
        return Err(BondError::ZeroAmount);
    }
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
        deltas: HashMap::default(),
    };
    value
        .deltas
        .insert(current_epoch + update_offset.value(params), amount);
    let bond = match current_bond {
        None => EpochedDelta::init(value, current_epoch, params),
        Some(mut bond) => {
            bond.add(value, current_epoch, params);
            bond
        }
    };

    // Update validator set. This has to be done before we update the
    // `validator_total_deltas`, because we need to look-up the validator with
    // its voting power before the change.
    let token_change = TokenChange::from(amount);
    update_validator_set(
        params,
        &bond_id.validator,
        token_change,
        update_offset,
        validator_set,
        validator_total_deltas.as_ref(),
        current_epoch,
    );

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
        + AddAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
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
    slashes: Slashes,
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
        + BorshSerialize
        + BorshSchema,
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + PartialOrd
        + Add<Output = TokenAmount>
        + AddAssign
        + Into<u64>
        + From<u64>
        + SubAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
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
        + BorshSerialize
        + BorshSchema,
{
    if amount == TokenAmount::default() {
        return Err(UnbondError::ZeroAmount);
    }
    // We can unbond tokens that are bonded for a future epoch (not yet
    // active), hence we check the total at the pipeline offset
    let unbondable_amount = bond
        .get_at_offset(current_epoch, DynEpochOffset::PipelineLen, params)
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
    let mut slashed_amount = TokenAmount::default();
    // Decrement the bond deltas starting from the rightmost value (a bond in a
    // future-most epoch) until whole amount is decremented
    bond.rev_update_while(
        |bonds, _epoch| {
            bonds.deltas.retain(|epoch_start, bond_delta| {
                if *to_unbond == 0.into() {
                    return true;
                }
                let mut unbonded = HashMap::default();
                let unbond_end =
                    current_epoch + update_offset.value(params) - 1;
                // We need to accumulate the slashed delta for multiple slashes
                // applicable to a bond, where each slash should be
                // calculated from the delta reduced by the previous slash.
                let applied_delta = if to_unbond > bond_delta {
                    unbonded.insert((*epoch_start, unbond_end), *bond_delta);
                    *to_unbond -= *bond_delta;
                    let applied_delta = *bond_delta;
                    *bond_delta = 0.into();
                    applied_delta
                } else {
                    unbonded.insert((*epoch_start, unbond_end), *to_unbond);
                    *bond_delta -= *to_unbond;
                    let applied_delta = *to_unbond;
                    *to_unbond = 0.into();
                    applied_delta
                };
                // Calculate how much the bond delta would be after slashing
                let mut slashed_bond_delta = applied_delta;
                for slash in &slashes {
                    if slash.epoch >= *epoch_start {
                        let raw_delta: u64 = slashed_bond_delta.into();
                        let raw_slashed_delta = slash.rate * raw_delta;
                        let slashed_delta =
                            TokenAmount::from(raw_slashed_delta);
                        slashed_bond_delta -= slashed_delta;
                    }
                }
                slashed_amount += slashed_bond_delta;

                // For each decremented bond value write a new unbond
                unbond.add(Unbond { deltas: unbonded }, current_epoch, params);
                // Remove bonds with no tokens left
                *bond_delta != 0.into()
            });
            // Stop the update once all the tokens are unbonded
            *to_unbond != 0.into()
        },
        current_epoch,
        params,
    );

    // Update validator set. This has to be done before we update the
    // `validator_total_deltas`, because we need to look-up the validator with
    // its voting power before the change.
    let token_change = -TokenChange::from(slashed_amount);
    update_validator_set(
        params,
        &bond_id.validator,
        token_change,
        update_offset,
        validator_set,
        Some(validator_total_deltas),
        current_epoch,
    );

    // Update validator's total deltas
    validator_total_deltas.add(token_change, current_epoch, params);

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
    validator_total_deltas: Option<&ValidatorTotalDeltas<TokenChange>>,
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
        + BorshSerialize
        + BorshSchema,
    TokenChange: Display
        + Default
        + Debug
        + Clone
        + Copy
        + Add<Output = TokenChange>
        + Sub
        + Into<i128>
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
{
    validator_set.update_from_offset(
        |validator_set, epoch| {
            // Find the validator's voting power at the epoch that's being
            // updated from its total deltas
            let tokens_pre = validator_total_deltas
                .and_then(|d| d.get(epoch))
                .unwrap_or_default();
            let tokens_post = tokens_pre + token_change;
            let tokens_pre: i128 = tokens_pre.into();
            let tokens_post: i128 = tokens_post.into();
            let tokens_pre: u64 = TryFrom::try_from(tokens_pre).unwrap();
            let tokens_post: u64 = TryFrom::try_from(tokens_post).unwrap();
            let voting_power_pre = VotingPower::from_tokens(tokens_pre, params);
            let voting_power_post =
                VotingPower::from_tokens(tokens_post, params);
            if voting_power_pre != voting_power_post {
                let validator_pre = WeightedValidator {
                    voting_power: voting_power_pre,
                    address: validator.clone(),
                };
                let validator_post = WeightedValidator {
                    voting_power: voting_power_post,
                    address: validator.clone(),
                };

                if validator_set.inactive.contains(&validator_pre) {
                    let min_active_validator =
                        validator_set.active.first_shim();
                    let min_voting_power = min_active_validator
                        .map(|v| v.voting_power)
                        .unwrap_or_default();
                    if voting_power_post > min_voting_power {
                        let deactivate_min =
                            validator_set.active.pop_first_shim();
                        let popped =
                            validator_set.inactive.remove(&validator_pre);
                        debug_assert!(popped);
                        validator_set.active.insert(validator_post);
                        if let Some(deactivate_min) = deactivate_min {
                            validator_set.inactive.insert(deactivate_min);
                        }
                    } else {
                        validator_set.inactive.remove(&validator_pre);
                        validator_set.inactive.insert(validator_post);
                    }
                } else {
                    debug_assert!(
                        validator_set.active.contains(&validator_pre)
                    );
                    let max_inactive_validator =
                        validator_set.inactive.last_shim();
                    let max_voting_power = max_inactive_validator
                        .map(|v| v.voting_power)
                        .unwrap_or_default();
                    if voting_power_post < max_voting_power {
                        let activate_max =
                            validator_set.inactive.pop_last_shim();
                        let popped =
                            validator_set.active.remove(&validator_pre);
                        debug_assert!(popped);
                        validator_set.inactive.insert(validator_post);
                        if let Some(activate_max) = activate_max {
                            validator_set.active.insert(activate_max);
                        }
                    } else {
                        validator_set.active.remove(&validator_pre);
                        validator_set.active.insert(validator_post);
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
        + BorshSerialize
        + BorshSchema,
{
    let change_offset = change_offset.value(params);
    let start_epoch = current_epoch + change_offset;
    // Update voting powers from the change offset to the the last epoch of
    // voting powers data (unbonding epoch)
    let epochs = start_epoch.iter_range(
        DynEpochOffset::UnbondingLen.value(params) - change_offset + 1,
    );
    for epoch in epochs {
        // Recalculate validator's voting power from validator's total deltas
        let total_deltas_at_pipeline =
            validator_total_deltas.get(epoch).unwrap_or_default();
        let total_deltas_at_pipeline: i128 = total_deltas_at_pipeline.into();
        let total_deltas_at_pipeline: u64 =
            TryFrom::try_from(total_deltas_at_pipeline).unwrap();
        let voting_power_at_pipeline =
            validator_voting_power.get(epoch).unwrap_or_default();
        let voting_power_delta = VotingPowerDelta::try_from_tokens(
            total_deltas_at_pipeline,
            params,
        )? - voting_power_at_pipeline;

        validator_voting_power.add_at_epoch(
            voting_power_delta,
            current_epoch,
            epoch,
            params,
        );

        // Update total voting power
        total_voting_power.add_at_epoch(
            voting_power_delta,
            current_epoch,
            epoch,
            params,
        );
    }
    Ok(())
}

struct WithdrawData<TokenAmount>
where
    TokenAmount: Debug
        + Default
        + Clone
        + Copy
        + Add<Output = TokenAmount>
        + AddAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
{
    pub unbond: Unbonds<TokenAmount>,
    pub withdrawn: TokenAmount,
    pub slashed: TokenAmount,
}

/// Withdraw tokens from unbonds of self-bonds or delegations.
fn withdraw_unbonds<Address, TokenAmount>(
    params: &PosParams,
    bond_id: &BondId<Address>,
    unbond: Option<Unbonds<TokenAmount>>,
    slashes: Vec<Slash>,
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
        + BorshSerialize
        + BorshSchema,
    TokenAmount: Display
        + Debug
        + Default
        + Clone
        + Copy
        + PartialOrd
        + Add<Output = TokenAmount>
        + AddAssign
        + Into<u64>
        + From<u64>
        + SubAssign
        + BorshDeserialize
        + BorshSerialize
        + BorshSchema,
{
    let mut unbond =
        unbond.ok_or_else(|| WithdrawError::NoUnbondFound(bond_id.clone()))?;
    let withdrawable_unbond = unbond
        .get(current_epoch)
        .ok_or_else(|| WithdrawError::NoWithdrawableUnbond(bond_id.clone()))?;
    let mut slashed = TokenAmount::default();
    let withdrawn_amount = withdrawable_unbond.deltas.iter().fold(
        TokenAmount::default(),
        |sum, ((epoch_start, epoch_end), delta)| {
            let mut delta = *delta;
            // Check and apply slashes, if any
            for slash in &slashes {
                if slash.epoch >= *epoch_start && slash.epoch <= *epoch_end {
                    let raw_delta: u64 = delta.into();
                    let current_slashed =
                        TokenAmount::from(slash.rate * raw_delta);
                    slashed += current_slashed;
                    delta -= current_slashed;
                }
            }
            sum + delta
        },
    );
    unbond.delete_current(current_epoch, params);
    Ok(WithdrawData {
        unbond,
        withdrawn: withdrawn_amount,
        slashed,
    })
}
