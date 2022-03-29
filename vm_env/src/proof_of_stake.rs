//! Proof of Stake system integration with functions for transactions

use anoma::ledger::pos::anoma_proof_of_stake::{
    BecomeValidatorError, BondError, UnbondError, WithdrawError,
};
use anoma::ledger::pos::types::Slash;
pub use anoma::ledger::pos::*;
use anoma::ledger::pos::{
    anoma_proof_of_stake, bond_key, params_key, total_voting_power_key,
    unbond_key, validator_address_raw_hash_key, validator_consensus_key_key,
    validator_set_key, validator_slashes_key,
    validator_staking_reward_address_key, validator_state_key,
    validator_total_deltas_key, validator_voting_power_key,
};
use anoma::types::address::{self, Address, InternalAddress};
use anoma::types::transaction::InitValidator;
use anoma::types::{key, token};
pub use anoma_proof_of_stake::{
    epoched, parameters, types, PosActions as PosWrite, PosReadOnly as PosRead,
};

use crate::imports::tx;

/// Self-bond tokens to a validator when `source` is `None` or equal to
/// the `validator` address, or delegate tokens from the `source` to the
/// `validator`.
pub fn bond_tokens(
    source: Option<&Address>,
    validator: &Address,
    amount: token::Amount,
) -> Result<(), BondError<Address>> {
    let current_epoch = tx::get_block_epoch();
    PoS.bond_tokens(source, validator, amount, current_epoch)
}

/// Unbond self-bonded tokens from a validator when `source` is `None` or
/// equal to the `validator` address, or unbond delegated tokens from
/// the `source` to the `validator`.
pub fn unbond_tokens(
    source: Option<&Address>,
    validator: &Address,
    amount: token::Amount,
) -> Result<(), UnbondError<Address, token::Amount>> {
    let current_epoch = tx::get_block_epoch();
    PoS.unbond_tokens(source, validator, amount, current_epoch)
}

/// Withdraw unbonded tokens from a self-bond to a validator when `source`
/// is `None` or equal to the `validator` address, or withdraw unbonded
/// tokens delegated to the `validator` to the `source`.
pub fn withdraw_tokens(
    source: Option<&Address>,
    validator: &Address,
) -> Result<token::Amount, WithdrawError<Address>> {
    let current_epoch = tx::get_block_epoch();
    PoS.withdraw_tokens(source, validator, current_epoch)
}

/// Attempt to initialize a validator account. On success, returns the
/// initialized validator account's address and its staking reward address.
pub fn init_validator(
    InitValidator {
        account_key,
        consensus_key,
        rewards_account_key,
        protocol_key,
        dkg_key,
        validator_vp_code,
        rewards_vp_code,
    }: InitValidator,
) -> Result<(Address, Address), BecomeValidatorError<Address>> {
    let current_epoch = tx::get_block_epoch();
    // Init validator account
    let validator_address = tx::init_account(&validator_vp_code);
    let pk_key = key::pk_key(&validator_address);
    tx::write(&pk_key.to_string(), &account_key);
    let protocol_pk_key = key::protocol_pk_key(&validator_address);
    tx::write(&protocol_pk_key.to_string(), &protocol_key);
    let dkg_pk_key = key::dkg_session_keys::dkg_pk_key(&validator_address);
    tx::write(&dkg_pk_key.to_string(), &dkg_key);

    // Init staking reward account
    let rewards_address = tx::init_account(&rewards_vp_code);
    let pk_key = key::pk_key(&rewards_address);
    tx::write(&pk_key.to_string(), &rewards_account_key);

    PoS.become_validator(
        &validator_address,
        &rewards_address,
        &consensus_key,
        current_epoch,
    )?;
    Ok((validator_address, rewards_address))
}

/// Proof of Stake system. This struct integrates and gives access to
/// lower-level PoS functions.
pub struct PoS;

impl anoma_proof_of_stake::PosReadOnly for PoS {
    type Address = Address;
    type PublicKey = key::common::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = Address::Internal(InternalAddress::PoS);

    fn staking_token_address() -> Self::Address {
        address::xan()
    }

    fn read_pos_params(&self) -> PosParams {
        tx::read(params_key().to_string()).unwrap()
    }

    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> Option<Self::Address> {
        tx::read(validator_staking_reward_address_key(key).to_string())
    }

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys> {
        tx::read(validator_consensus_key_key(key).to_string())
    }

    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorStates> {
        tx::read(validator_state_key(key).to_string())
    }

    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorTotalDeltas> {
        tx::read(validator_total_deltas_key(key).to_string())
    }

    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorVotingPowers> {
        tx::read(validator_voting_power_key(key).to_string())
    }

    fn read_validator_slashes(&self, key: &Self::Address) -> Vec<Slash> {
        tx::read(validator_slashes_key(key).to_string()).unwrap_or_default()
    }

    fn read_bond(&self, key: &BondId) -> Option<Bonds> {
        tx::read(bond_key(key).to_string())
    }

    fn read_unbond(&self, key: &BondId) -> Option<Unbonds> {
        tx::read(unbond_key(key).to_string())
    }

    fn read_validator_set(&self) -> ValidatorSets {
        tx::read(validator_set_key().to_string()).unwrap()
    }

    fn read_total_voting_power(&self) -> TotalVotingPowers {
        tx::read(total_voting_power_key().to_string()).unwrap()
    }
}

impl anoma_proof_of_stake::PosActions for PoS {
    fn write_pos_params(&mut self, params: &PosParams) {
        tx::write(params_key().to_string(), params)
    }

    fn write_validator_address_raw_hash(&mut self, address: &Self::Address) {
        let raw_hash = address.raw_hash().unwrap().to_owned();
        tx::write(
            validator_address_raw_hash_key(raw_hash).to_string(),
            address,
        )
    }

    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: Self::Address,
    ) {
        tx::write(
            validator_staking_reward_address_key(key).to_string(),
            &value,
        )
    }

    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: ValidatorConsensusKeys,
    ) {
        tx::write(validator_consensus_key_key(key).to_string(), &value)
    }

    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: ValidatorStates,
    ) {
        tx::write(validator_state_key(key).to_string(), &value)
    }

    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: ValidatorTotalDeltas,
    ) {
        tx::write(validator_total_deltas_key(key).to_string(), &value)
    }

    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: ValidatorVotingPowers,
    ) {
        tx::write(validator_voting_power_key(key).to_string(), &value)
    }

    fn write_bond(&mut self, key: &BondId, value: Bonds) {
        tx::write(bond_key(key).to_string(), &value)
    }

    fn write_unbond(&mut self, key: &BondId, value: Unbonds) {
        tx::write(unbond_key(key).to_string(), &value)
    }

    fn write_validator_set(&mut self, value: ValidatorSets) {
        tx::write(validator_set_key().to_string(), &value)
    }

    fn write_total_voting_power(&mut self, value: TotalVotingPowers) {
        tx::write(total_voting_power_key().to_string(), &value)
    }

    fn delete_bond(&mut self, key: &BondId) {
        tx::delete(bond_key(key).to_string())
    }

    fn delete_unbond(&mut self, key: &BondId) {
        tx::delete(unbond_key(key).to_string())
    }

    fn transfer(
        &mut self,
        token: &Self::Address,
        amount: Self::TokenAmount,
        src: &Self::Address,
        dest: &Self::Address,
    ) {
        crate::token::tx::transfer(src, dest, token, amount, &None)
    }
}
