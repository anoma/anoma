//! Proof of Stake system integration with functions for transactions

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

use super::*;

impl Ctx {
    /// Self-bond tokens to a validator when `source` is `None` or equal to
    /// the `validator` address, or delegate tokens from the `source` to the
    /// `validator`.
    pub fn bond_tokens(
        &mut self,
        source: Option<&Address>,
        validator: &Address,
        amount: token::Amount,
    ) -> TxResult {
        let current_epoch = self.get_block_epoch()?;
        anoma_proof_of_stake::PosActions::bond_tokens(
            self,
            source,
            validator,
            amount,
            current_epoch,
        )
    }

    /// Unbond self-bonded tokens from a validator when `source` is `None` or
    /// equal to the `validator` address, or unbond delegated tokens from
    /// the `source` to the `validator`.
    pub fn unbond_tokens(
        &mut self,
        source: Option<&Address>,
        validator: &Address,
        amount: token::Amount,
    ) -> TxResult {
        let current_epoch = self.get_block_epoch()?;
        anoma_proof_of_stake::PosActions::unbond_tokens(
            self,
            source,
            validator,
            amount,
            current_epoch,
        )
    }

    /// Withdraw unbonded tokens from a self-bond to a validator when `source`
    /// is `None` or equal to the `validator` address, or withdraw unbonded
    /// tokens delegated to the `validator` to the `source`.
    pub fn withdraw_tokens(
        &mut self,
        source: Option<&Address>,
        validator: &Address,
    ) -> EnvResult<token::Amount> {
        let current_epoch = self.get_block_epoch()?;
        anoma_proof_of_stake::PosActions::withdraw_tokens(
            self,
            source,
            validator,
            current_epoch,
        )
    }

    /// Attempt to initialize a validator account. On success, returns the
    /// initialized validator account's address and its staking reward address.
    pub fn init_validator(
        &mut self,
        InitValidator {
            account_key,
            consensus_key,
            rewards_account_key,
            protocol_key,
            dkg_key,
            validator_vp_code,
            rewards_vp_code,
        }: InitValidator,
    ) -> EnvResult<(Address, Address)> {
        let current_epoch = self.get_block_epoch()?;
        // Init validator account
        let validator_address = self.init_account(&validator_vp_code)?;
        let pk_key = key::pk_key(&validator_address);
        self.write(&pk_key, &account_key)?;
        let protocol_pk_key = key::protocol_pk_key(&validator_address);
        self.write(&protocol_pk_key, &protocol_key)?;
        let dkg_pk_key = key::dkg_session_keys::dkg_pk_key(&validator_address);
        self.write(&dkg_pk_key, &dkg_key)?;

        // Init staking reward account
        let rewards_address = self.init_account(&rewards_vp_code)?;
        let pk_key = key::pk_key(&rewards_address);
        self.write(&pk_key, &rewards_account_key)?;

        self.become_validator(
            &validator_address,
            &rewards_address,
            &consensus_key,
            current_epoch,
        )?;

        Ok((validator_address, rewards_address))
    }
}

impl anoma_proof_of_stake::PosReadOnly for Ctx {
    type Address = Address;
    type Error = crate::Error;
    type PublicKey = key::common::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = Address::Internal(InternalAddress::PoS);

    fn staking_token_address() -> Self::Address {
        address::xan()
    }

    fn read_pos_params(&self) -> Result<PosParams, Self::Error> {
        let params = self.read(&params_key())?;
        Ok(params.expect("PoS params should always be set"))
    }

    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> Result<Option<Self::Address>, Self::Error> {
        self.read(&validator_staking_reward_address_key(key))
            .into_env_result()
    }

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Result<Option<ValidatorConsensusKeys>, Self::Error> {
        self.read(&validator_consensus_key_key(key))
            .into_env_result()
    }

    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Result<Option<ValidatorStates>, Self::Error> {
        self.read(&validator_state_key(key)).into_env_result()
    }

    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Result<Option<ValidatorTotalDeltas>, Self::Error> {
        self.read(&validator_total_deltas_key(key))
            .into_env_result()
    }

    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Result<Option<ValidatorVotingPowers>, Self::Error> {
        self.read(&validator_voting_power_key(key))
            .into_env_result()
    }

    fn read_validator_slashes(
        &self,
        key: &Self::Address,
    ) -> Result<Vec<Slash>, Self::Error> {
        let val = self.read(&validator_slashes_key(key))?;
        Ok(val.unwrap_or_default())
    }

    fn read_bond(&self, key: &BondId) -> Result<Option<Bonds>, Self::Error> {
        self.read(&bond_key(key)).into_env_result()
    }

    fn read_unbond(
        &self,
        key: &BondId,
    ) -> Result<Option<Unbonds>, Self::Error> {
        self.read(&unbond_key(key)).into_env_result()
    }

    fn read_validator_set(&self) -> Result<ValidatorSets, Self::Error> {
        let val = self.read(&validator_set_key())?;
        Ok(val.expect("Validator sets must always have a value"))
    }

    fn read_total_voting_power(
        &self,
    ) -> Result<TotalVotingPowers, Self::Error> {
        let val = self.read(&total_voting_power_key())?;
        Ok(val.expect("Total voting power must always have a value"))
    }
}

impl From<anoma_proof_of_stake::BecomeValidatorError<Address>> for Error {
    fn from(err: anoma_proof_of_stake::BecomeValidatorError<Address>) -> Self {
        Self::new(err)
    }
}

impl From<anoma_proof_of_stake::BondError<Address>> for Error {
    fn from(err: anoma_proof_of_stake::BondError<Address>) -> Self {
        Self::new(err)
    }
}

impl From<anoma_proof_of_stake::UnbondError<Address, token::Amount>> for Error {
    fn from(
        err: anoma_proof_of_stake::UnbondError<Address, token::Amount>,
    ) -> Self {
        Self::new(err)
    }
}

impl From<anoma_proof_of_stake::WithdrawError<Address>> for Error {
    fn from(err: anoma_proof_of_stake::WithdrawError<Address>) -> Self {
        Self::new(err)
    }
}

impl anoma_proof_of_stake::PosActions for Ctx {
    type BecomeValidatorError = crate::Error;
    type BondError = crate::Error;
    type UnbondError = crate::Error;
    type WithdrawError = crate::Error;

    fn write_pos_params(
        &mut self,
        params: &PosParams,
    ) -> Result<(), Self::Error> {
        self.write(&params_key(), params).into_env_result()
    }

    fn write_validator_address_raw_hash(
        &mut self,
        address: &Self::Address,
    ) -> Result<(), Self::Error> {
        let raw_hash = address.raw_hash().unwrap().to_owned();
        self.write(&validator_address_raw_hash_key(raw_hash), address)
            .into_env_result()
    }

    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: Self::Address,
    ) -> Result<(), Self::Error> {
        self.write(&validator_staking_reward_address_key(key), &value)
            .into_env_result()
    }

    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: ValidatorConsensusKeys,
    ) -> Result<(), Self::Error> {
        self.write(&validator_consensus_key_key(key), &value)
            .into_env_result()
    }

    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: ValidatorStates,
    ) -> Result<(), Self::Error> {
        self.write(&validator_state_key(key), &value)
            .into_env_result()
    }

    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: ValidatorTotalDeltas,
    ) -> Result<(), Self::Error> {
        self.write(&validator_total_deltas_key(key), &value)
            .into_env_result()
    }

    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: ValidatorVotingPowers,
    ) -> Result<(), Self::Error> {
        self.write(&validator_voting_power_key(key), &value)
            .into_env_result()
    }

    fn write_bond(
        &mut self,
        key: &BondId,
        value: Bonds,
    ) -> Result<(), Self::Error> {
        self.write(&bond_key(key), &value).into_env_result()
    }

    fn write_unbond(
        &mut self,
        key: &BondId,
        value: Unbonds,
    ) -> Result<(), Self::Error> {
        self.write(&unbond_key(key), &value).into_env_result()
    }

    fn write_validator_set(
        &mut self,
        value: ValidatorSets,
    ) -> Result<(), Self::Error> {
        self.write(&validator_set_key(), &value).into_env_result()
    }

    fn write_total_voting_power(
        &mut self,
        value: TotalVotingPowers,
    ) -> Result<(), Self::Error> {
        self.write(&total_voting_power_key(), &value)
            .into_env_result()
    }

    fn delete_bond(&mut self, key: &BondId) -> Result<(), Self::Error> {
        self.delete(&bond_key(key)).into_env_result()
    }

    fn delete_unbond(&mut self, key: &BondId) -> Result<(), Self::Error> {
        self.delete(&unbond_key(key)).into_env_result()
    }

    fn transfer(
        &mut self,
        token: &Self::Address,
        amount: Self::TokenAmount,
        src: &Self::Address,
        dest: &Self::Address,
    ) -> Result<(), Self::Error> {
        crate::token::transfer(self, src, dest, token, amount)
    }
}
