//! Proof of Stake system integration with functions for transactions
use anoma::types::address::{self, Address, InternalAddress};
pub use anoma::types::token::*;
use anoma::types::{key, token};

use crate::imports::tx;

/// Proof of Stake system
pub struct PoS;

impl anoma_proof_of_stake::PoSReadOnly for PoS {
    type Address = Address;
    type PublicKey = key::ed25519::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = Address::Internal(InternalAddress::PoS);

    fn staking_token_address() -> Self::Address {
        address::xan()
    }

    fn read_params(&self) -> anoma_proof_of_stake::parameters::PosParams {
        todo!()
    }

    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> Option<Self::Address> {
        todo!()
    }

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<
        anoma_proof_of_stake::epoched::Epoched<
            Self::PublicKey,
            anoma_proof_of_stake::epoched::OffsetPipelineLen,
        >,
    > {
        todo!()
    }

    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<
        anoma_proof_of_stake::epoched::Epoched<
            anoma_proof_of_stake::types::ValidatorState,
            anoma_proof_of_stake::epoched::OffsetPipelineLen,
        >,
    > {
        todo!()
    }

    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<
        anoma_proof_of_stake::epoched::EpochedDelta<
            Self::TokenChange,
            anoma_proof_of_stake::epoched::OffsetUnboundingLen,
        >,
    > {
        todo!()
    }

    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<
        anoma_proof_of_stake::epoched::EpochedDelta<
            anoma_proof_of_stake::types::VotingPowerDelta,
            anoma_proof_of_stake::epoched::OffsetUnboundingLen,
        >,
    > {
        todo!()
    }

    fn read_bond(
        &mut self,
        key: &anoma_proof_of_stake::types::BondId<Self::Address>,
    ) -> Option<
        anoma_proof_of_stake::epoched::EpochedDelta<
            anoma_proof_of_stake::types::Bond<Self::TokenAmount>,
            anoma_proof_of_stake::epoched::OffsetPipelineLen,
        >,
    > {
        todo!()
    }

    fn read_unbond(
        &mut self,
        key: &anoma_proof_of_stake::types::BondId<Self::Address>,
    ) -> Option<
        anoma_proof_of_stake::epoched::EpochedDelta<
            anoma_proof_of_stake::types::Unbond<Self::TokenAmount>,
            anoma_proof_of_stake::epoched::OffsetUnboundingLen,
        >,
    > {
        todo!()
    }

    fn read_validator_set(
        &mut self,
    ) -> anoma_proof_of_stake::epoched::Epoched<
        anoma_proof_of_stake::types::ValidatorSet<Self::Address>,
        anoma_proof_of_stake::epoched::OffsetUnboundingLen,
    > {
        todo!()
    }

    fn read_total_voting_power(
        &mut self,
    ) -> anoma_proof_of_stake::epoched::EpochedDelta<
        anoma_proof_of_stake::types::VotingPowerDelta,
        anoma_proof_of_stake::epoched::OffsetUnboundingLen,
    > {
        todo!()
    }
}

impl anoma_proof_of_stake::PoS for PoS {
    fn write_params(
        &mut self,
        params: &anoma_proof_of_stake::parameters::PosParams,
    ) {
        todo!()
    }

    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: Self::Address,
    ) {
        todo!()
    }

    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: anoma_proof_of_stake::epoched::Epoched<
            Self::PublicKey,
            anoma_proof_of_stake::epoched::OffsetPipelineLen,
        >,
    ) {
        todo!()
    }

    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: anoma_proof_of_stake::epoched::Epoched<
            anoma_proof_of_stake::types::ValidatorState,
            anoma_proof_of_stake::epoched::OffsetPipelineLen,
        >,
    ) {
        todo!()
    }

    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: anoma_proof_of_stake::epoched::EpochedDelta<
            Self::TokenChange,
            anoma_proof_of_stake::epoched::OffsetUnboundingLen,
        >,
    ) {
        todo!()
    }

    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: anoma_proof_of_stake::epoched::EpochedDelta<
            anoma_proof_of_stake::types::VotingPowerDelta,
            anoma_proof_of_stake::epoched::OffsetUnboundingLen,
        >,
    ) {
        todo!()
    }

    fn write_bond(
        &mut self,
        key: &anoma_proof_of_stake::types::BondId<Self::Address>,
        value: anoma_proof_of_stake::epoched::EpochedDelta<
            anoma_proof_of_stake::types::Bond<Self::TokenAmount>,
            anoma_proof_of_stake::epoched::OffsetPipelineLen,
        >,
    ) {
        todo!()
    }

    fn write_unbond(
        &mut self,
        key: &anoma_proof_of_stake::types::BondId<Self::Address>,
        value: anoma_proof_of_stake::epoched::EpochedDelta<
            anoma_proof_of_stake::types::Unbond<Self::TokenAmount>,
            anoma_proof_of_stake::epoched::OffsetUnboundingLen,
        >,
    ) {
        todo!()
    }

    fn write_validator_set(
        &mut self,
        value: anoma_proof_of_stake::epoched::Epoched<
            anoma_proof_of_stake::types::ValidatorSet<Self::Address>,
            anoma_proof_of_stake::epoched::OffsetUnboundingLen,
        >,
    ) {
        todo!()
    }

    fn write_total_voting_power(
        &mut self,
        value: anoma_proof_of_stake::epoched::EpochedDelta<
            anoma_proof_of_stake::types::VotingPowerDelta,
            anoma_proof_of_stake::epoched::OffsetUnboundingLen,
        >,
    ) {
        todo!()
    }

    fn delete_bond(
        &mut self,
        key: &anoma_proof_of_stake::types::BondId<Self::Address>,
    ) {
        todo!()
    }

    fn delete_unbond(
        &mut self,
        key: &anoma_proof_of_stake::types::BondId<Self::Address>,
    ) {
        todo!()
    }

    fn transfer(
        &mut self,
        token: &Self::Address,
        amount: Self::TokenAmount,
        src: &Self::Address,
        dest: &Self::Address,
    ) {
        crate::token::tx::transfer(src, dest, token, amount)
    }
}
