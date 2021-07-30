//! Proof-of-Stake integration as a native validity predicate

use std::collections::HashSet;

use thiserror::Error;

use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{self, Address, InternalAddress};
use crate::types::storage::Key;
use crate::types::{key, token};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// PoS functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Proof-of-Stake VP
pub struct PoS<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

/// Initialize storage in the genesis block.
pub fn init_genesis_storage<DB, H>(_storage: &mut Storage<DB, H>)
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
}

impl<'a, DB, H> NativeVp for PoS<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::PoS;

    fn validate_tx(
        &self,
        _tx_data: &[u8],
        _keys_changed: &HashSet<Key>,
        _verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        Ok(false)
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}

impl<D, H> anoma_proof_of_stake::Pos for Storage<D, H>
where
    D: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    type Address = Address;
    type PublicKey = key::ed25519::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = Address::Internal(InternalAddress::PoS);

    fn staking_token_address() -> Self::Address {
        address::xan()
    }

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
        source: &Self::Address,
        target: &Self::Address,
    ) {
        todo!()
    }
}
