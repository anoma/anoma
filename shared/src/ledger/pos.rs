//! Proof-of-Stake integration as a native validity predicate

use std::collections::HashSet;

use anoma_proof_of_stake::types::BondId;
pub use borsh::{BorshDeserialize, BorshSerialize};
use thiserror::Error;

use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{self, Address, InternalAddress};
use crate::types::storage::{Key, KeySeg};
use crate::types::{key, token};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// PoS functions result
pub type Result<T> = std::result::Result<T, Error>;

const ADDRESS: Address = Address::Internal(InternalAddress::PoS);

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

const PARAMS_STORAGE_KEY: &str = "params";
const VALIDATOR_STORAGE_KEY: &str = "validator";
const VALIDATOR_STAKING_REWARD_ADDRESS_STORAGE_KEY: &str =
    "staking_reward_address";
const VALIDATOR_CONSENSUS_KEY_STORAGE_KEY: &str = "consensus_key";
const VALIDATOR_STATE_STORAGE_KEY: &str = "state";
const VALIDATOR_TOTAL_DELTAS_STORAGE_KEY: &str = "total_deltas";
const VALIDATOR_VOTING_POWER_STORAGE_KEY: &str = "voting_power";
const BOND_STORAGE_KEY: &str = "bond";
const UNBOND_STORAGE_KEY: &str = "bond";
const VALIDATOR_SET_STORAGE_KEY: &str = "validator_set";
const TOTAL_VOTING_POWER_STORAGE_KEY: &str = "total_voting_power";

/// Storage key for PoS parameters.
pub fn params_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&PARAMS_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key prefix for validator data.
fn validator_prefix(validator: &Address) -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&VALIDATOR_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&validator.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator's staking reward address.
pub fn validator_staking_reward_address_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_STAKING_REWARD_ADDRESS_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator's consensus key.
pub fn validator_consensus_key_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_CONSENSUS_KEY_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator's state.
pub fn validator_state_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_STATE_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator's total deltas.
pub fn validator_total_deltas_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_TOTAL_DELTAS_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator's voting power.
pub fn validator_voting_power_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_VOTING_POWER_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key prefix for all bonds of the given source address.
fn bonds_prefix(source: &Address) -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&BOND_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&source.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Storage key for a bond with the given ID (source and validator).
pub fn bond_key(bond_id: &BondId<Address>) -> Key {
    bonds_prefix(&bond_id.source)
        .push(&bond_id.validator.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Storage key prefix for all unbonds of the given source address.
fn unbonds_prefix(source: &Address) -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&UNBOND_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&source.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Storage key for an unbond with the given ID (source and validator).
pub fn unbond_key(bond_id: &BondId<Address>) -> Key {
    unbonds_prefix(&bond_id.source)
        .push(&bond_id.validator.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator set (active and inactive).
pub fn validator_set_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&VALIDATOR_SET_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key for total voting power.
pub fn total_voting_power_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&TOTAL_VOTING_POWER_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

impl<D, H> anoma_proof_of_stake::PoSReadOnly for PoS<'_, D, H>
where
    D: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    type Address = Address;
    type PublicKey = key::ed25519::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = ADDRESS;

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

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}
