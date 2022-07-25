//! Proof-of-Stake integration as a native validity predicate

mod storage;
pub mod vp;

pub use anoma_proof_of_stake;
pub use anoma_proof_of_stake::parameters::PosParams;
pub use anoma_proof_of_stake::types::{
    self, Slash, Slashes, TotalVotingPowers, ValidatorStates,
    ValidatorVotingPowers,
};
use anoma_proof_of_stake::PosBase;
pub use storage::*;
pub use vp::PosVP;

use crate::ledger::storage::{self as ledger_storage, Storage, StorageHasher};
use crate::types::address::{self, Address, InternalAddress};
use crate::types::storage::Epoch;
use crate::types::{key, token};

/// Address of the PoS account implemented as a native VP
pub const ADDRESS: Address = Address::Internal(InternalAddress::PoS);

/// Address of the PoS slash pool account
pub const SLASH_POOL_ADDRESS: Address =
    Address::Internal(InternalAddress::PosSlashPool);

/// Address of the staking token (XAN)
pub fn staking_token_address() -> Address {
    address::xan()
}

/// Initialize storage in the genesis block.
pub fn init_genesis_storage<'a, DB, H>(
    storage: &mut Storage<DB, H>,
    params: &'a PosParams,
    validators: impl Iterator<Item = &'a GenesisValidator> + Clone + 'a,
    current_epoch: Epoch,
) where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
{
    storage
        .init_genesis(params, validators, current_epoch)
        .expect("Initialize PoS genesis storage")
}

/// Alias for a PoS type with the same name with concrete type parameters
pub type ValidatorConsensusKeys =
    anoma_proof_of_stake::types::ValidatorConsensusKeys<key::common::PublicKey>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type ValidatorTotalDeltas =
    anoma_proof_of_stake::types::ValidatorTotalDeltas<token::Change>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type Bonds = anoma_proof_of_stake::types::Bonds<token::Amount>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type Unbonds = anoma_proof_of_stake::types::Unbonds<token::Amount>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type ValidatorSets = anoma_proof_of_stake::types::ValidatorSets<Address>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type BondId = anoma_proof_of_stake::types::BondId<Address>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type GenesisValidator = anoma_proof_of_stake::types::GenesisValidator<
    Address,
    token::Amount,
    key::common::PublicKey,
>;

impl From<Epoch> for anoma_proof_of_stake::types::Epoch {
    fn from(epoch: Epoch) -> Self {
        let epoch: u64 = epoch.into();
        anoma_proof_of_stake::types::Epoch::from(epoch)
    }
}

impl From<anoma_proof_of_stake::types::Epoch> for Epoch {
    fn from(epoch: anoma_proof_of_stake::types::Epoch) -> Self {
        let epoch: u64 = epoch.into();
        Epoch(epoch)
    }
}

#[macro_use]
mod macros {
    /// Implement `PosReadOnly` for a type that implements
    /// [`trait@crate::ledger::read::StorageRead`].
    ///
    /// Excuse the horrible syntax - we haven't found a better way to use this
    /// for `CtxPreStorageRead`/`CtxPostStorageRead`, which have generics
    /// and explicit lifetimes.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// impl_pos_read_only! { impl PosReadOnly for X }
    /// ```
    #[macro_export]
    macro_rules! impl_pos_read_only {
    ( $( $any:tt )*)
     => {
        $( $any )*
        {
            type Address = crate::types::address::Address;
            type Error = crate::ledger::native_vp::Error;
            type PublicKey = crate::types::key::common::PublicKey;
            type TokenAmount = crate::types::token::Amount;
            type TokenChange = crate::types::token::Change;

            const POS_ADDRESS: Self::Address = crate::ledger::pos::ADDRESS;

            fn staking_token_address() -> Self::Address {
                crate::ledger::pos::staking_token_address()
            }

            fn read_pos_params(&self) -> std::result::Result<PosParams, Self::Error> {
                let value = crate::ledger::read::StorageRead::read_bytes(self, &params_key())?.unwrap();
                Ok(decode(value).unwrap())
            }

            fn read_validator_staking_reward_address(
                &self,
                key: &Self::Address,
            ) -> std::result::Result<Option<Self::Address>, Self::Error> {
                let value = crate::ledger::read::StorageRead::read_bytes(
                    self,
                    &validator_staking_reward_address_key(key),
                )?;
                Ok(value.map(|value| decode(value).unwrap()))
            }

            fn read_validator_consensus_key(
                &self,
                key: &Self::Address,
            ) -> std::result::Result<Option<ValidatorConsensusKeys>, Self::Error> {
                let value =
                    crate::ledger::read::StorageRead::read_bytes(self, &validator_consensus_key_key(key))?;
                Ok(value.map(|value| decode(value).unwrap()))
            }

            fn read_validator_state(
                &self,
                key: &Self::Address,
            ) -> std::result::Result<Option<ValidatorStates>, Self::Error> {
                let value = crate::ledger::read::StorageRead::read_bytes(self, &validator_state_key(key))?;
                Ok(value.map(|value| decode(value).unwrap()))
            }

            fn read_validator_total_deltas(
                &self,
                key: &Self::Address,
            ) -> std::result::Result<Option<ValidatorTotalDeltas>, Self::Error> {
                let value =
                    crate::ledger::read::StorageRead::read_bytes(self, &validator_total_deltas_key(key))?;
                Ok(value.map(|value| decode(value).unwrap()))
            }

            fn read_validator_voting_power(
                &self,
                key: &Self::Address,
            ) -> std::result::Result<Option<ValidatorVotingPowers>, Self::Error> {
                let value =
                    crate::ledger::read::StorageRead::read_bytes(self, &validator_voting_power_key(key))?;
                Ok(value.map(|value| decode(value).unwrap()))
            }

            fn read_validator_slashes(
                &self,
                key: &Self::Address,
            ) -> std::result::Result<Vec<types::Slash>, Self::Error> {
                let value = crate::ledger::read::StorageRead::read_bytes(self, &validator_slashes_key(key))?;
                Ok(value
                    .map(|value| decode(value).unwrap())
                    .unwrap_or_default())
            }

            fn read_bond(
                &self,
                key: &BondId,
            ) -> std::result::Result<Option<Bonds>, Self::Error> {
                let value = crate::ledger::read::StorageRead::read_bytes(self, &bond_key(key))?;
                Ok(value.map(|value| decode(value).unwrap()))
            }

            fn read_unbond(
                &self,
                key: &BondId,
            ) -> std::result::Result<Option<Unbonds>, Self::Error> {
                let value = crate::ledger::read::StorageRead::read_bytes(self, &unbond_key(key))?;
                Ok(value.map(|value| decode(value).unwrap()))
            }

            fn read_validator_set(
                &self,
            ) -> std::result::Result<ValidatorSets, Self::Error> {
                let value =
                    crate::ledger::read::StorageRead::read_bytes(self, &validator_set_key())?.unwrap();
                Ok(decode(value).unwrap())
            }

            fn read_total_voting_power(
                &self,
            ) -> std::result::Result<TotalVotingPowers, Self::Error> {
                let value =
                    crate::ledger::read::StorageRead::read_bytes(self, &total_voting_power_key())?.unwrap();
                Ok(decode(value).unwrap())
            }
        }
    }
}
}
