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
    anoma_proof_of_stake::types::ValidatorConsensusKeys<
        key::ed25519::PublicKey,
    >;

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
    key::ed25519::PublicKey,
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
