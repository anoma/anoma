//! Proof-of-Stake native validity predicate.

use std::collections::HashSet;
use std::panic::{RefUnwindSafe, UnwindSafe};

pub use anoma_proof_of_stake;
pub use anoma_proof_of_stake::parameters::PosParams;
pub use anoma_proof_of_stake::types::{
    self, Slash, Slashes, TotalVotingPowers, ValidatorStates,
    ValidatorVotingPowers,
};
use anoma_proof_of_stake::validation::validate;
use anoma_proof_of_stake::{validation, PosReadOnly};
use borsh::BorshDeserialize;
use itertools::Itertools;
use thiserror::Error;

use super::{
    bond_key, is_bond_key, is_params_key, is_total_voting_power_key,
    is_unbond_key, is_validator_set_key,
    is_validator_staking_reward_address_key, is_validator_total_deltas_key,
    is_validator_voting_power_key, params_key, staking_token_address,
    total_voting_power_key, unbond_key, validator_consensus_key_key,
    validator_set_key, validator_slashes_key,
    validator_staking_reward_address_key, validator_state_key,
    validator_total_deltas_key, validator_voting_power_key, BondId, Bonds,
    Unbonds, ValidatorConsensusKeys, ValidatorSets, ValidatorTotalDeltas,
};
use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::types::decode;
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
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

/// Proof-of-Stake validity predicate
pub struct PosVP<'a, DB, H>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

// TODO this is temporarily to run PoS native VP in a new thread to avoid
// crashing the ledger (in apps/src/lib/node/ledger/protocol/mod.rs). The
// RefCells contained within PosVP are not thread-safe, but each thread has its
// own instances.
impl<DB, H> UnwindSafe for PosVP<'_, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
}

// TODO this is temporarily to run PoS native VP in a new thread to avoid
// crashing the ledger (in apps/src/lib/node/ledger/protocol/mod.rs). The
// RefCells contained within PosVP are not thread-safe, but each thread has its
// own instances.
impl<DB, H> RefUnwindSafe for PosVP<'_, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
}

impl<'a, DB, H> NativeVp for PosVP<'a, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::PoS;

    fn validate_tx(
        &self,
        _tx_data: &[u8],
        keys_changed: &HashSet<Key>,
        _verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        use validation::Data;
        use validation::DataUpdate::{self, *};
        use validation::ValidatorUpdate::*;

        let mut changes: Vec<DataUpdate<_, _, _>> = vec![];
        let current_epoch = self.ctx.get_block_epoch()?;
        for key in keys_changed {
            if is_params_key(key) {
                // TODO parameters changes are not yet implemented
                return Ok(false);
            } else if is_validator_set_key(key) {
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    ValidatorSets::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    ValidatorSets::try_from_slice(&bytes[..]).ok()
                });
                changes.push(ValidatorSet(Data { pre, post }));
            } else if let Some(validator) =
                is_validator_staking_reward_address_key(key)
            {
                let pre = self
                    .ctx
                    .read_pre(key)?
                    .and_then(|bytes| Address::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_post(key)?
                    .and_then(|bytes| Address::try_from_slice(&bytes[..]).ok());
                changes.push(Validator {
                    address: validator.clone(),
                    update: StakingRewardAddress(Data { pre, post }),
                });
            } else if let Some(validator) = is_validator_total_deltas_key(key) {
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    ValidatorTotalDeltas::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    ValidatorTotalDeltas::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Validator {
                    address: validator.clone(),
                    update: TotalDeltas(Data { pre, post }),
                });
            } else if let Some(validator) = is_validator_voting_power_key(key) {
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    ValidatorVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    ValidatorVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Validator {
                    address: validator.clone(),
                    update: VotingPowerUpdate(Data { pre, post }),
                });
            } else if let Some(owner) =
                token::is_balance_key(&staking_token_address(), key)
            {
                if owner != &Address::Internal(Self::ADDR) {
                    continue;
                }
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    token::Amount::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    token::Amount::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Balance(Data { pre, post }));
            } else if let Some(bond_id) = is_bond_key(key) {
                let pre = self
                    .ctx
                    .read_pre(key)?
                    .and_then(|bytes| Bonds::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_post(key)?
                    .and_then(|bytes| Bonds::try_from_slice(&bytes[..]).ok());
                // For bonds, we need to look-up slashes
                let slashes = self
                    .ctx
                    .read_pre(&validator_slashes_key(&bond_id.validator))?
                    .and_then(|bytes| Slashes::try_from_slice(&bytes[..]).ok())
                    .unwrap_or_default();
                changes.push(Bond {
                    id: bond_id.clone(),
                    data: Data { pre, post },
                    slashes,
                });
            } else if let Some(unbond_id) = is_unbond_key(key) {
                let pre = self
                    .ctx
                    .read_pre(key)?
                    .and_then(|bytes| Unbonds::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_post(key)?
                    .and_then(|bytes| Unbonds::try_from_slice(&bytes[..]).ok());
                // For unbonds, we need to look-up slashes
                let slashes = self
                    .ctx
                    .read_pre(&validator_slashes_key(&unbond_id.validator))?
                    .and_then(|bytes| Slashes::try_from_slice(&bytes[..]).ok())
                    .unwrap_or_default();
                changes.push(Unbond {
                    id: unbond_id.clone(),
                    data: Data { pre, post },
                    slashes,
                });
            } else if is_total_voting_power_key(key) {
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    TotalVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    TotalVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                changes.push(TotalVotingPower(Data { pre, post }));
            } else {
                tracing::info!("PoS unrecognized key change {} rejected", key);
                return Ok(false);
            }
        }

        let params = self.read_pos_params();
        let errors = validate(&params, changes, current_epoch);
        Ok(if errors.is_empty() {
            true
        } else {
            tracing::info!(
                "PoS validation errors:\n - {}",
                errors.iter().format("\n - ")
            );
            false
        })
    }
}

impl<D, H> PosReadOnly for PosVP<'_, D, H>
where
    D: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    type Address = Address;
    type PublicKey = key::ed25519::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = super::ADDRESS;

    fn staking_token_address() -> Self::Address {
        super::staking_token_address()
    }

    fn read_pos_params(&self) -> PosParams {
        let value = self.ctx.read_pre(&params_key()).unwrap().unwrap();
        decode(value).unwrap()
    }

    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> Option<Self::Address> {
        let value = self
            .ctx
            .read_pre(&validator_staking_reward_address_key(key))
            .unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys> {
        let value = self
            .ctx
            .read_pre(&validator_consensus_key_key(key))
            .unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorStates> {
        let value = self.ctx.read_pre(&validator_state_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorTotalDeltas> {
        let value =
            self.ctx.read_pre(&validator_total_deltas_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorVotingPowers> {
        let value =
            self.ctx.read_pre(&validator_voting_power_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_slashes(&self, key: &Self::Address) -> Vec<types::Slash> {
        let value = self.ctx.read_pre(&validator_slashes_key(key)).unwrap();
        value
            .map(|value| decode(value).unwrap())
            .unwrap_or_default()
    }

    fn read_bond(&self, key: &BondId) -> Option<Bonds> {
        let value = self.ctx.read_pre(&bond_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_unbond(&self, key: &BondId) -> Option<Unbonds> {
        let value = self.ctx.read_pre(&unbond_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_set(&self) -> ValidatorSets {
        let value = self.ctx.read_pre(&validator_set_key()).unwrap().unwrap();
        decode(value).unwrap()
    }

    fn read_total_voting_power(&self) -> TotalVotingPowers {
        let value = self
            .ctx
            .read_pre(&total_voting_power_key())
            .unwrap()
            .unwrap();
        decode(value).unwrap()
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}
