//! Proof-of-Stake native validity predicate.

use std::collections::BTreeSet;
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
use crate::ledger::governance::vp::is_proposal_accepted;
use crate::ledger::native_vp::{self, Ctx, NativeVp, VpEnv};
use crate::ledger::pos::{
    is_validator_address_raw_hash_key, is_validator_consensus_key_key,
    is_validator_state_key,
};
use crate::ledger::storage::types::decode;
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{Key, KeySeg};
use crate::types::{key, token};
use crate::vm::WasmCacheAccess;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// PoS functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Proof-of-Stake validity predicate
pub struct PosVP<'a, DB, H, CA>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H, CA>,
}

impl<'a, DB, H, CA> PosVP<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    /// Instantiate a `PosVP`.
    pub fn new(ctx: Ctx<'a, DB, H, CA>) -> Self {
        Self { ctx }
    }
}

// TODO this is temporarily to run PoS native VP in a new thread to avoid
// crashing the ledger (in apps/src/lib/node/ledger/protocol/mod.rs). The
// RefCells contained within PosVP are not thread-safe, but each thread has its
// own instances.
impl<DB, H, CA> UnwindSafe for PosVP<'_, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
}

// TODO this is temporarily to run PoS native VP in a new thread to avoid
// crashing the ledger (in apps/src/lib/node/ledger/protocol/mod.rs). The
// RefCells contained within PosVP are not thread-safe, but each thread has its
// own instances.
impl<DB, H, CA> RefUnwindSafe for PosVP<'_, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
}

impl<'a, DB, H, CA> NativeVp for PosVP<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::PoS;

    fn validate_tx(
        &self,
        tx_data: &[u8],
        keys_changed: &BTreeSet<Key>,
        verifiers: &BTreeSet<Address>,
    ) -> Result<bool> {
        use validation::Data;
        use validation::DataUpdate::{self, *};
        use validation::ValidatorUpdate::*;

        let addr = Address::Internal(Self::ADDR);
        let mut changes: Vec<DataUpdate<_, _, _, _>> = vec![];
        let current_epoch = self.ctx.get_block_epoch()?;
        for key in keys_changed {
            if is_params_key(key) {
                let proposal_id = u64::try_from_slice(tx_data).ok();
                match proposal_id {
                    Some(id) => return Ok(is_proposal_accepted(&self.ctx, id)),
                    _ => return Ok(false),
                }
            } else if let Some(owner) = key.is_validity_predicate() {
                let has_pre = self.ctx.has_key_pre(key)?;
                let has_post = self.ctx.has_key_post(key)?;
                if has_pre && has_post {
                    // VP updates must be verified by the owner
                    return Ok(!verifiers.contains(owner));
                } else if has_pre || !has_post {
                    // VP cannot be deleted
                    return Ok(false);
                }
            } else if is_validator_set_key(key) {
                let pre = self.ctx.read_bytes_pre(key)?.and_then(|bytes| {
                    ValidatorSets::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_bytes_post(key)?.and_then(|bytes| {
                    ValidatorSets::try_from_slice(&bytes[..]).ok()
                });
                changes.push(ValidatorSet(Data { pre, post }));
            } else if let Some(validator) = is_validator_state_key(key) {
                let pre = self.ctx.read_bytes_pre(key)?.and_then(|bytes| {
                    ValidatorStates::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_bytes_post(key)?.and_then(|bytes| {
                    ValidatorStates::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Validator {
                    address: validator.clone(),
                    update: State(Data { pre, post }),
                });
            } else if let Some(validator) =
                is_validator_staking_reward_address_key(key)
            {
                let pre = self
                    .ctx
                    .read_bytes_pre(key)?
                    .and_then(|bytes| Address::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_bytes_post(key)?
                    .and_then(|bytes| Address::try_from_slice(&bytes[..]).ok());
                changes.push(Validator {
                    address: validator.clone(),
                    update: StakingRewardAddress(Data { pre, post }),
                });
            } else if let Some(validator) = is_validator_consensus_key_key(key)
            {
                let pre = self.ctx.read_bytes_pre(key)?.and_then(|bytes| {
                    ValidatorConsensusKeys::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_bytes_post(key)?.and_then(|bytes| {
                    ValidatorConsensusKeys::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Validator {
                    address: validator.clone(),
                    update: ConsensusKey(Data { pre, post }),
                });
            } else if let Some(validator) = is_validator_total_deltas_key(key) {
                let pre = self.ctx.read_bytes_pre(key)?.and_then(|bytes| {
                    ValidatorTotalDeltas::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_bytes_post(key)?.and_then(|bytes| {
                    ValidatorTotalDeltas::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Validator {
                    address: validator.clone(),
                    update: TotalDeltas(Data { pre, post }),
                });
            } else if let Some(validator) = is_validator_voting_power_key(key) {
                let pre = self.ctx.read_bytes_pre(key)?.and_then(|bytes| {
                    ValidatorVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_bytes_post(key)?.and_then(|bytes| {
                    ValidatorVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Validator {
                    address: validator.clone(),
                    update: VotingPowerUpdate(Data { pre, post }),
                });
            } else if let Some(raw_hash) =
                is_validator_address_raw_hash_key(key)
            {
                let pre = self
                    .ctx
                    .read_bytes_pre(key)?
                    .and_then(|bytes| Address::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_bytes_post(key)?
                    .and_then(|bytes| Address::try_from_slice(&bytes[..]).ok());
                // Find the raw hashes of the addresses
                let pre = pre.map(|pre| {
                    let raw_hash =
                        pre.raw_hash().map(String::from).unwrap_or_default();
                    (pre, raw_hash)
                });
                let post = post.map(|post| {
                    let raw_hash =
                        post.raw_hash().map(String::from).unwrap_or_default();
                    (post, raw_hash)
                });
                changes.push(ValidatorAddressRawHash {
                    raw_hash: raw_hash.to_string(),
                    data: Data { pre, post },
                });
            } else if let Some(owner) =
                token::is_balance_key(&staking_token_address(), key)
            {
                if owner != &addr {
                    continue;
                }
                let pre = self.ctx.read_bytes_pre(key)?.and_then(|bytes| {
                    token::Amount::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_bytes_post(key)?.and_then(|bytes| {
                    token::Amount::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Balance(Data { pre, post }));
            } else if let Some(bond_id) = is_bond_key(key) {
                let pre = self
                    .ctx
                    .read_bytes_pre(key)?
                    .and_then(|bytes| Bonds::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_bytes_post(key)?
                    .and_then(|bytes| Bonds::try_from_slice(&bytes[..]).ok());
                // For bonds, we need to look-up slashes
                let slashes = self
                    .ctx
                    .read_bytes_pre(&validator_slashes_key(&bond_id.validator))?
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
                    .read_bytes_pre(key)?
                    .and_then(|bytes| Unbonds::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_bytes_post(key)?
                    .and_then(|bytes| Unbonds::try_from_slice(&bytes[..]).ok());
                // For unbonds, we need to look-up slashes
                let slashes = self
                    .ctx
                    .read_bytes_pre(&validator_slashes_key(
                        &unbond_id.validator,
                    ))?
                    .and_then(|bytes| Slashes::try_from_slice(&bytes[..]).ok())
                    .unwrap_or_default();
                changes.push(Unbond {
                    id: unbond_id.clone(),
                    data: Data { pre, post },
                    slashes,
                });
            } else if is_total_voting_power_key(key) {
                let pre = self.ctx.read_bytes_pre(key)?.and_then(|bytes| {
                    TotalVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_bytes_post(key)?.and_then(|bytes| {
                    TotalVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                changes.push(TotalVotingPower(Data { pre, post }));
            } else if key.segments.get(0) == Some(&addr.to_db_key()) {
                // Unknown changes to this address space are disallowed
                tracing::info!("PoS unrecognized key change {} rejected", key);
                return Ok(false);
            } else {
                // Unknown changes anywhere else are permitted
                return Ok(true);
            }
        }

        let params = self.read_pos_params()?;
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

impl<D, H, CA> PosReadOnly for PosVP<'_, D, H, CA>
where
    D: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Address = Address;
    type Error = native_vp::Error;
    type PublicKey = key::common::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = super::ADDRESS;

    fn staking_token_address() -> Self::Address {
        super::staking_token_address()
    }

    fn read_pos_params(&self) -> std::result::Result<PosParams, Self::Error> {
        let value = self.ctx.read_bytes_pre(&params_key())?.unwrap();
        Ok(decode(value).unwrap())
    }

    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> std::result::Result<Option<Self::Address>, Self::Error> {
        let value = self
            .ctx
            .read_bytes_pre(&validator_staking_reward_address_key(key))?;
        Ok(value.map(|value| decode(value).unwrap()))
    }

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> std::result::Result<Option<ValidatorConsensusKeys>, Self::Error> {
        let value =
            self.ctx.read_bytes_pre(&validator_consensus_key_key(key))?;
        Ok(value.map(|value| decode(value).unwrap()))
    }

    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> std::result::Result<Option<ValidatorStates>, Self::Error> {
        let value = self.ctx.read_bytes_pre(&validator_state_key(key))?;
        Ok(value.map(|value| decode(value).unwrap()))
    }

    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> std::result::Result<Option<ValidatorTotalDeltas>, Self::Error> {
        let value =
            self.ctx.read_bytes_pre(&validator_total_deltas_key(key))?;
        Ok(value.map(|value| decode(value).unwrap()))
    }

    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> std::result::Result<Option<ValidatorVotingPowers>, Self::Error> {
        let value =
            self.ctx.read_bytes_pre(&validator_voting_power_key(key))?;
        Ok(value.map(|value| decode(value).unwrap()))
    }

    fn read_validator_slashes(
        &self,
        key: &Self::Address,
    ) -> std::result::Result<Vec<types::Slash>, Self::Error> {
        let value = self.ctx.read_bytes_pre(&validator_slashes_key(key))?;
        Ok(value
            .map(|value| decode(value).unwrap())
            .unwrap_or_default())
    }

    fn read_bond(
        &self,
        key: &BondId,
    ) -> std::result::Result<Option<Bonds>, Self::Error> {
        let value = self.ctx.read_bytes_pre(&bond_key(key))?;
        Ok(value.map(|value| decode(value).unwrap()))
    }

    fn read_unbond(
        &self,
        key: &BondId,
    ) -> std::result::Result<Option<Unbonds>, Self::Error> {
        let value = self.ctx.read_bytes_pre(&unbond_key(key))?;
        Ok(value.map(|value| decode(value).unwrap()))
    }

    fn read_validator_set(
        &self,
    ) -> std::result::Result<ValidatorSets, Self::Error> {
        let value = self.ctx.read_bytes_pre(&validator_set_key())?.unwrap();
        Ok(decode(value).unwrap())
    }

    fn read_total_voting_power(
        &self,
    ) -> std::result::Result<TotalVotingPowers, Self::Error> {
        let value =
            self.ctx.read_bytes_pre(&total_voting_power_key())?.unwrap();
        Ok(decode(value).unwrap())
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}
