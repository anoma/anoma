use std::collections::BTreeSet;

use borsh::BorshDeserialize;
use thiserror::Error;

use super::storage as gov_storage;
use crate::ledger::native_vp::{self, Ctx};
use crate::ledger::pos::{self as pos_storage, BondId, Bonds};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::address::{xan as m1t, Address, InternalAddress};
use crate::types::storage::{Epoch, Key};
use crate::types::token;
use crate::vm::WasmCacheAccess;

/// Internal governance address
pub const ADDRESS: Address = Address::Internal(InternalAddress::Governance);

/// Governance functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Validate an unknown key
pub fn validate_unknown_key() -> bool {
    true
}

/// Validate an unknown governance key
pub fn validate_unknown_governance_key() -> bool {
    false
}

/// Validate a governance parameter
pub fn validate_parameter_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    tx_data: &[u8],
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let proposal_id = u64::try_from_slice(tx_data).ok();
    match proposal_id {
        Some(id) => is_proposal_accepted(ctx, id),
        _ => false,
    }
}

/// Validate a balance key
pub fn validate_balance_key<'a, DB, H, CA>(ctx: &Ctx<'a, DB, H, CA>) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let balance_key = token::balance_key(&m1t(), &ADDRESS);
    let min_funds_parameter_key = gov_storage::get_min_proposal_fund_key();
    let min_funds_parameter: Option<token::Amount> =
        read(ctx, &min_funds_parameter_key, ReadType::PRE).ok();
    let pre_balance: Option<token::Amount> =
        read(ctx, &balance_key, ReadType::PRE).ok();
    let post_balance: Option<token::Amount> =
        read(ctx, &balance_key, ReadType::POST).ok();
    match (min_funds_parameter, pre_balance, post_balance) {
        (Some(min_funds_parameter), Some(pre_balance), Some(post_balance)) => {
            post_balance > pre_balance
                && post_balance - pre_balance >= min_funds_parameter
        }
        (Some(min_funds_parameter), None, Some(post_balance)) => {
            post_balance >= min_funds_parameter
        }
        _ => false,
    }
}

/// Validate a author key
pub fn validate_author_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    proposal_id: u64,
    verifiers: &BTreeSet<Address>,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let author_key = gov_storage::get_author_key(proposal_id);
    let author = read(ctx, &author_key, ReadType::POST).ok();
    let has_pre_author = ctx.has_key_pre(&author_key).ok();
    match (has_pre_author, author) {
        (Some(has_pre_author), Some(author)) => {
            // TODO: if author is an implicit address, we should asssume its
            // existence we should reuse the same logic as in
            // check_address_existence in shared/src/vm/host_env.rs
            let address_exist_key = Key::validity_predicate(&author);
            let address_exist = ctx.has_key_post(&address_exist_key).ok();
            if let Some(address_exist) = address_exist {
                !has_pre_author && verifiers.contains(&author) && address_exist
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Validate a counter key
pub fn validate_counter_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    set_count: u64,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let counter_key = gov_storage::get_counter_key();
    let pre_counter: Option<u64> = read(ctx, &counter_key, ReadType::PRE).ok();
    let post_counter: Option<u64> =
        read(ctx, &counter_key, ReadType::POST).ok();
    match (pre_counter, post_counter) {
        (Some(pre_counter), Some(post_counter)) => {
            pre_counter + set_count == post_counter
        }
        _ => false,
    }
}

/// Validate a commit key
pub fn validate_commit_key<'a, DB, H, CA>(ctx: &Ctx<'a, DB, H, CA>) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let counter_key = gov_storage::get_counter_key();
    let pre_counter: Option<u64> = read(ctx, &counter_key, ReadType::PRE).ok();
    let post_counter: Option<u64> =
        read(ctx, &counter_key, ReadType::POST).ok();
    match (pre_counter, post_counter) {
        (Some(pre_counter), Some(post_counter)) => {
            // NOTE: can't do pre_counter + set_count == post_counter here
            // because someone may update an empty proposal that just register a
            // committing key causing a bug
            pre_counter < post_counter
        }
        _ => false,
    }
}

/// Validate a funds key
pub fn validate_funds_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    proposal_id: u64,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let funds_key = gov_storage::get_funds_key(proposal_id);
    let balance_key = token::balance_key(&m1t(), &ADDRESS);
    let min_funds_parameter_key = gov_storage::get_min_proposal_fund_key();
    let min_funds_parameter: Option<token::Amount> =
        read(ctx, &min_funds_parameter_key, ReadType::PRE).ok();
    let pre_balance: Option<token::Amount> =
        read(ctx, &balance_key, ReadType::PRE).ok();
    let post_balance: Option<token::Amount> =
        read(ctx, &balance_key, ReadType::POST).ok();
    let post_funds: Option<token::Amount> =
        read(ctx, &funds_key, ReadType::POST).ok();
    match (min_funds_parameter, pre_balance, post_balance, post_funds) {
        (
            Some(min_funds_parameter),
            Some(pre_balance),
            Some(post_balance),
            Some(post_funds),
        ) => {
            post_funds >= min_funds_parameter
                && post_balance - pre_balance == post_funds
        }
        (
            Some(min_funds_parameter),
            None,
            Some(post_balance),
            Some(post_funds),
        ) => post_funds >= min_funds_parameter && post_balance == post_funds,
        _ => false,
    }
}

/// Validate a start_epoch key
pub fn validate_start_epoch_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    proposal_id: u64,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let start_epoch_key = gov_storage::get_voting_start_epoch_key(proposal_id);
    let end_epoch_key = gov_storage::get_voting_end_epoch_key(proposal_id);
    let start_epoch: Option<Epoch> =
        read(ctx, &start_epoch_key, ReadType::POST).ok();
    let end_epoch: Option<Epoch> =
        read(ctx, &end_epoch_key, ReadType::POST).ok();
    let current_epoch = ctx.get_block_epoch().ok();
    let min_period_parameter_key = gov_storage::get_min_proposal_period_key();
    let min_period: Option<u64> =
        read(ctx, &min_period_parameter_key, ReadType::PRE).ok();
    let has_pre_start_epoch = ctx.has_key_pre(&start_epoch_key).ok();
    let has_pre_end_epoch = ctx.has_key_pre(&end_epoch_key).ok();
    match (
        has_pre_start_epoch,
        has_pre_end_epoch,
        min_period,
        start_epoch,
        end_epoch,
        current_epoch,
    ) {
        (
            Some(has_pre_start_epoch),
            Some(has_pre_end_epoch),
            Some(min_period),
            Some(start_epoch),
            Some(end_epoch),
            Some(current_epoch),
        ) => {
            if end_epoch <= start_epoch || start_epoch <= current_epoch {
                return false;
            }
            !has_pre_start_epoch
                && !has_pre_end_epoch
                && (end_epoch - start_epoch) % min_period == 0
                && (end_epoch - start_epoch).0 >= min_period
        }
        _ => false,
    }
}

/// Validate a end_epoch key
pub fn validate_end_epoch_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    proposal_id: u64,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let start_epoch_key = gov_storage::get_voting_start_epoch_key(proposal_id);
    let end_epoch_key = gov_storage::get_voting_end_epoch_key(proposal_id);
    let start_epoch: Option<Epoch> =
        read(ctx, &start_epoch_key, ReadType::POST).ok();
    let end_epoch: Option<Epoch> =
        read(ctx, &end_epoch_key, ReadType::POST).ok();
    let current_epoch = ctx.get_block_epoch().ok();
    let min_period_parameter_key = gov_storage::get_min_proposal_period_key();
    let min_period: Option<u64> =
        read(ctx, &min_period_parameter_key, ReadType::PRE).ok();
    let has_pre_start_epoch = ctx.has_key_pre(&start_epoch_key).ok();
    let has_pre_end_epoch = ctx.has_key_pre(&end_epoch_key).ok();
    match (
        has_pre_start_epoch,
        has_pre_end_epoch,
        min_period,
        start_epoch,
        end_epoch,
        current_epoch,
    ) {
        (
            Some(has_pre_start_epoch),
            Some(has_pre_end_epoch),
            Some(min_period),
            Some(start_epoch),
            Some(end_epoch),
            Some(current_epoch),
        ) => {
            if end_epoch <= start_epoch || start_epoch <= current_epoch {
                return false;
            }
            !has_pre_start_epoch
                && !has_pre_end_epoch
                && (end_epoch - start_epoch) % min_period == 0
                && (end_epoch - start_epoch).0 >= min_period
        }
        _ => false,
    }
}

/// Validate a grace_epoch key
pub fn validate_grace_epoch_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    proposal_id: u64,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let end_epoch_key = gov_storage::get_voting_end_epoch_key(proposal_id);
    let grace_epoch_key = gov_storage::get_grace_epoch_key(proposal_id);
    let min_grace_epoch_key = gov_storage::get_min_proposal_grace_epoch_key();
    let end_epoch: Option<u64> = read(ctx, &end_epoch_key, ReadType::POST).ok();
    let grace_epoch: Option<u64> =
        read(ctx, &grace_epoch_key, ReadType::POST).ok();
    let min_grace_epoch: Option<u64> =
        read(ctx, &min_grace_epoch_key, ReadType::PRE).ok();
    let has_pre_grace_epoch = ctx.has_key_pre(&grace_epoch_key).ok();
    match (has_pre_grace_epoch, min_grace_epoch, grace_epoch, end_epoch) {
        (
            Some(has_pre_grace_epoch),
            Some(min_grace_epoch),
            Some(grace_epoch),
            Some(end_epoch),
        ) => {
            let committing_epoch_key =
                gov_storage::get_committing_proposals_key(
                    proposal_id,
                    grace_epoch,
                );
            let committing_epoch = ctx.has_key_post(&committing_epoch_key);
            match committing_epoch {
                Ok(committing_epoch_exists) => {
                    !has_pre_grace_epoch
                        && end_epoch < grace_epoch
                        && grace_epoch - end_epoch >= min_grace_epoch
                        && committing_epoch_exists
                }
                _ => false,
            }
        }
        _ => false,
    }
}

/// Validate a proposal_code key
pub fn validate_proposal_code_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    proposal_id: u64,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let content_key: Key = gov_storage::get_content_key(proposal_id);
    let max_content_length_parameter_key =
        gov_storage::get_max_proposal_content_key();
    let max_content_length =
        read(ctx, &max_content_length_parameter_key, ReadType::PRE).ok();
    let has_pre_content = ctx.has_key_pre(&content_key).ok();
    let post_content = ctx.read_post(&content_key).unwrap();
    match (has_pre_content, post_content, max_content_length) {
        (
            Some(has_pre_content),
            Some(post_content),
            Some(max_content_length),
        ) => !has_pre_content && post_content.len() < max_content_length,
        _ => false,
    }
}

/// Validate a content key
pub fn validate_content_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    proposal_id: u64,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let content_key: Key = gov_storage::get_content_key(proposal_id);
    let max_content_length_parameter_key =
        gov_storage::get_max_proposal_content_key();
    let max_content_length =
        read(ctx, &max_content_length_parameter_key, ReadType::PRE).ok();
    let has_pre_content = ctx.has_key_pre(&content_key).ok();
    let post_content = ctx.read_post(&content_key).unwrap();
    match (has_pre_content, post_content, max_content_length) {
        (
            Some(has_pre_content),
            Some(post_content),
            Some(max_content_length),
        ) => !has_pre_content && post_content.len() < max_content_length,
        _ => false,
    }
}

/// Validate a vote key
pub fn validate_vote_key<'a, DB, H, CA>(
    ctx: &Ctx<'a, DB, H, CA>,
    proposal_id: u64,
    key: &Key,
    verifiers: &BTreeSet<Address>,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let counter_key = gov_storage::get_counter_key();
    let voting_start_epoch_key =
        gov_storage::get_voting_start_epoch_key(proposal_id);
    let voting_end_epoch_key =
        gov_storage::get_voting_end_epoch_key(proposal_id);
    let current_epoch = ctx.get_block_epoch().ok();
    let pre_voting_start_epoch: Option<Epoch> =
        read(ctx, &voting_start_epoch_key, ReadType::PRE).ok();
    let pre_voting_end_epoch: Option<Epoch> =
        read(ctx, &voting_end_epoch_key, ReadType::PRE).ok();
    let pre_counter: Option<u64> = read(ctx, &counter_key, ReadType::PRE).ok();
    let voter = gov_storage::get_voter_address(key);
    let delegation_address = gov_storage::get_vote_delegation_address(key);

    match (
        pre_counter,
        voter,
        delegation_address,
        current_epoch,
        pre_voting_start_epoch,
        pre_voting_end_epoch,
    ) {
        (
            Some(pre_counter),
            Some(voter_address),
            Some(delegation_address),
            Some(current_epoch),
            Some(pre_voting_start_epoch),
            Some(pre_voting_end_epoch),
        ) => {
            let is_delegator = is_delegator(
                ctx,
                pre_voting_start_epoch,
                verifiers,
                voter_address,
                delegation_address,
            );

            let is_validator = is_validator(
                ctx,
                pre_voting_start_epoch,
                verifiers,
                voter_address,
                delegation_address,
            );

            let is_valid_validator_voting_period =
                is_valid_validator_voting_period(
                    current_epoch,
                    pre_voting_start_epoch,
                    pre_voting_end_epoch,
                );

            pre_counter > proposal_id
                && current_epoch >= pre_voting_start_epoch
                && current_epoch <= pre_voting_end_epoch
                && (is_delegator
                    || (is_validator && is_valid_validator_voting_period))
        }
        _ => false,
    }
}

/// Read options
#[allow(clippy::upper_case_acronyms)]
pub enum ReadType {
    /// Read pre storage
    #[allow(clippy::upper_case_acronyms)]
    PRE,
    /// Read post storage
    #[allow(clippy::upper_case_acronyms)]
    POST,
}

/// Check if a proposal id is beign executed
pub fn is_proposal_accepted<DB, H, CA>(
    context: &Ctx<DB, H, CA>,
    proposal_id: u64,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let proposal_execution_key =
        gov_storage::get_proposal_execution_key(proposal_id);
    context
        .has_key_pre(&proposal_execution_key)
        .unwrap_or(false)
}

/// Read a value from the storage
pub fn read<T, DB, H, CA>(
    context: &Ctx<DB, H, CA>,
    key: &Key,
    read_type: ReadType,
) -> Result<T>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
    T: Clone + BorshDeserialize,
{
    let storage_result = match read_type {
        ReadType::PRE => context.read_pre(key),
        ReadType::POST => context.read_post(key),
    };

    match storage_result {
        Ok(value) => match value {
            Some(bytes) => T::try_from_slice(&bytes)
                .map_err(Error::NativeVpDeserializationError),
            None => Err(Error::NativeVpNonExistingKeyError(key.to_string())),
        },
        Err(err) => Err(Error::NativeVpError(err)),
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
    #[error("Native VP error deserialization: {0}")]
    NativeVpDeserializationError(std::io::Error),
    #[error("Native VP error non-existing key: {0}")]
    NativeVpNonExistingKeyError(String),
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}

/// Check if a vote is from a delegator
pub fn is_delegator<DB, H, CA>(
    context: &Ctx<DB, H, CA>,
    epoch: Epoch,
    verifiers: &BTreeSet<Address>,
    address: &Address,
    delegation_address: &Address,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let bond_key = pos_storage::bond_key(&BondId {
        source: address.clone(),
        validator: delegation_address.clone(),
    });
    let bonds: Option<Bonds> = read(context, &bond_key, ReadType::PRE).ok();

    if let Some(bonds) = bonds {
        bonds.get(epoch).is_some() && verifiers.contains(address)
    } else {
        false
    }
}

/// Checks if it's a valid epoch window for a validator to vote
pub fn is_valid_validator_voting_period(
    current_epoch: Epoch,
    voting_start_epoch: Epoch,
    voting_end_epoch: Epoch,
) -> bool {
    voting_start_epoch < voting_end_epoch
        && current_epoch * 3 <= voting_start_epoch + voting_end_epoch * 2
}

/// Check if a vote is from a validator
pub fn is_validator<DB, H, CA>(
    context: &Ctx<DB, H, CA>,
    epoch: Epoch,
    verifiers: &BTreeSet<Address>,
    address: &Address,
    delegation_address: &Address,
) -> bool
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let validator_set_key = pos_storage::validator_set_key();
    let pre_validator_set: pos_storage::ValidatorSets =
        read(context, &validator_set_key, ReadType::PRE).unwrap();
    let validator_set = pre_validator_set.get(epoch);

    match validator_set {
        Some(validator_set) => {
            let all_validators =
                validator_set.active.union(&validator_set.inactive);
            all_validators.into_iter().any(|weighted_validator| {
                weighted_validator.address.eq(address)
            }) && verifiers.contains(address)
                && delegation_address.eq(address)
        }
        None => false,
    }
}

/// Reads bytes from storage either before or after the execution of a tx
pub fn read_bytes<DB, H, CA>(
    context: &Ctx<DB, H, CA>,
    key: &Key,
    read_type: ReadType,
) -> Option<Vec<u8>>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let storage_result = match read_type {
        ReadType::PRE => context.read_pre(key),
        ReadType::POST => context.read_post(key),
    };

    match storage_result {
        Ok(value) => value,
        Err(_err) => None,
    }
}
