//! Governance VP

/// governance parameters
pub mod parameters;
/// governance storage
pub mod storage;

use std::collections::BTreeSet;

use borsh::BorshDeserialize;
use thiserror::Error;

use self::storage as gov_storage;
use super::pos::{self as pos_storage, BondId, Bonds};
use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::address::{xan as m1t, Address, InternalAddress};
use crate::types::storage::{Epoch, Key};
use crate::types::token as token_storage;
use crate::types::token::Amount;
use crate::vm::WasmCacheAccess;

/// Internal governance address
pub const ADDRESS: Address = Address::Internal(InternalAddress::Governance);

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

/// Governance functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Governance VP
pub struct GovernanceVp<'a, DB, H, CA>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H, CA>,
}

impl<'a, DB, H, CA> NativeVp for GovernanceVp<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::Governance;

    fn validate_tx(
        &self,
        _tx_data: &[u8],
        keys_changed: &BTreeSet<Key>,
        verifiers: &BTreeSet<Address>,
    ) -> Result<bool> {
        let (is_valid_keys_set, set_count) =
            is_valid_key_set(&self.ctx, keys_changed);
        if !is_valid_keys_set {
            return Ok(false);
        };

        let result = keys_changed.iter().all(|key| {
            let proposal_id = gov_storage::get_id(key);

            let key_type: KeyType = key.into();
            match (key_type, proposal_id) {
                (KeyType::VOTE, Some(proposal_id)) => {
                    let counter_key = gov_storage::get_counter_key();
                    let voting_start_epoch_key =
                        gov_storage::get_voting_start_epoch_key(proposal_id);
                    let voting_end_epoch_key =
                        gov_storage::get_voting_end_epoch_key(proposal_id);
                    let current_epoch = self.ctx.get_block_epoch().ok();
                    let pre_voting_start_epoch: Option<Epoch> =
                        read(&self.ctx, &voting_start_epoch_key, ReadType::PRE)
                            .ok();
                    let pre_voting_end_epoch: Option<Epoch> =
                        read(&self.ctx, &voting_end_epoch_key, ReadType::PRE)
                            .ok();
                    let pre_counter: Option<u64> =
                        read(&self.ctx, &counter_key, ReadType::PRE).ok();
                    let voter = gov_storage::get_voter_address(key);
                    let delegation_address =
                        gov_storage::get_vote_delegation_address(key);

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
                                &self.ctx,
                                pre_voting_start_epoch,
                                verifiers,
                                voter_address,
                                delegation_address,
                            );

                            let is_validator = is_validator(
                                &self.ctx,
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
                                    || (is_validator
                                        && is_valid_validator_voting_period))
                        }
                        _ => false,
                    }
                }
                (KeyType::CONTENT, Some(proposal_id)) => {
                    let content_key: Key =
                        gov_storage::get_content_key(proposal_id);
                    let max_content_length_parameter_key =
                        gov_storage::get_max_proposal_content_key();
                    let max_content_length = read(
                        &self.ctx,
                        &max_content_length_parameter_key,
                        ReadType::PRE,
                    )
                    .ok();
                    let has_pre_content =
                        self.ctx.has_key_pre(&content_key).ok();
                    let post_content =
                        self.ctx.read_post(&content_key).unwrap();
                    match (has_pre_content, post_content, max_content_length) {
                        (
                            Some(has_pre_content),
                            Some(post_content),
                            Some(max_content_length),
                        ) => {
                            !has_pre_content
                                && post_content.len() < max_content_length
                        }
                        _ => false,
                    }
                }
                (KeyType::PROPOSAL_CODE, Some(proposal_id)) => {
                    let proposal_code_key =
                        gov_storage::get_proposal_code_key(proposal_id);
                    let max_proposal_code_size_parameter_key =
                        gov_storage::get_max_proposal_code_size_key();
                    let max_proposal_code_size: Option<usize> = read(
                        &self.ctx,
                        &max_proposal_code_size_parameter_key,
                        ReadType::PRE,
                    )
                    .ok();
                    let has_pre_proposal_code =
                        self.ctx.has_key_pre(&proposal_code_key).ok();
                    let post_proposal_code: Option<Vec<u8>> =
                        read(&self.ctx, &proposal_code_key, ReadType::POST)
                            .ok();
                    match (
                        has_pre_proposal_code,
                        post_proposal_code,
                        max_proposal_code_size,
                    ) {
                        (
                            Some(has_pre_proposal_code),
                            Some(post_proposal_code),
                            Some(max_proposal_code_size),
                        ) => {
                            !has_pre_proposal_code
                                && post_proposal_code.len()
                                    < max_proposal_code_size
                        }
                        _ => false,
                    }
                }
                (KeyType::GRACE_EPOCH, Some(proposal_id)) => {
                    let end_epoch_key =
                        gov_storage::get_voting_end_epoch_key(proposal_id);
                    let grace_epoch_key =
                        gov_storage::get_grace_epoch_key(proposal_id);
                    let min_grace_epoch_key =
                        gov_storage::get_min_proposal_grace_epoch_key();
                    let end_epoch: Option<u64> =
                        read(&self.ctx, &end_epoch_key, ReadType::POST).ok();
                    let grace_epoch: Option<u64> =
                        read(&self.ctx, &grace_epoch_key, ReadType::POST).ok();
                    let min_grace_epoch: Option<u64> =
                        read(&self.ctx, &min_grace_epoch_key, ReadType::PRE)
                            .ok();
                    let has_pre_grace_epoch =
                        self.ctx.has_key_pre(&grace_epoch_key).ok();
                    match (
                        has_pre_grace_epoch,
                        min_grace_epoch,
                        grace_epoch,
                        end_epoch,
                    ) {
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
                            let committing_epoch =
                                self.ctx.has_key_post(&committing_epoch_key);
                            match committing_epoch {
                                Ok(committing_epoch_exists) => {
                                    !has_pre_grace_epoch
                                        && end_epoch < grace_epoch
                                        && grace_epoch - end_epoch
                                            >= min_grace_epoch
                                        && committing_epoch_exists
                                }
                                _ => false,
                            }
                        }
                        _ => false,
                    }
                }
                (
                    KeyType::START_EPOCH | KeyType::END_EPOCH,
                    Some(proposal_id),
                ) => {
                    let start_epoch_key =
                        gov_storage::get_voting_start_epoch_key(proposal_id);
                    let end_epoch_key =
                        gov_storage::get_voting_end_epoch_key(proposal_id);
                    let start_epoch: Option<Epoch> =
                        read(&self.ctx, &start_epoch_key, ReadType::POST).ok();
                    let end_epoch: Option<Epoch> =
                        read(&self.ctx, &end_epoch_key, ReadType::POST).ok();
                    let current_epoch = self.ctx.get_block_epoch().ok();
                    let min_period_parameter_key =
                        gov_storage::get_min_proposal_period_key();
                    let min_period: Option<u64> = read(
                        &self.ctx,
                        &min_period_parameter_key,
                        ReadType::PRE,
                    )
                    .ok();
                    let has_pre_start_epoch =
                        self.ctx.has_key_pre(&start_epoch_key).ok();
                    let has_pre_end_epoch =
                        self.ctx.has_key_pre(&end_epoch_key).ok();
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
                            if end_epoch <= start_epoch
                                || start_epoch <= current_epoch
                            {
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
                (KeyType::FUNDS, Some(proposal_id)) => {
                    let funds_key = gov_storage::get_funds_key(proposal_id);
                    let balance_key =
                        token_storage::balance_key(&m1t(), &ADDRESS);
                    let min_funds_parameter_key =
                        gov_storage::get_min_proposal_fund_key();
                    let min_funds_parameter: Option<Amount> = read(
                        &self.ctx,
                        &min_funds_parameter_key,
                        ReadType::PRE,
                    )
                    .ok();
                    let pre_balance: Option<Amount> =
                        read(&self.ctx, &balance_key, ReadType::PRE).ok();
                    let post_balance: Option<Amount> =
                        read(&self.ctx, &balance_key, ReadType::POST).ok();
                    let post_funds: Option<Amount> =
                        read(&self.ctx, &funds_key, ReadType::POST).ok();
                    match (
                        min_funds_parameter,
                        pre_balance,
                        post_balance,
                        post_funds,
                    ) {
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
                        ) => {
                            post_funds >= min_funds_parameter
                                && post_balance == post_funds
                        }
                        _ => false,
                    }
                }
                (KeyType::AUTHOR, Some(proposal_id)) => {
                    let author_key = gov_storage::get_author_key(proposal_id);
                    let author =
                        read(&self.ctx, &author_key, ReadType::POST).ok();
                    let has_pre_author = self.ctx.has_key_pre(&author_key).ok();
                    match (has_pre_author, author) {
                        (Some(has_pre_author), Some(author)) => {
                            !has_pre_author && verifiers.contains(&author)
                        }
                        _ => false,
                    }
                }
                (KeyType::COUNTER | KeyType::PROPOSAL_COMMIT, _) => {
                    let counter_key = gov_storage::get_counter_key();
                    let pre_counter: Option<u64> =
                        read(&self.ctx, &counter_key, ReadType::PRE).ok();
                    let post_counter: Option<u64> =
                        read(&self.ctx, &counter_key, ReadType::POST).ok();
                    match (pre_counter, post_counter) {
                        (Some(pre_counter), Some(post_counter)) => {
                            pre_counter + set_count == post_counter
                        }
                        _ => false,
                    }
                }
                (KeyType::BALANCE, _) => {
                    let balance_key =
                        token_storage::balance_key(&m1t(), &ADDRESS);
                    let min_funds_parameter_key =
                        gov_storage::get_min_proposal_fund_key();
                    let min_funds_parameter: Option<Amount> = read(
                        &self.ctx,
                        &min_funds_parameter_key,
                        ReadType::PRE,
                    )
                    .ok();
                    let pre_balance: Option<Amount> =
                        read(&self.ctx, &balance_key, ReadType::PRE).ok();
                    let post_balance: Option<Amount> =
                        read(&self.ctx, &balance_key, ReadType::POST).ok();
                    match (min_funds_parameter, pre_balance, post_balance) {
                        (
                            Some(min_funds_parameter),
                            Some(pre_balance),
                            Some(post_balance),
                        ) => {
                            post_balance > pre_balance
                                && post_balance - pre_balance
                                    >= min_funds_parameter
                        }
                        (
                            Some(min_funds_parameter),
                            None,
                            Some(post_balance),
                        ) => post_balance >= min_funds_parameter,
                        _ => false,
                    }
                }
                (KeyType::PARAMETER, _) => false,
                (KeyType::UNKNOWN_GOVERNANCE, _) => false,
                (KeyType::UNKNOWN, _) => true,
                _ => false,
            }
        });
        Ok(result)
    }
}

fn read<T, DB, H, CA>(
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

fn is_valid_key_set<DB, H, CA>(
    context: &Ctx<DB, H, CA>,
    keys: &BTreeSet<Key>,
) -> (bool, u64)
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    is_valid_proposal_init_key_set(context, keys)
}

fn is_valid_proposal_init_key_set<DB, H, CA>(
    context: &Ctx<DB, H, CA>,
    keys: &BTreeSet<Key>,
) -> (bool, u64)
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    let counter_key = gov_storage::get_counter_key();
    let pre_counter = match read(context, &counter_key, ReadType::PRE) {
        Ok(v) => v,
        Err(_) => return (false, 0),
    };

    let post_counter = match read(context, &counter_key, ReadType::POST) {
        Ok(v) => v,
        Err(_) => return (false, 0),
    };

    if post_counter < pre_counter {
        return (false, 0);
    }

    for counter in pre_counter..post_counter {
        // Construct the set of expected keys
        // NOTE: we don't check the existance of committing_epoch because it's
        // going to be checked later into the VP
        let mandatory_keys = BTreeSet::from([
            counter_key.clone(),
            gov_storage::get_content_key(counter),
            gov_storage::get_author_key(counter),
            gov_storage::get_funds_key(counter),
            gov_storage::get_voting_start_epoch_key(counter),
            gov_storage::get_voting_end_epoch_key(counter),
            gov_storage::get_grace_epoch_key(counter),
        ]);

        // Check that expected set is a subset the actual one
        if !keys.is_superset(&mandatory_keys) {
            return (false, 0);
        }
    }

    (true, post_counter - pre_counter)
}

fn is_delegator<DB, H, CA>(
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

fn is_valid_validator_voting_period(
    current_epoch: Epoch,
    voting_start_epoch: Epoch,
    voting_end_epoch: Epoch,
) -> bool {
    voting_start_epoch < voting_end_epoch
        && current_epoch * 3 <= voting_start_epoch + voting_end_epoch * 2
}

fn is_validator<DB, H, CA>(
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
        Some(set) => {
            set.active.iter().any(|weighted_validator| {
                weighted_validator.address.eq(address)
            }) && verifiers.contains(address)
                && delegation_address.eq(address)
        }
        None => false,
    }
}

#[allow(clippy::upper_case_acronyms)]
enum KeyType {
    #[allow(clippy::upper_case_acronyms)]
    COUNTER,
    #[allow(clippy::upper_case_acronyms)]
    VOTE,
    #[allow(clippy::upper_case_acronyms)]
    CONTENT,
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    PROPOSAL_CODE,
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    PROPOSAL_COMMIT,
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    GRACE_EPOCH,
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    START_EPOCH,
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    END_EPOCH,
    #[allow(clippy::upper_case_acronyms)]
    FUNDS,
    #[allow(clippy::upper_case_acronyms)]
    BALANCE,
    #[allow(clippy::upper_case_acronyms)]
    AUTHOR,
    #[allow(clippy::upper_case_acronyms)]
    PARAMETER,
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    UNKNOWN_GOVERNANCE,
    #[allow(clippy::upper_case_acronyms)]
    UNKNOWN,
}

impl From<&Key> for KeyType {
    fn from(value: &Key) -> Self {
        if gov_storage::is_vote_key(value) {
            KeyType::VOTE
        } else if gov_storage::is_content_key(value) {
            KeyType::CONTENT
        } else if gov_storage::is_proposal_code_key(value) {
            KeyType::PROPOSAL_CODE
        } else if gov_storage::is_grace_epoch_key(value) {
            KeyType::GRACE_EPOCH
        } else if gov_storage::is_start_epoch_key(value) {
            KeyType::START_EPOCH
        } else if gov_storage::is_min_grace_epoch_key(value) {
            KeyType::PROPOSAL_COMMIT
        } else if gov_storage::is_end_epoch_key(value) {
            KeyType::END_EPOCH
        } else if gov_storage::is_balance_key(value) {
            KeyType::FUNDS
        } else if gov_storage::is_author_key(value) {
            KeyType::AUTHOR
        } else if gov_storage::is_counter_key(value) {
            KeyType::COUNTER
        } else if gov_storage::is_parameter_key(value) {
            KeyType::PARAMETER
        } else if token_storage::is_balance_key(&m1t(), value).is_some() {
            KeyType::BALANCE
        } else if gov_storage::is_governance_key(value) {
            KeyType::UNKNOWN_GOVERNANCE
        } else {
            KeyType::UNKNOWN
        }
    }
}
#[allow(clippy::upper_case_acronyms)]
enum ReadType {
    #[allow(clippy::upper_case_acronyms)]
    PRE,
    #[allow(clippy::upper_case_acronyms)]
    POST,
}
