//! Protocol parameters

pub mod storage;
pub mod parameters;

use std::collections::HashSet;
use thiserror::Error;

use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::types::{self, encode};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{Key, DbKeySeg};
use crate::vm::WasmCacheAccess;

use borsh::BorshDeserialize;
use self::storage as gov_storage;

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
        keys_changed: &HashSet<Key>,
        verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        let result = keys_changed.iter().all(|key| {
            let proposal_id = get_id(key);

            let key_type: KeyType = key.into();
            match (key_type, proposal_id)  {
                (KeyType::VOTE, Some(proposal_id)) => {
                    return false;
                },
                (KeyType::CONTENT, Some(proposal_id)) => {
                    let content_key: Key = gov_storage::get_content_key(proposal_id);
                    let max_content_length_parameter_key = gov_storage::get_max_proposal_content_key();
                    let max_content_length = read(&self.ctx, &max_content_length_parameter_key, ReadType::PRE).ok();
                    let has_pre_content = self.ctx.has_key_pre(&content_key).ok();
                    let post_content: Option<String> = read(&self.ctx, &content_key, ReadType::POST).ok();
                    match (has_pre_content, post_content, max_content_length) {
                        (Some(has_pre_content), Some(post_content), Some(max_content_length)) => {
                            return !has_pre_content && post_content.len() < max_content_length
                        },
                        _ => return false
                    }
                },
                (KeyType::PROPOSAL_CODE, Some(proposal_id)) => {
                    let proposal_code_key = gov_storage::get_proposal_code_key(proposal_id);
                    let max_proposal_code_size_parameter_key = gov_storage::get_max_proposal_code_size_key();
                    let max_proposal_code_size: Option<usize> = read(&self.ctx, &max_proposal_code_size_parameter_key, ReadType::PRE).ok();
                    let has_pre_proposal_code = self.ctx.has_key_pre(&proposal_code_key).ok();
                    let post_proposal_code: Option<Vec<u8>> = read(&self.ctx, &proposal_code_key, ReadType::POST).ok();
                    match (has_pre_proposal_code, post_proposal_code, max_proposal_code_size) {
                        (Some(has_pre_proposal_code), Some(post_proposal_code), Some(max_proposal_code_size)) => {
                            return !has_pre_proposal_code && post_proposal_code.len() < max_proposal_code_size
                        },
                        _ => return false
                    }
                },
                (KeyType::GRACE_EPOCH, Some(proposal_id)) => {
                    let end_epoch_key = gov_storage::get_voting_end_epoch_key(proposal_id);
                    let grace_epoch_key = gov_storage::get_voting_grace_epoch_key(proposal_id);
                    let end_epoch: Option<u64> = read(&self.ctx, &end_epoch_key, ReadType::POST).ok();
                    let grace_epoch: Option<u64> = read(&self.ctx, &grace_epoch_key, ReadType::POST).ok();
                    let has_pre_grace_epoch = self.ctx.has_key_pre(&grace_epoch_key).ok();
                    match (has_pre_grace_epoch, grace_epoch, end_epoch) {
                        (Some(has_pre_grace_epoch), Some(grace_epoch), Some(end_epoch)) => return !has_pre_grace_epoch && end_epoch < grace_epoch,
                        _ => return false
                    }
                },
                (KeyType::START_EPOCH | KeyType::END_EPOCH, Some(proposal_id)) => {
                    let start_epoch_key = gov_storage::get_voting_start_epoch_key(proposal_id);
                    let end_epoch_key = gov_storage::get_voting_end_epoch_key(proposal_id);
                    let start_epoch: Option<u64> = read(&self.ctx, &start_epoch_key, ReadType::PRE).ok();
                    let end_epoch: Option<u64> = read(&self.ctx, &end_epoch_key, ReadType::PRE).ok();
                    let current_epoch = self.ctx.get_block_epoch().ok();
                    let min_period_parameter_key = gov_storage::get_min_proposal_period_key();
                    let min_period: Option<u64> = read(&self.ctx, &min_period_parameter_key, ReadType::PRE).ok();
                    let has_pre_start_epoch = self.ctx.has_key_pre(&start_epoch_key).ok();
                    let has_pre_end_epoch = self.ctx.has_key_pre(&end_epoch_key).ok();
                    match (has_pre_start_epoch, has_pre_end_epoch, min_period, start_epoch, end_epoch, current_epoch) {
                        (Some(has_pre_start_epoch), Some(has_pre_end_epoch), Some(min_period), Some(start_epoch), Some(end_epoch), Some(current_epoch)) => {
                            let current_epoch = current_epoch.0;
                            return !has_pre_start_epoch && !has_pre_end_epoch && start_epoch < end_epoch && start_epoch - end_epoch % min_period == 0 && start_epoch - current_epoch >= min_period
                        },
                        _ => return false
                    }
                },
                (KeyType::FUNDS, Some(proposal_id)) => {
                    let funds_key = gov_storage::get_funds_key(proposal_id);
                    let min_funds_parameter_key = gov_storage::get_min_proposal_fund_key();
                    let pre_funds: Option<u64> = read(&self.ctx, &funds_key, ReadType::PRE).ok();
                    let post_funds: Option<u64> = read(&self.ctx, &funds_key, ReadType::POST).ok();
                    let min_funds_parameter: Option<u64> = read(&self.ctx, &min_funds_parameter_key, ReadType::PRE).ok();
                    if !pre_funds.is_none() {
                        match (pre_funds, post_funds, min_funds_parameter) {
                            (Some(pre_funds), Some(post_funds), Some(min_funds_parameter)) => {
                                return post_funds - pre_funds >= min_funds_parameter
                            },
                            _ => return false
                        }
                    } else {
                        let current_epoch = self.ctx.get_block_epoch().ok();
                        let end_epoch_key = gov_storage::get_voting_end_epoch_key(proposal_id);
                        let end_epoch = read(&self.ctx, &end_epoch_key, ReadType::PRE).ok();
                        match (current_epoch, end_epoch) {
                            (Some(current_epoch) , Some(end_epoch)) => {
                                return current_epoch >= end_epoch && is_allowed_transfer(verifiers)
                            },
                            _ => return false
                        }
                    }
                },
                (KeyType::AUTHOR, Some(proposal_id)) => {
                    let author_key = gov_storage::get_author_key(proposal_id);
                    let author = read(&self.ctx, &author_key, ReadType::POST).ok();
                    let has_pre_author = self.ctx.has_key_pre(&author_key).ok();
                    match (has_pre_author, author) {
                        (Some(has_pre_author), Some(author)) => return has_pre_author && verifiers.contains(&author),
                        _ => return false
                    }
                },
                (KeyType::COUNTER, _) => {
                    let counter_key = gov_storage::get_counter_key();
                    let pre_counter: Option<u64> = read(&self.ctx, &counter_key, ReadType::PRE).ok();
                    let post_counter: Option<u64> = read(&self.ctx, &counter_key, ReadType::POST).ok();
                    match (pre_counter, post_counter) {
                        (Some(pre_counter), Some(post_counter)) => {
                            return pre_counter + 1 == post_counter
                        }
                        _ => return false
                    }
                },
                // KeyType::UNKNOWN | KeyType::PARAMETER => {
                //     return false
                // },
                _ => {
                    return true
                }
            }
        });
        Ok(result)
        // if proposal keys are valid, we still need to check for mandatory rules:
        // - counter is incremented
        // - funds have been locked
        // if result && !ids.is_empty() {
        //     let counter_key = gov_storage::get_counter_key();
        //     let pre_counter: Option<u64> = read(&self.ctx, &counter_key, ReadType::PRE).ok();
        //     let post_counter: Option<u64> = read(&self.ctx, &counter_key, ReadType::POST).ok();

        //     // we checked at the beginning if the array is empty
        //     let proposal_id = ids.get(0).unwrap();

        //     if let (Some(pre_counter), Some(post_counter)) = (pre_counter, post_counter) {
        //         if !(pre_counter + 1 == post_counter && ids.iter().all(|id| id.eq(proposal_id)) && *proposal_id == pre_counter) {
        //             return Ok(false)
        //         }
        //     } else {
        //         return Ok(false)
        //     }

        //     let funds_key = gov_storage::get_funds_key(*proposal_id);
        //     let min_funds_parameter_key = gov_storage::get_min_proposal_fund_key();
        //     let post_funds: Option<u64> = read(&self.ctx, &funds_key, ReadType::POST).ok();
        //     let min_funds: Option<u64> = read(&self.ctx, &min_funds_parameter_key, ReadType::PRE).ok();
            
        //     if let (Some(min_funds), Some(post_funds)) = (min_funds, post_funds) {
        //         if !(post_funds > min_funds) {
        //             return Ok(false)
        //         }
        //     } else {
        //         return Ok(false)
        //     }

        //     return Ok(true)
        // } else {
        //     return Ok(false)
        // }
    }
}

fn get_id(key: &Key) -> Option<u64> {
    match key.get_at(2) {
        Some(id) => match id {
            DbKeySeg::AddressSeg(_) => None,
            DbKeySeg::StringSeg(res) => res.parse::<u64>().ok(),
        }
        None => None
    }
}

fn read<T, DB, H, CA>(context: &Ctx<DB, H, CA>, key: &Key, read_type: ReadType) -> Result<T>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
    T: Clone + BorshDeserialize
{
    let storage_result = match read_type {
        ReadType::PRE => context.read_pre(key),
        ReadType::POST => context.read_post(key)
    };

    match storage_result {
        Ok(value) => match value {
            Some(bytes) => T::try_from_slice(&bytes).map_err(Error::NativeVpDeserializationError),
            None => Err(Error::NativeVpNonExistingKeyError(key.to_string())),
        }
        Err(err) => Err(Error::NativeVpError(err)),
    }
}

fn is_allowed_transfer(verifiers: &HashSet<Address>) -> bool {
    // TODO: implement
    return true;
}

enum KeyType {
    COUNTER,
    VOTE,
    CONTENT,
    #[allow(non_camel_case_types)]
    PROPOSAL_CODE,
    #[allow(non_camel_case_types)]
    GRACE_EPOCH,
    #[allow(non_camel_case_types)]
    START_EPOCH,
    #[allow(non_camel_case_types)]
    END_EPOCH,
    FUNDS,
    AUTHOR,
    PARAMETER,
    UNKNOWN
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
        } else {
            KeyType::UNKNOWN
        }
    }
}

enum ReadType {
    PRE,
    POST
}