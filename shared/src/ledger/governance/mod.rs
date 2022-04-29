//! Governance VP

/// governance parameters
pub mod parameters;
/// governance storage
pub mod storage;
/// utility function
pub mod utils;
/// vp functions
pub mod vp;

use std::collections::BTreeSet;

/// Governance functions result
pub use vp::Result;

use self::storage as gov_storage;
use crate::ledger::native_vp::{Ctx, NativeVp};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::address::{xan as m1t, Address, InternalAddress};
use crate::types::storage::Key;
use crate::types::token as token_storage;
use crate::vm::WasmCacheAccess;

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
    type Error = vp::Error;

    const ADDR: InternalAddress = InternalAddress::Governance;

    fn validate_tx(
        &self,
        tx_data: &[u8],
        keys_changed: &BTreeSet<Key>,
        verifiers: &BTreeSet<Address>,
    ) -> Result<bool> {
        let (is_valid_keys_set, set_count) =
            is_valid_key_set(&self.ctx, keys_changed);
        if !is_valid_keys_set {
            return Ok(false);
        };

        let result = keys_changed.iter().all(|key| {
            let proposal_id = gov_storage::get_proposal_id(key);

            let key_type: KeyType<DB, H, CA> = key.into();
            match (key_type, proposal_id) {
                (KeyType::VOTE(validate), Some(proposal_id)) => {
                    validate(&self.ctx, proposal_id, key, verifiers)
                }
                (KeyType::CONTENT(validate), Some(proposal_id)) => {
                    validate(&self.ctx, proposal_id)
                }
                (KeyType::PROPOSAL_CODE(validate), Some(proposal_id)) => {
                    validate(&self.ctx, proposal_id)
                }
                (KeyType::GRACE_EPOCH(validate), Some(proposal_id)) => {
                    validate(&self.ctx, proposal_id)
                }
                (KeyType::START_EPOCH(validate), Some(proposal_id)) => {
                    validate(&self.ctx, proposal_id)
                }
                (KeyType::END_EPOCH(validate), Some(proposal_id)) => {
                    validate(&self.ctx, proposal_id)
                }
                (KeyType::FUNDS(validate), Some(proposal_id)) => {
                    validate(&self.ctx, proposal_id)
                }
                (KeyType::AUTHOR(validate), Some(proposal_id)) => {
                    validate(&self.ctx, proposal_id, verifiers)
                }
                (KeyType::COUNTER(validate), _) => {
                    validate(&self.ctx, set_count)
                }
                (KeyType::PROPOSAL_COMMIT(validate), _) => validate(&self.ctx),
                (KeyType::BALANCE(validate), _) => validate(&self.ctx),
                (KeyType::PARAMETER(validate), _) => {
                    validate(&self.ctx, tx_data)
                }
                (KeyType::UNKNOWN_GOVERNANCE(validate), _) => validate(),
                (KeyType::UNKNOWN(validate), _) => validate(),
                _ => false,
            }
        });
        Ok(result)
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
    let pre_counter = match vp::read(context, &counter_key, vp::ReadType::PRE) {
        Ok(v) => v,
        Err(_) => return (false, 0),
    };

    let post_counter = match vp::read(context, &counter_key, vp::ReadType::POST)
    {
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

#[allow(clippy::upper_case_acronyms)]
enum KeyType<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    #[allow(clippy::upper_case_acronyms)]
    COUNTER(fn(&Ctx<'a, DB, H, CA>, u64) -> bool),
    #[allow(clippy::type_complexity)]
    #[allow(clippy::upper_case_acronyms)]
    VOTE(fn(&Ctx<'a, DB, H, CA>, u64, &Key, &BTreeSet<Address>) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    CONTENT(fn(&Ctx<'a, DB, H, CA>, u64) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    PROPOSAL_CODE(fn(&Ctx<'a, DB, H, CA>, u64) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    PROPOSAL_COMMIT(fn(&Ctx<'a, DB, H, CA>) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    GRACE_EPOCH(fn(&Ctx<'a, DB, H, CA>, u64) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    START_EPOCH(fn(&Ctx<'a, DB, H, CA>, u64) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    END_EPOCH(fn(&Ctx<'a, DB, H, CA>, u64) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    FUNDS(fn(&Ctx<'a, DB, H, CA>, u64) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    BALANCE(fn(&Ctx<'a, DB, H, CA>) -> bool),
    #[allow(clippy::type_complexity)]
    #[allow(clippy::upper_case_acronyms)]
    AUTHOR(fn(&Ctx<'a, DB, H, CA>, u64, &BTreeSet<Address>) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    PARAMETER(fn(&Ctx<'a, DB, H, CA>, &[u8]) -> bool),
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    UNKNOWN_GOVERNANCE(fn() -> bool),
    #[allow(clippy::upper_case_acronyms)]
    UNKNOWN(fn() -> bool),
}

impl<'a, DB, H, CA> From<&Key> for KeyType<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    fn from(value: &Key) -> Self {
        if gov_storage::is_vote_key(value) {
            KeyType::VOTE(vp::validate_vote_key)
        } else if gov_storage::is_content_key(value) {
            KeyType::CONTENT(vp::validate_content_key)
        } else if gov_storage::is_proposal_code_key(value) {
            KeyType::PROPOSAL_CODE(vp::validate_proposal_code_key)
        } else if gov_storage::is_grace_epoch_key(value) {
            KeyType::GRACE_EPOCH(vp::validate_grace_epoch_key)
        } else if gov_storage::is_start_epoch_key(value) {
            KeyType::START_EPOCH(vp::validate_start_epoch_key)
        } else if gov_storage::is_commit_proposal_key(value) {
            KeyType::PROPOSAL_COMMIT(vp::validate_commit_key)
        } else if gov_storage::is_end_epoch_key(value) {
            KeyType::END_EPOCH(vp::validate_end_epoch_key)
        } else if gov_storage::is_balance_key(value) {
            KeyType::FUNDS(vp::validate_funds_key)
        } else if gov_storage::is_author_key(value) {
            KeyType::AUTHOR(vp::validate_author_key)
        } else if gov_storage::is_counter_key(value) {
            KeyType::COUNTER(vp::validate_counter_key)
        } else if gov_storage::is_parameter_key(value) {
            KeyType::PARAMETER(vp::validate_parameter_key)
        } else if token_storage::is_balance_key(&m1t(), value).is_some() {
            KeyType::BALANCE(vp::validate_balance_key)
        } else if gov_storage::is_governance_key(value) {
            KeyType::UNKNOWN_GOVERNANCE(vp::validate_unknown_governance_key)
        } else {
            KeyType::UNKNOWN(vp::validate_unknown_key)
        }
    }
}
