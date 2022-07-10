//! Treasury VP

use std::collections::BTreeSet;
/// treasury parameters
pub mod parameters;
/// treasury storage
pub mod storage;

use borsh::BorshDeserialize;
use thiserror::Error;

use self::storage as treasury_storage;
use super::governance::vp::is_proposal_accepted;
use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::address::{xan as nam, Address, InternalAddress};
use crate::types::storage::Key;
use crate::types::token;
use crate::vm::WasmCacheAccess;

/// Internal treasury address
pub const ADDRESS: Address = Address::Internal(InternalAddress::Treasury);

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// Treasury functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Treasury VP
pub struct TreasuryVp<'a, DB, H, CA>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H, CA>,
}

impl<'a, DB, H, CA> NativeVp for TreasuryVp<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::Treasury;

    fn validate_tx(
        &self,
        tx_data: &[u8],
        keys_changed: &BTreeSet<Key>,
        _verifiers: &BTreeSet<Address>,
    ) -> Result<bool> {
        let result = keys_changed.iter().all(|key| {
            let key_type: KeyType = key.into();
            match key_type {
                KeyType::PARAMETER => {
                    let proposal_id = u64::try_from_slice(tx_data).ok();
                    match proposal_id {
                        Some(id) => is_proposal_accepted(&self.ctx, id),
                        _ => false,
                    }
                }
                KeyType::BALANCE(addr) => {
                    let proposal_id = u64::try_from_slice(tx_data).ok();
                    if let Some(id) = proposal_id {
                        if !is_proposal_accepted(&self.ctx, id) {
                            return false;
                        }
                    } else {
                        return false;
                    };
                    let is_max_funds_transfer_key =
                        treasury_storage::get_max_transferable_fund_key();
                    let balance_key = token::balance_key(&nam(), &ADDRESS);
                    let max_transfer_amount =
                        self.ctx.read_pre(&is_max_funds_transfer_key);
                    let pre_balance = self.ctx.read_pre(&balance_key);
                    let post_balance = self.ctx.read_post(&balance_key);
                    if addr.ne(&ADDRESS) {
                        return true;
                    }
                    match (max_transfer_amount, pre_balance, post_balance) {
                        (
                            Ok(max_transfer_amount),
                            Ok(pre_balance),
                            Ok(post_balance),
                        ) => {
                            match (
                                max_transfer_amount,
                                pre_balance,
                                post_balance,
                            ) {
                                (
                                    Some(max_transfer_amount),
                                    Some(pre_balance),
                                    Some(post_balance),
                                ) => {
                                    let max_transfer_amount =
                                        token::Amount::try_from_slice(
                                            &max_transfer_amount[..],
                                        )
                                        .ok();
                                    let pre_balance =
                                        token::Amount::try_from_slice(
                                            &pre_balance[..],
                                        )
                                        .ok();
                                    let post_balance =
                                        token::Amount::try_from_slice(
                                            &post_balance[..],
                                        )
                                        .ok();
                                    match (
                                        max_transfer_amount,
                                        pre_balance,
                                        post_balance,
                                    ) {
                                        (
                                            Some(max_transfer_amount),
                                            Some(pre_balance),
                                            Some(post_balance),
                                        ) => {
                                            post_balance > pre_balance
                                                || (pre_balance - post_balance
                                                    <= max_transfer_amount)
                                        }
                                        _ => false,
                                    }
                                }
                                _ => false,
                            }
                        }
                        _ => false,
                    }
                }
                KeyType::UNKNOWN_TREASURY => false,
                KeyType::UNKNOWN => true,
            }
        });
        Ok(result)
    }
}

#[allow(clippy::upper_case_acronyms)]
enum KeyType {
    #[allow(clippy::upper_case_acronyms)]
    BALANCE(Address),
    #[allow(clippy::upper_case_acronyms)]
    PARAMETER,
    #[allow(clippy::upper_case_acronyms)]
    #[allow(non_camel_case_types)]
    UNKNOWN_TREASURY,
    #[allow(clippy::upper_case_acronyms)]
    UNKNOWN,
}

impl From<&Key> for KeyType {
    fn from(value: &Key) -> Self {
        if treasury_storage::is_parameter_key(value) {
            KeyType::PARAMETER
        } else if treasury_storage::is_treasury_key(value) {
            KeyType::UNKNOWN_TREASURY
        } else if token::is_any_token_balance_key(value).is_some() {
            match token::is_balance_key(&nam(), value) {
                Some(addr) => KeyType::BALANCE(addr.clone()),
                None => KeyType::UNKNOWN,
            }
        } else {
            KeyType::UNKNOWN
        }
    }
}
