//! NFT validity predicate

use std::collections::BTreeSet;

use anoma::ledger::native_vp::VpEnv;
use anoma::types::address::Address;
pub use anoma::types::nft::*;
use anoma::types::storage::Key;

use super::{accept, reject, Ctx, EnvResult, VpResult};

enum KeyType {
    Metadata(Address, String),
    Approval(Address, String),
    CurrentOwner(Address, String),
    Creator(Address),
    PastOwners(Address, String),
    Unknown,
}

pub fn vp(
    ctx: &Ctx,
    _tx_da_ta: Vec<u8>,
    nft_address: &Address,
    keys_changed: &BTreeSet<Key>,
    verifiers: &BTreeSet<Address>,
) -> VpResult {
    for key in keys_changed {
        match get_key_type(key, nft_address) {
            KeyType::Creator(_creator_addr) => {
                super::log_string("creator cannot be changed.");
                return reject();
            }
            KeyType::Approval(nft_address, token_id) => {
                super::log_string(format!(
                    "nft vp, checking approvals with token id: {}",
                    token_id
                ));

                if !(is_creator(ctx, &nft_address, verifiers)?
                    || is_approved(
                        ctx,
                        &nft_address,
                        token_id.as_ref(),
                        verifiers,
                    )?)
                {
                    return reject();
                }
            }
            KeyType::Metadata(nft_address, token_id) => {
                super::log_string(format!(
                    "nft vp, checking if metadata changed: {}",
                    token_id
                ));
                if !is_creator(ctx, &nft_address, verifiers)? {
                    return reject();
                }
            }
            _ => {
                if !is_creator(ctx, nft_address, verifiers)? {
                    return reject();
                }
            }
        }
    }
    accept()
}

fn is_approved(
    ctx: &Ctx,
    nft_address: &Address,
    nft_token_id: &str,
    verifiers: &BTreeSet<Address>,
) -> EnvResult<bool> {
    let approvals_key = get_token_approval_key(nft_address, nft_token_id);
    let approval_addresses: Vec<Address> =
        ctx.read_pre(&approvals_key)?.unwrap_or_default();
    return Ok(approval_addresses
        .iter()
        .any(|addr| verifiers.contains(addr)));
}

fn is_creator(
    ctx: &Ctx,
    nft_address: &Address,
    verifiers: &BTreeSet<Address>,
) -> EnvResult<bool> {
    let creator_key = get_creator_key(nft_address);
    let creator_address: Address = ctx.read_pre(&creator_key)?.unwrap();
    Ok(verifiers.contains(&creator_address))
}

fn get_key_type(key: &Key, nft_address: &Address) -> KeyType {
    let is_creator_key = is_nft_creator_key(key, nft_address);
    let is_metadata_key = is_nft_metadata_key(key, nft_address);
    let is_approval_key = is_nft_approval_key(key, nft_address);
    let is_current_owner_key = is_nft_current_owner_key(key, nft_address);
    let is_past_owner_key = is_nft_past_owners_key(key, nft_address);
    if let Some(nft_address) = is_creator_key {
        return KeyType::Creator(nft_address);
    }
    if let Some((nft_address, token_id)) = is_metadata_key {
        return KeyType::Metadata(nft_address, token_id);
    }
    if let Some((nft_address, token_id)) = is_approval_key {
        return KeyType::Approval(nft_address, token_id);
    }
    if let Some((nft_address, token_id)) = is_current_owner_key {
        return KeyType::CurrentOwner(nft_address, token_id);
    }
    if let Some((nft_address, token_id)) = is_past_owner_key {
        return KeyType::PastOwners(nft_address, token_id);
    }
    KeyType::Unknown
}
