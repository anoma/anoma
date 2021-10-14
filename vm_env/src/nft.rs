use anoma::types::nft;

/// Tx imports and functions.
pub mod tx {
    use anoma::types::address::Address;
    use anoma::types::nft::NftToken;
    use anoma::types::transaction::{CreateNft, MintNft};

    use super::*;
    use crate::imports::tx;
    pub fn init_nft(nft: CreateNft) -> Address {
        let address = tx::init_account(&nft.vp_code);
        let owner_key = nft::get_creator_key(&address, &nft.owner);
        // write creator
        tx::write(&owner_key.to_string(), ());

        // mint tokens
        aux_mint_token(&address, &nft.owner, nft.tokens, &nft.owner);

        address
    }

    pub fn mint_tokens(nft: MintNft) {
        aux_mint_token(&nft.address, &nft.owner, nft.tokens, &nft.verifier);
    }

    fn aux_mint_token(
        nft_address: &Address,
        creator_address: &Address,
        tokens: Vec<NftToken>,
        verifier: &Address,
    ) {
        for token in tokens {
            // write token metadata
            let metadata_key = nft::get_token_metadata_key(
                nft_address,
                &token.id.to_string(),
                &token.metadata.to_string(),
            );
            tx::write(&metadata_key.to_string(), ());

            // write current owner token as creator
            let current_owner_key = nft::get_token_current_owner_key(
                nft_address,
                &token.id.to_string(),
                creator_address,
            );
            tx::write(&current_owner_key.to_string(), ());

            // write burnt propriety
            let burnt_key =
                nft::get_token_burnt_key(nft_address, &token.id.to_string());
            tx::write(&burnt_key.to_string(), 0);

            // write each approved address
            for addr in token.approvals {
                let approval_key = nft::get_token_approval_key(
                    nft_address,
                    &token.id.to_string(),
                    &addr,
                );
                tx::write(&approval_key.to_string(), ());
            }
        }
        tx::insert_verifier(verifier);
    }
}

/// A Nft validity predicate
pub mod vp {
    use std::collections::HashSet;

    use anoma::types::address::Address;
    pub use anoma::types::nft::*;
    use anoma::types::storage::Key;

    use crate::imports::vp::{self};

    enum KeyType {
        Metadata(String),
        Approval(String),
        CurrentOwner(String),
        Creator(Address),
        PastOwners(String),
        Unknown,
    }

    pub fn vp(
        _tx_da_ta: Vec<u8>,
        nft_address: &Address,
        keys_changed: &HashSet<Key>,
        verifiers: &HashSet<Address>,
    ) -> bool {
        keys_changed.iter().all(|key| {
            match get_key_type(key, nft_address) {
                KeyType::Creator(_creator_addr) => {
                    vp::log_string("creator cannot be changed.".to_string());
                    false
                }
                KeyType::Approval(token_id) => {
                    vp::log_string(format!(
                        "nft vp, checking approvals with token id: {}",
                        token_id
                    ));

                    is_creator(nft_address, verifiers)
                        || is_approved(
                            nft_address,
                            token_id.as_ref(),
                            verifiers,
                        )
                }
                KeyType::Metadata(token_id) => {
                    vp::log_string(format!(
                        "nft vp, checking if metadata changed: {}",
                        token_id
                    ));
                    is_creator(nft_address, verifiers)
                }
                KeyType::CurrentOwner(token_id) => {
                    vp::log_string(format!(
                        "nft vp, checking current_owner with token id: {}",
                        token_id
                    ));
                    let past_owners_key = get_token_past_owners_key(
                        nft_address,
                        token_id.as_ref(),
                    )
                    .to_string();

                    let prev_past_owners: Vec<Address> =
                        vp::read_pre(&past_owners_key).unwrap_or_default();
                    let post_past_owners: Vec<Address> =
                        vp::read_post(&past_owners_key).unwrap_or_default();

                    // no prev owners
                    if prev_past_owners.is_empty() {
                        // this is the first trasfer from creator
                        // (initial owner)
                        return is_creator(nft_address, verifiers);
                    } else if post_past_owners.is_empty() {
                        // is this is empty (but prev_past_owners) is
                        // not a valid condition
                        return false;
                    }

                    let mut prev_owner = None;
                    for past_owner in post_past_owners {
                        if !prev_past_owners.contains(&past_owner) {
                            if prev_owner.is_none() {
                                prev_owner = Some(past_owner);
                            } else {
                                return false;
                            }
                        }
                    }

                    // check if the prev_owner or an approval signed the
                    // tx
                    if let Some(owner) = prev_owner {
                        let check_key = get_token_current_owner_key(
                            nft_address,
                            &token_id,
                            &owner,
                        );
                        return vp::has_key_pre(check_key.to_string())
                            || is_approved(
                                nft_address,
                                token_id.as_ref(),
                                verifiers,
                            );
                    }
                    false
                }
                _ => is_creator(nft_address, verifiers),
            }
        })
    }

    fn is_approved(
        nft_address: &Address,
        nft_token_id: &str,
        verifiers: &HashSet<Address>,
    ) -> bool {
        for verifier in verifiers {
            let approval_key =
                get_token_approval_key(nft_address, nft_token_id, verifier)
                    .to_string();
            if vp::has_key_pre(approval_key) {
                return true;
            }
        }
        false
    }

    fn is_creator(nft_address: &Address, verifiers: &HashSet<Address>) -> bool {
        vp::log_string(format!("nft_address: {:#?}", nft_address));
        for verifier in verifiers {
            let creator_key =
                get_creator_key(nft_address, verifier).to_string();
            // vp::log_string(format!("creator_key: {:#?}", creator_key));
            vp::log_string(format!("creator_key: {:#?}", creator_key));
            if vp::has_key_pre(creator_key) {
                return true;
            }
        }
        false
    }

    fn get_key_type(key: &Key, nft_address: &Address) -> KeyType {
        let is_creator_key = is_nft_creator_key(key, nft_address);
        let is_metadata_key = is_nft_metadata_key(key, nft_address);
        let is_approval_key = is_nft_approval_key(key, nft_address);
        let is_current_owner_key = is_nft_current_owner_key(key, nft_address);
        let is_past_owner_key = is_nft_past_owners_key(key, nft_address);
        if let Some(addr) = is_creator_key {
            return KeyType::Creator(addr);
        }
        if let Some(token_id) = is_metadata_key {
            return KeyType::Metadata(token_id);
        }
        if let Some(token_id) = is_approval_key {
            return KeyType::Approval(token_id);
        }
        if let Some(token_id) = is_current_owner_key {
            return KeyType::CurrentOwner(token_id);
        }
        if let Some(token_id) = is_past_owner_key {
            return KeyType::PastOwners(token_id);
        }
        KeyType::Unknown
    }
}
