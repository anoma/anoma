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
        let owner_key = nft::get_creator_key(&nft.owner, &nft.owner);
        // write creator
        tx::write(&owner_key.to_string(), ());

        _mint_token(&address, &nft.owner, nft.tokens);

        address
    }

    pub fn mint_tokens(nft: MintNft) {
        let nft_creator_key = nft::get_creator_key(&nft.address, &nft.owner);
        let nft_exist = tx::has_key(&nft_creator_key.to_string());

        if !nft_exist {
            return;
        }

        // need to check signature for `creator` address
        let _creator: Option<String> = tx::read(&nft_creator_key.to_string());

        _mint_token(&nft.address, &nft.owner, nft.tokens);
    }

    fn _mint_token(
        nft_address: &Address,
        creator_address: &Address,
        tokens: Vec<NftToken>,
    ) {
        for token in tokens {
            let token_key = nft::get_token_approval_key(
                nft_address,
                &token.id.to_string(),
                creator_address,
            );
            let token_id_exist = tx::has_key(&token_key.to_string());
            if token_id_exist {
                continue;
            }

            // write token metadata
            let metadata_key =
                nft::get_token_metadata_key(nft_address, &token.id.to_string());
            tx::write(&metadata_key.to_string(), token.metadata);
            // write creator into approval
            let owner_approval_key = nft::get_token_approval_key(
                nft_address,
                &token.id.to_string(),
                creator_address,
            );
            tx::write(&owner_approval_key.to_string(), creator_address);
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
    }
}

/// A Nft validity predicate
pub mod vp {
    use std::collections::HashSet;

    use anoma::types::address::Address;
    pub use anoma::types::nft::*;
    use anoma::types::storage::Key;

    use crate::imports::{
        tx,
        vp::{self},
    };

    enum KeyType {
        Metadata(String),
        Approval(String),
        CurrentOwner(String),
        Creator(Address),
        Unknown,
    }

    // TODO: add vector of errors
    pub fn vp(
        nft_address: &Address,
        keys_changed: &HashSet<Key>,
        verifiers: &HashSet<Address>,
    ) -> bool {
        keys_changed.iter().all(|key| {
            match get_key_type(key, nft_address) {
                KeyType::Creator(_creator_addr) => {
                    tx::log_string(format!(
                        "nft vp, checking creator: {}",
                        _creator_addr
                    ));
                    let key = key.to_string();
                    // creator has changed
                    if vp::has_key_pre(&key) && vp::has_key_post(&key) {
                        false
                    // creator has not changed
                    } else if vp::has_key_pre(&key) && !vp::has_key_post(&key) {
                        true
                    // created new nft
                    } else {
                        !vp::has_key_pre(&key) && vp::has_key_post(&key)
                    }
                }
                KeyType::Metadata(_token_id) => {
                    tx::log_string(format!(
                        "nft vp, checking metadata with token id: {}",
                        _token_id
                    ));
                    for verifier in verifiers {
                        let creator_key =
                            get_creator_key(nft_address, verifier);
                        if vp::has_key_pre(&creator_key.to_string()) {
                            return true;
                        }
                    }
                    false
                }
                KeyType::Approval(token_id) => {
                    tx::log_string(format!(
                        "nft vp, checking approvals with token id: {}",
                        token_id
                    ));
                    is_approved(nft_address, token_id.as_ref(), verifiers)
                }
                KeyType::CurrentOwner(token_id) => {
                    tx::log_string(format!(
                        "nft vp, checking current_owner with token id: {}",
                        token_id
                    ));
                    let key = key.to_string();
                    let past_owners_key = get_token_past_owners_key(
                        nft_address,
                        token_id.as_ref(),
                    )
                    .to_string();

                    let prev_past_owners: Vec<Address> =
                        vp::read_pre(&past_owners_key).unwrap_or_default();
                    let post_past_owners: Vec<Address> =
                        vp::read_post(&past_owners_key).unwrap_or_default();

                    // if there was a previous owner
                    if vp::has_key_pre(key) {
                        // past owners should be -1 current
                        if prev_past_owners.len() != post_past_owners.len() - 1
                        {
                            return false;
                        }
                        // check that the diff owner was the past owner
                        for owner in post_past_owners {
                            if !prev_past_owners.contains(&owner) {
                                let pre_owner_key = get_token_current_owner_key(
                                    nft_address,
                                    token_id.as_ref(),
                                    &owner,
                                );
                                return vp::has_key_pre(
                                    pre_owner_key.to_string(),
                                );
                            }
                        }
                        false
                    } else {
                        is_approved(nft_address, token_id.as_ref(), verifiers)
                            && post_past_owners.is_empty()
                    }
                }
                KeyType::Unknown => true,
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

    fn get_key_type(key: &Key, nft_address: &Address) -> KeyType {
        let is_creator_key = is_nft_creator_key(key, nft_address);
        let is_metadata_key = is_nft_metadata_key(key, nft_address);
        let is_approval_key = is_nft_approval_key(key, nft_address);
        let is_current_owner_key = is_nft_current_owner_key(key, nft_address);
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
        KeyType::Unknown
    }
}
