use anoma::types::nft;

/// Tx imports and functions.
pub mod tx {
    use anoma::types::address::Address;
    use anoma::types::nft::NftToken;
    use anoma::types::transaction::nft::{CreateNft, MintNft};

    use super::*;
    use crate::imports::tx;
    pub fn init_nft(nft: CreateNft) -> Address {
        let address = tx::init_account(&nft.vp_code);

        // write tag
        let tag_key = nft::get_tag_key(&address);
        tx::write(&tag_key.to_string(), &nft.tag);

        // write creator
        let creator_key = nft::get_creator_key(&address);
        tx::write(&creator_key.to_string(), &nft.creator);

        // write keys
        let keys_key = nft::get_keys_key(&address);
        tx::write(&keys_key.to_string(), &nft.keys);

        // write optional keys
        let optional_keys_key = nft::get_optional_keys_key(&address);
        tx::write(&optional_keys_key.to_string(), nft.opt_keys);

        // mint tokens
        aux_mint_token(&address, &nft.creator, nft.tokens, &nft.creator);

        tx::insert_verifier(&nft.creator);

        address
    }

    pub fn mint_tokens(nft: MintNft) {
        aux_mint_token(&nft.address, &nft.creator, nft.tokens, &nft.creator);
    }

    fn aux_mint_token(
        nft_address: &Address,
        creator_address: &Address,
        tokens: Vec<NftToken>,
        verifier: &Address,
    ) {
        for token in tokens {
            // write token metadata
            let metadata_key =
                nft::get_token_metadata_key(nft_address, &token.id.to_string());
            tx::write(&metadata_key.to_string(), &token.metadata);

            // write current owner token as creator
            let current_owner_key = nft::get_token_current_owner_key(
                nft_address,
                &token.id.to_string(),
            );
            tx::write(
                &current_owner_key.to_string(),
                &token
                    .current_owner
                    .unwrap_or_else(|| creator_address.clone()),
            );

            // write value key
            let value_key =
                nft::get_token_value_key(nft_address, &token.id.to_string());
            tx::write(&value_key.to_string(), &token.values);

            // write optional value keys
            let optional_value_key = nft::get_token_optional_value_key(
                nft_address,
                &token.id.to_string(),
            );
            tx::write(&optional_value_key.to_string(), &token.opt_values);

            // write approval addresses
            let approval_key =
                nft::get_token_approval_key(nft_address, &token.id.to_string());
            tx::write(&approval_key.to_string(), &token.approvals);

            // write burnt propriety
            let burnt_key =
                nft::get_token_burnt_key(nft_address, &token.id.to_string());
            tx::write(&burnt_key.to_string(), token.burnt);
        }
        tx::insert_verifier(verifier);
    }
}

/// A Nft validity predicate
pub mod vp {
    use std::collections::BTreeSet;

    use anoma::types::address::Address;
    pub use anoma::types::nft::*;
    use anoma::types::storage::Key;

    use crate::imports::vp;

    enum KeyType {
        Metadata(Address, String),
        Approval(Address, String),
        CurrentOwner(Address, String),
        Creator(Address),
        PastOwners(Address, String),
        Unknown,
    }

    pub fn vp(
        _tx_da_ta: Vec<u8>,
        nft_address: &Address,
        keys_changed: &BTreeSet<Key>,
        verifiers: &BTreeSet<Address>,
    ) -> bool {
        keys_changed
            .iter()
            .all(|key| match get_key_type(key, nft_address) {
                KeyType::Creator(_creator_addr) => {
                    vp::log_string("creator cannot be changed.");
                    false
                }
                KeyType::Approval(nft_address, token_id) => {
                    vp::log_string(format!(
                        "nft vp, checking approvals with token id: {}",
                        token_id
                    ));

                    is_creator(&nft_address, verifiers)
                        || is_approved(
                            &nft_address,
                            token_id.as_ref(),
                            verifiers,
                        )
                }
                KeyType::Metadata(nft_address, token_id) => {
                    vp::log_string(format!(
                        "nft vp, checking if metadata changed: {}",
                        token_id
                    ));
                    is_creator(&nft_address, verifiers)
                }
                _ => is_creator(nft_address, verifiers),
            })
    }

    fn is_approved(
        nft_address: &Address,
        nft_token_id: &str,
        verifiers: &BTreeSet<Address>,
    ) -> bool {
        let approvals_key =
            get_token_approval_key(nft_address, nft_token_id).to_string();
        let approval_addresses: Vec<Address> =
            vp::read_pre(approvals_key).unwrap_or_default();
        return approval_addresses
            .iter()
            .any(|addr| verifiers.contains(addr));
    }

    fn is_creator(
        nft_address: &Address,
        verifiers: &BTreeSet<Address>,
    ) -> bool {
        let creator_key = get_creator_key(nft_address).to_string();
        let creator_address: Address = vp::read_pre(creator_key).unwrap();
        verifiers.contains(&creator_address)
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
}
