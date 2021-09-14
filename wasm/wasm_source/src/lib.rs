#[cfg(feature = "mm_token_exch")]
pub mod mm_token_exch;

#[cfg(feature = "vp_user")]
pub mod vp_user;

/// A tx to initialize a new established address with a given public key and
/// a validity predicate.
#[cfg(feature = "tx_init_account")]
pub mod tx_init_account {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let tx_data =
            transaction::InitAccount::try_from_slice(&signed.data.unwrap()[..])
                .unwrap();
        log_string(
            "apply_tx called to init a new established account".to_string(),
        );

        let address = init_account(&tx_data.vp_code);
        let pk_key = key::ed25519::pk_key(&address);
        write(&pk_key.to_string(), &tx_data.public_key);
    }
}

/// A tx to create a new NFT.
#[cfg(feature = "tx_create_nft")]
pub mod tx_create_nft {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let tx_data =
            transaction::CreateNft::try_from_slice(&signed.data.unwrap()[..])
                .unwrap();
        log_string("apply_tx called to create a new NFT".to_string());

        nft::init_nft(tx_data);
    }
}

/// A tx to mint new nft tokens.
#[cfg(feature = "tx_mint_nft_tokens")]
pub mod tx_mint_nft {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let tx_data =
            transaction::MintNft::try_from_slice(&signed.data.unwrap()[..])
                .unwrap();
        log_string("apply_tx called to mint a new NFT tokens".to_string());

        nft::mint_tokens(tx_data);
    }
}

/// A tx for a token transfer crafted by matchmaker from intents.
/// This tx uses `intent::IntentTransfers` wrapped inside
/// `key::ed25519::SignedTxData` as its input as declared in `shared` crate.
#[cfg(feature = "tx_from_intent")]
pub mod tx_from_intent {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();

        let tx_data =
            intent::IntentTransfers::try_from_slice(&signed.data.unwrap()[..]);

        let tx_data = tx_data.unwrap();

        // make sure that the matchmaker has to validate this tx
        insert_verifier(&address::matchmaker());

        for token::Transfer {
            source,
            target,
            token,
            amount,
        } in tx_data.transfers
        {
            token::transfer(&source, &target, &token, amount);
        }

        tx_data
            .exchanges
            .values()
            .into_iter()
            .for_each(intent::invalidate_exchange);
    }
}

/// A tx for token transfer.
/// This tx uses `token::Transfer` wrapped inside `key::ed25519::SignedTxData`
/// as its input as declared in `shared` crate.
#[cfg(feature = "tx_transfer")]
pub mod tx_transfer {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let transfer =
            token::Transfer::try_from_slice(&signed.data.unwrap()[..]).unwrap();
        log_string(format!("apply_tx called with transfer: {:#?}", transfer));
        let token::Transfer {
            source,
            target,
            token,
            amount,
        } = transfer;
        token::transfer(&source, &target, &token, amount)
    }
}

/// A tx for updating an account's validity predicate.
/// This tx wraps the validity predicate inside `key::ed25519::SignedTxData` as
/// its input as declared in `shared` crate.
#[cfg(feature = "tx_update_vp")]
pub mod tx_update_vp {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let update_vp =
            transaction::UpdateVp::try_from_slice(&signed.data.unwrap()[..])
                .unwrap();
        log_string(format!("update VP for: {:#?}", update_vp.addr));
        update_validity_predicate(&update_vp.addr, update_vp.vp_code)
    }
}

/// A VP for a token.
#[cfg(feature = "vp_token")]
pub mod vp_token {
    use anoma_vm_env::vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        _tx_data: Vec<u8>,
        addr: Address,
        keys_changed: HashSet<storage::Key>,
        verifiers: HashSet<Address>,
    ) -> bool {
        log_string(format!(
            "validate_tx called with token addr: {}, key_changed: {:#?}, \
             verifiers: {:?}",
            addr, keys_changed, verifiers
        ));

        token::vp(&addr, &keys_changed, &verifiers)
    }
}

/// Matchmaker filter for token exchange
#[cfg(feature = "mm_filter_token_exch")]
pub mod mm_filter_token_exch {
    use anoma_vm_env::filter_prelude::intent::FungibleTokenIntent;
    use anoma_vm_env::filter_prelude::*;

    #[filter]
    fn validate_intent(intent: Vec<u8>) -> bool {
        // TODO: check if signature is valid
        let intent = decode_intent_data(intent);
        intent.is_some()
    }

    fn decode_intent_data(bytes: Vec<u8>) -> Option<FungibleTokenIntent> {
        FungibleTokenIntent::try_from_slice(&bytes[..]).ok()
    }
}
