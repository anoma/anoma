#[cfg(feature = "vp_user")]
pub mod vp_user;

#[cfg(feature = "vp_testnet_faucet")]
pub mod vp_testnet_faucet;

/// A tx to initialize a new established address with a given public key and
/// a validity predicate.
#[cfg(feature = "tx_init_account")]
pub mod tx_init_account {
    use anoma_tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let tx_data =
            transaction::InitAccount::try_from_slice(&signed.data.unwrap()[..])
                .unwrap();
        debug_log!("apply_tx called to init a new established account");

        let address = init_account(&tx_data.vp_code);
        let pk_key = key::ed25519::pk_key(&address);
        write(&pk_key.to_string(), &tx_data.public_key);
    }
}

/// A tx to initialize a new validator account and staking reward account with a
/// given public keys and a validity predicates.
#[cfg(feature = "tx_init_validator")]
pub mod tx_init_validator {
    use anoma_tx_prelude::transaction::InitValidator;
    use anoma_tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let init_validator =
            InitValidator::try_from_slice(&signed.data.unwrap()[..]).unwrap();
        debug_log!("apply_tx called to init a new validator account");

        // Register the validator in PoS
        match proof_of_stake::init_validator(init_validator) {
            Ok((validator_address, staking_reward_address)) => {
                debug_log!(
                    "Created validator {} and staking reward account {}",
                    validator_address.encode(),
                    staking_reward_address.encode()
                )
            }
            Err(err) => {
                debug_log!("Validator creation failed with: {}", err);
                panic!()
            }
        }
    }
}

/// A tx for a PoS bond that stakes tokens via a self-bond or delegation.
#[cfg(feature = "tx_bond")]
pub mod tx_bond {
    use anoma_tx_prelude::proof_of_stake::bond_tokens;
    use anoma_tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let bond =
            transaction::pos::Bond::try_from_slice(&signed.data.unwrap()[..])
                .unwrap();

        if let Err(err) =
            bond_tokens(bond.source.as_ref(), &bond.validator, bond.amount)
        {
            debug_log!("Bond failed with: {}", err);
            panic!()
        }
    }
}

/// A tx for a PoS unbond that removes staked tokens from a self-bond or a
/// delegation to be withdrawn in or after unbonding epoch.
#[cfg(feature = "tx_unbond")]
pub mod tx_unbond {
    use anoma_tx_prelude::proof_of_stake::unbond_tokens;
    use anoma_tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let unbond =
            transaction::pos::Unbond::try_from_slice(&signed.data.unwrap()[..])
                .unwrap();

        if let Err(err) = unbond_tokens(
            unbond.source.as_ref(),
            &unbond.validator,
            unbond.amount,
        ) {
            debug_log!("Unbonding failed with: {}", err);
            panic!()
        }
    }
}

/// A tx for a PoS unbond that removes staked tokens from a self-bond or a
/// delegation to be withdrawn in or after unbonding epoch.
#[cfg(feature = "tx_withdraw")]
pub mod tx_withdraw {
    use anoma_tx_prelude::proof_of_stake::withdraw_tokens;
    use anoma_tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let withdraw = transaction::pos::Withdraw::try_from_slice(
            &signed.data.unwrap()[..],
        )
        .unwrap();

        match withdraw_tokens(withdraw.source.as_ref(), &withdraw.validator) {
            Ok(slashed) => {
                debug_log!("Withdrawal slashed for {}", slashed);
            }
            Err(err) => {
                debug_log!("Withdrawal failed with: {}", err);
                panic!()
            }
        }
    }
}

/// A tx for a token transfer crafted by matchmaker from intents.
/// This tx uses `intent::IntentTransfers` wrapped inside
/// `key::ed25519::SignedTxData` as its input as declared in `shared` crate.
#[cfg(feature = "tx_from_intent")]
pub mod tx_from_intent {
    use anoma_tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();

        let tx_data =
            intent::IntentTransfers::try_from_slice(&signed.data.unwrap()[..]);

        let tx_data = tx_data.unwrap();

        // make sure that the matchmaker has to validate this tx
        insert_verifier(&tx_data.source);

        for token::Transfer {
            source,
            target,
            token,
            amount,
        } in tx_data.matches.transfers
        {
            token::transfer(&source, &target, &token, amount);
        }

        tx_data
            .matches
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
    use anoma_tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let transfer =
            token::Transfer::try_from_slice(&signed.data.unwrap()[..]).unwrap();
        debug_log!("apply_tx called with transfer: {:#?}", transfer);
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
    use anoma_tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let update_vp =
            transaction::UpdateVp::try_from_slice(&signed.data.unwrap()[..])
                .unwrap();
        debug_log!("update VP for: {:#?}", update_vp.addr);
        update_validity_predicate(&update_vp.addr, update_vp.vp_code)
    }
}

/// A VP for a token.
#[cfg(feature = "vp_token")]
pub mod vp_token {
    use anoma_vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        _tx_data: Vec<u8>,
        addr: Address,
        keys_changed: HashSet<storage::Key>,
        verifiers: HashSet<Address>,
    ) -> bool {
        debug_log!(
            "validate_tx called with token addr: {}, key_changed: {:?}, \
             verifiers: {:?}",
            addr,
            keys_changed,
            verifiers
        );

        token::vp(&addr, &keys_changed, &verifiers)
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
        let tx_data = transaction::nft::CreateNft::try_from_slice(
            &signed.data.unwrap()[..],
        )
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
        let tx_data = transaction::nft::MintNft::try_from_slice(
            &signed.data.unwrap()[..],
        )
        .unwrap();
        log_string("apply_tx called to mint a new NFT tokens".to_string());

        nft::mint_tokens(tx_data);
    }
}

/// A VP for a nft.
#[cfg(feature = "vp_nft")]
pub mod vp_nft;
