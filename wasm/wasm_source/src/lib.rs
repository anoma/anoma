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

/// A tx for a PoS bond that stakes tokens via a self-bond or delegation.
#[cfg(feature = "tx_bond")]
pub mod tx_bond {
    use anoma_vm_env::tx_prelude::proof_of_stake::BondId;
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let bond =
            pos::Bond::try_from_slice(&signed.data.unwrap()[..]).unwrap();

        // TODO temporary for logging:
        let bond_id = BondId {
            source: bond.source.as_ref().unwrap_or(&bond.validator).clone(),
            validator: bond.validator.clone(),
        };
        let bond_pre = PoS.read_bond(&bond_id);
        let validator_set_pre = PoS.read_validator_set();
        let total_deltas_pre = PoS.read_validator_total_deltas(&bond.validator);
        let vp_pre = PoS.read_validator_voting_power(&bond.validator);

        let epoch = get_block_epoch();
        if let Err(err) = PoS.bond_tokens(
            bond.source.as_ref(),
            &bond.validator,
            bond.amount,
            epoch,
        ) {
            log_string(format!("Bond failed with: {}", err));
            panic!()
        }

        // TODO temporary for logging:
        let bond_post = PoS.read_bond(&bond_id);
        let validator_set_post = PoS.read_validator_set();
        let total_deltas_post =
            PoS.read_validator_total_deltas(&bond.validator);
        let vp_post = PoS.read_validator_voting_power(&bond.validator);
        log_string(format!("bond pre {:#?}, post {:#?}", bond_pre, bond_post));
        log_string(format!(
            "validator set pre {:#?}, post {:#?}",
            validator_set_pre, validator_set_post
        ));
        log_string(format!(
            "validator total deltas pre {:#?}, post {:#?}",
            total_deltas_pre, total_deltas_post
        ));
        log_string(format!(
            "validator voting power pre {:#?}, post {:#?}",
            vp_pre, vp_post
        ));
    }
}

/// A tx for a PoS unbond that removes staked tokens from a self-bond or a
/// delegation to be withdrawn in or after unbonding epoch.
#[cfg(feature = "tx_unbond")]
pub mod tx_unbond {
    use anoma_vm_env::tx_prelude::proof_of_stake::BondId;
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let unbond =
            pos::Unbond::try_from_slice(&signed.data.unwrap()[..]).unwrap();

        // TODO temporary for logging:
        let bond_id = BondId {
            source: unbond.source.as_ref().unwrap_or(&unbond.validator).clone(),
            validator: unbond.validator.clone(),
        };
        let bond_pre = PoS.read_bond(&bond_id);
        let unbond_pre = PoS.read_unbond(&bond_id);
        let validator_set_pre = PoS.read_validator_set();
        let total_deltas_pre =
            PoS.read_validator_total_deltas(&unbond.validator);
        let vp_pre = PoS.read_validator_voting_power(&unbond.validator);
        let total_vp_pre = PoS.read_total_voting_power();

        let epoch = get_block_epoch();
        if let Err(err) = PoS.unbond_tokens(
            unbond.source.as_ref(),
            &unbond.validator,
            unbond.amount,
            epoch,
        ) {
            log_string(format!("Unbonding failed with: {}", err));
            panic!()
        }

        // TODO temporary for logging:
        let bond_post = PoS.read_bond(&bond_id);
        let unbond_post = PoS.read_unbond(&bond_id);
        let validator_set_post = PoS.read_validator_set();
        let total_deltas_post =
            PoS.read_validator_total_deltas(&unbond.validator);
        let vp_post = PoS.read_validator_voting_power(&unbond.validator);
        let total_vp_post = PoS.read_total_voting_power();
        log_string(format!("bond pre {:#?}, post {:#?}", bond_pre, bond_post));
        log_string(format!(
            "unbond pre {:#?}, post {:#?}",
            unbond_pre, unbond_post
        ));
        log_string(format!(
            "validator set pre {:#?}, post {:#?}",
            validator_set_pre, validator_set_post
        ));
        log_string(format!(
            "validator total deltas pre {:#?}, post {:#?}",
            total_deltas_pre, total_deltas_post
        ));
        log_string(format!(
            "validator voting power pre {:#?}, post {:#?}",
            vp_pre, vp_post
        ));
        log_string(format!(
            "total voting power pre {:#?}, post {:#?}",
            total_vp_pre, total_vp_post
        ));
    }
}

/// A tx for a PoS unbond that removes staked tokens from a self-bond or a
/// delegation to be withdrawn in or after unbonding epoch.
#[cfg(feature = "tx_withdraw")]
pub mod tx_withdraw {
    use anoma_vm_env::tx_prelude::proof_of_stake::BondId;
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            key::ed25519::SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let withdraw =
            pos::Withdraw::try_from_slice(&signed.data.unwrap()[..]).unwrap();

        // TODO temporary for logging:
        let bond_id = BondId {
            source: withdraw
                .source
                .as_ref()
                .unwrap_or(&withdraw.validator)
                .clone(),
            validator: withdraw.validator.clone(),
        };
        let unbond_pre = PoS.read_unbond(&bond_id);

        let epoch = get_block_epoch();
        match PoS.withdraw_tokens(
            withdraw.source.as_ref(),
            &withdraw.validator,
            epoch,
        ) {
            Ok(slashed) => {
                log_string(format!("Withdrawal slashed for {}", slashed));
            }
            Err(err) => {
                log_string(format!("Withdrawal failed with: {}", err));
                panic!()
            }
        }

        // TODO temporary for logging:
        let unbond_post = PoS.read_unbond(&bond_id);
        log_string(format!(
            "unbond pre {:#?}, post {:#?}",
            unbond_pre, unbond_post
        ));
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
            "validate_tx called with token addr: {}, key_changed: {:?}, \
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
