/// A tx that doesn't do anything.
#[cfg(feature = "tx_no_op")]
pub mod main {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(_tx_data: Vec<u8>) {}
}

/// A tx that allocates a memory of size given from the `tx_data: usize`.
#[cfg(feature = "tx_memory_limit")]
pub mod main {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let len = usize::try_from_slice(&tx_data[..]).unwrap();
        log_string(format!("allocate len {}", len));
        let bytes: Vec<u8> = vec![6_u8; len];
        // use the variable to prevent it from compiler optimizing it away
        log_string(format!("{:?}", &bytes[..8]));
    }
}

/// A tx to be used as proposal_code
#[cfg(feature = "tx_proposal_code")]
pub mod main {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(_tx_data: Vec<u8>) {
        // governance
        let target_key = storage::get_min_proposal_grace_epoch_key();
        write(&target_key.to_string(), 9_u64);

        // treasury
        let target_key = treasury_storage::get_max_transferable_fund_key();
        write(&target_key.to_string(), token::Amount::whole(20_000));

        // parameters
        let target_key = parameters_storage::get_tx_whitelist_storage_key();
        write(&target_key.to_string(), vec!["hash"]);
    }
}

/// A tx that attempts to read the given key from storage.
#[cfg(feature = "tx_read_storage_key")]
pub mod main {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        // Allocates a memory of size given from the `tx_data (usize)`
        let key = Key::try_from_slice(&tx_data[..]).unwrap();
        log_string(format!("key {}", key));
        let _result: Vec<u8> = read(key.to_string()).unwrap();
    }
}

/// A tx that attempts to mint tokens in the transfer's target without debiting
/// the tokens from the source. This tx is expected to be rejected by the
/// token's VP.
#[cfg(feature = "tx_mint_tokens")]
pub mod main {
    use anoma_vm_env::tx_prelude::*;

    #[transaction]
    fn apply_tx(tx_data: Vec<u8>) {
        let signed =
            SignedTxData::try_from_slice(&tx_data[..]).unwrap();
        let transfer =
            token::Transfer::try_from_slice(&signed.data.unwrap()[..]).unwrap();
        log_string(format!("apply_tx called to mint tokens: {:#?}", transfer));
        let token::Transfer {
            source: _,
            target,
            token,
            amount,
            shielded: _,
        } = transfer;
        let target_key = token::balance_key(&token, &target);
        let mut target_bal: token::Amount =
            read(&target_key.to_string()).unwrap_or_default();
        target_bal.receive(&amount);
        write(&target_key.to_string(), target_bal);
    }
}

/// A VP that always returns `true`.
#[cfg(feature = "vp_always_true")]
pub mod main {
    use anoma_vm_env::vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        _tx_data: Vec<u8>,
        _addr: Address,
        _keys_changed: BTreeSet<storage::Key>,
        _verifiers: BTreeSet<Address>,
    ) -> bool {
        true
    }
}

/// A VP that always returns `false`.
#[cfg(feature = "vp_always_false")]
pub mod main {
    use anoma_vm_env::vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        _tx_data: Vec<u8>,
        _addr: Address,
        _keys_changed: BTreeSet<storage::Key>,
        _verifiers: BTreeSet<Address>,
    ) -> bool {
        false
    }
}

/// A VP that runs the VP given in `tx_data` via `eval`. It returns the result
/// of `eval`.
#[cfg(feature = "vp_eval")]
pub mod main {
    use anoma_vm_env::vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        tx_data: Vec<u8>,
        _addr: Address,
        _keys_changed: BTreeSet<storage::Key>,
        _verifiers: BTreeSet<Address>,
    ) -> bool {
        use validity_predicate::EvalVp;
        let EvalVp { vp_code, input }: EvalVp =
            EvalVp::try_from_slice(&tx_data[..]).unwrap();
        eval(vp_code, input)
    }
}

// A VP that allocates a memory of size given from the `tx_data: usize`.
// Returns `true`, if the allocation is within memory limits.
#[cfg(feature = "vp_memory_limit")]
pub mod main {
    use anoma_vm_env::vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        tx_data: Vec<u8>,
        _addr: Address,
        _keys_changed: BTreeSet<storage::Key>,
        _verifiers: BTreeSet<Address>,
    ) -> bool {
        let len = usize::try_from_slice(&tx_data[..]).unwrap();
        log_string(format!("allocate len {}", len));
        let bytes: Vec<u8> = vec![6_u8; len];
        // use the variable to prevent it from compiler optimizing it away
        log_string(format!("{:?}", &bytes[..8]));
        true
    }
}

/// A VP that attempts to read the given key from storage (state prior to tx
/// execution). Returns `true`, if the allocation is within memory limits.
#[cfg(feature = "vp_read_storage_key")]
pub mod main {
    use anoma_vm_env::vp_prelude::*;

    #[validity_predicate]
    fn validate_tx(
        tx_data: Vec<u8>,
        _addr: Address,
        _keys_changed: BTreeSet<storage::Key>,
        _verifiers: BTreeSet<Address>,
    ) -> bool {
        // Allocates a memory of size given from the `tx_data (usize)`
        let key = Key::try_from_slice(&tx_data[..]).unwrap();
        log_string(format!("key {}", key));
        let _result: Vec<u8> = read_pre(key.to_string()).unwrap();
        true
    }
}
