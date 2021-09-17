//! Client RPC queries

use std::borrow::Cow;
use std::convert::TryInto;
use std::io::{self, Write};

use anoma::ledger::pos::types::{
    Epoch as PosEpoch, VotingPower, WeightedValidator,
};
use anoma::ledger::pos::{
    self, is_validator_slashes_key, Bonds, Slash, Unbonds,
};
use anoma::types::address::Address;
use anoma::types::key::ed25519;
use anoma::types::storage::Epoch;
use anoma::types::{address, storage, token};
use borsh::BorshDeserialize;
use itertools::Itertools;
use tendermint_rpc::{Client, HttpClient};

use crate::cli::{self, args, Context};
use crate::node::ledger::rpc::{Path, PrefixValue};

/// Dry run a transaction
pub async fn dry_run_tx(
    ledger_address: &tendermint::net::Address,
    tx_bytes: Vec<u8>,
) {
    let client = HttpClient::new(ledger_address.clone()).unwrap();
    let path = Path::DryRunTx;
    let response = client
        .abci_query(Some(path.into()), tx_bytes, None, false)
        .await
        .unwrap();
    println!("{:#?}", response);
}

/// Query the epoch of the last committed block
pub async fn query_epoch(args: args::Query) -> Option<Epoch> {
    let client = HttpClient::new(args.ledger_address).unwrap();
    let path = Path::Epoch;
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .unwrap();
    match response.code {
        tendermint::abci::Code::Ok => {
            match Epoch::try_from_slice(&response.value[..]) {
                Ok(epoch) => {
                    println!("Last committed epoch: {}", epoch);
                    return Some(epoch);
                }

                Err(err) => {
                    eprintln!("Error decoding the epoch value: {}", err)
                }
            }
        }
        tendermint::abci::Code::Err(err) => eprintln!(
            "Error in the query {} (error code {})",
            response.info, err
        ),
    }
    cli::safe_exit(1)
}

/// Query token balance(s)
pub async fn query_balance(ctx: Context, args: args::QueryBalance) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    let tokens = address::tokens();
    match (args.token, args.owner) {
        (Some(token), Some(owner)) => {
            let token = ctx.get(token);
            let owner = ctx.get(owner);
            let key = token::balance_key(&token, &owner);
            let currency_code = tokens
                .get(&token)
                .map(|c| Cow::Borrowed(*c))
                .unwrap_or_else(|| Cow::Owned(token.to_string()));
            match query_storage_value::<token::Amount>(client, key).await {
                Some(balance) => {
                    println!("{}: {}", currency_code, balance);
                }
                None => {
                    println!("No {} balance found for {}", currency_code, owner)
                }
            }
        }
        (None, Some(owner)) => {
            let owner = ctx.get(owner);
            let mut found_any = false;
            for (token, currency_code) in tokens {
                let key = token::balance_key(&token, &owner);
                if let Some(balance) =
                    query_storage_value::<token::Amount>(client.clone(), key)
                        .await
                {
                    println!("{}: {}", currency_code, balance);
                    found_any = true;
                }
            }
            if !found_any {
                println!("No balance found for {}", owner);
            }
        }
        (Some(token), None) => {
            let token = ctx.get(token);
            let key = token::balance_prefix(&token);
            let balances =
                query_storage_prefix::<token::Amount>(client, key).await;
            match balances {
                Some(balances) => {
                    let currency_code = tokens
                        .get(&token)
                        .map(|c| Cow::Borrowed(*c))
                        .unwrap_or_else(|| Cow::Owned(token.to_string()));
                    let stdout = io::stdout();
                    let mut w = stdout.lock();
                    writeln!(w, "Token {}:", currency_code).unwrap();
                    for (key, balance) in balances {
                        let owner =
                            token::is_any_token_balance_key(&key).unwrap();
                        writeln!(w, "  {}, owned by {}", balance, owner)
                            .unwrap();
                    }
                }
                None => println!("No balances for token {}", token.encode()),
            }
        }
        (None, None) => {
            let stdout = io::stdout();
            let mut w = stdout.lock();
            for (token, currency_code) in tokens {
                let key = token::balance_prefix(&token);
                let balances =
                    query_storage_prefix::<token::Amount>(client.clone(), key)
                        .await;
                match balances {
                    Some(balances) => {
                        writeln!(w, "Token {}:", currency_code).unwrap();
                        for (key, balance) in balances {
                            let owner =
                                token::is_any_token_balance_key(&key).unwrap();
                            writeln!(w, "  {}, owned by {}", balance, owner)
                                .unwrap();
                        }
                    }
                    None => {
                        println!("No balances for token {}", token.encode())
                    }
                }
            }
        }
    }
}

/// Accumulate slashes starting from `epoch_start` until (optionally)
/// `withdraw_epoch` and apply them to the token amount `delta`.
fn apply_slashes(
    slashes: &[Slash],
    mut delta: token::Amount,
    epoch_start: PosEpoch,
    withdraw_epoch: Option<PosEpoch>,
    w: &mut std::io::StdoutLock,
) -> token::Amount {
    let mut slashed = token::Amount::default();
    for slash in slashes {
        if slash.epoch >= epoch_start
            && slash.epoch < withdraw_epoch.unwrap_or_else(|| u64::MAX.into())
        {
            writeln!(
                w,
                "    ⚠ Slash: {} from epoch {}",
                slash.rate, slash.epoch
            )
            .unwrap();
            let raw_delta: u64 = delta.into();
            let current_slashed = token::Amount::from(slash.rate * raw_delta);
            slashed += current_slashed;
            delta -= current_slashed;
        }
    }
    if slashed != 0.into() {
        writeln!(w, "    ⚠ Slash total: {}", slashed).unwrap();
        writeln!(w, "    ⚠ After slashing: Δ {}", delta).unwrap();
    }
    delta
}

/// Process the result of a blonds query to determine total bonds
/// and total active bonds. This includes taking into account
/// an aggregation of slashes since the start of the given epoch.
fn process_bonds_query(
    bonds: &Bonds,
    slashes: &[Slash],
    epoch: &Epoch,
    source: Option<&Address>,
    total: Option<token::Amount>,
    total_active: Option<token::Amount>,
    w: &mut std::io::StdoutLock,
) -> (token::Amount, token::Amount) {
    let mut total_active = total_active.unwrap_or_else(|| 0.into());
    let mut current_total: token::Amount = 0.into();
    for bond in bonds.iter() {
        for (epoch_start, &(mut delta)) in bond.deltas.iter().sorted() {
            writeln!(w, "  Active from epoch {}: Δ {}", epoch_start, delta)
                .unwrap();
            delta = apply_slashes(slashes, delta, *epoch_start, None, w);
            current_total += delta;
            let epoch_start: Epoch = (*epoch_start).into();
            if epoch >= &epoch_start {
                total_active += delta;
            }
        }
    }
    let total = total.unwrap_or_else(|| 0.into()) + current_total;
    match source {
        Some(addr) => {
            writeln!(w, "  Bonded total from {}: {}", addr, current_total)
                .unwrap();
        }
        None => {
            if total_active != 0.into() && total_active != total {
                writeln!(w, "Active bonds total: {}", total_active).unwrap();
            }
            writeln!(w, "Bonds total: {}", total).unwrap();
        }
    }
    (total, total_active)
}

/// Process the result of an unbonds query to determine total bonds
/// and total withdrawable bonds. This includes taking into account
/// an aggregation of slashes since the start of the given epoch up
/// until the withdrawal epoch.
fn process_unbonds_query(
    unbonds: &Unbonds,
    slashes: &[Slash],
    epoch: &Epoch,
    source: Option<&Address>,
    total: Option<token::Amount>,
    total_withdrawable: Option<token::Amount>,
    w: &mut std::io::StdoutLock,
) -> (token::Amount, token::Amount) {
    let mut withdrawable = total_withdrawable.unwrap_or_else(|| 0.into());
    let mut current_total: token::Amount = 0.into();
    for deltas in unbonds.iter() {
        for ((epoch_start, epoch_end), &(mut delta)) in
            deltas.deltas.iter().sorted()
        {
            let withdraw_epoch = *epoch_end + 1_u64;
            writeln!(
                w,
                "  Withdrawable from epoch {} (active from {}): Δ {}",
                withdraw_epoch, epoch_start, delta
            )
            .unwrap();
            delta = apply_slashes(
                slashes,
                delta,
                *epoch_start,
                Some(withdraw_epoch),
                w,
            );
            current_total += delta;
            let epoch_end: Epoch = (*epoch_end).into();
            if epoch > &epoch_end {
                withdrawable += delta;
            }
        }
    }
    let total = total.unwrap_or_else(|| 0.into()) + current_total;
    match source {
        Some(addr) => {
            writeln!(w, "  Unbonded total from {}: {}", addr, current_total)
                .unwrap();
        }
        None => {
            if withdrawable != 0.into() {
                writeln!(w, "Withdrawable total: {}", withdrawable).unwrap();
            }
            writeln!(w, "Unbonded total: {}", total).unwrap();
        }
    }
    (total, withdrawable)
}

/// Query PoS bond(s)
pub async fn query_bonds(ctx: Context, args: args::QueryBonds) {
    let epoch = query_epoch(args.query.clone()).await;
    if let Some(epoch) = epoch {
        let client = HttpClient::new(args.query.ledger_address).unwrap();
        match (args.owner, args.validator) {
            (Some(owner), Some(validator)) => {
                let source = ctx.get(owner);
                let validator = ctx.get(validator);
                // Find owner's delegations to the given validator
                let bond_id = pos::BondId { source, validator };
                let bond_key = pos::bond_key(&bond_id);
                let bonds =
                    query_storage_value::<pos::Bonds>(client.clone(), bond_key)
                        .await;
                // Find owner's unbonded delegations from the given validator
                let unbond_key = pos::unbond_key(&bond_id);
                let unbonds = query_storage_value::<pos::Unbonds>(
                    client.clone(),
                    unbond_key,
                )
                .await;
                // Find validator's slashes, if any
                let slashes_key =
                    pos::validator_slashes_key(&bond_id.validator);
                let slashes =
                    query_storage_value::<pos::Slashes>(client, slashes_key)
                        .await
                        .unwrap_or_default();

                let stdout = io::stdout();
                let mut w = stdout.lock();

                if let Some(bonds) = &bonds {
                    let bond_type = if bond_id.source == bond_id.validator {
                        "Self-bonds"
                    } else {
                        "Delegations"
                    };
                    writeln!(w, "{}:", bond_type).unwrap();
                    process_bonds_query(
                        bonds, &slashes, &epoch, None, None, None, &mut w,
                    );
                }

                if let Some(unbonds) = &unbonds {
                    let bond_type = if bond_id.source == bond_id.validator {
                        "Unbonded self-bonds"
                    } else {
                        "Unbonded delegations"
                    };
                    writeln!(w, "{}:", bond_type).unwrap();
                    process_unbonds_query(
                        unbonds, &slashes, &epoch, None, None, None, &mut w,
                    );
                }

                if bonds.is_none() && unbonds.is_none() {
                    writeln!(
                        w,
                        "No delegations found for {} to validator {}",
                        bond_id.source,
                        bond_id.validator.encode()
                    )
                    .unwrap();
                }
            }
            (None, Some(validator)) => {
                let validator = ctx.get(validator);
                // Find validator's self-bonds
                let bond_id = pos::BondId {
                    source: validator.clone(),
                    validator,
                };
                let bond_key = pos::bond_key(&bond_id);
                let bonds =
                    query_storage_value::<pos::Bonds>(client.clone(), bond_key)
                        .await;
                // Find validator's unbonded self-bonds
                let unbond_key = pos::unbond_key(&bond_id);
                let unbonds = query_storage_value::<pos::Unbonds>(
                    client.clone(),
                    unbond_key,
                )
                .await;
                // Find validator's slashes, if any
                let slashes_key =
                    pos::validator_slashes_key(&bond_id.validator);
                let slashes =
                    query_storage_value::<pos::Slashes>(client, slashes_key)
                        .await
                        .unwrap_or_default();

                let stdout = io::stdout();
                let mut w = stdout.lock();

                if let Some(bonds) = &bonds {
                    writeln!(w, "Self-bonds:").unwrap();
                    process_bonds_query(
                        bonds, &slashes, &epoch, None, None, None, &mut w,
                    );
                }

                if let Some(unbonds) = &unbonds {
                    writeln!(w, "Unbonded self-bonds:").unwrap();
                    process_unbonds_query(
                        unbonds, &slashes, &epoch, None, None, None, &mut w,
                    );
                }

                if bonds.is_none() && unbonds.is_none() {
                    writeln!(
                        w,
                        "No self-bonds found for validator {}",
                        bond_id.validator.encode()
                    )
                    .unwrap();
                }
            }
            (Some(owner), None) => {
                let owner = ctx.get(owner);
                // Find owner's bonds to any validator
                let bonds_prefix = pos::bonds_for_source_prefix(&owner);
                let bonds = query_storage_prefix::<pos::Bonds>(
                    client.clone(),
                    bonds_prefix,
                )
                .await;
                // Find owner's unbonds to any validator
                let unbonds_prefix = pos::unbonds_for_source_prefix(&owner);
                let unbonds = query_storage_prefix::<pos::Unbonds>(
                    client.clone(),
                    unbonds_prefix,
                )
                .await;

                let mut total: token::Amount = 0.into();
                let mut total_active: token::Amount = 0.into();
                let mut any_bonds = false;
                if let Some(bonds) = bonds {
                    for (key, bonds) in bonds {
                        match pos::is_bond_key(&key) {
                            Some(pos::BondId { source, validator }) => {
                                // Find validator's slashes, if any
                                let slashes_key =
                                    pos::validator_slashes_key(&validator);
                                let slashes =
                                    query_storage_value::<pos::Slashes>(
                                        client.clone(),
                                        slashes_key,
                                    )
                                    .await
                                    .unwrap_or_default();

                                let stdout = io::stdout();
                                let mut w = stdout.lock();
                                any_bonds = true;
                                let bond_type: Cow<str> = if source == validator
                                {
                                    "Self-bonds".into()
                                } else {
                                    format!("Delegations from {}", source)
                                        .into()
                                };
                                writeln!(w, "{}:", bond_type).unwrap();
                                let (tot, tot_active) = process_bonds_query(
                                    &bonds,
                                    &slashes,
                                    &epoch,
                                    Some(&source),
                                    Some(total),
                                    Some(total_active),
                                    &mut w,
                                );
                                total = tot;
                                total_active = tot_active;
                            }
                            None => panic!("Unexpected storage key {}", key),
                        }
                    }
                }
                if total_active != 0.into() && total_active != total {
                    println!("Active bonds total: {}", total_active);
                }

                let mut total: token::Amount = 0.into();
                let mut total_withdrawable: token::Amount = 0.into();
                if let Some(unbonds) = unbonds {
                    for (key, unbonds) in unbonds {
                        match pos::is_unbond_key(&key) {
                            Some(pos::BondId { source, validator }) => {
                                // Find validator's slashes, if any
                                let slashes_key =
                                    pos::validator_slashes_key(&validator);
                                let slashes =
                                    query_storage_value::<pos::Slashes>(
                                        client.clone(),
                                        slashes_key,
                                    )
                                    .await
                                    .unwrap_or_default();

                                let stdout = io::stdout();
                                let mut w = stdout.lock();
                                any_bonds = true;
                                let bond_type: Cow<str> = if source == validator
                                {
                                    "Unbonded self-bonds".into()
                                } else {
                                    format!(
                                        "Unbonded delegations from {}",
                                        source
                                    )
                                    .into()
                                };
                                writeln!(w, "{}:", bond_type).unwrap();
                                let (tot, tot_withdrawable) =
                                    process_unbonds_query(
                                        &unbonds,
                                        &slashes,
                                        &epoch,
                                        Some(&source),
                                        Some(total),
                                        Some(total_withdrawable),
                                        &mut w,
                                    );
                                total = tot;
                                total_withdrawable = tot_withdrawable;
                            }
                            None => panic!("Unexpected storage key {}", key),
                        }
                    }
                }
                if total_withdrawable != 0.into() {
                    println!("Withdrawable total: {}", total_withdrawable);
                }

                if !any_bonds {
                    println!(
                        "No self-bonds or delegations found for {}",
                        owner
                    );
                }
            }
            (None, None) => {
                // Find all the bonds
                let bonds_prefix = pos::bonds_prefix();
                let bonds = query_storage_prefix::<pos::Bonds>(
                    client.clone(),
                    bonds_prefix,
                )
                .await;
                // Find all the unbonds
                let unbonds_prefix = pos::unbonds_prefix();
                let unbonds = query_storage_prefix::<pos::Unbonds>(
                    client.clone(),
                    unbonds_prefix,
                )
                .await;

                let mut total: token::Amount = 0.into();
                let mut total_active: token::Amount = 0.into();
                if let Some(bonds) = bonds {
                    for (key, bonds) in bonds {
                        match pos::is_bond_key(&key) {
                            Some(pos::BondId { source, validator }) => {
                                // Find validator's slashes, if any
                                let slashes_key =
                                    pos::validator_slashes_key(&validator);
                                let slashes =
                                    query_storage_value::<pos::Slashes>(
                                        client.clone(),
                                        slashes_key,
                                    )
                                    .await
                                    .unwrap_or_default();

                                let stdout = io::stdout();
                                let mut w = stdout.lock();
                                let bond_type = if source == validator {
                                    format!(
                                        "Self-bonds for {}",
                                        validator.encode()
                                    )
                                } else {
                                    format!(
                                        "Delegations from {} to validator {}",
                                        source,
                                        validator.encode()
                                    )
                                };
                                writeln!(w, "{}:", bond_type).unwrap();
                                let (tot, tot_active) = process_bonds_query(
                                    &bonds,
                                    &slashes,
                                    &epoch,
                                    Some(&source),
                                    Some(total),
                                    Some(total_active),
                                    &mut w,
                                );
                                total = tot;
                                total_active = tot_active;
                            }
                            None => panic!("Unexpected storage key {}", key),
                        }
                    }
                }
                if total_active != 0.into() && total_active != total {
                    println!("Bond total active: {}", total_active);
                }
                println!("Bond total: {}", total);

                let mut total: token::Amount = 0.into();
                let mut total_withdrawable: token::Amount = 0.into();
                if let Some(unbonds) = unbonds {
                    for (key, unbonds) in unbonds {
                        match pos::is_unbond_key(&key) {
                            Some(pos::BondId { source, validator }) => {
                                // Find validator's slashes, if any
                                let slashes_key =
                                    pos::validator_slashes_key(&validator);
                                let slashes =
                                    query_storage_value::<pos::Slashes>(
                                        client.clone(),
                                        slashes_key,
                                    )
                                    .await
                                    .unwrap_or_default();

                                let stdout = io::stdout();
                                let mut w = stdout.lock();
                                let bond_type = if source == validator {
                                    format!(
                                        "Unbonded self-bonds for {}",
                                        validator.encode()
                                    )
                                } else {
                                    format!(
                                        "Unbonded delegations from {} to \
                                         validator {}",
                                        source,
                                        validator.encode()
                                    )
                                };
                                writeln!(w, "{}:", bond_type).unwrap();
                                let (tot, tot_withdrawable) =
                                    process_unbonds_query(
                                        &unbonds,
                                        &slashes,
                                        &epoch,
                                        Some(&source),
                                        Some(total),
                                        Some(total_withdrawable),
                                        &mut w,
                                    );
                                total = tot;
                                total_withdrawable = tot_withdrawable;
                            }
                            None => panic!("Unexpected storage key {}", key),
                        }
                    }
                }
                if total_withdrawable != 0.into() {
                    println!("Withdrawable total: {}", total_withdrawable);
                }
                println!("Unbonded total: {}", total);
            }
        }
    }
}

/// Query PoS voting power
pub async fn query_voting_power(ctx: Context, args: args::QueryVotingPower) {
    let epoch = match args.epoch {
        Some(_) => args.epoch,
        None => query_epoch(args.query.clone()).await,
    };
    if let Some(epoch) = epoch {
        let client = HttpClient::new(args.query.ledger_address).unwrap();

        // Find the validator set
        let validator_set_key = pos::validator_set_key();
        let validator_sets = query_storage_value::<pos::ValidatorSets>(
            client.clone(),
            validator_set_key,
        )
        .await
        .expect("Validator set should always be set");
        let validator_set = validator_sets
            .get(epoch)
            .expect("Validator set should be always set in the current epoch");
        match args.validator {
            Some(validator) => {
                let validator = ctx.get(validator);
                // Find voting power for the given validator
                let voting_power_key =
                    pos::validator_voting_power_key(&validator);
                let voting_powers = query_storage_value::<
                    pos::ValidatorVotingPowers,
                >(
                    client.clone(), voting_power_key
                )
                .await;
                match voting_powers.and_then(|data| data.get(epoch)) {
                    Some(voting_power_delta) => {
                        let voting_power: VotingPower =
                            voting_power_delta.try_into().expect(
                                "The sum voting power deltas shouldn't be \
                                 negative",
                            );
                        let weighted = WeightedValidator {
                            address: validator.clone(),
                            voting_power,
                        };
                        let is_active =
                            validator_set.active.contains(&weighted);
                        if !is_active {
                            debug_assert!(
                                validator_set.inactive.contains(&weighted)
                            );
                        }
                        println!(
                            "Validator {} is {}, voting power: {}",
                            validator.encode(),
                            if is_active { "active" } else { "inactive" },
                            voting_power
                        )
                    }
                    None => println!(
                        "No voting power found for {}",
                        validator.encode()
                    ),
                }
            }
            None => {
                // Iterate all validators
                let stdout = io::stdout();
                let mut w = stdout.lock();

                writeln!(w, "Active validators:").unwrap();
                for active in &validator_set.active {
                    writeln!(
                        w,
                        "  {}: {}",
                        active.address.encode(),
                        active.voting_power
                    )
                    .unwrap();
                }
                if !validator_set.inactive.is_empty() {
                    writeln!(w, "Inactive validators:").unwrap();
                    for inactive in &validator_set.inactive {
                        writeln!(
                            w,
                            "  {}: {}",
                            inactive.address.encode(),
                            inactive.voting_power
                        )
                        .unwrap();
                    }
                }
            }
        }
        let total_voting_power_key = pos::total_voting_power_key();
        let total_voting_powers =
            query_storage_value::<pos::TotalVotingPowers>(
                client,
                total_voting_power_key,
            )
            .await
            .expect("Total voting power should always be set");
        let total_voting_power = total_voting_powers.get(epoch).expect(
            "Total voting power should be always set in the current epoch",
        );
        println!("Total voting power: {}", total_voting_power);
    }
}

/// Query PoS slashes
pub async fn query_slashes(ctx: Context, args: args::QuerySlashes) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    match args.validator {
        Some(validator) => {
            let validator = ctx.get(validator);
            // Find slashes for the given validator
            let slashes_key = pos::validator_slashes_key(&validator);
            let slashes = query_storage_value::<pos::Slashes>(
                client.clone(),
                slashes_key,
            )
            .await;
            match slashes {
                Some(slashes) => {
                    let stdout = io::stdout();
                    let mut w = stdout.lock();
                    for slash in slashes {
                        writeln!(
                            w,
                            "Slash epoch {}, rate {}, type {}",
                            slash.epoch, slash.rate, slash.r#type
                        )
                        .unwrap();
                    }
                }
                None => {
                    println!("No slashes found for {}", validator.encode())
                }
            }
        }
        None => {
            // Iterate slashes for all validators
            let slashes_prefix = pos::slashes_prefix();
            let slashes = query_storage_prefix::<pos::Slashes>(
                client.clone(),
                slashes_prefix,
            )
            .await;

            match slashes {
                Some(slashes) => {
                    let stdout = io::stdout();
                    let mut w = stdout.lock();
                    for (slashes_key, slashes) in slashes {
                        if let Some(validator) =
                            is_validator_slashes_key(&slashes_key)
                        {
                            for slash in slashes {
                                writeln!(
                                    w,
                                    "Slash epoch {}, block height {}, rate \
                                     {}, type {}, validator {}",
                                    slash.epoch,
                                    slash.block_height,
                                    slash.rate,
                                    slash.r#type,
                                    validator,
                                )
                                .unwrap();
                            }
                        } else {
                            eprintln!("Unexpected slashes key {}", slashes_key);
                        }
                    }
                }
                None => {
                    println!("No slashes found")
                }
            }
        }
    }
}

/// Get account's public key stored in its storage sub-space
pub async fn get_public_key(
    address: &Address,
    ledger_address: tendermint::net::Address,
) -> Option<ed25519::PublicKey> {
    let client = HttpClient::new(ledger_address).unwrap();
    let key = ed25519::pk_key(address);
    query_storage_value(client, key).await
}

/// Query a storage value and decode it with [`BorshDeserialize`].
async fn query_storage_value<T>(
    client: HttpClient,
    key: storage::Key,
) -> Option<T>
where
    T: BorshDeserialize,
{
    let path = Path::Value(key);
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .unwrap();
    match response.code {
        tendermint::abci::Code::Ok => {
            match T::try_from_slice(&response.value[..]) {
                Ok(value) => return Some(value),
                Err(err) => eprintln!("Error decoding the value: {}", err),
            }
        }
        tendermint::abci::Code::Err(err) => {
            if err == 1 {
                return None;
            } else {
                eprintln!(
                    "Error in the query {} (error code {})",
                    response.info, err
                )
            }
        }
    }
    cli::safe_exit(1)
}

/// Query a range of storage values with a matching prefix and decode them with
/// [`BorshDeserialize`]. Returns an iterator of the storage keys paired with
/// their associated values.
async fn query_storage_prefix<T>(
    client: HttpClient,
    key: storage::Key,
) -> Option<impl Iterator<Item = (storage::Key, T)>>
where
    T: BorshDeserialize,
{
    let path = Path::Prefix(key);
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .unwrap();
    match response.code {
        tendermint::abci::Code::Ok => {
            match Vec::<PrefixValue>::try_from_slice(&response.value[..]) {
                Ok(values) => {
                    let decode = |PrefixValue { key, value }: PrefixValue| {
                        match T::try_from_slice(&value[..]) {
                            Err(err) => {
                                eprintln!(
                                    "Skipping a value for key {}. Error in \
                                     decoding: {}",
                                    key, err
                                );
                                None
                            }
                            Ok(value) => Some((key, value)),
                        }
                    };
                    return Some(values.into_iter().filter_map(decode));
                }
                Err(err) => eprintln!("Error decoding the values: {}", err),
            }
        }
        tendermint::abci::Code::Err(err) => {
            if err == 1 {
                return None;
            } else {
                eprintln!(
                    "Error in the query {} (error code {})",
                    response.info, err
                )
            }
        }
    }
    cli::safe_exit(1)
}
