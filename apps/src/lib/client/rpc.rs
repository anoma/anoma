//! Client RPC queries

use std::borrow::Cow;
use std::io::{self, Write};

use anoma::ledger::pos;
use anoma::types::storage::Epoch;
use anoma::types::{address, storage, token};
use borsh::BorshDeserialize;
use itertools::Itertools;
use tendermint_rpc::{Client, HttpClient};

use crate::cli::args;
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
    std::process::exit(1)
}

/// Query token balance(s)
pub async fn query_balance(args: args::QueryBalance) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    let tokens = address::tokens();
    match (args.token.as_ref(), args.owner.as_ref()) {
        (Some(token), Some(owner)) => {
            let key = token::balance_key(token, owner);
            let currency_code = tokens
                .get(token)
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
            let mut found_any = false;
            for (token, currency_code) in tokens {
                let key = token::balance_key(&token, owner);
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
            let key = token::balance_prefix(token);
            let balances =
                query_storage_prefix::<token::Amount>(client, key).await;
            let currency_code = tokens
                .get(token)
                .map(|c| Cow::Borrowed(*c))
                .unwrap_or_else(|| Cow::Owned(token.to_string()));
            let stdout = io::stdout();
            let mut w = stdout.lock();
            writeln!(w, "Token {}:", currency_code).unwrap();
            for (key, balance) in balances {
                let owner = token::is_any_token_balance_key(&key).unwrap();
                writeln!(w, "  {}, owned by {}", balance, owner).unwrap();
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
                writeln!(w, "Token {}:", currency_code).unwrap();
                for (key, balance) in balances {
                    let owner = token::is_any_token_balance_key(&key).unwrap();
                    writeln!(w, "  {}, owned by {}", balance, owner).unwrap();
                }
            }
        }
    }
}

/// Query PoS bond(s)
pub async fn query_bonds(args: args::QueryBonds) {
    let epoch = query_epoch(args.query.clone()).await;
    if let Some(epoch) = epoch {
        let client = HttpClient::new(args.query.ledger_address).unwrap();
        match (args.owner.as_ref(), args.validator.as_ref()) {
            (Some(owner), Some(validator)) => {
                // Find owner's delegations to the given validator
                let bond_id = pos::BondId {
                    source: owner.clone(),
                    validator: validator.clone(),
                };
                let bond_key = pos::bond_key(&bond_id);
                let bonds =
                    query_storage_value::<pos::Bonds>(client.clone(), bond_key)
                        .await;
                // Find owner's unbonded delegations from the given validator
                let unbond_key = pos::unbond_key(&bond_id);
                let unbonds =
                    query_storage_value::<pos::Unbonds>(client, unbond_key)
                        .await;

                let stdout = io::stdout();
                let mut w = stdout.lock();

                if let Some(bonds) = &bonds {
                    writeln!(w, "Delegations:").unwrap();
                    let mut total: token::Amount = 0.into();
                    let mut total_active: token::Amount = 0.into();
                    for deltas in bonds.iter() {
                        for (epoch_start, delta) in deltas.delta.iter().sorted()
                        {
                            writeln!(
                                w,
                                "  Active from epoch {}: Δ {}",
                                epoch_start, delta
                            )
                            .unwrap();
                            total += *delta;
                            let epoch_start: Epoch = (*epoch_start).into();
                            if epoch >= epoch_start {
                                total_active += *delta;
                            }
                        }
                    }
                    if total_active != 0.into() && total_active != total {
                        writeln!(w, "Active bonds total: {}", total_active)
                            .unwrap();
                    }
                    writeln!(w, "Bonds total: {}", total).unwrap();
                }

                if let Some(unbonds) = &unbonds {
                    writeln!(w, "Unbonded delegations:").unwrap();
                    let mut total: token::Amount = 0.into();
                    let mut withdrawable: token::Amount = 0.into();
                    for deltas in unbonds.iter() {
                        for ((epoch_start, epoch_end), delta) in
                            deltas.deltas.iter().sorted()
                        {
                            writeln!(
                                w,
                                "  Active from epoch {} to {}: Δ {}",
                                epoch_start, epoch_end, delta
                            )
                            .unwrap();
                            total += *delta;
                            let epoch_end: Epoch = (*epoch_end).into();
                            if epoch >= epoch_end {
                                withdrawable += *delta;
                            }
                        }
                    }
                    if withdrawable != 0.into() {
                        writeln!(w, "Withdrawable total: {}", withdrawable)
                            .unwrap();
                    }
                    writeln!(w, "Unbonded total: {}", total).unwrap();
                }
                if bonds.is_none() && unbonds.is_none() {
                    writeln!(
                        w,
                        "No delegations found for {} to validator {}",
                        owner, validator
                    )
                    .unwrap();
                }
            }
            (None, Some(validator)) => {
                // Find validator's self-bonds
                let bond_id = pos::BondId {
                    source: validator.clone(),
                    validator: validator.clone(),
                };
                let bond_key = pos::bond_key(&bond_id);
                let bonds =
                    query_storage_value::<pos::Bonds>(client.clone(), bond_key)
                        .await;
                // Find validator's unbonded self-bonds
                let unbond_key = pos::unbond_key(&bond_id);
                let unbonds =
                    query_storage_value::<pos::Unbonds>(client, unbond_key)
                        .await;

                let stdout = io::stdout();
                let mut w = stdout.lock();

                if let Some(bonds) = &bonds {
                    writeln!(w, "Self-bonds:").unwrap();
                    let mut total: token::Amount = 0.into();
                    let mut total_active: token::Amount = 0.into();
                    for deltas in bonds.iter() {
                        for (epoch_start, delta) in deltas.delta.iter().sorted()
                        {
                            writeln!(
                                w,
                                "  Active from epoch {}: Δ {}",
                                epoch_start, delta
                            )
                            .unwrap();
                            total += *delta;
                            let epoch_start: Epoch = (*epoch_start).into();
                            if epoch >= epoch_start {
                                total_active += *delta;
                            }
                        }
                    }
                    if total_active != 0.into() && total_active != total {
                        writeln!(w, "Total active: {}", total_active).unwrap();
                    }
                    writeln!(w, "Total: {}", total).unwrap();
                }

                if let Some(unbonds) = &unbonds {
                    writeln!(w, "Unbonded self-bonds:").unwrap();
                    let mut total: token::Amount = 0.into();
                    let mut withdrawable: token::Amount = 0.into();
                    for deltas in unbonds.iter() {
                        for ((epoch_start, epoch_end), delta) in
                            deltas.deltas.iter().sorted()
                        {
                            writeln!(
                                w,
                                "  Active from epoch {} to {}: Δ {}",
                                epoch_start, epoch_end, delta
                            )
                            .unwrap();
                            total += *delta;
                            let epoch_end: Epoch = (*epoch_end).into();
                            if epoch >= epoch_end {
                                withdrawable += *delta;
                            }
                        }
                    }
                    if withdrawable != 0.into() {
                        writeln!(w, "Withdrawable total: {}", withdrawable)
                            .unwrap();
                    }
                    writeln!(w, "Unbonded total: {}", total).unwrap();
                }

                if bonds.is_none() && unbonds.is_none() {
                    writeln!(
                        w,
                        "No self-bonds found for validator {}",
                        validator
                    )
                    .unwrap();
                }
            }
            (Some(owner), None) => {
                // Find owner's bonds to any validator
                let bonds_prefix = pos::bonds_for_source_prefix(owner);
                let bonds = query_storage_prefix::<pos::Bonds>(
                    client.clone(),
                    bonds_prefix,
                )
                .await;
                // Find owner's unbonds to any validator
                let unbonds_prefix = pos::unbonds_for_source_prefix(owner);
                let unbonds =
                    query_storage_prefix::<pos::Bonds>(client, unbonds_prefix)
                        .await;

                let stdout = io::stdout();
                let mut w = stdout.lock();

                let mut total: token::Amount = 0.into();
                let mut total_active: token::Amount = 0.into();
                let mut any_bonds = false;
                for (key, bonds) in bonds {
                    match pos::is_bond_key(&key) {
                        Some(pos::BondId { source, validator }) => {
                            any_bonds = true;
                            let bond_type: Cow<str> = if source == validator {
                                "Self-bonds".into()
                            } else {
                                format!("Delegations from {}", source).into()
                            };
                            writeln!(w, "{}:", bond_type).unwrap();
                            let mut current_total: token::Amount = 0.into();
                            for deltas in bonds.iter() {
                                for (epoch_start, delta) in
                                    deltas.delta.iter().sorted()
                                {
                                    writeln!(
                                        w,
                                        "  Active from epoch {}: Δ {}",
                                        epoch_start, delta
                                    )
                                    .unwrap();
                                    current_total += *delta;
                                    total += *delta;
                                    let epoch_start: Epoch =
                                        (*epoch_start).into();
                                    if epoch >= epoch_start {
                                        total_active += *delta;
                                    }
                                }
                            }
                            writeln!(
                                w,
                                "  Bonded total from {}: {}",
                                source, current_total
                            )
                            .unwrap();
                        }
                        None => panic!("Unexpected storage key {}", key),
                    }
                }
                if total_active != 0.into() && total_active != total {
                    writeln!(w, "Active bonds total: {}", total_active)
                        .unwrap();
                }

                let mut total: token::Amount = 0.into();
                let mut total_withdrawable: token::Amount = 0.into();
                for (key, bonds) in unbonds {
                    match pos::is_unbond_key(&key) {
                        Some(pos::BondId { source, validator }) => {
                            any_bonds = true;
                            let bond_type: Cow<str> = if source == validator {
                                "Unbonded self-bonds".into()
                            } else {
                                format!("Unbonded delegations from {}", source)
                                    .into()
                            };
                            writeln!(w, "{}:", bond_type).unwrap();
                            let mut current_total: token::Amount = 0.into();
                            for deltas in bonds.iter() {
                                for (epoch_start, delta) in
                                    deltas.delta.iter().sorted()
                                {
                                    writeln!(
                                        w,
                                        "  Withdrawable from epoch {}: Δ {}",
                                        epoch_start, delta
                                    )
                                    .unwrap();
                                    current_total += *delta;
                                    total += *delta;
                                    let epoch_start: Epoch =
                                        (*epoch_start).into();
                                    if epoch >= epoch_start {
                                        total_withdrawable += *delta;
                                    }
                                }
                            }
                            writeln!(
                                w,
                                "  Unbonded total from {}: {}",
                                source, current_total
                            )
                            .unwrap();
                        }
                        None => panic!("Unexpected storage key {}", key),
                    }
                }
                if total_withdrawable != 0.into() {
                    writeln!(w, "Withdrawable total: {}", total_withdrawable)
                        .unwrap();
                }

                if !any_bonds {
                    writeln!(
                        w,
                        "No self-bonds or delegations found for {}",
                        owner
                    )
                    .unwrap();
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
                    client,
                    unbonds_prefix,
                )
                .await;

                let stdout = io::stdout();
                let mut w = stdout.lock();

                let mut total: token::Amount = 0.into();
                let mut total_active: token::Amount = 0.into();
                for (key, bonds) in bonds {
                    match pos::is_bond_key(&key) {
                        Some(pos::BondId { source, validator }) => {
                            let bond_type = if source == validator {
                                format!("Self-bonds for {}", validator)
                            } else {
                                format!(
                                    "Delegations from {} to validator {}",
                                    source, validator
                                )
                            };
                            writeln!(w, "{}:", bond_type).unwrap();
                            let mut current_total: token::Amount = 0.into();
                            for deltas in bonds.iter() {
                                for (epoch_start, delta) in
                                    deltas.delta.iter().sorted()
                                {
                                    writeln!(
                                        w,
                                        "  Active from epoch {}: Δ {}",
                                        epoch_start, delta
                                    )
                                    .unwrap();
                                    current_total += *delta;
                                    total += *delta;
                                    let epoch_start: Epoch =
                                        (*epoch_start).into();
                                    if epoch >= epoch_start {
                                        total_active += *delta;
                                    }
                                }
                            }
                            writeln!(
                                w,
                                "  Bond total from {}: {}",
                                source, current_total
                            )
                            .unwrap();
                        }
                        None => panic!("Unexpected storage key {}", key),
                    }
                }
                if total_active != 0.into() && total_active != total {
                    writeln!(w, "Bond total active: {}", total_active).unwrap();
                }
                writeln!(w, "Bond total: {}", total).unwrap();

                let mut total: token::Amount = 0.into();
                let mut total_withdrawable: token::Amount = 0.into();
                for (key, unbonds) in unbonds {
                    match pos::is_unbond_key(&key) {
                        Some(pos::BondId { source, validator }) => {
                            let bond_type = if source == validator {
                                format!("Unbonded self-bonds for {}", validator)
                            } else {
                                format!(
                                    "Unbonded delegations from {} to \
                                     validator {}",
                                    source, validator
                                )
                            };
                            writeln!(w, "{}:", bond_type).unwrap();
                            let mut current_total: token::Amount = 0.into();
                            for deltas in unbonds.iter() {
                                for ((epoch_start, epoch_end), delta) in
                                    deltas.deltas.iter().sorted()
                                {
                                    writeln!(
                                        w,
                                        "  Withdrawable from epoch {}: Δ {}",
                                        epoch_start, delta
                                    )
                                    .unwrap();
                                    current_total += *delta;
                                    total += *delta;
                                    let epoch_end: Epoch = (*epoch_end).into();
                                    if epoch >= epoch_end {
                                        total_withdrawable += *delta;
                                    }
                                }
                            }
                            writeln!(
                                w,
                                "  Unbonded total from {}: {}",
                                source, current_total
                            )
                            .unwrap();
                        }
                        None => panic!("Unexpected storage key {}", key),
                    }
                }
                if total_withdrawable != 0.into() && total_withdrawable != total
                {
                    writeln!(w, "Withdrawable total: {}", total_withdrawable)
                        .unwrap();
                }
                writeln!(w, "Unbonded total: {}", total).unwrap();
            }
        }
    }
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
    std::process::exit(1)
}

/// Query a range of storage values with a matching prefix and decode them with
/// [`BorshDeserialize`]. Returns an iterator of the storage keys paired with
/// their associated values.
async fn query_storage_prefix<T>(
    client: HttpClient,
    key: storage::Key,
) -> impl Iterator<Item = (storage::Key, T)>
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
                    return values.into_iter().filter_map(decode);
                }
                Err(err) => eprintln!("Error decoding the values: {}", err),
            }
        }
        tendermint::abci::Code::Err(err) => eprintln!(
            "Error in the query {} (error code {})",
            response.info, err
        ),
    }
    std::process::exit(1);
}
