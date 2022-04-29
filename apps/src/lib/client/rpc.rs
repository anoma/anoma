//! Client RPC queries

use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::{self, Write};

use anoma::ledger::governance::storage as gov_storage;
use anoma::ledger::pos::types::{
    Epoch as PosEpoch, VotingPower, WeightedValidator,
};
use anoma::ledger::pos::{
    self, is_validator_slashes_key, BondId, Bonds, Slash, Unbonds,
};
use anoma::types::address::Address;
use anoma::types::governance::{ProposalVote, TallyResult};
use anoma::types::key::*;
use anoma::types::storage::{Epoch, PrefixValue};
use anoma::types::token::{balance_key, Amount};
use anoma::types::{address, storage, token};
use borsh::BorshDeserialize;
use itertools::Itertools;
#[cfg(not(feature = "ABCI"))]
use tendermint::abci::Code;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::net::Address as TendermintAddress;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::error::Error as TError;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::query::Query;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::{Client, HttpClient};
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::{Order, SubscriptionClient, WebSocketClient};
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::error::Error as TError;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::query::Query;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::{Client, HttpClient};
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::{Order, SubscriptionClient, WebSocketClient};
#[cfg(feature = "ABCI")]
use tendermint_stable::abci::Code;

use crate::cli::{self, args, Context};
use crate::client::tx::TxResponse;
use crate::node::ledger::rpc::Path;

/// Query the epoch of the last committed block
pub async fn query_epoch(args: args::Query) -> Epoch {
    let client = HttpClient::new(args.ledger_address).unwrap();
    let path = Path::Epoch;
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .unwrap();
    match response.code {
        Code::Ok => match Epoch::try_from_slice(&response.value[..]) {
            Ok(epoch) => {
                println!("Last committed epoch: {}", epoch);
                return epoch;
            }

            Err(err) => {
                eprintln!("Error decoding the epoch value: {}", err)
            }
        },
        Code::Err(err) => eprintln!(
            "Error in the query {} (error code {})",
            response.info, err
        ),
    }
    cli::safe_exit(1)
}

/// Query the raw bytes of given storage key
pub async fn query_raw_bytes(_ctx: Context, args: args::QueryRawBytes) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    let path = Path::Value(args.storage_key);
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .unwrap();
    match response.code {
        Code::Ok => {
            println!("{}", hex::encode(&response.value));
        }
        Code::Err(err) => {
            eprintln!(
                "Error in the query {}  (error code {})",
                response.info, err
            );
            cli::safe_exit(1)
        }
    }
}

/// Query token balance(s)
pub async fn query_balance(ctx: Context, args: args::QueryBalance) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    let tokens = address::tokens();
    match (args.token, args.owner) {
        (Some(token), Some(owner)) => {
            let token = ctx.get(&token);
            let owner = ctx.get(&owner);
            let key = token::balance_key(&token, &owner);
            let currency_code = tokens
                .get(&token)
                .map(|c| Cow::Borrowed(*c))
                .unwrap_or_else(|| Cow::Owned(token.to_string()));
            match query_storage_value::<token::Amount>(&client, &key).await {
                Some(balance) => {
                    println!("{}: {}", currency_code, balance);
                }
                None => {
                    println!("No {} balance found for {}", currency_code, owner)
                }
            }
        }
        (None, Some(owner)) => {
            let owner = ctx.get(&owner);
            let mut found_any = false;
            for (token, currency_code) in tokens {
                let key = token::balance_key(&token, &owner);
                if let Some(balance) =
                    query_storage_value::<token::Amount>(&client, &key).await
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
            let token = ctx.get(&token);
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
                None => {
                    println!("No balances for token {}", token.encode())
                }
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

/// Query Proposals
pub async fn query_proposal(_ctx: Context, args: args::QueryProposal) {
    async fn print_proposal(
        client: &HttpClient,
        id: u64,
        details: bool,
    ) -> Option<()> {
        let author_key = gov_storage::get_author_key(id);
        let start_epoch_key = gov_storage::get_voting_start_epoch_key(id);
        let end_epoch_key = gov_storage::get_voting_end_epoch_key(id);

        let author =
            query_storage_value::<Address>(client, &author_key).await?;
        let start_epoch =
            query_storage_value::<Epoch>(client, &start_epoch_key).await?;
        let end_epoch =
            query_storage_value::<Epoch>(client, &end_epoch_key).await?;

        if details {
            let content_key = gov_storage::get_content_key(id);
            let grace_epoch_key = gov_storage::get_grace_epoch_key(id);
            let content = query_storage_value::<HashMap<String, String>>(
                client,
                &content_key,
            )
            .await?;
            let grace_epoch =
                query_storage_value::<Epoch>(client, &grace_epoch_key).await?;

            println!("Proposal: {}", id);
            println!("{:4}Author: {}", "", author);
            println!("{:4}Content:", "");
            for (key, value) in &content {
                println!("{:8}{}: {}", "", key, value);
            }
            println!("{:4}Start Epoch: {}", "", start_epoch);
            println!("{:4}End Epoch: {}", "", end_epoch);
            println!("{:4}Grace Epoch: {}", "", grace_epoch);
            println!(
                "{:4}Result: {}",
                "",
                compute_tally(client, start_epoch, id).await
            );
        } else {
            println!("Proposal: {}", id);
            println!("{:4}Author: {}", "", author);
            println!("{:4}Start Epoch: {}", "", start_epoch);
            println!("{:4}End Epoch: {}", "", end_epoch);
        }

        Some(())
    }

    let client = HttpClient::new(args.query.ledger_address.clone()).unwrap();
    match args.proposal_id {
        Some(id) => {
            if print_proposal(&client, id, true).await.is_none() {
                eprintln!("No valid proposal was found with id {}", id)
            }
        }
        None => {
            let last_proposal_id_key = gov_storage::get_counter_key();
            let last_proposal_id =
                query_storage_value::<u64>(&client, &last_proposal_id_key)
                    .await
                    .unwrap();

            for id in 0..last_proposal_id {
                if print_proposal(&client, id, false).await.is_none() {
                    eprintln!("No valid proposal was found with id {}", id)
                };
            }
        }
    }
}

/// Query token amount of owner.
pub async fn get_token_balance(
    client: &HttpClient,
    token: &Address,
    owner: &Address,
) -> Option<Amount> {
    let balance_key = balance_key(token, owner);
    query_storage_value(client, &balance_key).await
}

/// Query PoS bond(s)
pub async fn query_bonds(ctx: Context, args: args::QueryBonds) {
    let epoch = query_epoch(args.query.clone()).await;
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    match (args.owner, args.validator) {
        (Some(owner), Some(validator)) => {
            let source = ctx.get(&owner);
            let validator = ctx.get(&validator);
            // Find owner's delegations to the given validator
            let bond_id = pos::BondId { source, validator };
            let bond_key = pos::bond_key(&bond_id);
            let bonds =
                query_storage_value::<pos::Bonds>(&client, &bond_key).await;
            // Find owner's unbonded delegations from the given
            // validator
            let unbond_key = pos::unbond_key(&bond_id);
            let unbonds =
                query_storage_value::<pos::Unbonds>(&client, &unbond_key).await;
            // Find validator's slashes, if any
            let slashes_key = pos::validator_slashes_key(&bond_id.validator);
            let slashes =
                query_storage_value::<pos::Slashes>(&client, &slashes_key)
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
            let validator = ctx.get(&validator);
            // Find validator's self-bonds
            let bond_id = pos::BondId {
                source: validator.clone(),
                validator,
            };
            let bond_key = pos::bond_key(&bond_id);
            let bonds =
                query_storage_value::<pos::Bonds>(&client, &bond_key).await;
            // Find validator's unbonded self-bonds
            let unbond_key = pos::unbond_key(&bond_id);
            let unbonds =
                query_storage_value::<pos::Unbonds>(&client, &unbond_key).await;
            // Find validator's slashes, if any
            let slashes_key = pos::validator_slashes_key(&bond_id.validator);
            let slashes =
                query_storage_value::<pos::Slashes>(&client, &slashes_key)
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
            let owner = ctx.get(&owner);
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
                            let slashes = query_storage_value::<pos::Slashes>(
                                &client,
                                &slashes_key,
                            )
                            .await
                            .unwrap_or_default();

                            let stdout = io::stdout();
                            let mut w = stdout.lock();
                            any_bonds = true;
                            let bond_type: Cow<str> = if source == validator {
                                "Self-bonds".into()
                            } else {
                                format!(
                                    "Delegations from {} to {}",
                                    source, validator
                                )
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
                        None => {
                            panic!("Unexpected storage key {}", key)
                        }
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
                            let slashes = query_storage_value::<pos::Slashes>(
                                &client,
                                &slashes_key,
                            )
                            .await
                            .unwrap_or_default();

                            let stdout = io::stdout();
                            let mut w = stdout.lock();
                            any_bonds = true;
                            let bond_type: Cow<str> = if source == validator {
                                "Unbonded self-bonds".into()
                            } else {
                                format!("Unbonded delegations from {}", source)
                                    .into()
                            };
                            writeln!(w, "{}:", bond_type).unwrap();
                            let (tot, tot_withdrawable) = process_unbonds_query(
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
                        None => {
                            panic!("Unexpected storage key {}", key)
                        }
                    }
                }
            }
            if total_withdrawable != 0.into() {
                println!("Withdrawable total: {}", total_withdrawable);
            }

            if !any_bonds {
                println!("No self-bonds or delegations found for {}", owner);
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
                            let slashes = query_storage_value::<pos::Slashes>(
                                &client,
                                &slashes_key,
                            )
                            .await
                            .unwrap_or_default();

                            let stdout = io::stdout();
                            let mut w = stdout.lock();
                            let bond_type = if source == validator {
                                format!("Self-bonds for {}", validator.encode())
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
                        None => {
                            panic!("Unexpected storage key {}", key)
                        }
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
                            let slashes = query_storage_value::<pos::Slashes>(
                                &client,
                                &slashes_key,
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
                            let (tot, tot_withdrawable) = process_unbonds_query(
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
                        None => {
                            panic!("Unexpected storage key {}", key)
                        }
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

/// Query PoS voting power
pub async fn query_voting_power(ctx: Context, args: args::QueryVotingPower) {
    let epoch = match args.epoch {
        Some(epoch) => epoch,
        None => query_epoch(args.query.clone()).await,
    };
    let client = HttpClient::new(args.query.ledger_address).unwrap();

    // Find the validator set
    let validator_set_key = pos::validator_set_key();
    let validator_sets =
        query_storage_value::<pos::ValidatorSets>(&client, &validator_set_key)
            .await
            .expect("Validator set should always be set");
    let validator_set = validator_sets
        .get(epoch)
        .expect("Validator set should be always set in the current epoch");
    match args.validator {
        Some(validator) => {
            let validator = ctx.get(&validator);
            // Find voting power for the given validator
            let voting_power_key = pos::validator_voting_power_key(&validator);
            let voting_powers =
                query_storage_value::<pos::ValidatorVotingPowers>(
                    &client,
                    &voting_power_key,
                )
                .await;
            match voting_powers.and_then(|data| data.get(epoch)) {
                Some(voting_power_delta) => {
                    let voting_power: VotingPower =
                        voting_power_delta.try_into().expect(
                            "The sum voting power deltas shouldn't be negative",
                        );
                    let weighted = WeightedValidator {
                        address: validator.clone(),
                        voting_power,
                    };
                    let is_active = validator_set.active.contains(&weighted);
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
                None => {
                    println!("No voting power found for {}", validator.encode())
                }
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
    let total_voting_powers = query_storage_value::<pos::TotalVotingPowers>(
        &client,
        &total_voting_power_key,
    )
    .await
    .expect("Total voting power should always be set");
    let total_voting_power = total_voting_powers
        .get(epoch)
        .expect("Total voting power should be always set in the current epoch");
    println!("Total voting power: {}", total_voting_power);
}

/// Query PoS slashes
pub async fn query_slashes(ctx: Context, args: args::QuerySlashes) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    match args.validator {
        Some(validator) => {
            let validator = ctx.get(&validator);
            // Find slashes for the given validator
            let slashes_key = pos::validator_slashes_key(&validator);
            let slashes =
                query_storage_value::<pos::Slashes>(&client, &slashes_key)
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

/// Dry run a transaction
pub async fn dry_run_tx(ledger_address: &TendermintAddress, tx_bytes: Vec<u8>) {
    let client = HttpClient::new(ledger_address.clone()).unwrap();
    let path = Path::DryRunTx;
    let response = client
        .abci_query(Some(path.into()), tx_bytes, None, false)
        .await
        .unwrap();
    println!("{:#?}", response);
}

/// Get account's public key stored in its storage sub-space
pub async fn get_public_key(
    address: &Address,
    ledger_address: TendermintAddress,
) -> Option<common::PublicKey> {
    let client = HttpClient::new(ledger_address).unwrap();
    let key = pk_key(address);
    query_storage_value(&client, &key).await
}

/// Check if the given address is a known validator.
pub async fn is_validator(
    address: &Address,
    ledger_address: TendermintAddress,
) -> bool {
    let client = HttpClient::new(ledger_address).unwrap();
    // Check if there's any validator state
    let key = pos::validator_state_key(address);
    // We do not need to decode it
    let state: Option<pos::ValidatorStates> =
        query_storage_value(&client, &key).await;
    // If there is, then the address is a validator
    state.is_some()
}

/// Check if the address exists on chain. Established address exists if it has a
/// stored validity predicate. Implicit and internal addresses always return
/// true.
pub async fn known_address(
    address: &Address,
    ledger_address: TendermintAddress,
) -> bool {
    let client = HttpClient::new(ledger_address).unwrap();
    match address {
        Address::Established(_) => {
            // Established account exists if it has a VP
            let key = storage::Key::validity_predicate(address);
            query_has_storage_key(client, key).await
        }
        Address::Implicit(_) | Address::Internal(_) => true,
    }
}

/// Accumulate slashes starting from `epoch_start` until (optionally)
/// `withdraw_epoch` and apply them to the token amount `delta`.
fn apply_slashes(
    slashes: &[Slash],
    mut delta: token::Amount,
    epoch_start: PosEpoch,
    withdraw_epoch: Option<PosEpoch>,
    mut w: Option<&mut std::io::StdoutLock>,
) -> token::Amount {
    let mut slashed = token::Amount::default();
    for slash in slashes {
        if slash.epoch >= epoch_start
            && slash.epoch < withdraw_epoch.unwrap_or_else(|| u64::MAX.into())
        {
            if let Some(w) = w.as_mut() {
                writeln!(
                    *w,
                    "    ⚠ Slash: {} from epoch {}",
                    slash.rate, slash.epoch
                )
                .unwrap();
            }
            let raw_delta: u64 = delta.into();
            let current_slashed = token::Amount::from(slash.rate * raw_delta);
            slashed += current_slashed;
            delta -= current_slashed;
        }
    }
    if let Some(w) = w.as_mut() {
        if slashed != 0.into() {
            writeln!(*w, "    ⚠ Slash total: {}", slashed).unwrap();
            writeln!(*w, "    ⚠ After slashing: Δ {}", delta).unwrap();
        }
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
            delta = apply_slashes(slashes, delta, *epoch_start, None, Some(w));
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
                Some(w),
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

/// Query a storage value and decode it with [`BorshDeserialize`].
pub async fn query_storage_value<T>(
    client: &HttpClient,
    key: &storage::Key,
) -> Option<T>
where
    T: BorshDeserialize,
{
    let path = Path::Value(key.to_owned());
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .unwrap();
    match response.code {
        Code::Ok => match T::try_from_slice(&response.value[..]) {
            Ok(value) => return Some(value),
            Err(err) => eprintln!("Error decoding the value: {}", err),
        },
        Code::Err(err) => {
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
pub async fn query_storage_prefix<T>(
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
        Code::Ok => {
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
        Code::Err(err) => {
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

/// Query to check if the given storage key exists.
pub async fn query_has_storage_key(
    client: HttpClient,
    key: storage::Key,
) -> bool {
    let path = Path::HasKey(key);
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .unwrap();
    match response.code {
        Code::Ok => match bool::try_from_slice(&response.value[..]) {
            Ok(value) => return value,
            Err(err) => eprintln!("Error decoding the value: {}", err),
        },
        Code::Err(err) => {
            eprintln!(
                "Error in the query {} (error code {})",
                response.info, err
            )
        }
    }
    cli::safe_exit(1)
}

/// Represents a query for an event pertaining to the specified transaction
#[derive(Debug, Clone)]
pub enum TxEventQuery {
    Accepted(String),
    Applied(String),
}

impl TxEventQuery {
    /// The event type to which this event query pertains
    fn event_type(&self) -> &'static str {
        match self {
            TxEventQuery::Accepted(_tx_hash) => "accepted",
            TxEventQuery::Applied(_tx_hash) => "applied",
        }
    }

    /// The transaction to which this event query pertains
    fn tx_hash(&self) -> &String {
        match self {
            TxEventQuery::Accepted(tx_hash) => tx_hash,
            TxEventQuery::Applied(tx_hash) => tx_hash,
        }
    }
}

/// Transaction event queries are semantically a subset of general queries
impl From<TxEventQuery> for Query {
    fn from(tx_query: TxEventQuery) -> Self {
        match tx_query {
            TxEventQuery::Accepted(tx_hash) => {
                Query::default().and_eq("accepted.hash", tx_hash)
            }
            TxEventQuery::Applied(tx_hash) => {
                Query::default().and_eq("applied.hash", tx_hash)
            }
        }
    }
}

/// Lookup the full response accompanying the specified transaction event
pub async fn query_tx_response(
    ledger_address: &TendermintAddress,
    tx_query: TxEventQuery,
) -> Result<TxResponse, TError> {
    // Connect to the Tendermint server holding the transactions
    let (client, driver) = WebSocketClient::new(ledger_address.clone()).await?;
    let driver_handle = tokio::spawn(async move { driver.run().await });
    // Find all blocks that apply a transaction with the specified hash
    let blocks = &client
        .block_search(Query::from(tx_query.clone()), 1, 255, Order::Ascending)
        .await
        .expect("Unable to query for transaction with given hash")
        .blocks;
    // Get the block results corresponding to a block to which
    // the specified transaction belongs
    let block = &blocks
        .get(0)
        .ok_or_else(|| {
            TError::server(
                "Unable to find a block applying the given transaction"
                    .to_string(),
            )
        })?
        .block;
    let response_block_results = client
        .block_results(block.header.height)
        .await
        .expect("Unable to retrieve block containing transaction");
    // Search for the event where the specified transaction is
    // applied to the blockchain
    let query_event_opt =
        response_block_results.end_block_events.and_then(|events| {
            (&events)
                .iter()
                .find(|event| {
                    event.type_str == tx_query.event_type()
                        && (&event.attributes).iter().any(|tag| {
                            tag.key.as_ref() == "hash"
                                && tag.value.as_ref() == tx_query.tx_hash()
                        })
                })
                .cloned()
        });
    let query_event = query_event_opt.ok_or_else(|| {
        TError::server(
            "Unable to find the event corresponding to the specified \
             transaction"
                .to_string(),
        )
    })?;
    // Reformat the event attributes so as to ease value extraction
    let event_map: std::collections::HashMap<&str, &str> = (&query_event
        .attributes)
        .iter()
        .map(|tag| (tag.key.as_ref(), tag.value.as_ref()))
        .collect();
    // Summarize the transaction results that we were searching for
    let result = TxResponse {
        info: event_map["info"].to_string(),
        log: event_map["log"].to_string(),
        height: event_map["height"].to_string(),
        hash: event_map["hash"].to_string(),
        code: event_map["code"].to_string(),
        gas_used: event_map["gas_used"].to_string(),
        initialized_accounts: serde_json::from_str(
            event_map["initialized_accounts"],
        )
        .unwrap_or_default(),
    };
    // Signal to the driver to terminate.
    client.close()?;
    // Await the driver's termination to ensure proper connection closure.
    let _ = driver_handle.await.unwrap_or_else(|x| {
        eprintln!("{}", x);
        cli::safe_exit(1)
    });
    Ok(result)
}

/// Lookup the results of applying the specified transaction to the
/// blockchain.

pub async fn query_result(_ctx: Context, args: args::QueryResult) {
    // First try looking up application event pertaining to given hash.
    let tx_response = query_tx_response(
        &args.query.ledger_address,
        TxEventQuery::Applied(args.tx_hash.clone()),
    )
    .await;
    match tx_response {
        Ok(result) => {
            println!(
                "Transaction was applied with result: {}",
                serde_json::to_string_pretty(&result).unwrap()
            )
        }
        Err(err1) => {
            // If this fails then instead look for an acceptance event.
            let tx_response = query_tx_response(
                &args.query.ledger_address,
                TxEventQuery::Accepted(args.tx_hash),
            )
            .await;
            match tx_response {
                Ok(result) => println!(
                    "Transaction was accepted with result: {}",
                    serde_json::to_string_pretty(&result).unwrap()
                ),
                Err(err2) => {
                    // Print the errors that caused the lookups to fail
                    eprintln!("{}\n{}", err1, err2);
                    cli::safe_exit(1)
                }
            }
        }
    }
}

pub async fn compute_tally(
    client: &HttpClient,
    epoch: Epoch,
    proposal_id: u64,
) -> TallyResult {
    let active_validators = get_all_active_validators(client, epoch).await;
    let vote_prefix_key = gov_storage::get_proposal_prefix_key(proposal_id);
    let votes =
        query_storage_prefix::<ProposalVote>(client.clone(), vote_prefix_key)
            .await;

    if let Some(votes) = votes {
        let (validator_voters, delegator_voters) = votes.fold(
            (HashMap::new(), HashMap::new()),
            |(mut validator_voters, mut delegator_voters), (key, vote)| {
                let address = gov_storage::get_voter_address(&key)
                    .expect("Vote key should contains an address.")
                    .clone();
                if active_validators.contains_key(&address) {
                    validator_voters.insert(address, vote);
                } else {
                    delegator_voters.insert(address, vote);
                }
                (validator_voters, delegator_voters)
            },
        );

        let mut data_map: HashMap<Address, (Address, token::Amount)> =
            HashMap::new();
        for validator_addr in validator_voters.keys() {
            let bond_amount = get_bond_amount_at(
                client,
                validator_addr.clone(),
                validator_addr.clone(),
                epoch,
            )
            .await
            .expect("Validator self-bond must exist.");
            data_map.insert(
                validator_addr.clone(),
                (validator_addr.clone(), bond_amount),
            );
            for delegator_addr in delegator_voters.keys() {
                match get_bond_amount_at(
                    client,
                    delegator_addr.clone(),
                    validator_addr.clone(),
                    epoch,
                )
                .await
                {
                    Some(bond_amount) => {
                        data_map.insert(
                            delegator_addr.clone(),
                            (validator_addr.clone(), bond_amount),
                        );
                    }
                    None => continue,
                }
            }
        }

        let mut total_stacked_tokens = token::Amount::from(0);

        let mut yay_votes_tokens = token::Amount::whole(0);
        for (addr, vote) in validator_voters.clone() {
            if vote.is_yay() {
                yay_votes_tokens += data_map.get(&addr).unwrap().1;
            }
            let validator_total_deltas = pos::validator_total_deltas_key(&addr);
            let epoched_validator_deltas = query_storage_value::<
                pos::ValidatorTotalDeltas,
            >(
                client, &validator_total_deltas
            )
            .await
            .expect("Validator delta should exist.");
            let amount = epoched_validator_deltas.get(epoch).unwrap();

            total_stacked_tokens += token::Amount::from_change(amount);
        }

        for (addr, vote) in delegator_voters {
            if !data_map.contains_key(&addr) {
                if vote.is_yay() {
                    yay_votes_tokens += data_map.get(&addr).unwrap().1;
                }
            } else {
                let delegator_data = data_map.get(&addr).unwrap();
                let validator_vote =
                    validator_voters.get(&delegator_data.0).unwrap();
                if validator_vote.is_yay() && validator_vote.ne(&vote) {
                    yay_votes_tokens -= delegator_data.1;
                } else {
                    yay_votes_tokens += delegator_data.1;
                }
            }
        }

        if 3 * yay_votes_tokens >= 2 * total_stacked_tokens {
            TallyResult::Passed
        } else {
            TallyResult::Rejected
        }
    } else {
        TallyResult::Unknown
    }
}

pub async fn get_bond_amount_at(
    client: &HttpClient,
    delegator: Address,
    validator: Address,
    epoch: Epoch,
) -> Option<token::Amount> {
    let slashes_key = pos::validator_slashes_key(&validator);
    let slashes = query_storage_value::<pos::Slashes>(client, &slashes_key)
        .await
        .unwrap_or_default();
    let bond_key = pos::bond_key(&BondId {
        source: delegator,
        validator,
    });
    let epoched_bonds = query_storage_value::<Bonds>(client, &bond_key).await;
    match epoched_bonds {
        Some(epoched_bonds) => {
            let mut delegated_amount: token::Amount = 0.into();
            for bond in epoched_bonds.iter() {
                for (epoch_start, &(mut delta)) in bond.deltas.iter().sorted() {
                    delta = apply_slashes(
                        &slashes,
                        delta,
                        *epoch_start,
                        None,
                        None,
                    );
                    let epoch_start: Epoch = (*epoch_start).into();
                    if epoch >= epoch_start {
                        delegated_amount += delta;
                    }
                }
            }
            Some(delegated_amount)
        }
        None => None,
    }
}

pub async fn get_all_active_validators(
    client: &HttpClient,
    epoch: Epoch,
) -> HashMap<Address, VotingPower> {
    let validator_set_key = pos::validator_set_key();
    let validator_sets =
        query_storage_value::<pos::ValidatorSets>(client, &validator_set_key)
            .await
            .expect("Validator set should always be set");
    let validator_set = validator_sets
        .get(epoch)
        .expect("Validator set should be always set in the current epoch");
    validator_set.active.iter().fold(
        HashMap::new(),
        |mut acc, weighted_validator| {
            acc.insert(
                weighted_validator.address.clone(),
                weighted_validator.voting_power,
            );
            acc
        },
    )
}
