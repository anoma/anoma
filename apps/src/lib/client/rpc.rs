//! Client RPC queries

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::fs::File;
use std::io::{self, Write};
use std::iter::Iterator;

use anoma::ledger::governance::storage as gov_storage;
use anoma::ledger::governance::utils::Votes;
use anoma::ledger::parameters::{storage as param_storage, EpochDuration};
use anoma::ledger::pos::types::{
    Epoch as PosEpoch, VotingPower, WeightedValidator,
};
use anoma::ledger::pos::{
    self, is_validator_slashes_key, BondId, Bonds, PosParams, Slash, Unbonds,
};
use anoma::ledger::treasury::storage as treasury_storage;
use anoma::types::address::Address;
use anoma::types::governance::{
    OfflineProposal, OfflineVote, ProposalVote, TallyResult,
};
use anoma::types::key::*;
use anoma::types::masp::BalanceOwner;
use anoma::types::storage::{Epoch, PrefixValue};
use anoma::types::token::{balance_key, Amount};
use anoma::types::{address, storage, token};
use async_std::fs::{self};
use async_std::path::PathBuf;
use async_std::prelude::*;
use borsh::{BorshDeserialize, BorshSerialize};
use itertools::Itertools;
use masp_primitives::asset_type::AssetType;
use masp_primitives::zip32::ExtendedFullViewingKey;
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
pub async fn query_balance(mut ctx: Context, args: args::QueryBalance) {
    // Query the balances of shielded or transparent account types depending on
    // the CLI arguments
    match args.owner.as_ref().map(|x| ctx.get_cached(x)) {
        Some(BalanceOwner::FullViewingKey(_viewing_key)) => {
            query_shielded_balance(&mut ctx, args).await
        }
        Some(BalanceOwner::Address(_owner)) => {
            query_transparent_balance(&mut ctx, args).await
        }
        None => {
            // Print shielded balance
            query_shielded_balance(&mut ctx, args.clone()).await;
            // Then print transparent balance
            query_transparent_balance(&mut ctx, args).await;
        }
    };
}

/// Query token balance(s)
pub async fn query_transparent_balance(
    ctx: &mut Context,
    args: args::QueryBalance,
) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    let tokens = address::tokens();
    match (args.token, args.owner) {
        (Some(token), Some(owner)) => {
            let token = ctx.get(&token);
            let owner = ctx
                .get_cached(&owner)
                .address()
                .expect("a transparent address");
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
            let owner = ctx
                .get_cached(&owner)
                .address()
                .expect("a transparent address");
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

/// Query token shielded balance(s)
pub async fn query_shielded_balance(
    ctx: &mut Context,
    args: args::QueryBalance,
) {
    // Used to control whether balances for all keys or a specific key are
    // printed
    let owner = args
        .owner
        .and_then(|x| ctx.get_cached(&x).full_viewing_key());
    // Viewing keys are used to query shielded balances. If a spending key is
    // provided, then convert to a viewing key first.
    let viewing_keys = match owner {
        Some(viewing_key) => vec![viewing_key],
        None => ctx.wallet.get_viewing_keys().values().copied().collect(),
    };
    // Build up the context that will be queried for balances
    let _ = ctx.shielded.load();
    ctx.shielded
        .fetch(
            &args.query.ledger_address,
            vec![].into_iter(),
            viewing_keys
                .iter()
                .map(|fvk| ExtendedFullViewingKey::from(*fvk).fvk.vk),
        )
        .await;
    // Save the update state so that future fetches can be short-circuited
    let _ = ctx.shielded.save();
    // Map addresses to token names
    let tokens = address::tokens();
    match (args.token, owner.is_some()) {
        // Here the user wants to know the balance for a specific token
        (Some(token), true) => {
            // Query the multi-asset balance at the given spending key
            let viewing_key =
                ExtendedFullViewingKey::from(viewing_keys[0]).fvk.vk;
            let balance = ctx
                .shielded
                .compute_shielded_balance(&viewing_key)
                .expect("context should contain viewing key");
            // Compute the unique asset identifier from the token address
            let token = ctx.get(&token);
            let asset_type = AssetType::new(
                token
                    .try_to_vec()
                    .expect("token addresses should serialize")
                    .as_ref(),
            )
            .unwrap();
            let currency_code = tokens
                .get(&token)
                .map(|c| Cow::Borrowed(*c))
                .unwrap_or_else(|| Cow::Owned(token.to_string()));
            if balance[&asset_type] == 0 {
                println!(
                    "No shielded {} balance found for given key",
                    currency_code
                );
            } else {
                let asset_value =
                    token::Amount::from(balance[&asset_type] as u64);
                println!("{}: {}", currency_code, asset_value);
            }
        }
        // Here the user wants to know the balance of all tokens across users
        (None, false) => {
            // Maps asset types to balances divided by viewing key
            let mut balances = HashMap::new();
            for fvk in viewing_keys {
                // Query the multi-asset balance at the given spending key
                let viewing_key = ExtendedFullViewingKey::from(fvk).fvk.vk;
                let balance = ctx
                    .shielded
                    .compute_shielded_balance(&viewing_key)
                    .expect("context should contain viewing key");
                for (asset_type, value) in balance.components() {
                    if !balances.contains_key(asset_type) {
                        balances.insert(*asset_type, Vec::new());
                    }
                    balances.get_mut(asset_type).unwrap().push((fvk, *value));
                }
            }

            // These are the asset types for which we have human-readable names
            let mut readable_tokens = HashSet::new();
            // Print those balances corresponding to human-readable token names
            for (token, currency_code) in tokens {
                println!("Shielded Token {}:", currency_code);
                // Compute the unique asset identifier from the token address
                let asset_type = AssetType::new(
                    token
                        .try_to_vec()
                        .expect("token addresses should serialize")
                        .as_ref(),
                )
                .unwrap();
                // Take note of the asset types corresponding to readable tokens
                readable_tokens.insert(asset_type);
                let mut found_any = false;
                if balances.contains_key(&asset_type) {
                    for (fvk, asset_value) in &balances[&asset_type] {
                        let asset_value =
                            token::Amount::from(*asset_value as u64);
                        println!("  {}, owned by {}", asset_value, fvk);
                        found_any = true;
                    }
                }
                if !found_any {
                    println!(
                        "No shielded {} balance found for any wallet key",
                        currency_code
                    );
                }
            }
            // Print those balances that do not correspond to human-readable
            // token names
            for (asset_type, balances) in balances {
                if !readable_tokens.contains(&asset_type) {
                    println!("Shielded Token {}:", asset_type);
                    let mut found_any = false;
                    for (fvk, value) in balances {
                        let value = token::Amount::from(value as u64);
                        println!("{}: {}", fvk, value);
                        found_any = true;
                    }
                    if !found_any {
                        println!(
                            "No shielded {} balance found for any wallet key",
                            asset_type
                        );
                    }
                }
            }
        }
        // Here the user wants to know the balance for a specific token across
        // users
        (Some(token), false) => {
            // Compute the unique asset identifier from the token address
            let token = ctx.get(&token);
            let asset_type = AssetType::new(
                token
                    .try_to_vec()
                    .expect("token addresses should serialize")
                    .as_ref(),
            )
            .unwrap();
            let currency_code = tokens
                .get(&token)
                .map(|c| Cow::Borrowed(*c))
                .unwrap_or_else(|| Cow::Owned(token.to_string()));
            println!("Shielded Token {}:", currency_code);
            let mut found_any = false;
            for fvk in viewing_keys {
                // Query the multi-asset balance at the given spending key
                let viewing_key = ExtendedFullViewingKey::from(fvk).fvk.vk;
                let balance = ctx
                    .shielded
                    .compute_shielded_balance(&viewing_key)
                    .expect("context should contain viewing key");
                if balance[&asset_type] != 0 {
                    let asset_value =
                        token::Amount::from(balance[&asset_type] as u64);
                    println!("  {}, owned by {}", asset_value, fvk);
                    found_any = true;
                }
            }
            if !found_any {
                println!(
                    "No shielded {} balance found for any wallet key",
                    currency_code
                );
            }
        }
        // Here the user wants to know all possible token balances for a key
        (None, true) => {
            // Query the multi-asset balance at the given spending key
            let viewing_key =
                ExtendedFullViewingKey::from(viewing_keys[0]).fvk.vk;
            let mut balance = ctx
                .shielded
                .compute_shielded_balance(&viewing_key)
                .expect("context should contain viewing key");
            let mut found_any = false;
            // Print those balances corresponding to human-readable token names
            for (token, currency_code) in tokens {
                // Compute the unique asset identifier from the token address
                let asset_type = AssetType::new(
                    token
                        .try_to_vec()
                        .expect("token addresses should serialize")
                        .as_ref(),
                )
                .unwrap();
                let asset_value = balance[&asset_type];
                if asset_value > 0 {
                    balance -= balance.project(asset_type);
                    let asset_value = token::Amount::from(asset_value as u64);
                    println!("{}: {}", currency_code, asset_value);
                    found_any = true;
                }
            }
            // Now print those balances that do not correspond to human-readable
            // token names
            for (asset_type, value) in balance.components() {
                let value = token::Amount::from(*value as u64);
                println!("{}: {}", asset_type, value);
                found_any = true;
            }
            if !found_any {
                println!("No shielded balance found for given key");
            }
        }
    }
}

/// Query Proposals
pub async fn query_proposal(_ctx: Context, args: args::QueryProposal) {
    async fn print_proposal(
        client: &HttpClient,
        id: u64,
        current_epoch: Epoch,
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
            if start_epoch > current_epoch {
                println!("{:4}Status: pending", "");
            } else if start_epoch <= current_epoch && current_epoch <= end_epoch
            {
                println!("{:4}Status: on-going", "");
            } else {
                let votes = get_proposal_votes(client, start_epoch, id).await;
                let proposal_result =
                    compute_tally(client, start_epoch, votes).await;
                println!("{:4}Status: done", "");
                println!("{:4}Result: {}", "", proposal_result);
            }
        } else {
            println!("Proposal: {}", id);
            println!("{:4}Author: {}", "", author);
            println!("{:4}Start Epoch: {}", "", start_epoch);
            println!("{:4}End Epoch: {}", "", end_epoch);
            if start_epoch > current_epoch {
                println!("{:4}Status: pending", "");
            } else if start_epoch <= current_epoch && current_epoch <= end_epoch
            {
                println!("{:4}Status: on-going", "");
            } else {
                println!("{:4}Status: done", "");
            }
        }

        Some(())
    }

    let client = HttpClient::new(args.query.ledger_address.clone()).unwrap();
    let current_epoch = query_epoch(args.query.clone()).await;
    match args.proposal_id {
        Some(id) => {
            if print_proposal(&client, id, current_epoch, true)
                .await
                .is_none()
            {
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
                if print_proposal(&client, id, current_epoch, false)
                    .await
                    .is_none()
                {
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

pub async fn query_proposal_result(
    _ctx: Context,
    args: args::QueryProposalResult,
) {
    let client = HttpClient::new(args.query.ledger_address.clone()).unwrap();
    let current_epoch = query_epoch(args.query.clone()).await;

    match args.proposal_id {
        Some(id) => {
            let start_epoch_key = gov_storage::get_voting_start_epoch_key(id);
            let end_epoch_key = gov_storage::get_voting_end_epoch_key(id);
            let start_epoch =
                query_storage_value::<Epoch>(&client, &start_epoch_key).await;
            let end_epoch =
                query_storage_value::<Epoch>(&client, &end_epoch_key).await;

            match (start_epoch, end_epoch) {
                (Some(start_epoch), Some(end_epoch)) => {
                    if current_epoch > end_epoch {
                        let votes =
                            get_proposal_votes(&client, start_epoch, id).await;
                        let proposal_result =
                            compute_tally(&client, start_epoch, votes).await;
                        println!("Proposal: {}", id);
                        println!("{:4}Result: {}", "", proposal_result);
                    } else {
                        eprintln!("Proposal is still in progress.");
                        cli::safe_exit(1)
                    }
                }
                _ => {
                    eprintln!("Error while retriving proposal.");
                    cli::safe_exit(1)
                }
            }
        }
        None => {
            if args.offline {
                match args.proposal_folder {
                    Some(path) => {
                        let mut dir = fs::read_dir(&path)
                            .await
                            .expect("Should be able to read the directory.");
                        let mut files = HashSet::new();
                        let mut is_proposal_present = false;

                        while let Some(entry) = dir.next().await {
                            match entry {
                                Ok(entry) => match entry.file_type().await {
                                    Ok(entry_stat) => {
                                        if entry_stat.is_file() {
                                            if entry.file_name().eq(&"proposal")
                                            {
                                                is_proposal_present = true
                                            } else {
                                                files.insert(entry.path());
                                            }
                                        }
                                    }
                                    Err(e) => {
                                        eprintln!(
                                            "Can't read entry type: {}.",
                                            e
                                        );
                                        cli::safe_exit(1)
                                    }
                                },
                                Err(e) => {
                                    eprintln!("Can't read entry: {}.", e);
                                    cli::safe_exit(1)
                                }
                            }
                        }

                        if !is_proposal_present {
                            eprintln!(
                                "The folder must contain a the offline \
                                 proposal in a file named proposal"
                            );
                            cli::safe_exit(1)
                        }

                        let file = File::open(&path.join("proposal"))
                            .expect("Proposal file must exist.");
                        let proposal: OfflineProposal =
                            serde_json::from_reader(file).expect(
                                "JSON was not well-formatted for proposal.",
                            );

                        let public_key = get_public_key(
                            &proposal.address,
                            args.query.ledger_address.clone(),
                        )
                        .await
                        .expect("Public key should exist.");

                        if !proposal.check_signature(&public_key) {
                            eprintln!("Bad proposal signature.");
                            cli::safe_exit(1)
                        }

                        let votes = get_proposal_offline_votes(
                            &client,
                            proposal.clone(),
                            files,
                        )
                        .await;
                        let proposal_result =
                            compute_tally(&client, proposal.tally_epoch, votes)
                                .await;

                        println!("{:4}Result: {}", "", proposal_result);
                    }
                    None => {
                        eprintln!(
                            "Offline flag must be followed by data-path."
                        );
                        cli::safe_exit(1)
                    }
                };
            } else {
                eprintln!("Either id or offline should be used as arguments.");
                cli::safe_exit(1)
            }
        }
    }
}

pub async fn query_protocol_parameters(
    _ctx: Context,
    args: args::QueryProtocolParameters,
) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();

    println!("Goveranance parameters");
    let key = gov_storage::get_max_proposal_code_size_key();
    let max_proposal_code_size = query_storage_value::<u64>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!(
        "{:4}Max. proposal code size: {}",
        "", max_proposal_code_size
    );

    let key = gov_storage::get_max_proposal_content_key();
    let max_proposal_content = query_storage_value::<u64>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!(
        "{:4}Max. proposal content size: {}",
        "", max_proposal_content
    );

    let key = gov_storage::get_min_proposal_fund_key();
    let min_proposal_fund = query_storage_value::<Amount>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!("{:4}Min. proposal funds: {}", "", min_proposal_fund);

    let key = gov_storage::get_min_proposal_grace_epoch_key();
    let min_proposal_grace_epoch = query_storage_value::<u64>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!(
        "{:4}Min. proposal grace epoch: {}",
        "", min_proposal_grace_epoch
    );

    let key = gov_storage::get_min_proposal_period_key();
    let min_proposal_period = query_storage_value::<u64>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!("{:4}Min. proposal period: {}", "", min_proposal_period);

    println!("Protocol parameters");
    let key = param_storage::get_epoch_storage_key();
    let epoch_duration = query_storage_value::<EpochDuration>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!(
        "{:4}Min. epoch duration: {}",
        "", epoch_duration.min_duration
    );
    println!(
        "{:4}Min. number of blocks: {}",
        "", epoch_duration.min_num_of_blocks
    );

    let key = param_storage::get_max_expected_time_per_block_key();
    let max_block_duration = query_storage_value::<u64>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!("{:4}Max. block duration: {}", "", max_block_duration);

    let key = param_storage::get_tx_whitelist_storage_key();
    let vp_whitelist = query_storage_value::<Vec<String>>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!("{:4}VP whitelist: {:?}", "", vp_whitelist);

    let key = param_storage::get_tx_whitelist_storage_key();
    let tx_whitelist = query_storage_value::<Vec<String>>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!("{:4}Transactions whitelist: {:?}", "", tx_whitelist);

    println!("Treasury parameters");
    let key = treasury_storage::get_max_transferable_fund_key();
    let max_transferable_amount = query_storage_value::<Amount>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!(
        "{:4}Max. transferable amount: {}",
        "", max_transferable_amount
    );

    println!("PoS parameters");
    let key = pos::params_key();
    let pos_params = query_storage_value::<PosParams>(&client, &key)
        .await
        .expect("Parameter should be definied.");
    println!(
        "{:4}Block proposer reward: {}",
        "", pos_params.block_proposer_reward
    );
    println!(
        "{:4}Block vote reward: {}",
        "", pos_params.block_vote_reward
    );
    println!(
        "{:4}Duplicate vote slash rate: {}",
        "", pos_params.duplicate_vote_slash_rate
    );
    println!(
        "{:4}Light client attack slash rate: {}",
        "", pos_params.light_client_attack_slash_rate
    );
    println!(
        "{:4}Max. validator slots: {}",
        "", pos_params.max_validator_slots
    );
    println!("{:4}Pipeline length: {}", "", pos_params.pipeline_len);
    println!("{:4}Unbonding length: {}", "", pos_params.unbonding_len);
    println!("{:4}Votes per token: {}", "", pos_params.votes_per_token);
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
    let key = pos::validator_state_key(address);
    let state: Option<pos::ValidatorStates> =
        query_storage_value(&client, &key).await;
    state.is_some()
}

/// Check if a given address is a known delegator
pub async fn is_delegator(
    address: &Address,
    ledger_address: TendermintAddress,
) -> bool {
    let client = HttpClient::new(ledger_address).unwrap();
    let bonds_prefix = pos::bonds_for_source_prefix(address);
    let bonds =
        query_storage_prefix::<pos::Bonds>(client.clone(), bonds_prefix).await;
    bonds.is_some() && bonds.unwrap().count() > 0
}

pub async fn is_delegator_at(
    client: &HttpClient,
    address: &Address,
    epoch: Epoch,
) -> bool {
    let key = pos::bonds_for_source_prefix(address);
    let bonds_iter =
        query_storage_prefix::<pos::Bonds>(client.clone(), key).await;
    if let Some(mut bonds) = bonds_iter {
        bonds.any(|(_, bond)| bond.get(epoch).is_some())
    } else {
        false
    }
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

pub async fn get_proposal_votes(
    client: &HttpClient,
    epoch: Epoch,
    proposal_id: u64,
) -> Votes {
    let validators = get_all_validators(client, epoch).await;

    let vote_prefix_key =
        gov_storage::get_proposal_vote_prefix_key(proposal_id);
    let vote_iter =
        query_storage_prefix::<ProposalVote>(client.clone(), vote_prefix_key)
            .await;

    let mut yay_validators: HashMap<Address, Amount> = HashMap::new();
    let mut yay_delegators: HashMap<Address, Amount> = HashMap::new();
    let mut nay_delegators: HashMap<Address, Amount> = HashMap::new();

    if let Some(vote_iter) = vote_iter {
        for (key, vote) in vote_iter {
            let voter_address = gov_storage::get_voter_address(&key)
                .expect("Vote key should contains the voting address.")
                .clone();
            if vote.is_yay() && validators.contains(&voter_address) {
                let amount =
                    get_validator_stake(client, epoch, &voter_address).await;
                yay_validators.insert(voter_address, amount);
            } else if !validators.contains(&voter_address) {
                let validator_address =
                    gov_storage::get_vote_delegation_address(&key)
                        .expect(
                            "Vote key should contains the delegation address.",
                        )
                        .clone();
                let delegator_token_amount = get_bond_amount_at(
                    client,
                    &voter_address,
                    &validator_address,
                    epoch,
                )
                .await;
                if let Some(amount) = delegator_token_amount {
                    if vote.is_yay() {
                        yay_delegators.insert(voter_address, amount);
                    } else {
                        nay_delegators.insert(voter_address, amount);
                    }
                }
            }
        }
    }

    Votes {
        yay_validators,
        yay_delegators,
        nay_delegators,
    }
}

pub async fn get_proposal_offline_votes(
    client: &HttpClient,
    proposal: OfflineProposal,
    files: HashSet<PathBuf>,
) -> Votes {
    let validators = get_all_validators(client, proposal.tally_epoch).await;

    let proposal_hash = proposal.compute_hash();

    let mut yay_validators: HashMap<Address, Amount> = HashMap::new();
    let mut yay_delegators: HashMap<Address, Amount> = HashMap::new();
    let mut nay_delegators: HashMap<Address, Amount> = HashMap::new();

    for path in files {
        let file = File::open(&path).expect("Proposal file must exist.");
        let proposal_vote: OfflineVote = serde_json::from_reader(file)
            .expect("JSON was not well-formatted for offline vote.");

        let key = pk_key(&proposal_vote.address);
        let public_key = query_storage_value(client, &key)
            .await
            .expect("Public key should exist.");

        if !proposal_vote.proposal_hash.eq(&proposal_hash)
            || !proposal_vote.check_signature(&public_key)
        {
            continue;
        }

        if proposal_vote.vote.is_yay()
            && validators.contains(&proposal_vote.address)
        {
            let amount = get_validator_stake(
                client,
                proposal.tally_epoch,
                &proposal_vote.address,
            )
            .await;
            yay_validators.insert(proposal_vote.address, amount);
        } else if is_delegator_at(
            client,
            &proposal_vote.address,
            proposal.tally_epoch,
        )
        .await
        {
            let key = pos::bonds_for_source_prefix(&proposal_vote.address);
            let bonds_iter =
                query_storage_prefix::<pos::Bonds>(client.clone(), key).await;
            if let Some(bonds) = bonds_iter {
                for (key, epoched_amount) in bonds {
                    let bond = epoched_amount
                        .get(proposal.tally_epoch)
                        .expect("Delegation bond should be definied.");
                    let epoch = anoma::ledger::pos::types::Epoch::from(
                        proposal.tally_epoch.0,
                    );
                    let amount = *bond
                        .deltas
                        .get(&epoch)
                        .expect("Delegation amount should be definied.");
                    let validator_address =
                        pos::get_validator_address_from_bond(&key).expect(
                            "Delegation key should contain validator address.",
                        );
                    if proposal_vote.vote.is_yay() {
                        yay_delegators.insert(validator_address, amount);
                    } else {
                        nay_delegators.insert(validator_address, amount);
                    }
                }
            }
        }
    }

    Votes {
        yay_validators,
        yay_delegators,
        nay_delegators,
    }
}

// Compute the result of a proposal
pub async fn compute_tally(
    client: &HttpClient,
    epoch: Epoch,
    votes: Votes,
) -> TallyResult {
    let validators = get_all_validators(client, epoch).await;
    let total_stacked_tokens =
        get_total_staked_tokes(client, epoch, &validators).await;

    let Votes {
        yay_validators,
        yay_delegators,
        nay_delegators,
    } = votes;

    let mut total_yay_stacked_tokens = Amount::from(0);
    for (_, amount) in yay_validators.clone().into_iter() {
        total_yay_stacked_tokens += amount;
    }

    // YAY: Add delegator amount whose validator didn't vote / voted nay
    for (validator_address, amount) in yay_delegators.into_iter() {
        if !yay_validators.contains_key(&validator_address) {
            total_yay_stacked_tokens += amount;
        }
    }

    // NAY: Remove delegator amount whose validator validator vote yay
    for (validator_address, amount) in nay_delegators.into_iter() {
        if yay_validators.contains_key(&validator_address) {
            total_yay_stacked_tokens -= amount;
        }
    }

    if 3 * total_yay_stacked_tokens >= 2 * total_stacked_tokens {
        TallyResult::Passed
    } else {
        TallyResult::Rejected
    }
}

pub async fn get_bond_amount_at(
    client: &HttpClient,
    delegator: &Address,
    validator: &Address,
    epoch: Epoch,
) -> Option<token::Amount> {
    let slashes_key = pos::validator_slashes_key(validator);
    let slashes = query_storage_value::<pos::Slashes>(client, &slashes_key)
        .await
        .unwrap_or_default();
    let bond_key = pos::bond_key(&BondId {
        source: delegator.clone(),
        validator: validator.clone(),
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

pub async fn get_all_validators(
    client: &HttpClient,
    epoch: Epoch,
) -> Vec<Address> {
    let validator_set_key = pos::validator_set_key();
    let validator_sets =
        query_storage_value::<pos::ValidatorSets>(client, &validator_set_key)
            .await
            .expect("Validator set should always be set");
    let validator_set = validator_sets
        .get(epoch)
        .expect("Validator set should be always set in the current epoch");
    let all_validators = validator_set.active.union(&validator_set.inactive);
    all_validators
        .map(|validator| validator.address.clone())
        .collect()
}

pub async fn get_total_staked_tokes(
    client: &HttpClient,
    epoch: Epoch,
    validators: &[Address],
) -> token::Amount {
    let mut total = Amount::from(0);

    for validator in validators {
        total += get_validator_stake(client, epoch, validator).await;
    }
    total
}

async fn get_validator_stake(
    client: &HttpClient,
    epoch: Epoch,
    validator: &Address,
) -> token::Amount {
    let total_voting_power_key = pos::validator_total_deltas_key(validator);
    let total_voting_power = query_storage_value::<pos::ValidatorTotalDeltas>(
        client,
        &total_voting_power_key,
    )
    .await
    .expect("Total deltas should be defined");
    let epoched_total_voting_power = total_voting_power.get(epoch);
    if let Some(epoched_total_voting_power) = epoched_total_voting_power {
        token::Amount::from_change(epoched_total_voting_power)
    } else {
        token::Amount::from(0)
    }
}

pub async fn get_delegators_delegation(
    client: &HttpClient,
    address: &Address,
    _epoch: Epoch,
) -> Vec<Address> {
    let key = pos::bonds_for_source_prefix(address);
    let bonds_iter =
        query_storage_prefix::<pos::Bonds>(client.clone(), key).await;

    let mut delegation_addresses: Vec<Address> = Vec::new();
    if let Some(bonds) = bonds_iter {
        for (key, _epoched_amount) in bonds {
            let validator_address = pos::get_validator_address_from_bond(&key)
                .expect("Delegation key should contain validator address.");
            delegation_addresses.push(validator_address);
        }
    }
    delegation_addresses
}
