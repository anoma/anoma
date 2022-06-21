//! Client RPC queries

use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::convert::TryInto;
use std::io::{self, Write};
use std::str::FromStr;

use anoma::ledger::pos::types::{
    Epoch as PosEpoch, VotingPower, WeightedValidator,
};
use anoma::ledger::pos::{
    self, is_validator_slashes_key, Bonds, Slash, Unbonds,
};
use anoma::proto::{Tx, SignedTxData};
use anoma::types::address::{Address, tokens};
use anoma::types::key::*;
use anoma::types::masp::{BalanceOwner, ExtendedViewingKey, PaymentAddress};
use anoma::types::storage::{Epoch, PrefixValue};
use anoma::types::transaction::{process_tx, TxType, DecryptedTx};
use anoma::types::transaction::AffineCurve;
use anoma::types::transaction::PairingEngine;
use anoma::types::transaction::EllipticCurve;
use anoma::types::{address, storage, token};
use anoma::types::token::Transfer;
use borsh::{BorshDeserialize, BorshSerialize};
use itertools::{Either, Itertools};
use masp_primitives::asset_type::AssetType;
use masp_primitives::primitives::ViewingKey;
use masp_primitives::transaction::components::Amount;
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
use crate::client::tx::{PinnedBalanceError, ShieldedContext, TxResponse};
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

/// Extract the payload from the given Tx object
fn extract_payload(tx: Tx) -> Option<Transfer> {
    match process_tx(tx) {
        Ok(TxType::Wrapper(wrapper_tx)) => {
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            extract_payload(Tx::from(match wrapper_tx.decrypt(privkey) {
                Ok(tx) => DecryptedTx::Decrypted(tx),
                _ => DecryptedTx::Undecryptable(wrapper_tx.clone()),
            }))
        }
        Ok(TxType::Decrypted(DecryptedTx::Decrypted(tx))) => {
            let empty_vec = vec![];
            let tx_data = tx.data.as_ref().unwrap_or(&empty_vec);
            let signed = SignedTxData::try_from_slice(tx_data).ok()?;
            Transfer::try_from_slice(&signed.data.unwrap()[..]).ok()
        },
        _ => None
    }
}

/// Query the specified accepted transfers from the ledger
pub async fn query_transfers(ctx: Context, args: args::Query) {
    // Connect to the Tendermint server holding the transactions
    let (client, driver) = WebSocketClient::new(args.ledger_address.clone()).await.unwrap();
    let driver_handle = tokio::spawn(async move { driver.run().await });
    let mut transfers = BTreeMap::new();
    // Find all transactions to or from addresses in our address book
    for (_alias, addr) in ctx.wallet.get_addresses() {
        for prop in ["transfer.source", "transfer.target"] {
            // Query transactions involving the current address
            let tx_query = Query::eq(prop, addr.encode());
            let txs = &client
                .tx_search(tx_query, true, 1, 255, Order::Ascending)
                .await
                .expect("Unable to query for transaction with given hash")
                .txs;
            for response_tx in txs {
                // Only process yet unprocessed transactions
                if !transfers.contains_key(&(response_tx.height, response_tx.index)) {
                    let tx = Tx::try_from(response_tx.tx.as_ref())
                        .expect("Ill-formed Tx");
                    if let Some(transfer) = extract_payload(tx) {
                        transfers.insert((response_tx.height, response_tx.index), transfer);
                    }
                }
            }
        }
    }
    // To facilitate lookups of human-readable token names
    let tokens = tokens();
    for ((height, idx), transfer) in transfers {
        println!("Height: {}, Index: {}, Transparent Transfer:", height, idx);
        println!("  Source: {}", transfer.source);
        println!("  Target: {}", transfer.target);
        println!("  Amount: {}", transfer.amount);
        println!(
            "  Token: {}",
            tokens.get(&transfer.token)
                .cloned()
                .unwrap_or(transfer.token.encode().as_str())
        );
    }
    // Signal to the driver to terminate.
    client.close().unwrap();
    // Await the driver's termination to ensure proper connection closure.
    let _ = driver_handle.await.unwrap_or_else(|x| {
        eprintln!("{}", x);
        cli::safe_exit(1)
    });
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
        Some(BalanceOwner::PaymentAddress(_owner)) => {
            query_pinned_balance(&mut ctx, args).await
        }
        None => {
            // Print pinned balance
            query_pinned_balance(&mut ctx, args.clone()).await;
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
            let owner = ctx
                .get_cached(&owner)
                .address()
                .expect("a transparent address");
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

/// Query the token pinned balance(s)
pub async fn query_pinned_balance(ctx: &mut Context, args: args::QueryBalance) {
    // Map addresses to token names
    let tokens = address::tokens();
    let owners = if let Some(pa) = args
        .owner
        .and_then(|x| ctx.get_cached(&x).payment_address())
    {
        vec![pa]
    } else {
        ctx.wallet
            .get_payment_addrs()
            .into_values()
            .filter(PaymentAddress::is_pinned)
            .collect()
    };
    // Get the viewing keys with which to try note decryptions
    let viewing_keys: Vec<ViewingKey> = ctx
        .wallet
        .get_viewing_keys()
        .values()
        .map(|fvk| ExtendedFullViewingKey::from(*fvk).fvk.vk)
        .collect();
    // Print the token balances by payment address
    for owner in owners {
        let mut balance = Err(PinnedBalanceError::InvalidViewingKey);
        // Find the viewing key that can recognize payments the current payment
        // address
        for vk in &viewing_keys {
            balance = ShieldedContext::compute_pinned_balance(
                &args.query.ledger_address,
                owner,
                vk,
            )
            .await;
            if balance != Err(PinnedBalanceError::InvalidViewingKey) {
                break;
            }
        }
        // If a suitable viewing key was not found, then demand it from the user
        if balance == Err(PinnedBalanceError::InvalidViewingKey) {
            print!("Enter the viewing key for {}: ", owner);
            io::stdout().flush().unwrap();
            let mut vk_str = String::new();
            io::stdin().read_line(&mut vk_str).unwrap();
            let fvk = match ExtendedViewingKey::from_str(vk_str.trim()) {
                Ok(fvk) => fvk,
                _ => {
                    eprintln!("Invalid viewing key entered");
                    continue;
                }
            };
            let vk = ExtendedFullViewingKey::from(fvk).fvk.vk;
            // Use the given viewing key to decrypt pinned transaction data
            balance = ShieldedContext::compute_pinned_balance(
                &args.query.ledger_address,
                owner,
                &vk,
            )
            .await
        }
        // Now print out the received quantities according to CLI arguments
        match (balance, args.token.as_ref()) {
            (Err(PinnedBalanceError::InvalidViewingKey), _) => println!(
                "Supplied viewing key cannot decode transactions to given \
                 payment address."
            ),
            (Err(PinnedBalanceError::NoTransactionPinned), _) => {
                println!("Payment address {} has not yet been consumed.", owner)
            }
            (Ok((balance, epoch)), Some(token)) => {
                let token = ctx.get(token);
                // Extract and print only the specified token from the total
                let (_asset_type, balance) =
                    value_by_address(&balance, token.clone(), epoch);
                let currency_code = tokens
                    .get(&token)
                    .map(|c| Cow::Borrowed(*c))
                    .unwrap_or_else(|| Cow::Owned(token.to_string()));
                if balance == 0 {
                    println!(
                        "Payment address {} was consumed during epoch {}. \
                         Received no shielded {}",
                        owner, epoch, currency_code
                    );
                } else {
                    let asset_value = token::Amount::from(balance as u64);
                    println!(
                        "Payment address {} was consumed during epoch {}. \
                         Received {} {}",
                        owner, epoch, asset_value, currency_code
                    );
                }
            }
            (Ok((balance, epoch)), None) => {
                let mut found_any = false;
                // Print balances by asset IDs and human-readable token names
                for (asset_type, value) in decode_asset_types(balance, epoch) {
                    if !found_any {
                        println!(
                            "Payment address {} was consumed during epoch {}. \
                             Received:",
                            owner, epoch
                        );
                        found_any = true;
                    }
                    let asset_value = token::Amount::from(value as u64);
                    match asset_type {
                        Either::Left(asset_type) => {
                            println!("  {}: {}", asset_type, asset_value)
                        }
                        Either::Right(token) => {
                            // If corresponding token address was found, print
                            // that instead
                            let currency_code = tokens
                                .get(&token)
                                .map(|c| Cow::Borrowed(*c))
                                .unwrap_or_else(|| {
                                    Cow::Owned(token.to_string())
                                });
                            println!("  {}: {}", currency_code, asset_value)
                        }
                    }
                }
                if !found_any {
                    println!(
                        "Payment address {} was consumed during epoch {}. \
                         Received no shielded assets.",
                        owner, epoch
                    );
                }
            }
        }
    }
}

/// Get the component of the given amount corresponding to the given token
pub fn value_by_address(
    amt: &Amount,
    token: Address,
    epoch: Epoch,
) -> (AssetType, i64) {
    // Compute the unique asset identifier from the token address
    let asset_type = AssetType::new(
        (token, epoch.0)
            .try_to_vec()
            .expect("token addresses should serialize")
            .as_ref(),
    )
    .unwrap();
    (asset_type, amt[&asset_type])
}

/// Determine the token addresses yielding the given asset types if possible
pub fn decode_asset_types(
    mut amt: Amount,
    epoch: Epoch,
) -> BTreeMap<Either<AssetType, Address>, i64> {
    // Map addresses to token names
    let tokens = address::tokens();
    let mut decoded = BTreeMap::new();
    // Print those balances corresponding to human-readable token names
    for (token, _currency_code) in tokens {
        // Compute the unique asset identifier from the token address
        let (asset_type, asset_value) =
            value_by_address(&amt, token.clone(), epoch);
        if asset_value != 0 {
            amt -= amt.project(asset_type);
            decoded.insert(Either::Right(token), asset_value);
        }
    }
    // Now print those balances that do not correspond to human-readable
    // token names
    for (asset_type, value) in amt.components() {
        decoded.insert(Either::Left(*asset_type), *value);
    }
    decoded
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
    let fvks: Vec<_> = viewing_keys
        .iter()
        .map(|fvk| ExtendedFullViewingKey::from(*fvk).fvk.vk)
        .collect();
    ctx.shielded
        .fetch(&args.query.ledger_address, &[], &fvks)
        .await;
    // Save the update state so that future fetches can be short-circuited
    let _ = ctx.shielded.save();
    // The epoch is required to identify timestamped tokens
    let epoch = query_epoch(args.query.clone()).await;
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
                .compute_exchanged_balance(
                    args.query.ledger_address,
                    &viewing_key,
                )
                .await
                .expect("context should contain viewing key");
            // Compute the unique asset identifier from the token address
            let token = ctx.get(&token);
            let asset_type = AssetType::new(
                (token.clone(), epoch.0)
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
                    .compute_exchanged_balance(
                        args.query.ledger_address.clone(),
                        &viewing_key,
                    )
                    .await
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
                    (token, epoch.0)
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
                        println!("  {}, owned by {}", value, fvk);
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
                (token.clone(), epoch.0)
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
                    .compute_exchanged_balance(
                        args.query.ledger_address.clone(),
                        &viewing_key,
                    )
                    .await
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
            let balance = ctx
                .shielded
                .compute_exchanged_balance(
                    args.query.ledger_address,
                    &viewing_key,
                )
                .await
                .expect("context should contain viewing key");
            let mut found_any = false;
            // Print balances by asset IDs and human-readable token names
            for (asset_type, value) in decode_asset_types(balance, epoch) {
                let asset_value = token::Amount::from(value as u64);
                match asset_type {
                    Either::Left(asset_type) => {
                        println!("{}: {}", asset_type, asset_value)
                    }
                    Either::Right(token) => {
                        println!("{}: {}", tokens[&token], asset_value)
                    }
                }
                found_any = true;
            }
            if !found_any {
                println!("No shielded balance found for given key");
            }
        }
    }
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
                query_storage_value::<pos::Bonds>(client.clone(), bond_key)
                    .await;
            // Find owner's unbonded delegations from the given
            // validator
            let unbond_key = pos::unbond_key(&bond_id);
            let unbonds =
                query_storage_value::<pos::Unbonds>(client.clone(), unbond_key)
                    .await;
            // Find validator's slashes, if any
            let slashes_key = pos::validator_slashes_key(&bond_id.validator);
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
            let validator = ctx.get(&validator);
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
            let unbonds =
                query_storage_value::<pos::Unbonds>(client.clone(), unbond_key)
                    .await;
            // Find validator's slashes, if any
            let slashes_key = pos::validator_slashes_key(&bond_id.validator);
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
                                client.clone(),
                                slashes_key,
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
                                client.clone(),
                                slashes_key,
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
                                client.clone(),
                                slashes_key,
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
            let validator = ctx.get(&validator);
            // Find voting power for the given validator
            let voting_power_key = pos::validator_voting_power_key(&validator);
            let voting_powers =
                query_storage_value::<pos::ValidatorVotingPowers>(
                    client.clone(),
                    voting_power_key,
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
        client,
        total_voting_power_key,
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
    query_storage_value(client, key).await
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
        query_storage_value(client, key).await;
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

/// Query a storage value and decode it with [`BorshDeserialize`].
pub async fn query_storage_value<T>(
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
