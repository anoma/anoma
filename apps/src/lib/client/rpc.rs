//! Client RPC queries

use std::io::{self, Write};

use anoma::types::{address, storage, token};
use borsh::BorshDeserialize;
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

/// Query token balance(s)
pub async fn query_balance(args: args::QueryBalance) {
    let client = HttpClient::new(args.query.ledger_address).unwrap();
    let tokens = address::tokens();
    match (args.token.as_ref(), args.owner.as_ref()) {
        (Some(token), Some(owner)) => {
            let key = token::balance_key(token, owner);
            let balance: token::Amount = query_storage_value(client, key).await;
            let currency_code = tokens
                .get(token)
                .cloned()
                .unwrap_or_else(|| token.to_string());
            println!("{}: {}", currency_code, balance);
        }
        (None, Some(owner)) => {
            for (token, currency_code) in tokens {
                let key = token::balance_key(&token, owner);
                let balance: token::Amount =
                    query_storage_value(client.clone(), key).await;
                println!("{}: {}", currency_code, balance);
            }
        }
        (Some(token), None) => {
            let key = token::balance_prefix(token);
            let balances =
                query_storage_prefix::<token::Amount>(client, key).await;
            let currency_code = tokens
                .get(token)
                .cloned()
                .unwrap_or_else(|| token.to_string());
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

/// Query a storage value and decode it with [`BorshDeserialize`].
async fn query_storage_value<T>(client: HttpClient, key: storage::Key) -> T
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
                Ok(value) => return value,
                Err(err) => eprintln!("Error decoding the value: {}", err),
            }
        }
        tendermint::abci::Code::Err(err) => eprintln!(
            "Error in the query {} (error code {})",
            response.info, err
        ),
    }
    std::process::exit(1);
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
