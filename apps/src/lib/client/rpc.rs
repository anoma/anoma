//! Client RPC queries

use tendermint_rpc::{Client, HttpClient};

use crate::cli::args;
use crate::node::ledger::rpc::Path;

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

pub async fn query_balance(args: args::QueryBalance) {
    let _client = HttpClient::new(args.query.ledger_address).unwrap();
    todo!()
}
