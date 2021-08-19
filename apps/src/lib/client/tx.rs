use std::str::FromStr;

use anoma::proto::Tx;
use anoma::types::key::ed25519::Keypair;
use anoma::types::transaction::UpdateVp;
use anoma::types::{pos, token};
use borsh::BorshSerialize;
use tendermint_rpc::{Client, HttpClient};

use crate::cli::args;
use crate::wallet;

const TX_UPDATE_VP_WASM: &str = "wasm/tx_update_vp.wasm";
const TX_TRANSFER_WASM: &str = "wasm/tx_transfer.wasm";
const TX_BOND_WASM: &str = "wasm/tx_bond.wasm";
const TX_UNBOND_WASM: &str = "wasm/tx_unbond.wasm";

pub async fn submit_custom(args: args::TxCustom) {
    let tx_code = std::fs::read(args.code_path)
        .expect("Expected a file at given code path");
    let data = args.data_path.map(|data_path| {
        std::fs::read(data_path).expect("Expected a file at given data path")
    });
    let tx = Tx::new(tx_code, data);

    submit_tx(args.tx, tx).await
}

pub async fn submit_update_vp(args: args::TxUpdateVp) {
    let addr = args.addr;
    let source_key: Keypair = wallet::key_of(addr.encode());
    let vp_code = std::fs::read(args.vp_code_path)
        .expect("Expected a file at given code path");
    let tx_code = std::fs::read(TX_UPDATE_VP_WASM)
        .expect("Expected a file at given code path");

    let update_vp = UpdateVp { addr, vp_code };
    let data = update_vp.try_to_vec().expect(
        "Encoding transfer data to update a validity predicate shouldn't  fail",
    );
    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    submit_tx(args.tx, tx).await
}

pub async fn submit_transfer(args: args::TxTransfer) {
    let source_key: Keypair = wallet::key_of(args.source.encode());
    let tx_code = std::fs::read(TX_TRANSFER_WASM).unwrap();

    let transfer = token::Transfer {
        source: args.source,
        target: args.target,
        token: args.token,
        amount: args.amount,
    };
    tracing::debug!("Transfer data {:?}", transfer);
    let data = transfer
        .try_to_vec()
        .expect("Encoding unsigned transfer shouldn't fail");
    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    submit_tx(args.tx, tx).await
}

pub async fn submit_bond(args: args::Bond) {
    let source = args.source.as_ref().unwrap_or(&args.validator);
    let source_key: Keypair = wallet::key_of(source.encode());
    let tx_code = std::fs::read(TX_BOND_WASM).unwrap();

    let bond = pos::Bond {
        validator: args.validator,
        amount: args.amount,
        source: args.source,
    };
    tracing::debug!("Bond data {:?}", bond);
    let data = bond.try_to_vec().expect("Encoding tx data shouldn't fail");
    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    submit_tx(args.tx, tx).await
}

pub async fn submit_unbond(args: args::Unbond) {
    let source = args.source.as_ref().unwrap_or(&args.validator);
    let source_key: Keypair = wallet::key_of(source.encode());
    let tx_code = std::fs::read(TX_UNBOND_WASM).unwrap();

    let unbond = pos::Unbond {
        validator: args.validator,
        amount: args.amount,
        source: args.source,
    };
    tracing::debug!("Unbond data {:?}", unbond);
    let data = unbond
        .try_to_vec()
        .expect("Encoding tx data shouldn't fail");
    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    submit_tx(args.tx, tx).await
}

async fn submit_tx(args: args::Tx, tx: Tx) {
    let tx_bytes = tx.to_bytes();

    // NOTE: use this to print the request JSON body:

    // let request =
    // tendermint_rpc::endpoint::broadcast::tx_commit::Request::new(
    //     tx_bytes.clone().into(),
    // );
    // use tendermint_rpc::Request;
    // let request_body = request.into_json();
    // println!("HTTP request body: {}", request_body);

    let client = HttpClient::new(args.ledger_address).unwrap();
    // TODO broadcast_tx_commit shouldn't be used live;
    if args.dry_run {
        let path = FromStr::from_str("dry_run_tx").unwrap();

        let response = client
            .abci_query(Some(path), tx_bytes, None, false)
            .await
            .unwrap();
        println!("{:#?}", response);
    } else {
        let response =
            client.broadcast_tx_commit(tx_bytes.into()).await.unwrap();
        println!("{:#?}", response);
    }
}
