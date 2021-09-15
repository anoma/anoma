use std::collections::HashSet;
use std::convert::{TryFrom, TryInto};
use std::fs::File;
use std::io::Write;

use anoma::proto::{Intent, Tx};
use anoma::types::address::Address;
use anoma::types::intent::{DecimalWrapper, Exchange, FungibleTokenIntent};
use anoma::types::key::ed25519::{Keypair, Signed};
use anoma::types::nft::NftToken;
use anoma::types::time::DateTimeUtc;
use anoma::types::token::{self, Amount};
use anoma::types::transaction::{CreateNft, InitAccount, MintNft, UpdateVp};
use borsh::BorshSerialize;
use jsonpath_lib as jsonpath;
use serde::{Deserialize, Serialize};
use tendermint_rpc::query::{EventType, Query};
use tendermint_rpc::Client;

use super::rpc;
use crate::cli::args;
use crate::client::tendermint_websocket_client::{
    hash_tx, Error, TendermintWebsocketClient, WebSocketAddress,
};
use crate::proto::services::rpc_service_client::RpcServiceClient;
use crate::proto::{services, RpcMessage};
use crate::wallet;

const TX_INIT_ACCOUNT_WASM: &str = "wasm/tx_init_account.wasm";
const TX_CREATE_NFT: &str = "wasm/tx_create_nft.wasm";
const TX_UPDATE_VP_WASM: &str = "wasm/tx_update_vp.wasm";
const TX_TRANSFER_WASM: &str = "wasm/tx_transfer.wasm";
const VP_USER_WASM: &str = "wasm/vp_user.wasm";
const TX_MINT_NFT_TOKEN: &str = "wasm/mint_nft_tokens.wasm";
const VP_NFT: &str = "wasm/vp_nft.wasm";

#[derive(Debug, Clone, Deserialize)]
pub struct NftDefinition {
    /// The source address
    pub owner: Address,
    /// The path to the vp code
    pub vp_path: Option<String>,
    /// the nft tokens
    pub tokens: Vec<NftToken>,
}

#[derive(Debug, Clone, Deserialize, BorshSerialize)]
pub struct ExchangeDefinition {
    /// The source address
    pub addr: Address,
    /// The token to be sold
    pub token_sell: Address,
    /// The minimum rate
    pub rate_min: DecimalWrapper,
    /// The maximum amount of token to be sold
    pub max_sell: Amount,
    /// The token to be bought
    pub token_buy: Address,
    /// The amount of token to be bought
    pub min_buy: Amount,
    // The path to the wasm vp code
    pub vp_path: Option<String>,
}

impl TryInto<Exchange> for ExchangeDefinition {
    type Error = &'static str;

    fn try_into(self) -> Result<Exchange, Self::Error> {
        let exchange_vp_code = self
            .vp_path
            .as_ref()
            .map(|path| {
                Some(
                    std::fs::read(path)
                        .expect("Expected a file at given code path"),
                )
            })
            .unwrap_or_else(|| None);

        Ok(Exchange {
            addr: self.addr,
            token_sell: self.token_sell,
            rate_min: self.rate_min,
            max_sell: self.max_sell,
            token_buy: self.token_buy,
            min_buy: self.min_buy,
            vp: exchange_vp_code,
        })
    }
}

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
        "Encoding transfer data to update a validity predicate shouldn't fail",
    );
    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    submit_tx(args.tx, tx).await
}

pub async fn submit_init_account(args: args::TxInitAccount) {
    let source_key: Keypair = wallet::key_of(args.source.encode());
    let public_key = args.public_key;
    let vp_code = args
        .vp_code_path
        .map(|path| {
            std::fs::read(path).expect("Expected a file at given code path")
        })
        .unwrap_or_else(|| {
            std::fs::read(VP_USER_WASM)
                .expect("Expected a file at given code path")
        });
    let tx_code = std::fs::read(TX_INIT_ACCOUNT_WASM)
        .expect("Expected a file at given code path");

    let data = InitAccount {
        public_key,
        vp_code,
    };
    let data = data.try_to_vec().expect(
        "Encoding transfer data to initialize a new account shouldn't fail",
    );
    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    submit_tx(args.tx, tx).await
}

pub async fn mint_nft(args: args::NftMint) {
    let source_key: Keypair = wallet::key_of(args.key.encode());

    let file = File::open(&args.nft_data).expect("File must exist.");
    let nft_tokens: Vec<NftToken> =
        serde_json::from_reader(file).expect("JSON was not well-formatted");

    let data = MintNft {
        owner: args.key,
        address: args.nft_address,
        tokens: nft_tokens,
    };
    let data = data.try_to_vec().expect(
        "Encoding transfer data to initialize a new account shouldn't fail",
    );

    let tx_code = std::fs::read(TX_MINT_NFT_TOKEN)
        .expect("Expected a file at given code path");

    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    submit_tx(args.tx, tx).await
}

pub async fn create_nft(args: args::NftCreate) {
    let source_key: Keypair = wallet::key_of(args.key.encode());

    let file = File::open(&args.nft_data).expect("File must exist.");
    let nft_definition: NftDefinition =
        serde_json::from_reader(file).expect("JSON was not well-formatted");

    let nft_vp_code = nft_definition
        .vp_path
        .map(|path| {
            std::fs::read(path).expect("Expected a file at given code path")
        })
        .unwrap_or_else(|| {
            std::fs::read(VP_NFT).expect("Expected a file at given code path")
        });

    let data = CreateNft {
        owner: nft_definition.owner,
        vp_code: nft_vp_code,
        tokens: nft_definition.tokens,
    };
    let data = data.try_to_vec().expect(
        "Encoding transfer data to initialize a new account shouldn't fail",
    );

    let tx_code = std::fs::read(TX_CREATE_NFT)
        .expect("Expected a file at given code path");

    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    submit_tx(args.tx, tx).await
}

pub async fn gossip_intent(
    args::Intent {
        node_addr,
        topic,
        key,
        exchanges_definition,
        to_stdout,
    }: args::Intent,
) {
    let signing_key = wallet::key_of(key.encode());

    let file = File::open(exchanges_definition).expect("File must exist.");
    let exchanges_definitions: Vec<ExchangeDefinition> =
        serde_json::from_reader(file).expect("JSON was not well-formatted");

    let signed_exchanges: HashSet<Signed<Exchange>> = exchanges_definitions
        .iter()
        .map(|exchange_def| {
            let source_keypair = wallet::key_of(exchange_def.addr.encode());
            let exchange: Exchange =
                ExchangeDefinition::try_into(exchange_def.to_owned()).expect(
                    "Conversion from ExchangeDefinition to Exchange should \
                     fail.",
                );
            Signed::new(&source_keypair, exchange)
        })
        .collect();

    let signed_ft: Signed<FungibleTokenIntent> = Signed::new(
        &signing_key,
        FungibleTokenIntent {
            exchange: signed_exchanges,
        },
    );
    let data_bytes = signed_ft.try_to_vec().unwrap();

    if to_stdout {
        let mut out = std::io::stdout();
        out.write_all(&data_bytes).unwrap();
        out.flush().unwrap();
    } else {
        let node_addr = node_addr.expect(
            "Gossip node address must be defined to submit the intent to it.",
        );
        let topic = topic.expect(
            "The topic must be defined to submit the intent to a gossip node.",
        );
        let mut client = RpcServiceClient::connect(node_addr).await.unwrap();

        let intent = Intent {
            data: data_bytes,
            timestamp: DateTimeUtc::now(),
        };
        let message: services::RpcMessage =
            RpcMessage::new_intent(intent, topic).into();
        let response = client
            .send_message(message)
            .await
            .expect("failed to send message and/or receive rpc response");
        println!("{:#?}", response);
    }
}

pub async fn subscribe_topic(
    args::SubscribeTopic { node_addr, topic }: args::SubscribeTopic,
) {
    let mut client = RpcServiceClient::connect(node_addr).await.unwrap();
    let message: services::RpcMessage = RpcMessage::new_topic(topic).into();
    let response = client
        .send_message(message)
        .await
        .expect("failed to send message and/or receive rpc response");
    println!("{:#?}", response);
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

    if args.dry_run {
        rpc::dry_run_tx(&args.ledger_address, tx_bytes).await
    } else if let Err(err) = broadcast_tx(args.ledger_address, tx_bytes).await {
        eprintln!("Encountered error while broadcasting transaction: {}", err);
    }
}

pub async fn broadcast_tx(
    address: tendermint::net::Address,
    tx_bytes: Vec<u8>,
) -> Result<(), Error> {
    let mut client =
        TendermintWebsocketClient::open(WebSocketAddress::try_from(address)?)?;
    // It is better to subscribe to the transaction before it is broadcast
    //
    // Note that the `applied.hash` key comes from a custom event
    // created by the shell
    let query = Query::from(EventType::NewBlock)
        .and_eq("applied.hash", hash_tx(&tx_bytes).to_string());
    client.subscribe(query)?;
    println!(
        "Transaction added to mempool: {:?}",
        client
            .broadcast_tx_sync(tx_bytes.into())
            .await
            .map_err(|err| Error::Response(format!("{:?}", err)))?
    );
    let parsed = TxResponse::from(client.receive_response()?);
    println!(
        "Transaction applied with result: {}",
        serde_json::to_string_pretty(&parsed).unwrap()
    );
    client.unsubscribe()?;
    client.close();
    Ok(())
}

#[derive(Serialize)]
struct TxResponse {
    info: String,
    height: String,
    hash: String,
    code: String,
    gas_used: String,
    initialized_accounts: Vec<Address>,
}

impl From<serde_json::Value> for TxResponse {
    fn from(json: serde_json::Value) -> Self {
        let mut selector = jsonpath::selector(&json);
        let info = selector("$.events.['applied.info'][0]").unwrap();
        let height = selector("$.events.['applied.height'][0]").unwrap();
        let hash = selector("$.events.['applied.hash'][0]").unwrap();
        let code = selector("$.events.['applied.code'][0]").unwrap();
        let gas_used = selector("$.events.['applied.gas_used'][0]").unwrap();
        let initialized_accounts =
            selector("$.events.['applied.initialized_accounts'][0]");
        let initialized_accounts = match initialized_accounts {
            Ok(values) if !values.is_empty() => {
                // In a response, the initialized accounts are encoded as e.g.:
                // ```
                // "applied.initialized_accounts": Array([
                //   String(
                //     "[\"a1qq5qqqqq8qerqv3sxyuyz3zzxgcyxvecgerry333xce5z3fkg4pnj3zxgfqnzd69gsu5gwzr9wpjpe\"]",
                //   ),
                // ]),
                // ...
                // So we need to decode the inner string first ...
                let raw: String =
                    serde_json::from_value(values[0].clone()).unwrap();
                // ... and then decode the vec from the array inside the string
                serde_json::from_str(&raw).unwrap()
            }
            _ => vec![],
        };
        TxResponse {
            info: serde_json::from_value(info[0].clone()).unwrap(),
            height: serde_json::from_value(height[0].clone()).unwrap(),
            hash: serde_json::from_value(hash[0].clone()).unwrap(),
            code: serde_json::from_value(code[0].clone()).unwrap(),
            gas_used: serde_json::from_value(gas_used[0].clone()).unwrap(),
            initialized_accounts,
        }
    }
}
