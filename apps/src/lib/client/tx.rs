use std::borrow::Cow;
use std::convert::TryFrom;

use anoma::proto::Tx;
use anoma::types::address::Address;
use anoma::types::token;
use anoma::types::transaction::{pos, InitAccount, InitValidator, UpdateVp};
use async_std::io::{self, WriteExt};
use borsh::BorshSerialize;
use jsonpath_lib as jsonpath;
use serde::Serialize;
use tendermint_rpc::query::{EventType, Query};
use tendermint_rpc::Client;

use super::{rpc, signing};
use crate::cli::context::WalletAddress;
use crate::cli::{args, safe_exit, Context};
use crate::client::tendermint_websocket_client::{
    hash_tx, Error, TendermintWebsocketClient, WebSocketAddress,
};
use crate::node::ledger::tendermint_node;

const TX_INIT_ACCOUNT_WASM: &str = "tx_init_account.wasm";
const TX_INIT_VALIDATOR_WASM: &str = "tx_init_validator.wasm";
const TX_UPDATE_VP_WASM: &str = "tx_update_vp.wasm";
const TX_TRANSFER_WASM: &str = "tx_transfer.wasm";
const VP_USER_WASM: &str = "vp_user.wasm";
const TX_BOND_WASM: &str = "tx_bond.wasm";
const TX_UNBOND_WASM: &str = "tx_unbond.wasm";
const TX_WITHDRAW_WASM: &str = "tx_withdraw.wasm";

pub async fn submit_custom(ctx: Context, args: args::TxCustom) {
    let tx_code = ctx
        .read_wasm(args.code_path)
        .expect("Expected a file at given code path");
    let data = args.data_path.map(|data_path| {
        std::fs::read(data_path).expect("Expected a file at given data path")
    });
    let tx = Tx::new(tx_code, data);

    let (ctx, tx) = sign_tx(ctx, tx, &args.tx, None).await;
    let (ctx, initialized_accounts) = submit_tx(ctx, &args.tx, tx).await;
    save_initialized_accounts(ctx, &args.tx, initialized_accounts).await;
}

pub async fn submit_update_vp(ctx: Context, args: args::TxUpdateVp) {
    let addr = ctx.get(&args.addr);
    let vp_code = ctx
        .read_wasm(args.vp_code_path)
        .expect("Expected a file at given code path");
    let tx_code = ctx
        .read_wasm(TX_UPDATE_VP_WASM)
        .expect("Expected a file at given code path");
    let data = UpdateVp { addr, vp_code };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let (ctx, tx) = sign_tx(ctx, tx, &args.tx, Some(&args.addr)).await;
    submit_tx(ctx, &args.tx, tx).await;
}

pub async fn submit_init_account(mut ctx: Context, args: args::TxInitAccount) {
    let public_key = ctx.get_cached(&args.public_key);
    let vp_code = args
        .vp_code_path
        .map(|path| {
            ctx.read_wasm(path)
                .expect("Expected a file at given code path")
        })
        .unwrap_or_else(|| {
            ctx.read_wasm(VP_USER_WASM)
                .expect("Expected a file at given code path")
        });
    let tx_code = ctx
        .read_wasm(TX_INIT_ACCOUNT_WASM)
        .expect("Expected a file at given code path");
    let data = InitAccount {
        public_key,
        vp_code,
    };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let (ctx, tx) = sign_tx(ctx, tx, &args.tx, Some(&args.source)).await;
    let (ctx, initialized_accounts) = submit_tx(ctx, &args.tx, tx).await;
    save_initialized_accounts(ctx, &args.tx, initialized_accounts).await;
}

pub async fn submit_init_validator(
    mut ctx: Context,
    args::TxInitValidator {
        tx: tx_args,
        source,
        account_key,
        consensus_key,
        rewards_account_key,
        validator_vp_code_path,
        rewards_vp_code_path,
        unsafe_dont_encrypt,
    }: args::TxInitValidator,
) {
    let alias = tx_args
        .initialized_account_alias
        .as_ref()
        .cloned()
        .unwrap_or_else(|| "validator".to_string());

    let validator_key_alias = format!("{}-key", alias);
    let consensus_key_alias = format!("{}-consensus-key", alias);
    let rewards_key_alias = format!("{}-rewards-key", alias);
    let account_key = ctx.get_opt_cached(&account_key).unwrap_or_else(|| {
        println!("Generating validator account key...");
        ctx.wallet
            .gen_key(Some(validator_key_alias.clone()), unsafe_dont_encrypt)
            .1
            .public
            .clone()
    });

    let consensus_key =
        ctx.get_opt_cached(&consensus_key).unwrap_or_else(|| {
            println!("Generating consensus key...");
            ctx.wallet
                .gen_key(Some(consensus_key_alias.clone()), unsafe_dont_encrypt)
                .1
        });

    let rewards_account_key =
        ctx.get_opt_cached(&rewards_account_key).unwrap_or_else(|| {
            println!("Generating staking reward account key...");
            ctx.wallet
                .gen_key(Some(rewards_key_alias.clone()), unsafe_dont_encrypt)
                .1
                .public
                .clone()
        });

    ctx.wallet.save().unwrap_or_else(|err| eprintln!("{}", err));

    let validator_vp_code = validator_vp_code_path
        .map(|path| {
            ctx.read_wasm(path)
                .expect("Expected a file at given code path")
        })
        .unwrap_or_else(|| {
            ctx.read_wasm(VP_USER_WASM)
                .expect("Expected a file at given code path")
        });
    let rewards_vp_code = rewards_vp_code_path
        .map(|path| {
            ctx.read_wasm(path)
                .expect("Expected a file at given code path")
        })
        .unwrap_or_else(|| {
            ctx.read_wasm(VP_USER_WASM)
                .expect("Expected a file at given code path")
        });
    let tx_code = ctx
        .read_wasm(TX_INIT_VALIDATOR_WASM)
        .expect("Expected a file at given code path");

    let data = InitValidator {
        account_key,
        consensus_key: consensus_key.public.clone(),
        rewards_account_key,
        validator_vp_code,
        rewards_vp_code,
    };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");
    let tx = Tx::new(tx_code, Some(data));
    let (ctx, tx) = sign_tx(ctx, tx, &tx_args, Some(&source)).await;

    let (mut ctx, initialized_accounts) = submit_tx(ctx, &tx_args, tx).await;
    if !tx_args.dry_run {
        let (validator_address_alias, validator_address, rewards_address_alias) =
            match &initialized_accounts[..] {
                // There should be 2 accounts, one for the validator itself, one
                // for its staking reward address.
                [account_1, account_2] => {
                    // We need to find out which address is which
                    let (validator_address, rewards_address) =
                        if rpc::is_validator(account_1, tx_args.ledger_address)
                            .await
                        {
                            (account_1, account_2)
                        } else {
                            (account_2, account_1)
                        };

                    let validator_address_alias = match tx_args
                        .initialized_account_alias
                    {
                        Some(alias) => alias,
                        None => {
                            print!(
                                "Choose an alias for the validator address: "
                            );
                            io::stdout().flush().await.unwrap();
                            let mut alias = String::new();
                            io::stdin().read_line(&mut alias).await.unwrap();
                            alias.trim().to_owned()
                        }
                    };
                    let validator_address_alias =
                        if validator_address_alias.is_empty() {
                            println!(
                                "Empty alias given, using {} as the alias.",
                                validator_address.encode()
                            );
                            validator_address.encode()
                        } else {
                            validator_address_alias
                        };
                    if ctx.wallet.add_address(
                        validator_address_alias.clone(),
                        validator_address.clone(),
                    ) {
                        println!(
                            "Added alias {} for address {}.",
                            validator_address_alias,
                            validator_address.encode()
                        );
                    }
                    let rewards_address_alias =
                        format!("{}-rewards", validator_address_alias);
                    if ctx.wallet.add_address(
                        rewards_address_alias.clone(),
                        rewards_address.clone(),
                    ) {
                        println!(
                            "Added alias {} for address {}.",
                            rewards_address_alias,
                            rewards_address.encode()
                        );
                    }
                    (
                        validator_address_alias,
                        validator_address.clone(),
                        rewards_address_alias,
                    )
                }
                _ => {
                    eprintln!("Expected two accounts to be created");
                    safe_exit(1)
                }
            };

        ctx.wallet.save().unwrap_or_else(|err| eprintln!("{}", err));

        let tendermint_home = &ctx.config.ledger.tendermint;
        tendermint_node::write_validator_key(
            tendermint_home,
            &validator_address,
            &consensus_key,
        );
        tendermint_node::write_validator_state(tendermint_home);

        println!();
        println!(
            "The validator's addresses and keys were stored in the wallet:"
        );
        println!("  Validator address \"{}\"", validator_address_alias);
        println!("  Staking reward address \"{}\"", rewards_address_alias);
        println!("  Validator account key \"{}\"", validator_key_alias);
        println!("  Consensus key \"{}\"", consensus_key_alias);
        println!("  Staking reward key \"{}\"", rewards_key_alias);
        println!(
            "The ledger node has been setup to use this validator's address \
             and consensus key."
        );
    } else {
        println!("Transaction dry run. No addresses have been saved.")
    }
}

pub async fn submit_transfer(ctx: Context, args: args::TxTransfer) {
    let source = ctx.get(&args.source);
    let target = ctx.get(&args.target);
    let token = ctx.get(&args.token);
    let tx_code = ctx.read_wasm(TX_TRANSFER_WASM).unwrap();
    let transfer = token::Transfer {
        source,
        target,
        token,
        amount: args.amount,
    };
    tracing::debug!("Transfer data {:?}", transfer);
    let data = transfer
        .try_to_vec()
        .expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let (ctx, tx) = sign_tx(ctx, tx, &args.tx, Some(&args.source)).await;
    submit_tx(ctx, &args.tx, tx).await;
}

pub async fn submit_bond(ctx: Context, args: args::Bond) {
    let validator = ctx.get(&args.validator);
    let source = ctx.get_opt(&args.source);
    let tx_code = ctx.read_wasm(TX_BOND_WASM).unwrap();
    let bond = pos::Bond {
        validator,
        amount: args.amount,
        source,
    };
    let data = bond.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let default_signer = args.source.as_ref().unwrap_or(&args.validator);
    let (ctx, tx) = sign_tx(ctx, tx, &args.tx, Some(default_signer)).await;
    submit_tx(ctx, &args.tx, tx).await;
}

pub async fn submit_unbond(ctx: Context, args: args::Unbond) {
    let validator = ctx.get(&args.validator);
    let source = ctx.get_opt(&args.source);
    let tx_code = ctx.read_wasm(TX_UNBOND_WASM).unwrap();

    let data = pos::Unbond {
        validator,
        amount: args.amount,
        source,
    };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let default_signer = args.source.as_ref().unwrap_or(&args.validator);
    let (ctx, tx) = sign_tx(ctx, tx, &args.tx, Some(default_signer)).await;
    submit_tx(ctx, &args.tx, tx).await;
}

pub async fn submit_withdraw(ctx: Context, args: args::Withdraw) {
    let validator = ctx.get(&args.validator);
    let source = ctx.get_opt(&args.source);
    let tx_code = ctx.read_wasm(TX_WITHDRAW_WASM).unwrap();
    let data = pos::Withdraw { validator, source };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let default_signer = args.source.as_ref().unwrap_or(&args.validator);
    let (ctx, tx) = sign_tx(ctx, tx, &args.tx, Some(default_signer)).await;
    submit_tx(ctx, &args.tx, tx).await;
}

/// Sign a transaction with a given signing key or public key of a given signer.
/// If no explicit signer given, use the `default`. If no `default` is given,
/// returns unsigned transaction.
async fn sign_tx(
    mut ctx: Context,
    tx: Tx,
    args: &args::Tx,
    default: Option<&WalletAddress>,
) -> (Context, Tx) {
    let tx = if let Some(signing_key) = &args.signing_key {
        let signing_key = ctx.get_cached(signing_key);
        tx.sign(&signing_key)
    } else if let Some(signer) = args.signer.as_ref().or(default) {
        let signer = ctx.get(signer);
        let signing_key = signing::find_keypair(
            &mut ctx.wallet,
            &signer,
            args.ledger_address.clone(),
        )
        .await;
        tx.sign(&signing_key)
    } else {
        // Unsigned tx
        tx
    };
    (ctx, tx)
}

/// Submit transaction and wait for result. Returns a list of addresses
/// initialized in the transaction if any. In dry run, this is always empty.
async fn submit_tx(
    ctx: Context,
    args: &args::Tx,
    tx: Tx,
) -> (Context, Vec<Address>) {
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
        rpc::dry_run_tx(&args.ledger_address, tx_bytes).await;
        (ctx, vec![])
    } else {
        match broadcast_tx(args.ledger_address.clone(), tx_bytes).await {
            Ok(result) => (ctx, result.initialized_accounts),
            Err(err) => {
                eprintln!(
                    "Encountered error while broadcasting transaction: {}",
                    err
                );
                safe_exit(1)
            }
        }
    }
}

/// Save accounts initialized from a tx into the wallet, if any.
async fn save_initialized_accounts(
    mut ctx: Context,
    args: &args::Tx,
    initialized_accounts: Vec<Address>,
) {
    let len = initialized_accounts.len();
    if len != 0 {
        // Store newly initialized account addresses in the wallet
        println!(
            "The transaction initialized {} new account{}",
            len,
            if len == 1 { "" } else { "s" }
        );
        // Store newly initialized account addresses in the wallet
        let wallet = &mut ctx.wallet;
        for (ix, address) in initialized_accounts.iter().enumerate() {
            let encoded = address.encode();
            let mut added = false;
            while !added {
                let alias: Cow<str> = match &args.initialized_account_alias {
                    Some(initialized_account_alias) => {
                        if len == 1 {
                            // If there's only one account, use the
                            // alias as is
                            initialized_account_alias.into()
                        } else {
                            // If there're multiple accounts, use
                            // the alias as prefix, followed by
                            // index number
                            format!("{}{}", initialized_account_alias, ix)
                                .into()
                        }
                    }
                    None => {
                        print!("Choose an alias for {}: ", encoded);
                        io::stdout().flush().await.unwrap();
                        let mut alias = String::new();
                        io::stdin().read_line(&mut alias).await.unwrap();
                        alias.trim().to_owned().into()
                    }
                };
                added = if alias.is_empty() {
                    println!(
                        "Empty alias given, using {} as the alias.",
                        encoded
                    );
                    wallet.add_address(encoded.clone(), address.clone())
                } else {
                    let alias = alias.into_owned();
                    let added =
                        wallet.add_address(alias.clone(), address.clone());
                    if added {
                        println!(
                            "Added alias {} for address {}.",
                            alias, encoded
                        );
                    }
                    added
                }
            }
        }
        if !args.dry_run {
            wallet.save().unwrap_or_else(|err| eprintln!("{}", err));
        } else {
            println!("Transaction dry run. No addresses have been saved.")
        }
    }
}

pub async fn broadcast_tx(
    address: tendermint::net::Address,
    tx_bytes: Vec<u8>,
) -> Result<TxResponse, Error> {
    let mut client =
        TendermintWebsocketClient::open(WebSocketAddress::try_from(address)?)?;
    // It is better to subscribe to the transaction before it is broadcast
    //
    // Note that the `applied.hash` key comes from a custom event
    // created by the shell
    let query = Query::from(EventType::NewBlock)
        .and_eq("applied.hash", hash_tx(&tx_bytes).to_string());
    client.subscribe(query)?;
    let response = client
        .broadcast_tx_sync(tx_bytes.into())
        .await
        .map_err(|err| Error::Response(format!("{:?}", err)))?;

    let parsed = if response.code == 0.into() {
        println!("Transaction added to mempool: {:?}", response);
        let parsed = TxResponse::from(client.receive_response()?);
        println!(
            "Transaction applied with result: {}",
            serde_json::to_string_pretty(&parsed).unwrap()
        );
        Ok(parsed)
    } else {
        Err(Error::Response(response.log.to_string()))
    };
    client.unsubscribe()?;
    client.close();
    parsed
}

#[derive(Debug, Serialize)]
pub struct TxResponse {
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
                //     "[\"atest1...\"]",
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
