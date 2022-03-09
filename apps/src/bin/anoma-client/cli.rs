//! Anoma client CLI.

use anoma::ledger::rpc;
use anoma_apps::cli;
use anoma_apps::cli::args::{
    Query, QueryBalance, QueryBonds, QueryResult, QuerySlashes,
    QueryVotingPower,
};
use anoma_apps::cli::cmds::*;
use anoma_apps::cli::context::Context;
use anoma_apps::client::{gossip, tx, utils};
use color_eyre::eyre::Result;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::HttpClient;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::HttpClient;

pub async fn main() -> Result<()> {
    match cli::anoma_client_cli() {
        cli::AnomaClient::WithContext(cmd_box) => {
            let (cmd, ctx) = *cmd_box;
            use AnomaClientWithContext as Sub;
            match cmd {
                // Ledger cmds
                Sub::TxCustom(TxCustom(args)) => {
                    tx::submit_custom(ctx, args).await;
                }
                Sub::TxTransfer(TxTransfer(args)) => {
                    tx::submit_transfer(ctx, args).await;
                }
                Sub::TxUpdateVp(TxUpdateVp(args)) => {
                    tx::submit_update_vp(ctx, args).await;
                }
                Sub::TxInitAccount(TxInitAccount(args)) => {
                    tx::submit_init_account(ctx, args).await;
                }
                Sub::TxInitValidator(TxInitValidator(args)) => {
                    tx::submit_init_validator(ctx, args).await;
                }
                Sub::TxInitNft(TxInitNft(args)) => {
                    tx::submit_init_nft(ctx, args).await;
                }
                Sub::TxMintNft(TxMintNft(args)) => {
                    tx::submit_mint_nft(ctx, args).await;
                }
                Sub::Bond(Bond(args)) => {
                    tx::submit_bond(ctx, args).await;
                }
                Sub::Unbond(Unbond(args)) => {
                    tx::submit_unbond(ctx, args).await;
                }
                Sub::Withdraw(Withdraw(args)) => {
                    tx::submit_withdraw(ctx, args).await;
                }
                // Ledger queries
                Sub::QueryEpoch(QueryEpoch(args)) => {
                    query_epoch(args).await?;
                }
                Sub::QueryBalance(QueryBalance(args)) => {
                    query_balance(ctx, args).await?;
                }
                Sub::QueryBonds(QueryBonds(args)) => {
                    query_bonds(ctx, args).await?;
                }
                Sub::QueryVotingPower(QueryVotingPower(args)) => {
                    query_voting_power(ctx, args).await?;
                }
                Sub::QuerySlashes(QuerySlashes(args)) => {
                    query_slashes(ctx, args).await?;
                }
                Sub::QueryResult(QueryResult(args)) => {
                    query_result(args).await?;
                }
                // Gossip cmds
                Sub::Intent(Intent(args)) => {
                    gossip::gossip_intent(ctx, args).await;
                }
                Sub::SubscribeTopic(SubscribeTopic(args)) => {
                    gossip::subscribe_topic(ctx, args).await;
                }
            }
        }
        cli::AnomaClient::WithoutContext(cmd, global_args) => match cmd {
            // Utils cmds
            Utils::JoinNetwork(JoinNetwork(args)) => {
                utils::join_network(global_args, args).await
            }
            Utils::InitNetwork(InitNetwork(args)) => {
                utils::init_network(global_args, args)
            }
            Utils::InitGenesisValidator(InitGenesisValidator(args)) => {
                utils::init_genesis_validator(global_args, args)
            }
        },
    }

    Ok(())
}

async fn query_epoch(args: Query) -> Result<()> {
    let client = HttpClient::new(args.ledger_address)?;
    match rpc::query_epoch(client).await {
        Ok(v) => println!("Last committed epoch: {}", v),
        Err(e) => eprintln!("{}", e),
    }

    Ok(())
}

async fn query_balance(ctx: Context, args: QueryBalance) -> Result<()> {
    let client = HttpClient::new(args.query.ledger_address)?;

    let token = args.token.map(|t| ctx.get(&t));
    let owner = args.owner.map(|o| ctx.get(&o));

    match rpc::query_balance(client, token, owner).await {
        Ok(result) => print!("{}", result),
        Err(e) => eprintln!("Error while querying balance. {}", e),
    }

    Ok(())
}

async fn query_bonds(ctx: Context, args: QueryBonds) -> Result<()> {
    let client = HttpClient::new(args.query.ledger_address)?;

    let validator = args.validator.map(|v| ctx.get(&v));
    let owner = args.owner.map(|o| ctx.get(&o));

    match rpc::query_bonds_amount(client, owner.clone(), validator.clone())
        .await
    {
        Ok(result) => {
            match (owner, validator) {
                (Some(owner), Some(validator)) => println!(
                    "Bonds status for owner {} on validator {}:",
                    owner, validator
                ),
                (Some(owner), None) => {
                    println!("Bonds status for owner {}:", owner)
                }
                (None, Some(validator)) => {
                    println!("Bonds status for validator {}:", validator)
                }
                (None, None) => println!("Bonds status of the chain:"),
            }
            print!("{}", result);
        }
        Err(e) => eprintln!("Error while querying bonds. {}", e),
    }

    Ok(())
}

async fn query_voting_power(
    ctx: Context,
    args: QueryVotingPower,
) -> Result<()> {
    let client = HttpClient::new(args.query.ledger_address)?;

    let validator = args.validator.map(|v| ctx.get(&v));

    match rpc::query_voting_power(client, validator.as_ref(), args.epoch).await
    {
        Ok(result) => match (result, validator) {
            (Some(voting_power), Some(validator)) => println!(
                "Voting power for validator {}: {}",
                validator, voting_power
            ),
            (Some(voting_power), None) => {
                println!("Total voting power on chain: {}", voting_power)
            }
            (None, Some(validator)) => {
                println!("No voting power for validator {}", validator)
            }
            (None, None) => println!("No voting power on chain"),
        },
        Err(e) => eprintln!("Error while querying voting power. {}", e),
    }

    Ok(())
}

async fn query_slashes(ctx: Context, args: QuerySlashes) -> Result<()> {
    let client = HttpClient::new(args.query.ledger_address)?;

    let validator = args.validator.map(|v| ctx.get(&v));

    match rpc::query_slashes(client, validator.clone()).await {
        Ok(result) => println!("{}", result),
        Err(e) => eprintln!("Error while querying slashes. {}", e),
    }

    Ok(())
}

async fn query_result(args: QueryResult) -> Result<()> {
    let client = HttpClient::new(args.query.ledger_address.clone())?;

    match rpc::query_tx_result(client, args.tx_hash.clone()).await {
        Ok(response) => {
            println!("Response for tx {}: {}", args.tx_hash, response)
        }
        Err(e) => eprintln!("Error while querying tx result. {}", e),
    }

    Ok(())
}
