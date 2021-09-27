//! Anoma client CLI.

use anoma_apps::cli;
use anoma_apps::cli::cmds::*;
use anoma_apps::client::{gossip, rpc, tx, utils};
use color_eyre::eyre::Result;

pub async fn main() -> Result<()> {
    let (cmd, ctx) = cli::anoma_client_cli();
    match cmd {
        // Ledger cmds
        AnomaClient::TxCustom(TxCustom(args)) => {
            tx::submit_custom(ctx, args).await;
        }
        AnomaClient::TxTransfer(TxTransfer(args)) => {
            tx::submit_transfer(ctx, args).await;
        }
        AnomaClient::TxUpdateVp(TxUpdateVp(args)) => {
            tx::submit_update_vp(ctx, args).await;
        }
        AnomaClient::TxInitAccount(TxInitAccount(args)) => {
            tx::submit_init_account(ctx, args).await;
        }
        AnomaClient::TxInitValidator(TxInitValidator(args)) => {
            tx::submit_init_validator(ctx, args).await;
        }
        AnomaClient::Bond(Bond(args)) => {
            tx::submit_bond(ctx, args).await;
        }
        AnomaClient::Unbond(Unbond(args)) => {
            tx::submit_unbond(ctx, args).await;
        }
        AnomaClient::Withdraw(Withdraw(args)) => {
            tx::submit_withdraw(ctx, args).await;
        }
        // Ledger queries
        AnomaClient::QueryEpoch(QueryEpoch(args)) => {
            rpc::query_epoch(ctx, args).await;
        }
        AnomaClient::QueryBalance(QueryBalance(args)) => {
            rpc::query_balance(ctx, args).await;
        }
        AnomaClient::QueryBonds(QueryBonds(args)) => {
            rpc::query_bonds(ctx, args).await;
        }
        AnomaClient::QueryVotingPower(QueryVotingPower(args)) => {
            rpc::query_voting_power(ctx, args).await;
        }
        AnomaClient::QuerySlashes(QuerySlashes(args)) => {
            rpc::query_slashes(ctx, args).await;
        }
        // Gossip cmds
        AnomaClient::Intent(Intent(args)) => {
            gossip::gossip_intent(ctx, args).await;
        }
        AnomaClient::SubscribeTopic(SubscribeTopic(args)) => {
            gossip::subscribe_topic(ctx, args).await;
        }
        // Utils cmds
        AnomaClient::Utils(cmd) => match cmd {
            Utils::InitGenesisValidator(InitGenesisValidator(args)) => {
                utils::init_genesis_validator(ctx, args)
            }
        },
    }
    Ok(())
}
