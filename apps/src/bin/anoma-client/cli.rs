//! Anoma client CLI.

use anoma_apps::cli;
use anoma_apps::cli::cmds;
use anoma_apps::client::{rpc, tx};
use color_eyre::eyre::Result;

pub async fn main() -> Result<()> {
    let (cmd, ctx) = cli::anoma_client_cli();
    match cmd {
        // Ledger cmds
        cmds::AnomaClient::TxCustom(cmds::TxCustom(args)) => {
            tx::submit_custom(ctx, args).await;
        }
        cmds::AnomaClient::TxTransfer(cmds::TxTransfer(args)) => {
            tx::submit_transfer(ctx, args).await;
        }
        cmds::AnomaClient::TxUpdateVp(cmds::TxUpdateVp(args)) => {
            tx::submit_update_vp(ctx, args).await;
        }
        cmds::AnomaClient::TxInitAccount(cmds::TxInitAccount(args)) => {
            tx::submit_init_account(ctx, args).await;
        }
        cmds::AnomaClient::NftCreate(cmds::NftCreate(args)) => {
            tx::create_nft(ctx, args).await;
        }
        cmds::AnomaClient::NftMint(cmds::NftMint(args)) => {
            tx::mint_nft(ctx, args).await;
        }
        cmds::AnomaClient::Bond(cmds::Bond(args)) => {
            tx::submit_bond(ctx, args).await;
        }
        cmds::AnomaClient::Unbond(cmds::Unbond(args)) => {
            tx::submit_unbond(ctx, args).await;
        }
        cmds::AnomaClient::Withdraw(cmds::Withdraw(args)) => {
            tx::submit_withdraw(ctx, args).await;
        }
        cmds::AnomaClient::QueryEpoch(cmds::QueryEpoch(args)) => {
            rpc::query_epoch(args).await;
        }
        cmds::AnomaClient::QueryBalance(cmds::QueryBalance(args)) => {
            rpc::query_balance(ctx, args).await;
        }
        cmds::AnomaClient::QueryBonds(cmds::QueryBonds(args)) => {
            rpc::query_bonds(ctx, args).await;
        }
        cmds::AnomaClient::QueryVotingPower(cmds::QueryVotingPower(args)) => {
            rpc::query_voting_power(ctx, args).await;
        }
        cmds::AnomaClient::QuerySlashes(cmds::QuerySlashes(args)) => {
            rpc::query_slashes(ctx, args).await;
        }
        // Gossip cmds
        cmds::AnomaClient::Intent(cmds::Intent(args)) => {
            tx::gossip_intent(ctx, args).await;
        }
        cmds::AnomaClient::SubscribeTopic(cmds::SubscribeTopic(args)) => {
            tx::subscribe_topic(ctx, args).await;
        }
    }
    Ok(())
}
