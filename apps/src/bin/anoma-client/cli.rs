//! Anoma client CLI.

use anoma_apps::cli;
use anoma_apps::cli::cmds::*;
use anoma_apps::client::{gossip, rpc, tx, utils};
use color_eyre::eyre::Result;

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
                    rpc::query_epoch(args).await;
                }
                Sub::QueryTransfers(QueryTransfers(args)) => {
                    rpc::query_transfers(ctx, args).await;
                }
                Sub::QueryBalance(QueryBalance(args)) => {
                    rpc::query_balance(ctx, args).await;
                }
                Sub::QueryBonds(QueryBonds(args)) => {
                    rpc::query_bonds(ctx, args).await;
                }
                Sub::QueryVotingPower(QueryVotingPower(args)) => {
                    rpc::query_voting_power(ctx, args).await;
                }
                Sub::QuerySlashes(QuerySlashes(args)) => {
                    rpc::query_slashes(ctx, args).await;
                }
                Sub::QueryResult(QueryResult(args)) => {
                    rpc::query_result(ctx, args).await;
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
