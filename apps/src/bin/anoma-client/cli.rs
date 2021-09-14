//! Anoma client CLI.

use anoma_apps::cli;
use anoma_apps::cli::cmds;
use anoma_apps::client::{rpc, tx};
use color_eyre::eyre::Result;

pub async fn main() -> Result<()> {
    let (cmd, _global_args) = cli::anoma_client_cli();
    match cmd {
        cmds::AnomaClient::TxCustom(cmds::TxCustom(args)) => {
            tx::submit_custom(args).await;
        }
        cmds::AnomaClient::TxTransfer(cmds::TxTransfer(args)) => {
            tx::submit_transfer(args).await;
        }
        cmds::AnomaClient::TxUpdateVp(cmds::TxUpdateVp(args)) => {
            tx::submit_update_vp(args).await;
        }
        cmds::AnomaClient::TxInitAccount(cmds::TxInitAccount(args)) => {
            tx::submit_init_account(args).await;
        }
        cmds::AnomaClient::QueryBalance(cmds::QueryBalance(args)) => {
            rpc::query_balance(args).await;
        }
        cmds::AnomaClient::Intent(cmds::Intent(args)) => {
            tx::gossip_intent(args).await;
        }
        cmds::AnomaClient::SubscribeTopic(cmds::SubscribeTopic(args)) => {
            tx::subscribe_topic(args).await;
        }
        cmds::AnomaClient::NftCreate(cmds::NftCreate(args)) => {
            tx::create_nft(args).await;
        }
        cmds::AnomaClient::NftMint(cmds::NftMint(args)) => {
            tx::mint_nft(args).await;
        }
    }
    Ok(())
}
