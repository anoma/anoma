//! Anoma client CLI.

use std::fs::File;
use std::io::Write;

use anoma::types::intent::Intent;
use anoma::types::key::ed25519::Signed;
use anoma_apps::cli::{args, cmds};
use anoma_apps::client::tx;
use anoma_apps::proto::services::rpc_service_client::RpcServiceClient;
use anoma_apps::proto::{services, RpcMessage};
use anoma_apps::{cli, wallet};
use borsh::BorshSerialize;
use color_eyre::eyre::Result;

pub async fn main() -> Result<()> {
    let (cmd, _global_args) = cli::anoma_client_cli();
    match cmd {
        // Ledger cmds
        cmds::AnomaClient::TxCustom(cmds::TxCustom(args)) => {
            tx::submit_custom(args).await;
        }
        cmds::AnomaClient::TxTransfer(cmds::TxTransfer(args)) => {
            tx::submit_transfer(args).await;
        }
        cmds::AnomaClient::TxUpdateVp(cmds::TxUpdateVp(args)) => {
            tx::submit_update_vp(args).await;
        }
        cmds::AnomaClient::Bond(cmds::Bond(args)) => {
            tx::submit_bond(args).await;
        }
        cmds::AnomaClient::Unbond(cmds::Unbond(args)) => {
            tx::submit_unbond(args).await;
        }
        cmds::AnomaClient::Withdraw(cmds::Withdraw(args)) => {
            tx::submit_withdraw(args).await;
        }
        // Gossip cmds
        cmds::AnomaClient::Intent(cmds::Intent(args)) => {
            gossip_intent(args).await;
        }
        cmds::AnomaClient::CraftIntent(cmds::CraftIntent(args)) => {
            craft_intent(args);
        }
        cmds::AnomaClient::SubscribeTopic(cmds::SubscribeTopic(args)) => {
            subscribe_topic(args).await;
        }
    }
    Ok(())
}

async fn gossip_intent(
    args::Intent {
        node_addr,
        data_path,
        topic,
    }: args::Intent,
) {
    let mut client = RpcServiceClient::connect(node_addr).await.unwrap();
    let data = std::fs::read(data_path).expect("data file IO error");
    let intent = anoma::proto::Intent::new(data);
    let message: services::RpcMessage =
        RpcMessage::new_intent(intent, topic).into();
    let response = client
        .send_message(message)
        .await
        .expect("failed to send message and/or receive rpc response");
    println!("{:#?}", response);
}

async fn subscribe_topic(
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

fn craft_intent(
    args::CraftIntent {
        addr,
        token_sell,
        amount_sell,
        token_buy,
        amount_buy,
        file_path,
    }: args::CraftIntent,
) {
    let source_keypair = wallet::key_of(&addr.encode());

    let intent = Intent {
        addr,
        token_sell,
        amount_sell,
        token_buy,
        amount_buy,
    };
    let signed: Signed<Intent> = Signed::new(&source_keypair, intent);
    let data_bytes = signed.try_to_vec().unwrap();

    let mut file = File::create(file_path).unwrap();
    file.write_all(&data_bytes).unwrap();
}
