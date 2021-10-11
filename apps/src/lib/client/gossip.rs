use std::collections::HashSet;
use std::io::Write;

use anoma::types::intent::{Exchange, FungibleTokenIntent};
use anoma::types::key::ed25519::Signed;
use borsh::BorshSerialize;

use super::signing;
use crate::cli::{self, args, Context};
use crate::proto::services::rpc_service_client::RpcServiceClient;
use crate::proto::{services, RpcMessage};
use crate::wallet::Wallet;

/// Create an intent, sign it and submit it to the gossip node (unless
/// `to_stdout` is `true`).
pub async fn gossip_intent(
    mut ctx: Context,
    args::Intent {
        node_addr,
        topic,
        source,
        signing_key,
        exchanges,
        ledger_address,
        to_stdout,
    }: args::Intent,
) {
    let mut signed_exchanges: HashSet<Signed<Exchange>> =
        HashSet::with_capacity(exchanges.len());
    for exchange in exchanges {
        let signed =
            sign_exchange(&mut ctx.wallet, exchange, ledger_address.clone())
                .await;
        signed_exchanges.insert(signed);
    }

    let source_keypair = match ctx.get_opt_cached(&signing_key) {
        Some(key) => key,
        None => {
            let source = ctx.get_opt(&source).unwrap_or_else(|| {
                eprintln!("A source or a signing key is required.");
                cli::safe_exit(1)
            });
            signing::find_keypair(
                &mut ctx.wallet,
                &source,
                ledger_address.clone(),
            )
            .await
        }
    };
    let signed_ft: Signed<FungibleTokenIntent> = Signed::new(
        &source_keypair,
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

        let intent = anoma::proto::Intent::new(data_bytes);
        let message: services::RpcMessage =
            RpcMessage::new_intent(intent, topic).into();
        let response = client
            .send_message(message)
            .await
            .expect("failed to send message and/or receive rpc response");
        println!("{:#?}", response);
    }
}

/// Request an intent gossip node with a  matchmaker to subscribe to a given
/// topic.
pub async fn subscribe_topic(
    _ctx: Context,
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

async fn sign_exchange(
    wallet: &mut Wallet,
    exchange: Exchange,
    ledger_address: tendermint::net::Address,
) -> Signed<Exchange> {
    let source_keypair =
        signing::find_keypair(wallet, &exchange.addr, ledger_address).await;
    Signed::new(&source_keypair, exchange.clone())
}
