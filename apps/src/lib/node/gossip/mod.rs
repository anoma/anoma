pub mod intent_gossiper;
mod mempool;
pub mod p2p;
pub mod rpc;

use std::path::Path;

use anoma::proto::Intent;
use thiserror::Error;
use tokio::sync::mpsc;

use self::intent_gossiper::IntentGossiper;
use self::p2p::P2P;
use crate::config;
use crate::proto::services::{rpc_message, RpcResponse};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Error initializing p2p: {0}")]
    P2pInit(p2p::Error),
}

type Result<T> = std::result::Result<T, Error>;

/// RPC async receiver end of the channel
pub type RpcReceiver = tokio::sync::mpsc::Receiver<(
    rpc_message::Message,
    tokio::sync::oneshot::Sender<RpcResponse>,
)>;

#[tokio::main]
pub async fn run(
    config: config::IntentGossiper,
    base_dir: impl AsRef<Path>,
) -> Result<()> {
    // Prepare matchmakers server and dialer
    let (matchmakers_server, intent_gossiper) =
        intent_gossiper::MatchmakersServer::new_pair(
            &config.matchmakers_server_addr,
        );

    // Async channel for intents received from peer
    let (peer_intent_send, peer_intent_recv) = tokio::sync::mpsc::channel(100);

    // Create the P2P gossip network, which can send messages directly to the
    // matchmaker, if any
    let p2p = p2p::P2P::new(&config, base_dir, peer_intent_send)
        .await
        .map_err(Error::P2pInit)?;

    // Run the matchmakers server
    let mms_join_handle = tokio::task::spawn(async move {
        matchmakers_server.listen().await;
    });

    // Start the RPC server, if enabled in the config
    let rpc_receiver = config.rpc.map(|rpc_config| {
        let (rpc_sender, rpc_receiver) = mpsc::channel(100);
        tokio::spawn(async move {
            rpc::client::start_rpc_server(&rpc_config, rpc_sender).await
        });
        rpc_receiver
    });

    dispatcher(
        p2p,
        rpc_receiver,
        peer_intent_recv,
        intent_gossiper,
        mms_join_handle,
    )
    .await
}

// loop over all possible event. The event can be from the rpc, a matchmaker
// program or the gossip network. The gossip network event are a special case
// that does not need to be handle as it's taking care of by the libp2p internal
// logic.
pub async fn dispatcher(
    mut p2p: P2P,
    mut rpc_receiver: Option<RpcReceiver>,
    mut peer_intent_recv: tokio::sync::mpsc::Receiver<Intent>,
    mut intent_gossiper: IntentGossiper,
    _mms_join_handle: tokio::task::JoinHandle<()>,
) -> Result<()> {
    loop {
        tokio::select! {
            Some((event, inject_response)) = recv_rpc_option(rpc_receiver.as_mut()), if rpc_receiver.is_some() =>
            {
                let gossip_sub = &mut p2p.0.behaviour_mut().intent_gossip_behaviour;
                let (response, maybe_intent) = rpc::client::handle_rpc_event(event, gossip_sub).await;
                inject_response.send(response).expect("failed to send response to rpc server");

                if let Some(intent) = maybe_intent {
                    intent_gossiper.add_intent(intent).await;
                }
            },
            Some(intent) = peer_intent_recv.recv() => {
                intent_gossiper.add_intent(intent).await;
            }
            swarm_event = p2p.0.next() => {
                // Never occurs, but call for the event must exists.
                tracing::info!("event, {:?}", swarm_event);
            },
        };
    }
}

async fn recv_rpc_option(
    x: Option<&mut RpcReceiver>,
) -> Option<(
    rpc_message::Message,
    tokio::sync::oneshot::Sender<RpcResponse>,
)> {
    x?.recv().await
}
