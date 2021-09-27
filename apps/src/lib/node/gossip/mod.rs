mod behaviour;
mod intent_gossiper;
mod p2p;
mod rpc;

use std::borrow::Cow;
use std::rc::Rc;

use anoma::types::address::Address;
use anoma::types::key::ed25519::Keypair;
use thiserror::Error;
use tokio::sync::{mpsc, oneshot};

use self::intent_gossiper::GossipIntent;
use self::p2p::P2P;
use crate::config::IntentGossiper;
use crate::proto::services::{rpc_message, RpcResponse};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Error initializing p2p: {0}")]
    P2pInit(p2p::Error),
}

type Result<T> = std::result::Result<T, Error>;

pub fn run(
    config: IntentGossiper,
    tx_source_address: Option<Address>,
    tx_signing_key: Option<Rc<Keypair>>,
) -> Result<()> {
    // Start intent gossiper with matchmaker, if enabled.
    let intent_gossip_app = intent_gossiper::GossipIntent::new(
        &config,
        tx_source_address,
        tx_signing_key,
    )
    .unwrap();

    // Create the P2P gossip network, which can send messages directly to the
    // matchmaker, if any
    let gossip = p2p::P2P::new(&config, intent_gossip_app.mm_sender.clone())
        .map_err(Error::P2pInit)?;

    // Start the rpc socket, if enabled in the config
    let rpc_event_receiver =
        config.rpc.as_ref().map(|rpc| rpc::start_rpc_server(rpc));

    dispatcher(gossip, intent_gossip_app, rpc_event_receiver)
}

// loop over all possible event. The event can be from the rpc, a matchmaker
// program or the gossip network. The gossip network event are a special case
// that does not need to be handle as it's taking care of by the libp2p internal
// logic.
#[tokio::main]
pub async fn dispatcher(
    mut gossip: P2P,
    mut intent_gossip_app: GossipIntent,
    rpc_receiver: Option<
        mpsc::Receiver<(rpc_message::Message, oneshot::Sender<RpcResponse>)>,
    >,
) -> Result<()> {
    // TODO find a nice way to refactor here
    match (rpc_receiver, intent_gossip_app.mm_receiver.take()) {
        (Some(mut rpc_receiver), Some(mut mm_receiver)) => {
            loop {
                tokio::select! {
                    Some(message) = mm_receiver.recv() =>
                    {
                        intent_gossip_app.handle_mm_message(message).await
                    },
                    Some((event, inject_response)) = rpc_receiver.recv() =>
                    {
                        let gossip_sub = &mut gossip.0.behaviour_mut().intent_gossip_behaviour;
                        let (response, maybe_intent) = rpc::handle_rpc_event(event, gossip_sub).await;
                        inject_response.send(response).expect("failed to send response to rpc server");

                        // apply intents in matchmaker
                        if let Some(intent) = maybe_intent {
                            let mm_result: Cow<str> = match intent_gossip_app.apply_intent(intent) {
                                Ok(true) => "Accepted intent".into(),
                                Ok(false) => "Rejected intent".into(),
                                Err(err) => format!(
                                    "Error getting intent response from the matchmaker: {}",
                                    err
                                )
                                .into(),
                            };
                            tracing::info!("matchmaker intent result: {}", mm_result);
                        }
                    },
                    swarm_event = gossip.0.next() => {
                        // Never occurs, but call for the event must exists.
                        tracing::info!("event, {:?}", swarm_event);
                    },
                };
            }
        }
        (Some(mut rpc_receiver), None) => loop {
            tokio::select! {
                Some((event, inject_response)) = rpc_receiver.recv() =>
                {
                    let gossip_sub = &mut gossip.0.behaviour_mut().intent_gossip_behaviour;
                    let (response, _maybe_intent) = rpc::handle_rpc_event(event, gossip_sub).await;
                    inject_response.send(response).expect("failed to send response to rpc server")
                },
                swarm_event = gossip.0.next() => {
                    // Never occurs, but call for the event must exists.
                    tracing::info!("event, {:?}", swarm_event);
                },
            };
        },
        (None, Some(mut mm_receiver)) => loop {
            tokio::select! {
                Some(message) = mm_receiver.recv() =>
                {
                    intent_gossip_app.handle_mm_message(message).await
                },
                swarm_event = gossip.0.next() => {
                    // Never occurs, but call for the event must exists.
                    tracing::info!("event, {:?}", swarm_event);
                },
            };
        },
        (None, None) => loop {
            tokio::select! {
                swarm_event = gossip.0.next() => {
                    // Never occurs, but call for the event must exists.
                    tracing::info!("event, {:?}", swarm_event);
                },
            }
        },
    }
}
