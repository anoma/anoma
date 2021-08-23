mod behaviour;
mod intent_gossiper;
mod p2p;
mod rpc;

use thiserror::Error;
use tokio::sync::{mpsc, oneshot};

use self::p2p::P2P;
use crate::config::IntentGossiper;
use crate::proto::services::{rpc_message, RpcResponse};
use crate::types::MatchmakerMessage;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Error initializing p2p: {0}")]
    P2pInit(p2p::Error),
}

type Result<T> = std::result::Result<T, Error>;

pub fn run(config: IntentGossiper) -> Result<()> {
    // if enabled in the config start the rpc socket
    let rpc_event_receiver = config.rpc.as_ref().map(rpc::start_rpc_server);

    // create the gossip and possibly the matchmaker
    let (gossip, matchmaker_event_receiver) =
        p2p::P2P::new(&config).map_err(Error::P2pInit)?;

    dispatcher(gossip, rpc_event_receiver, matchmaker_event_receiver)
}

// loop over all possible event. The event can be from the rpc, a matchmaker
// program or the gossip network. The gossip network event are a special case
// that does not need to be handle as it's taking care of by the libp2p internal
// logic.
#[tokio::main]
pub async fn dispatcher(
    mut gossip: P2P,
    rpc_event_receiver: Option<
        mpsc::Receiver<(rpc_message::Message, oneshot::Sender<RpcResponse>)>,
    >,
    matchmaker_event_receiver: Option<mpsc::Receiver<MatchmakerMessage>>,
) -> Result<()> {
    // TODO find a nice way to refactor here
    match (rpc_event_receiver, matchmaker_event_receiver) {
        (Some(mut rpc_event_receiver), Some(mut matchmaker_event_receiver)) => {
            loop {
                tokio::select! {
                    Some(message) = matchmaker_event_receiver.recv() =>
                    {
                        gossip.handle_mm_message(message).await
                    },
                    Some((event, inject_response)) = rpc_event_receiver.recv() =>
                    {
                        let response = gossip.handle_rpc_event(event).await;
                        inject_response.send(response).expect("failed to send response to rpc server")
                    },
                    swarm_event = gossip.0.next() => {
                        // Never occurs, but call for the event must exists.
                        tracing::info!("event, {:?}", swarm_event);
                    },
                };
            }
        }
        (Some(mut rpc_event_receiver), None) => loop {
            tokio::select! {
                Some((event, inject_response)) = rpc_event_receiver.recv() =>
                {
                    let response = gossip.handle_rpc_event(event).await;
                    inject_response.send(response).expect("failed to send response to rpc server")
                },
                swarm_event = gossip.0.next() => {
                    // Never occurs, but call for the event must exists.
                    tracing::info!("event, {:?}", swarm_event);
                },
            };
        },
        (None, Some(mut matchmaker_event_receiver)) => loop {
            tokio::select! {
                Some(message) = matchmaker_event_receiver.recv() =>
                {
                    gossip.handle_mm_message(message).await
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
