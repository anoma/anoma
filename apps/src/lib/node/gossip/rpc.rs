use std::convert::TryFrom;
use std::net::SocketAddr;
use std::thread;

use anoma::proto::{Intent, IntentGossipMessage};
use libp2p::gossipsub::IdentTopic;
use tokio::sync::mpsc::{self, Sender};
use tokio::sync::oneshot;
use tonic::transport::Server;
use tonic::{Request as TonicRequest, Response as TonicResponse, Status};

use super::behaviour::Gossipsub;
use crate::config::RpcServer;
use crate::proto::services::rpc_service_server::{
    RpcService, RpcServiceServer,
};
use crate::proto::services::{rpc_message, RpcMessage, RpcResponse};
use crate::proto::{IntentMessage, SubscribeTopicMessage};

#[derive(Debug)]
struct Rpc {
    inject_message:
        mpsc::Sender<(rpc_message::Message, oneshot::Sender<RpcResponse>)>,
}

#[tonic::async_trait]
impl RpcService for Rpc {
    async fn send_message(
        &self,
        request: TonicRequest<RpcMessage>,
    ) -> Result<TonicResponse<RpcResponse>, Status> {
        if let RpcMessage { message: Some(msg) } = request.into_inner() {
            let (sender, receiver) = oneshot::channel();
            self.inject_message
                .send((msg, sender))
                .await
                .map_err(|err|
                         Status::cancelled(format!{"failed to send message to gossip app: {:?}",err})
                )?
                ;
            let response = receiver.await.map_err(|err|
                Status::data_loss(format!{"failed to receive response from gossip app: {:?}", err}))?;
            Ok(TonicResponse::new(response))
        } else {
            tracing::error!("Received empty rpc message, nothing can be done");
            Ok(TonicResponse::new(RpcResponse::default()))
        }
    }
}

#[tokio::main]
pub async fn rpc_server(
    addr: SocketAddr,
    inject_message: Sender<(
        rpc_message::Message,
        oneshot::Sender<RpcResponse>,
    )>,
) -> Result<(), tonic::transport::Error> {
    let rpc = Rpc { inject_message };
    let svc = RpcServiceServer::new(rpc);
    Server::builder().add_service(svc).serve(addr).await
}

/// Start a rpc server in it's own thread. The used address to listen is in the
/// `config` argument. All received event by the rpc are send to the channel
/// return by this function.
pub fn start_rpc_server(
    config: &RpcServer,
) -> mpsc::Receiver<(
    rpc_message::Message,
    tokio::sync::oneshot::Sender<RpcResponse>,
)> {
    let addr = config.address;
    let (rpc_sender, rpc_receiver) = mpsc::channel(100);
    thread::spawn(move || rpc_server(addr, rpc_sender).unwrap());
    rpc_receiver
}

pub async fn handle_rpc_event(
    event: rpc_message::Message,
    gossip_sub: &mut Gossipsub,
) -> (RpcResponse, Option<Intent>) {
    match event {
        rpc_message::Message::Intent(message) => {
            match IntentMessage::try_from(message) {
                Ok(message) => {
                    // Send the intent to gossip
                    let gossip_message =
                        IntentGossipMessage::new(message.intent.clone());
                    let intent_bytes = gossip_message.to_bytes();

                    let gossip_result = match gossip_sub
                        .publish(IdentTopic::new(message.topic), intent_bytes)
                    {
                        Ok(message_id) => {
                            format!(
                                "Intent published in intent gossiper with \
                                 message ID: {}",
                                message_id
                            )
                        }
                        Err(err) => {
                            format!(
                                "Failed to publish intent in gossiper: {:?}",
                                err
                            )
                        }
                    };
                    (
                        RpcResponse {
                            result: format!(
                                "Intent received. {}.",
                                gossip_result,
                            ),
                        },
                        Some(message.intent),
                    )
                }
                Err(err) => (
                    RpcResponse {
                        result: format!("Error decoding intent: {:?}", err),
                    },
                    None,
                ),
            }
        }
        rpc_message::Message::Dkg(dkg_msg) => {
            tracing::debug!("dkg not yet implemented {:?}", dkg_msg);
            (
                RpcResponse {
                    result: String::from(
                        "DKG application not yet
    implemented",
                    ),
                },
                None,
            )
        }
        rpc_message::Message::Topic(topic_message) => {
            let topic = SubscribeTopicMessage::from(topic_message);
            let topic = IdentTopic::new(&topic.topic);
            (
                match gossip_sub.subscribe(&topic) {
                    Ok(true) => {
                        let result = format!("Node subscribed to {}", topic);
                        tracing::info!("{}", result);
                        RpcResponse { result }
                    }
                    Ok(false) => {
                        let result =
                            format!("Node already subscribed to {}", topic);
                        tracing::info!("{}", result);
                        RpcResponse { result }
                    }
                    Err(err) => {
                        let result = format!(
                            "failed to subscribe to {}: {:?}",
                            topic, err
                        );
                        tracing::error!("{}", result);
                        RpcResponse { result }
                    }
                },
                None,
            )
        }
    }
}
