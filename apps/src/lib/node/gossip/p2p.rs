use std::convert::TryFrom;
use std::time::Duration;

use anoma::proto::IntentGossipMessage;
use libp2p::core::connection::ConnectionLimits;
use libp2p::core::muxing::StreamMuxerBox;
use libp2p::core::transport::Boxed;
use libp2p::dns::DnsConfig;
use libp2p::gossipsub::IdentTopic;
use libp2p::identity::Keypair;
use libp2p::swarm::SwarmBuilder;
use libp2p::tcp::TcpConfig;
use libp2p::websocket::WsConfig;
use libp2p::{core, mplex, noise, PeerId, Transport, TransportError};
use thiserror::Error;
use tokio::sync::mpsc::Receiver;

use super::behaviour::Behaviour;
use crate::proto::services::{rpc_message, RpcResponse};
use crate::proto::{IntentMessage, SubscribeTopicMessage};
use crate::types::MatchmakerMessage;

pub type Swarm = libp2p::Swarm<Behaviour>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed initializing the transport: {0}")]
    Transport(std::io::Error),
    #[error("Error with the network behavior: {0}")]
    Behavior(super::behaviour::Error),
    #[error("Error while dialing: {0}")]
    Dialing(libp2p::swarm::DialError),
    #[error("Error while starting to listing: {0}")]
    Listening(TransportError<std::io::Error>),
    #[error("Error decoding peer identity")]
    BadPeerIdentity(TransportError<std::io::Error>),
}
type Result<T> = std::result::Result<T, Error>;

pub struct P2P {
    pub swarm: Swarm,
}

impl P2P {
    pub fn new(
        config: &crate::config::IntentGossiper,
    ) -> Result<(Self, Option<Receiver<MatchmakerMessage>>)> {
        let peer_key = Keypair::Ed25519(config.gossiper.key.clone());
        let peer_id = PeerId::from(peer_key.public());

        tracing::info!("Peer id: {:?}", peer_id.clone());

        let transport = {
            let rt = tokio::runtime::Runtime::new().unwrap();
            rt.block_on(build_transport(peer_key.clone()))
        };

        let (gossipsub, matchmaker_event_receiver) =
            Behaviour::new(peer_key, config);

        let connection_limits = build_p2p_connections_limit();

        let mut swarm = SwarmBuilder::new(transport, gossipsub, peer_id)
            .connection_limits(connection_limits)
            .notify_handler_buffer_size(
                std::num::NonZeroUsize::new(20).expect("Not zero"),
            )
            .connection_event_buffer_size(64)
            .build();

        swarm
            .listen_on(config.address.clone())
            .map_err(Error::Listening)?;

        Ok((Self { swarm }, matchmaker_event_receiver))
    }

    pub async fn handle_mm_message(&mut self, mm_message: MatchmakerMessage) {
        self.swarm
            .behaviour_mut()
            .intent_gossip_app
            .handle_mm_message(mm_message)
            .await
    }

    pub async fn handle_rpc_event(
        &mut self,
        event: rpc_message::Message,
    ) -> RpcResponse {
        match event {
            rpc_message::Message::Intent(message) => {
                match IntentMessage::try_from(message) {
                    Ok(message) => {
                        match self
                            .swarm
                            .behaviour_mut()
                            .intent_gossip_app
                            .apply_intent(message.intent.clone())
                        {
                            Ok(true) => {
                                let gossip_message = IntentGossipMessage::new(
                                    message.intent.clone(),
                                );
                                let intent_bytes = gossip_message.to_bytes();
                                match self
                                    .swarm
                                    .behaviour_mut()
                                    .intent_gossip_behaviour
                                    .publish(
                                        IdentTopic::new(message.topic),
                                        intent_bytes,
                                    ) {
                                    Ok(message_id) => {
                                        tracing::info!(
                                            "publish intent with message_id {}",
                                            message_id
                                        );
                                        RpcResponse {
                                            result: String::from(
                                                "Intent sent correctly",
                                            ),
                                        }
                                    }
                                    Err(err) => {
                                        tracing::error!(
                                            "error while publishing intent \
                                             {:?}",
                                            err
                                        );
                                        RpcResponse {
                                            result: format!(
                                                "Failed to publish_intent {:?}",
                                                err
                                            ),
                                        }
                                    }
                                }
                            }
                            Ok(false) => RpcResponse {
                                result: String::from(
                                    "Failed to apply the intent",
                                ),
                            },
                            Err(err) => {
                                tracing::error!(
                                    "error while applying the intent {:?}",
                                    err
                                );
                                RpcResponse {
                                    result: format!(
                                        "Failed to apply the intent {:?}",
                                        err
                                    ),
                                }
                            }
                        }
                    }
                    Err(_) => {
                        let result = String::from(
                            "rpc intent command for topic is empty",
                        );
                        tracing::error!("{}", result);
                        RpcResponse { result }
                    }
                }
            }
            rpc_message::Message::Dkg(dkg_msg) => {
                tracing::debug!(
                    "dkg not yet
        implemented {:?}",
                    dkg_msg
                );
                RpcResponse {
                    result: String::from(
                        "DKG
        application not yet implemented",
                    ),
                }
            }
            rpc_message::Message::Topic(topic_message) => {
                let topic = SubscribeTopicMessage::from(topic_message);
                let topic = IdentTopic::new(&topic.topic);
                match self
                    .swarm
                    .behaviour_mut()
                    .intent_gossip_behaviour
                    .subscribe(&topic)
                {
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
                }
            }
        }
    }
}

pub async fn build_transport(
    peer_key: Keypair,
) -> Boxed<(PeerId, StreamMuxerBox)> {
    let transport = {
        let tcp_transport = TcpConfig::new().nodelay(true);
        let dns_tcp_transport = DnsConfig::system(tcp_transport).await.unwrap();
        let ws_dns_tcp_transport = WsConfig::new(dns_tcp_transport.clone());
        dns_tcp_transport.or_transport(ws_dns_tcp_transport)
    };

    let auth_config = {
        let dh_keys = noise::Keypair::<noise::X25519Spec>::new()
            .into_authentic(&peer_key)
            .expect("Noise key generation failed. Should never happen.");

        noise::NoiseConfig::xx(dh_keys).into_authenticated()
    };

    let mplex_config = {
        let mut mplex_config = mplex::MplexConfig::new();
        mplex_config.set_max_buffer_behaviour(mplex::MaxBufferBehaviour::Block);
        mplex_config.set_max_buffer_size(usize::MAX);

        let mut yamux_config = libp2p::yamux::YamuxConfig::default();
        yamux_config
            .set_window_update_mode(libp2p::yamux::WindowUpdateMode::on_read());
        // TODO: check if its enought
        yamux_config.set_max_buffer_size(16 * 1024 * 1024);
        yamux_config.set_receive_window_size(16 * 1024 * 1024);

        core::upgrade::SelectUpgrade::new(yamux_config, mplex_config)
    };

    transport
        .upgrade(core::upgrade::Version::V1)
        .authenticate(auth_config)
        .multiplex(mplex_config)
        .timeout(Duration::from_secs(20))
        .boxed()
}

pub fn build_p2p_connections_limit() -> ConnectionLimits {
    ConnectionLimits::default()
        .with_max_pending_incoming(Some(10))
        .with_max_pending_outgoing(Some(30))
        .with_max_established_incoming(Some(25))
        .with_max_established_outgoing(Some(25))
        .with_max_established_per_peer(Some(5))
}
