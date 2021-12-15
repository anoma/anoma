use std::time::Duration;

use libp2p::core::connection::ConnectionLimits;
use libp2p::core::muxing::StreamMuxerBox;
use libp2p::core::transport::Boxed;
use libp2p::dns::DnsConfig;
use libp2p::identity::Keypair;
use libp2p::swarm::SwarmBuilder;
use libp2p::tcp::TcpConfig;
use libp2p::websocket::WsConfig;
use libp2p::{core, mplex, noise, PeerId, Transport, TransportError};
use thiserror::Error;
use tokio::sync::mpsc::Sender;

use super::behaviour::Behaviour;
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

pub struct P2P(pub Swarm);

impl P2P {
    /// Create a new peer based on the configuration given. Used transport is
    /// tcp. A peer participate in the intent gossip system and helps the
    /// propagation of intents.
    pub fn new(
        config: &crate::config::IntentGossiper,
        mm_sender: Option<Sender<MatchmakerMessage>>,
    ) -> Result<Self> {
        let peer_key = Keypair::Ed25519(config.gossiper.key.clone());

        // Id of the node on the libp2p network derived from the public key
        let peer_id = PeerId::from(peer_key.public());

        tracing::info!("Peer id: {:?}", peer_id.clone());

        // TODO remove async runtime here and hide it in `build_transport`
        let transport = {
            let rt = tokio::runtime::Runtime::new().unwrap();
            rt.block_on(build_transport(peer_key.clone()))
        };

        // create intent gossip specific behaviour
        let intent_gossip_behaviour =
            Behaviour::new(peer_key, config, mm_sender);

        let connection_limits = build_p2p_connections_limit();

        // Swarm is
        let mut swarm =
            SwarmBuilder::new(transport, intent_gossip_behaviour, peer_id)
                .connection_limits(connection_limits)
                .notify_handler_buffer_size(
                    std::num::NonZeroUsize::new(20).expect("Not zero"),
                )
                .connection_event_buffer_size(64)
                .build();

        swarm
            .listen_on(config.address.clone())
            .map_err(Error::Listening)?;

        Ok(Self(swarm))
    }
}

// TODO explain a bit the choice made here
/// Create transport used by libp2p. see
/// https://docs.libp2p.io/concepts/transport/ for more information on libp2p
/// transport
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

// TODO document choice made here
// TODO inject it in the configuration instead of hard-coding it ?
pub fn build_p2p_connections_limit() -> ConnectionLimits {
    ConnectionLimits::default()
        .with_max_pending_incoming(Some(10))
        .with_max_pending_outgoing(Some(30))
        .with_max_established_incoming(Some(25))
        .with_max_established_outgoing(Some(25))
        .with_max_established_per_peer(Some(5))
}
