mod discovery;
use std::collections::hash_map::DefaultHasher;
use std::collections::VecDeque;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};
use std::task::{Context, Poll};
use std::time::Duration;

use anoma::proto::{self, Intent, IntentGossipMessage};
use libp2p::gossipsub::subscription_filter::regex::RegexSubscriptionFilter;
use libp2p::gossipsub::subscription_filter::{
    TopicSubscriptionFilter, WhitelistSubscriptionFilter,
};
use libp2p::gossipsub::{
    self, GossipsubEvent, GossipsubMessage, IdentTopic, IdentityTransform,
    MessageAcceptance, MessageAuthenticity, MessageId, TopicHash,
    ValidationMode,
};
use libp2p::identity::Keypair;
use libp2p::swarm::{
    NetworkBehaviourAction, NetworkBehaviourEventProcess, PollParameters,
};
use libp2p::{NetworkBehaviour, PeerId};
use thiserror::Error;
use tokio::sync::mpsc::Receiver;

use self::discovery::DiscoveryEvent;
use super::intent_gossiper;
use crate::node::gossip::behaviour::discovery::{
    DiscoveryBehaviour, DiscoveryConfigBuilder,
};
use crate::types::MatchmakerMessage;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to subscribe")]
    FailedSubscription(libp2p::gossipsub::error::SubscriptionError),
    #[error("Failed initializing the intent gossiper app: {0}")]
    GossipIntent(intent_gossiper::Error),
    #[error("Failed initializing the topic filter: {0}")]
    Filter(String),
    #[error("Failed initializing the gossip behaviour: {0}")]
    GossipConfig(String),
    #[error("Failed on the the discovery behaviour config: {0}")]
    DiscoveryConfig(String),
    #[error("Failed initializing the discovery behaviour: {0}")]
    Discovery(discovery::Error),
    #[error("Failed initializing mdns: {0}")]
    Mdns(std::io::Error),
}

// pub type Result<T> = std::result::Result<T, Error>;

pub type Gossipsub = libp2p::gossipsub::Gossipsub<
    IdentityTransform,
    IntentGossipSubscriptionFilter,
>;

// TODO merge type of config and this one ? Maybe not a good idea
pub enum IntentGossipSubscriptionFilter {
    RegexFilter(RegexSubscriptionFilter),
    WhitelistFilter(WhitelistSubscriptionFilter),
}

#[derive(Debug)]
pub struct IntentGossipEvent {
    pub propagation_source: PeerId,
    pub message_id: MessageId,
    pub source: Option<PeerId>,
    pub data: Vec<u8>,
    pub topic: TopicHash,
}

impl From<GossipsubEvent> for IntentGossipEvent {
    // To be used only with Message event
    fn from(event: GossipsubEvent) -> Self {
        if let GossipsubEvent::Message {
            propagation_source,
            message_id,
            message:
                GossipsubMessage {
                    source,
                    data,
                    topic,
                    sequence_number: _,
                },
        } = event
        {
            Self {
                propagation_source,
                message_id,
                source,
                data,
                topic,
            }
        } else {
            panic!("Expected a GossipsubEvent::Message got {:?}", event)
        }
    }
}
impl TopicSubscriptionFilter for IntentGossipSubscriptionFilter {
    fn can_subscribe(&mut self, topic_hash: &TopicHash) -> bool {
        match self {
            IntentGossipSubscriptionFilter::RegexFilter(filter) => {
                filter.can_subscribe(topic_hash)
            }
            IntentGossipSubscriptionFilter::WhitelistFilter(filter) => {
                filter.can_subscribe(topic_hash)
            }
        }
    }
}

#[derive(NetworkBehaviour)]
#[behaviour(out_event = "AnomaBehaviourEvent", poll_method = "poll")]
pub struct Behaviour {
    pub intent_gossip_behaviour: libp2p::gossipsub::Gossipsub<
        IdentityTransform,
        IntentGossipSubscriptionFilter,
    >,
    pub discovery: DiscoveryBehaviour,
    // TODO add another gossipsub (or floodsub ?) for dkg message propagation ?
    #[behaviour(ignore)]
    pub intent_gossip_app: intent_gossiper::GossipIntent,
    #[behaviour(ignore)]
    events: VecDeque<AnomaBehaviourEvent>,
}

#[derive(Debug)]
pub enum AnomaBehaviourEvent {
    PeerConnected(PeerId),
    PeerDisconnected(PeerId),
    Message(GossipsubMessage, PeerId, MessageId),
    Subscribed(PeerId, TopicHash),
    Unsubscribed(PeerId, TopicHash),
}

pub fn message_id(message: &GossipsubMessage) -> MessageId {
    let mut hasher = DefaultHasher::new();
    message.data.hash(&mut hasher);
    MessageId::from(hasher.finish().to_string())
}

impl Behaviour {
    fn poll<TBehaviourIn>(
        &mut self,
        _: &mut Context,
        _: &mut impl PollParameters,
    ) -> Poll<NetworkBehaviourAction<TBehaviourIn, AnomaBehaviourEvent>> {
        if !self.events.is_empty() {
            return Poll::Ready(NetworkBehaviourAction::GenerateEvent(
                self.events.pop_front().unwrap(),
            ));
        }
        Poll::Pending
    }

    pub fn new(
        key: Keypair,
        config: &crate::config::IntentGossiper,
    ) -> (Self, Option<Receiver<MatchmakerMessage>>) {
        let peer_id = PeerId::from_public_key(key.public());

        // Set a custom gossipsub
        let gossipsub_config = gossipsub::GossipsubConfigBuilder::default()
            .protocol_id_prefix("intent_gossip")
            .heartbeat_interval(Duration::from_secs(1))
            .validation_mode(ValidationMode::Strict)
            .message_id_fn(message_id)
            .validate_messages()
            .mesh_outbound_min(1)
            .mesh_n_low(2)
            .mesh_n(3)
            .mesh_n_high(6)
            .build()
            .unwrap();

        let filter = match &config.subscription_filter {
            crate::config::SubscriptionFilter::RegexFilter(regex) => {
                IntentGossipSubscriptionFilter::RegexFilter(
                    RegexSubscriptionFilter(regex.clone()),
                )
            }
            crate::config::SubscriptionFilter::WhitelistFilter(topics) => {
                IntentGossipSubscriptionFilter::WhitelistFilter(
                    WhitelistSubscriptionFilter(
                        topics
                            .iter()
                            .map(|topic| {
                                TopicHash::from(IdentTopic::new(topic))
                            })
                            .collect(),
                    ),
                )
            }
        };

        let mut intent_gossip_behaviour: Gossipsub =
            Gossipsub::new_with_subscription_filter(
                MessageAuthenticity::Signed(key),
                gossipsub_config,
                filter,
            )
            .unwrap();

        let (intent_gossip_app, matchmaker_event_receiver) =
            intent_gossiper::GossipIntent::new(config).unwrap();

        config
            .topics
            .iter()
            .try_for_each(|topic| {
                intent_gossip_behaviour
                    .subscribe(&IdentTopic::new(topic))
                    .map_err(Error::FailedSubscription)
                    // it returns bool signifying if it was already subscribed.
                    // discard because it can't be false as the config.topics is
                    // a hash set
                    .map(|_| ())
            })
            .expect("failed to subscribe to topic");

        // TODO: check silent fail if not bootstrap_peers is not multiaddr
        let discovery_opt = if let Some(dis_config) = &config.discover_peer {
            let discovery_config = DiscoveryConfigBuilder::default()
                .with_user_defined(dis_config.bootstrap_peers.clone())
                .discovery_limit(dis_config.max_discovery_peers)
                .with_kademlia(dis_config.kademlia)
                .with_mdns(dis_config.mdns)
                .use_kademlia_disjoint_query_paths(true)
                .build()
                .unwrap();

            DiscoveryBehaviour::new(peer_id, discovery_config)
        } else {
            let discovery_config =
                DiscoveryConfigBuilder::default().build().unwrap();
            DiscoveryBehaviour::new(peer_id, discovery_config)
        };

        (
            Self {
                intent_gossip_behaviour,
                discovery: discovery_opt.unwrap(),
                intent_gossip_app,
                events: VecDeque::new(),
            },
            matchmaker_event_receiver,
        )
    }

    fn handle_intent(&mut self, intent: Intent) -> MessageAcceptance {
        match self.intent_gossip_app.apply_intent(intent) {
            Ok(true) => MessageAcceptance::Accept,
            Ok(false) => MessageAcceptance::Reject,
            Err(e) => {
                tracing::error!("Error while trying to apply an intent: {}", e);
                match e {
                    intent_gossiper::Error::Decode(_) => {
                        panic!("can't happens, because intent already decoded")
                    }
                    intent_gossiper::Error::MatchmakerInit(err)
                    | intent_gossiper::Error::Matchmaker(err) => {
                        tracing::info!(
                            "error while running the matchmaker: {:?}",
                            err
                        );
                        MessageAcceptance::Ignore
                    }
                }
            }
        }
    }

    fn handle_raw_intent(
        &mut self,
        data: impl AsRef<[u8]>,
    ) -> MessageAcceptance {
        match IntentGossipMessage::try_from(data.as_ref()) {
            Ok(message) => self.handle_intent(message.intent),
            Err(proto::Error::NoIntentError) => {
                tracing::info!("Empty message, rejecting it");
                MessageAcceptance::Reject
            }
            Err(proto::Error::IntentDecodingError(err)) => {
                tracing::info!("error while decoding the intent: {:?}", err);
                MessageAcceptance::Reject
            }
            _ => unreachable!(),
        }
    }
}

impl NetworkBehaviourEventProcess<GossipsubEvent> for Behaviour {
    fn inject_event(&mut self, event: GossipsubEvent) {
        tracing::info!("received : {:?}", event);
        match event {
            GossipsubEvent::Message {
                message,
                propagation_source,
                message_id,
            } => {
                let validity = self.handle_raw_intent(message.data);
                self.intent_gossip_behaviour
                    .report_message_validation_result(
                        &message_id,
                        &propagation_source,
                        validity,
                    )
                    .expect("Failed to validate the message ");
            }
            GossipsubEvent::Subscribed { peer_id: _, topic } => {
                self.intent_gossip_behaviour
                    .subscribe(&IdentTopic::new(topic.into_string()))
                    .map_err(Error::FailedSubscription)
                    .unwrap_or_else(|e| {
                        tracing::error!("failed to subscribe: {:?}", e);
                        false
                    });
            }
            GossipsubEvent::Unsubscribed {
                peer_id: _,
                topic: _,
            } => {}
        }
    }
}

impl NetworkBehaviourEventProcess<DiscoveryEvent> for Behaviour {
    fn inject_event(&mut self, event: DiscoveryEvent) {
        match event {
            DiscoveryEvent::Connected(peer_id) => self
                .events
                .push_back(AnomaBehaviourEvent::PeerConnected(peer_id)),
            DiscoveryEvent::Disconnected(peer_id) => self
                .events
                .push_back(AnomaBehaviourEvent::PeerDisconnected(peer_id)),
        }
    }
}
