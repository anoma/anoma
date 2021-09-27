mod discovery;
use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};
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
use libp2p::swarm::NetworkBehaviourEventProcess;
use libp2p::{NetworkBehaviour, PeerId};
use thiserror::Error;
use tokio::sync::mpsc::Sender;
use tokio::sync::oneshot::channel;

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

pub type Gossipsub = libp2p::gossipsub::Gossipsub<
    IdentityTransform,
    IntentGossipSubscriptionFilter,
>;

// TODO merge type of config and this one ? Maybe not a good idea
// TODO extends with MaxSubscribionFilter
/// IntentGossipSubscriptionfilter is a wrapper of TopicSubscriptionFilter to
/// allows combination of any sort of filter.
pub enum IntentGossipSubscriptionFilter {
    RegexFilter(RegexSubscriptionFilter),
    WhitelistFilter(WhitelistSubscriptionFilter),
}

/// IntentGossipEvent describe events received/sent in the gossipsub network.
/// All information are extracted from the GossipsubEvent type. This type is
/// used as a wrapper of GossipsubEvent in order to have only information of
/// interest and possibly enforce some invariant.
#[derive(Debug)]
pub struct IntentGossipEvent {
    /// The PeerId that initially created this message
    pub propagation_source: PeerId,
    /// The MessageId of this message. This MessageId allows to discriminate
    /// already received message
    pub message_id: MessageId,
    // TODO maybe remove the Option of this field to make mandatory to have an
    // id.
    /// The peer that transmitted this message to us. It can be anonymous
    pub source: Option<PeerId>,
    /// The content of the data
    pub data: Vec<u8>,
    /// The topic from which we received the message
    pub topic: TopicHash,
}

impl From<GossipsubEvent> for IntentGossipEvent {
    /// Transforme a GossipsubEvent into an IntentGossipEvent. This function
    /// fails if the gossipsubEvent does not contain a GossipsubMessage.
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
    /// tcheck that the proposed topic can be subscribed
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

/// Behaviour is composed of a `DiscoveryBehaviour` and an GossipsubBehaviour`.
/// It automatically connect to newly discovered peer, except specified
/// otherwise, and propagates intents to other peers.
#[derive(NetworkBehaviour)]
pub struct Behaviour {
    pub intent_gossip_behaviour: Gossipsub,
    pub discover_behaviour: DiscoveryBehaviour,
    #[behaviour(ignore)]
    pub mm_sender: Option<Sender<MatchmakerMessage>>,
}

/// [message_id] use the hash of the message data as an id
pub fn message_id(message: &GossipsubMessage) -> MessageId {
    let mut hasher = DefaultHasher::new();
    message.data.hash(&mut hasher);
    MessageId::from(hasher.finish().to_string())
}

impl Behaviour {
    /// Create a new behaviour based on the config given
    pub fn new(
        key: Keypair,
        config: &crate::config::IntentGossiper,
        mm_sender: Option<Sender<MatchmakerMessage>>,
    ) -> Self {
        let peer_id = PeerId::from_public_key(key.public());

        // TODO remove hardcoded value and add them to the config Except
        // validation_mode, protocol_id_prefix, message_id_fn and
        // validate_messages
        // Set a custom gossipsub for our use case
        let gossipsub_config = gossipsub::GossipsubConfigBuilder::default()
            .protocol_id_prefix("intent_gossip")
            .heartbeat_interval(Duration::from_secs(1))
            .validation_mode(ValidationMode::Strict)
            .message_id_fn(message_id)
            .max_transmit_size(16 * 1024 * 1024)
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
                            .map(IdentTopic::new)
                            .map(TopicHash::from)
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

        // subscribe to all topic listed in the config.
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

        let discover_behaviour = {
            // TODO: check that bootstrap_peers are in multiaddr (otherwise it
            // fails silently)
            let discover_config = if let Some(discover_config) =
                &config.discover_peer
            {
                DiscoveryConfigBuilder::default()
                    .with_user_defined(discover_config.bootstrap_peers.clone())
                    .discovery_limit(discover_config.max_discovery_peers)
                    .with_kademlia(discover_config.kademlia)
                    .with_mdns(discover_config.mdns)
                    .use_kademlia_disjoint_query_paths(true)
                    .build()
                    .unwrap()
            } else {
                DiscoveryConfigBuilder::default().build().unwrap()
            };
            DiscoveryBehaviour::new(peer_id, discover_config).unwrap()
        };
        Self {
            intent_gossip_behaviour,
            discover_behaviour,
            mm_sender,
        }
    }

    /// tries to apply a new intent. Fails if the logic fails or if the intent
    /// is rejected. If the matchmaker fails the message is only ignore
    fn handle_intent(&mut self, intent: Intent) -> MessageAcceptance {
        if let Some(sender) = &self.mm_sender {
            let (response_sender, mut response_receiver) = channel::<bool>();
            sender
                .try_send(MatchmakerMessage::ApplyIntent(
                    intent,
                    response_sender,
                ))
                .unwrap_or_else(|err| {
                    tracing::error!(
                        "Error sending intent to the matchmaker: {}",
                        err
                    );
                });
            return match response_receiver.try_recv() {
                Ok(true) => MessageAcceptance::Accept,
                Ok(false) => MessageAcceptance::Reject,
                Err(err) => {
                    tracing::error!(
                        "Ignoring intent because error while trying to apply \
                         an intent in matchmaker: {}",
                        err
                    );
                    MessageAcceptance::Ignore
                }
            };
        }
        // When no is matchmaker running, accept any intent
        MessageAcceptance::Accept
    }

    /// Tries to decoded the arbitrary data in an intent then call
    /// [handle_intent]. fails if the data does not contains an intent
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
    /// When a new event is generated by the intent gossip behaviour
    fn inject_event(&mut self, event: GossipsubEvent) {
        tracing::info!("received a new message : {:?}", event);
        match event {
            GossipsubEvent::Message {
                message,
                propagation_source,
                message_id,
            } => {
                // validity is the type of response return to the network
                // (valid|reject|ignore)
                let validity = self.handle_raw_intent(message.data);
                self.intent_gossip_behaviour
                    .report_message_validation_result(
                        &message_id,
                        &propagation_source,
                        validity,
                    )
                    .expect("Failed to validate the message");
            }
            // When a peer subscribe to a new topic, this node also tries to
            // connect to it using the filter defined in the config
            GossipsubEvent::Subscribed { peer_id: _, topic } => {
                // try to subscribe to the new topic
                self.intent_gossip_behaviour
                    .subscribe(&IdentTopic::new(topic.into_string()))
                    .map_err(Error::FailedSubscription)
                    .unwrap_or_else(|e| {
                        tracing::error!("failed to subscribe: {:?}", e);
                        false
                    });
            }
            // Nothing to do when you are informed that a peer unsubscribed to a
            // topic.
            // TODO: It could be interesting to unsubscribe to a topic when the
            // node is not connected to anyone else.
            GossipsubEvent::Unsubscribed {
                peer_id: _,
                topic: _,
            } => {}
        }
    }
}

impl NetworkBehaviourEventProcess<DiscoveryEvent> for Behaviour {
    // The logic is part of the DiscoveryBehaviour, nothing to do here.
    fn inject_event(&mut self, event: DiscoveryEvent) {
        match event {
            DiscoveryEvent::Connected(peer) => {
                tracing::info!("Connect to a new peer: {:?}", peer)
            }
            DiscoveryEvent::Disconnected(peer) => {
                tracing::info!("Peer disconnected: {:?}", peer)
            }
            _ => {}
        }
    }
}
