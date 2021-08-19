use std::convert::{TryFrom, TryInto};

use anoma::proto::{Dkg, Error, Intent};

use super::generated::services;

pub type Result<T> = std::result::Result<T, Error>;

pub enum RpcMessage {
    IntentMessage(IntentMessage),
    SubscribeTopicMessage(SubscribeTopicMessage),
    Dkg(Dkg),
}

impl From<RpcMessage> for services::RpcMessage {
    fn from(message: RpcMessage) -> Self {
        let message = match message {
            RpcMessage::IntentMessage(m) => {
                services::rpc_message::Message::Intent(m.into())
            }
            RpcMessage::SubscribeTopicMessage(m) => {
                services::rpc_message::Message::Topic(m.into())
            }
            RpcMessage::Dkg(d) => services::rpc_message::Message::Dkg(d.into()),
        };
        services::RpcMessage {
            message: Some(message),
        }
    }
}

impl RpcMessage {
    pub fn new_intent(intent: Intent, topic: String) -> Self {
        RpcMessage::IntentMessage(IntentMessage::new(intent, topic))
    }

    pub fn new_topic(topic: String) -> Self {
        RpcMessage::SubscribeTopicMessage(SubscribeTopicMessage::new(topic))
    }

    pub fn new_dkg(dkg: Dkg) -> Self {
        RpcMessage::Dkg(dkg)
    }
}

#[derive(Debug, PartialEq)]
pub struct IntentMessage {
    pub intent: Intent,
    pub topic: String,
}

impl TryFrom<services::IntentMessage> for IntentMessage {
    type Error = Error;

    fn try_from(message: services::IntentMessage) -> Result<Self> {
        match message.intent {
            Some(intent) => Ok(IntentMessage {
                intent: intent.try_into()?,
                topic: message.topic,
            }),
            None => Err(Error::NoIntentError),
        }
    }
}

impl From<IntentMessage> for services::IntentMessage {
    fn from(message: IntentMessage) -> Self {
        services::IntentMessage {
            intent: Some(message.intent.into()),
            topic: message.topic,
        }
    }
}

impl IntentMessage {
    pub fn new(intent: Intent, topic: String) -> Self {
        IntentMessage { intent, topic }
    }
}

#[derive(Debug, PartialEq)]
pub struct SubscribeTopicMessage {
    pub topic: String,
}

impl From<services::SubscribeTopicMessage> for SubscribeTopicMessage {
    fn from(message: services::SubscribeTopicMessage) -> Self {
        SubscribeTopicMessage {
            topic: message.topic,
        }
    }
}

impl From<SubscribeTopicMessage> for services::SubscribeTopicMessage {
    fn from(message: SubscribeTopicMessage) -> Self {
        services::SubscribeTopicMessage {
            topic: message.topic,
        }
    }
}

impl SubscribeTopicMessage {
    pub fn new(topic: String) -> Self {
        SubscribeTopicMessage { topic }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intent_message() {
        let data = "arbitrary data".as_bytes().to_owned();
        let intent = Intent::new(data);
        let topic = "arbitrary string".to_owned();
        let intent_message = IntentMessage::new(intent.clone(), topic.clone());

        let intent_rpc_message = RpcMessage::new_intent(intent, topic);
        let services_rpc_message: services::RpcMessage =
            intent_rpc_message.into();
        match services_rpc_message.message {
            Some(services::rpc_message::Message::Intent(i)) => {
                let message_from_types =
                    IntentMessage::try_from(i).expect("no intent");
                assert_eq!(intent_message, message_from_types);
            }
            _ => panic!("no intent message"),
        }
    }

    #[test]
    fn test_topic_message() {
        let topic = "arbitrary string".to_owned();
        let topic_message = SubscribeTopicMessage::new(topic.clone());

        let topic_rpc_message = RpcMessage::new_topic(topic);
        let services_rpc_message: services::RpcMessage =
            topic_rpc_message.into();
        match services_rpc_message.message {
            Some(services::rpc_message::Message::Topic(t)) => {
                let message_from_types = SubscribeTopicMessage::from(t);
                assert_eq!(topic_message, message_from_types);
            }
            _ => panic!("no intent message"),
        }
    }
}
