use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::{self, Display};
use std::ops::{Index, IndexMut};

use anoma::ledger::governance::utils::ProposalEvent;
use anoma::types::ibc::IbcEvent;
use anoma::types::transaction::{hash_tx, TxType};
use borsh::BorshSerialize;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::abci::EventAttribute;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::EventAttribute;
use thiserror::Error;

/// Indicates if an event is emitted do to
/// an individual Tx or the nature of a finalized block
#[derive(Clone, Debug)]
pub enum EventLevel {
    Block,
    Tx,
}

/// Custom events that can be queried from Tendermint
/// using a websocket client
#[derive(Clone, Debug)]
pub struct Event {
    pub event_type: EventType,
    pub level: EventLevel,
    pub attributes: HashMap<String, String>,
}

/// The two types of custom events we currently use
#[derive(Clone, Debug)]
pub enum EventType {
    // The transaction was accepted to be included in a block
    Accepted,
    // The transaction was applied during block finalization
    Applied,
    // The IBC transaction was applied during block finalization
    Ibc(String),
    // The proposal that has been executed
    Proposal,
}

#[cfg(not(feature = "ABCI"))]
impl Display for EventType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EventType::Accepted => write!(f, "accepted"),
            EventType::Applied => write!(f, "applied"),
            EventType::Ibc(t) => write!(f, "{}", t),
            EventType::Proposal => write!(f, "proposal"),
        }?;
        Ok(())
    }
}

#[cfg(feature = "ABCI")]
impl Display for EventType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EventType::Accepted => write!(f, "applied"),
            EventType::Applied => write!(f, "applied"),
            EventType::Ibc(t) => write!(f, "{}", t),
            EventType::Proposal => write!(f, "proposal"),
        }?;
        Ok(())
    }
}

impl Event {
    /// Creates a new event with the hash and height of the transaction
    /// already filled in
    pub fn new_tx_event(tx: &TxType, height: u64) -> Self {
        let mut event = match tx {
            TxType::Wrapper(wrapper) => {
                let mut event = Event {
                    event_type: EventType::Accepted,
                    level: EventLevel::Tx,
                    attributes: HashMap::new(),
                };
                event["hash"] = if !cfg!(feature = "ABCI") {
                    hash_tx(
                        &wrapper
                            .try_to_vec()
                            .expect("Serializing wrapper should not fail"),
                    )
                    .to_string()
                } else {
                    wrapper.tx_hash.to_string()
                };
                event
            }
            TxType::Decrypted(decrypted) => {
                let mut event = Event {
                    event_type: EventType::Applied,
                    level: EventLevel::Tx,
                    attributes: HashMap::new(),
                };
                event["hash"] = decrypted.hash_commitment().to_string();
                event
            }
            tx @ TxType::Protocol(_) => {
                let mut event = Event {
                    event_type: EventType::Applied,
                    level: EventLevel::Tx,
                    attributes: HashMap::new(),
                };
                event["hash"] = hash_tx(
                    &tx.try_to_vec()
                        .expect("Serializing protocol tx should not fail"),
                )
                .to_string();
                event
            }
            _ => unreachable!(),
        };
        event["height"] = height.to_string();
        event["log"] = "".to_string();
        event
    }

    /// Check if the events keys contains a given string
    pub fn contains_key(&self, key: &str) -> bool {
        self.attributes.contains_key(key)
    }

    /// Get the value corresponding to a given key, if it exists.
    /// Else return None.
    pub fn get(&self, key: &str) -> Option<&String> {
        self.attributes.get(key)
    }
}

impl Index<&str> for Event {
    type Output = String;

    fn index(&self, index: &str) -> &Self::Output {
        &self.attributes[index]
    }
}

impl IndexMut<&str> for Event {
    fn index_mut(&mut self, index: &str) -> &mut Self::Output {
        if !self.attributes.contains_key(index) {
            self.attributes.insert(String::from(index), String::new());
        }
        self.attributes.get_mut(index).unwrap()
    }
}

impl From<IbcEvent> for Event {
    fn from(ibc_event: IbcEvent) -> Self {
        Self {
            event_type: EventType::Ibc(ibc_event.event_type),
            level: EventLevel::Tx,
            attributes: ibc_event.attributes,
        }
    }
}

impl From<ProposalEvent> for Event {
    fn from(proposal_event: ProposalEvent) -> Self {
        Self {
            event_type: EventType::Proposal,
            level: EventLevel::Block,
            attributes: proposal_event.attributes,
        }
    }
}

#[cfg(not(feature = "ABCI"))]
/// Convert our custom event into the necessary tendermint proto type
impl From<Event> for tendermint_proto::abci::Event {
    fn from(event: Event) -> Self {
        Self {
            r#type: event.event_type.to_string(),
            attributes: event
                .attributes
                .into_iter()
                .map(|(key, value)| EventAttribute {
                    key,
                    value,
                    index: true,
                })
                .collect(),
        }
    }
}

#[cfg(feature = "ABCI")]
/// Convert our custom event into the necessary tendermint proto type
impl From<Event> for tendermint_proto_abci::abci::Event {
    fn from(event: Event) -> Self {
        Self {
            r#type: event.event_type.to_string(),
            attributes: event
                .attributes
                .into_iter()
                .map(|(key, value)| EventAttribute {
                    key: key.into_bytes(),
                    value: value.into_bytes(),
                    index: true,
                })
                .collect(),
        }
    }
}

/// A thin wrapper around a HashMap for parsing event JSONs
/// returned in tendermint subscription responses.
#[derive(Debug)]
pub struct Attributes(HashMap<String, String>);

impl Attributes {
    /// Get a reference to the value associated with input key
    pub fn get(&self, key: &str) -> Option<&String> {
        self.0.get(key)
    }

    /// Get ownership of the value associated to the input key
    pub fn take(&mut self, key: &str) -> Option<String> {
        self.0.remove(key)
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Json missing `attributes` field")]
    MissingAttributes,
    #[error("Attributes missing key: {0}")]
    MissingKey(String),
    #[error("Attributes missing value: {0}")]
    MissingValue(String),
}

impl TryFrom<&serde_json::Value> for Attributes {
    type Error = Error;

    fn try_from(json: &serde_json::Value) -> Result<Self, Self::Error> {
        let mut attributes = HashMap::new();
        let attrs: Vec<serde_json::Value> = serde_json::from_value(
            json.get("attributes")
                .ok_or(Error::MissingAttributes)?
                .clone(),
        )
        .unwrap();

        for attr in attrs {
            attributes.insert(
                serde_json::from_value(
                    attr.get("key")
                        .ok_or_else(|| {
                            Error::MissingKey(
                                serde_json::to_string(&attr).unwrap(),
                            )
                        })?
                        .clone(),
                )
                .unwrap(),
                serde_json::from_value(
                    attr.get("value")
                        .ok_or_else(|| {
                            Error::MissingValue(
                                serde_json::to_string(&attr).unwrap(),
                            )
                        })?
                        .clone(),
                )
                .unwrap(),
            );
        }
        Ok(Attributes(attributes))
    }
}
