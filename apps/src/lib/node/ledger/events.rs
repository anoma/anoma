use std::collections::HashMap;
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

/// Custom events that can be queried from Tendermint
/// using a websocket client
#[derive(Clone)]
pub struct Event {
    pub event_type: EventType,
    pub attributes: HashMap<String, String>,
}

/// The two types of custom events we currently use
#[derive(Clone)]
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
                    attributes: HashMap::new(),
                };
                event["hash"] = decrypted.hash_commitment().to_string();
                event
            }
            tx @ TxType::Protocol(_) => {
                let mut event = Event {
                    event_type: EventType::Applied,
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

    pub fn contains_key(&self, key: &str) -> bool {
        self.attributes.contains_key(key)
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
            attributes: ibc_event.attributes,
        }
    }
}

impl From<ProposalEvent> for Event {
    fn from(proposal_event: ProposalEvent) -> Self {
        Self {
            event_type: EventType::Proposal,
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

impl From<&serde_json::Value> for Attributes {
    fn from(json: &serde_json::Value) -> Self {
        let mut attributes = HashMap::new();
        let attrs: Vec<serde_json::Value> = serde_json::from_value(
            json.get("attributes")
                .expect("Tendermint event missing attributes")
                .clone(),
        )
        .unwrap();
        for attr in attrs {
            attributes.insert(
                serde_json::from_value(
                    attr.get("key")
                        .expect("Attributes JSON missing key")
                        .clone(),
                )
                .unwrap(),
                serde_json::from_value(
                    attr.get("value")
                        .expect("Attributes JSON missing value")
                        .clone(),
                )
                .unwrap(),
            );
        }
        Attributes(attributes)
    }
}
