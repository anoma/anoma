use std::collections::HashMap;
use std::fmt::{self, Display};
use std::ops::{Index, IndexMut};

use sha2::{Digest, Sha256};
use tendermint_proto::abci::EventAttribute;

/// Custom events that can be queried from Tendermint
/// using a websocket client
#[derive(Clone)]
pub struct Event {
    event_type: EventType,
    attributes: HashMap<String, String>,
}

/// The two types of custom events we currently use
#[derive(Clone)]
pub enum EventType {
    // The transaction was accepted to be included in a block
    Accepted,
    // The transaction was applied during block finalization
    Applied,
}

impl Display for EventType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EventType::Accepted => write!(f, "accepted"),
            EventType::Applied => write!(f, "applied"),
        }?;
        Ok(())
    }
}

impl Event {
    /// Creates a new event with the hash and height of the transaction
    /// already filled in
    pub fn new_tx_event(ty: EventType, tx: &[u8], height: i64) -> Self {
        let mut event = Event {
            event_type: ty,
            attributes: HashMap::new(),
        };
        event["hash"] = hash_tx(tx);
        event["height"] = height.to_string();
        event
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

/// Convert our custom event into the necessary tendermint proto type
impl From<Event> for tendermint_proto::abci::Event {
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

struct Hash([u8; 32]);

impl Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in &self.0 {
            write!(f, "{:02X}", byte)?;
        }
        Ok(())
    }
}

/// Get the hash of a transaction and convert to a string
fn hash_tx(tx_bytes: &[u8]) -> String {
    let digest = Sha256::digest(tx_bytes);
    let mut hash_bytes = [0u8; 32];
    hash_bytes.copy_from_slice(&digest);
    Hash(hash_bytes).to_string()
}
