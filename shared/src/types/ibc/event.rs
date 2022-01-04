//! IBC event without IBC-related data types

use std::collections::HashMap;

use borsh::{BorshDeserialize, BorshSerialize};
#[cfg(all(not(feature = "ABCI"), feature = "ibc-vp"))]
use ibc::events::{Error as IbcEventError, IbcEvent as RawIbcEvent};
#[cfg(all(feature = "ABCI", feature = "ibc-vp-abci"))]
use ibc_abci::events::{Error as IbcEventError, IbcEvent as RawIbcEvent};
#[cfg(all(not(feature = "ABCI"), feature = "ibc-vp"))]
use tendermint::abci::Event as AbciEvent;
#[cfg(all(feature = "ABCI", feature = "ibc-vp-abci"))]
use tendermint_stable::abci::Event as AbciEvent;
#[cfg(any(feature = "ibc-vp", feature = "ibc-vp-abci"))]
use thiserror::Error;

#[allow(missing_docs)]
#[cfg(any(feature = "ibc-vp", feature = "ibc-vp-abci"))]
#[derive(Error, Debug)]
pub enum Error {
    #[error("IBC event error: {0}")]
    IbcEvent(IbcEventError),
}

/// Conversion functions result
#[cfg(any(feature = "ibc-vp", feature = "ibc-vp-abci"))]
pub type Result<T> = std::result::Result<T, Error>;

/// Wrapped IbcEvent
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize, PartialEq, Eq)]
pub struct IbcEvent {
    /// The IBC event type
    pub event_type: String,
    /// The attributes of the IBC event
    pub attributes: HashMap<String, String>,
}

impl std::fmt::Display for IbcEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attributes = self
            .attributes
            .iter()
            .map(|(k, v)| format!("{}: {};", k, v))
            .collect::<Vec<String>>()
            .join(", ");
        write!(
            f,
            "Event type: {}, Attributes: {}",
            self.event_type, attributes
        )
    }
}

#[cfg(any(feature = "ibc-vp", feature = "ibc-vp-abci"))]
impl TryFrom<RawIbcEvent> for IbcEvent {
    type Error = Error;

    fn try_from(e: RawIbcEvent) -> Result<Self> {
        let event_type = e.event_type().as_str().to_string();
        let mut attributes = HashMap::new();
        let abci_event = AbciEvent::try_from(e).map_err(Error::IbcEvent)?;
        for tag in abci_event.attributes.iter() {
            attributes.insert(tag.key.to_string(), tag.value.to_string());
        }
        Ok(Self {
            event_type,
            attributes,
        })
    }
}
