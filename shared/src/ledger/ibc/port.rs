//! IBC validity predicate for port module

use borsh::BorshDeserialize;
use ibc::ics04_channel::context::ChannelReader;
use ibc::ics05_port::capabilities::Capability;
use ibc::ics05_port::context::PortReader;
use ibc::ics24_host::identifier::PortId;
use thiserror::Error;

use super::storage::{
    capability, capability_index_key, capability_key, is_capability_index_key,
    port_id, port_key, Error as IbcStorageError,
};
use super::{Ibc, StateChange};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::types::storage::Key;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("State change error: {0}")]
    InvalidStateChange(String),
    #[error("Port error: {0}")]
    InvalidPort(String),
    #[error("Capability error: {0}")]
    NoCapability(String),
    #[error("IBC storage error: {0}")]
    IbcStorage(IbcStorageError),
}

/// IBC port functions result
pub type Result<T> = std::result::Result<T, Error>;

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    pub(super) fn validate_port(&self, key: &Key) -> Result<()> {
        let port_id = port_id(key)?;
        match self.get_port_state_change(&port_id)? {
            StateChange::Created => {
                match self.authenticated_capability(&port_id) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(Error::InvalidPort(format!(
                        "The port is not authenticated: ID {}, {}",
                        port_id, e
                    ))),
                }
            }
            _ => Err(Error::InvalidPort(format!(
                "The state change of the port is invalid: Port {}",
                port_id
            ))),
        }
    }

    fn get_port_state_change(&self, port_id: &PortId) -> Result<StateChange> {
        let key = port_key(port_id);
        self.get_state_change(&key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))
    }

    pub(super) fn validate_capability(&self, key: &Key) -> Result<()> {
        if is_capability_index_key(key) {
            if self.capability_index_pre()? < self.capability_index()? {
                Ok(())
            } else {
                Err(Error::InvalidPort(
                    "The capability index is invalid".to_owned(),
                ))
            }
        } else {
            match self
                .get_state_change(key)
                .map_err(|e| Error::InvalidStateChange(e.to_string()))?
            {
                StateChange::Created => {
                    let cap = capability(key)?;
                    let port_id = self.get_port_by_capability(&cap)?;
                    match self.lookup_module_by_port(&port_id) {
                        Some(c) if c == cap => Ok(()),
                        Some(_) => Err(Error::InvalidPort(format!(
                            "The port is invalid: ID {}",
                            port_id
                        ))),
                        None => Err(Error::NoCapability(format!(
                            "The capability is not mapped: Index {}, Port {}",
                            cap.index(),
                            port_id
                        ))),
                    }
                }
                _ => Err(Error::InvalidStateChange(format!(
                    "The state change of the capability is invalid: key {}",
                    key
                ))),
            }
        }
    }

    fn capability_index_pre(&self) -> Result<u64> {
        let key = capability_index_key();
        self.read_counter_pre(&key)
            .map_err(|e| Error::NoCapability(e.to_string()))
    }

    fn capability_index(&self) -> Result<u64> {
        let key = capability_index_key();
        Ok(self.read_counter(&key))
    }

    fn get_port_by_capability(&self, cap: &Capability) -> Result<PortId> {
        let key = capability_key(cap.index());
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => {
                PortId::try_from_slice(&value[..]).map_err(|e| {
                    Error::InvalidPort(format!(
                        "Decoding the port ID failed: {}",
                        e
                    ))
                })
            }
            Ok(None) => Err(Error::InvalidPort(
                "The capability is not mapped to any port".to_owned(),
            )),
            Err(e) => Err(Error::InvalidPort(format!(
                "Reading the port failed {}",
                e
            ))),
        }
    }
}

impl<'a, DB, H> PortReader for Ibc<'a, DB, H>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    fn lookup_module_by_port(&self, port_id: &PortId) -> Option<Capability> {
        let key = port_key(port_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => {
                let index = match u64::try_from_slice(&value[..]) {
                    Ok(i) => i,
                    Err(_) => return None,
                };
                Some(Capability::from(index))
            }
            _ => None,
        }
    }

    fn authenticate(&self, cap: &Capability, port_id: &PortId) -> bool {
        match self.get_port_by_capability(cap) {
            Ok(p) => p == *port_id,
            Err(_) => false,
        }
    }
}

impl From<IbcStorageError> for Error {
    fn from(err: IbcStorageError) -> Self {
        Self::IbcStorage(err)
    }
}
