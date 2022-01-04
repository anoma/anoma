//! IBC validity predicate for client module

use borsh::BorshDeserialize;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_consensus::AnyConsensusState;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_def::{AnyClient, ClientDef};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_state::AnyClientState;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_type::ClientType;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::context::ClientReader;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::error::Error as Ics02Error;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::height::Height;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::ClientMsg;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics24_host::identifier::ClientId;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics26_routing::msgs::Ics26Envelope;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_consensus::AnyConsensusState;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_def::{AnyClient, ClientDef};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_state::AnyClientState;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_type::ClientType;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::context::ClientReader;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::error::Error as Ics02Error;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::height::Height;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::ClientMsg;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics24_host::identifier::ClientId;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics26_routing::msgs::Ics26Envelope;
use thiserror::Error;

use super::super::handler::{
    make_create_client_event, make_update_client_event,
    make_upgrade_client_event,
};
use super::super::storage::{
    client_counter_key, client_state_key, client_type_key, consensus_state_key,
};
use super::{Ibc, StateChange};
use crate::ledger::storage::{self, StorageHasher};
use crate::types::ibc::data::{Error as IbcDataError, IbcMessage};
use crate::vm::WasmCacheAccess;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("State change error: {0}")]
    InvalidStateChange(String),
    #[error("Client error: {0}")]
    InvalidClient(String),
    #[error("Header error: {0}")]
    InvalidHeader(String),
    #[error("Proof verification error: {0}")]
    ProofVerificationFailure(String),
    #[error("Decoding TX data error: {0}")]
    DecodingTxData(std::io::Error),
    #[error("Decoding client data error: {0}")]
    DecodingClientData(std::io::Error),
    #[error("IBC data error: {0}")]
    InvalidIbcData(IbcDataError),
    #[error("IBC event error: {0}")]
    IbcEvent(String),
}

/// IBC client functions result
pub type Result<T> = std::result::Result<T, Error>;
/// ClientReader result
type Ics02Result<T> = core::result::Result<T, Ics02Error>;

impl<'a, DB, H, CA> Ibc<'a, DB, H, CA>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    pub(super) fn validate_client(
        &self,
        client_id: &ClientId,
        tx_data: &[u8],
    ) -> Result<()> {
        match self.get_client_state_change(client_id)? {
            StateChange::Created => {
                self.validate_created_client(client_id, tx_data)
            }
            StateChange::Updated => {
                self.validate_updated_client(client_id, tx_data)
            }
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the client is invalid: ID {}",
                client_id
            ))),
        }
    }

    fn get_client_state_change(
        &self,
        client_id: &ClientId,
    ) -> Result<StateChange> {
        let key = client_state_key(client_id);
        self.get_state_change(&key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))
    }

    fn validate_created_client(
        &self,
        client_id: &ClientId,
        tx_data: &[u8],
    ) -> Result<()> {
        let ibc_msg = IbcMessage::decode(tx_data)?;
        let msg = ibc_msg.msg_create_any_client()?;
        let client_type = self.client_type(client_id).map_err(|_| {
            Error::InvalidClient(format!(
                "The client type doesn't exist: ID {}",
                client_id
            ))
        })?;
        let client_state = ClientReader::client_state(self, client_id)
            .map_err(|_| {
                Error::InvalidClient(format!(
                    "The client state doesn't exist: ID {}",
                    client_id
                ))
            })?;
        let height = client_state.latest_height();
        let consensus_state =
            self.consensus_state(client_id, height).map_err(|_| {
                Error::InvalidClient(format!(
                    "The consensus state doesn't exist: ID {}, Height {}",
                    client_id, height
                ))
            })?;
        if client_type != client_state.client_type()
            || client_type != consensus_state.client_type()
        {
            return Err(Error::InvalidClient(
                "The client type is mismatched".to_owned(),
            ));
        }

        let event = make_create_client_event(client_id, &msg);
        self.check_emitted_event(event)
            .map_err(|e| Error::IbcEvent(e.to_string()))
    }

    fn validate_updated_client(
        &self,
        client_id: &ClientId,
        tx_data: &[u8],
    ) -> Result<()> {
        // check the type of data in tx_data
        let ibc_msg = IbcMessage::decode(tx_data)?;
        match ibc_msg.0 {
            Ics26Envelope::Ics2Msg(ClientMsg::UpdateClient(msg)) => {
                self.verify_update_client(client_id, msg)
            }
            Ics26Envelope::Ics2Msg(ClientMsg::UpgradeClient(msg)) => {
                self.verify_upgrade_client(client_id, msg)
            }
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the client is invalid: ID {}",
                client_id
            ))),
        }
    }

    fn verify_update_client(
        &self,
        client_id: &ClientId,
        msg: MsgUpdateAnyClient,
    ) -> Result<()> {
        if msg.client_id != *client_id {
            return Err(Error::InvalidClient(format!(
                "The client ID is mismatched: {} in the tx data, {} in the key",
                msg.client_id, client_id,
            )));
        }

        // check the posterior states
        let client_state = ClientReader::client_state(self, client_id)
            .map_err(|_| {
                Error::InvalidClient(format!(
                    "The client state doesn't exist: ID {}",
                    client_id
                ))
            })?;
        let height = client_state.latest_height();
        let consensus_state =
            self.consensus_state(client_id, height).map_err(|_| {
                Error::InvalidClient(format!(
                    "The consensus state doesn't exist: ID {}, Height {}",
                    client_id, height
                ))
            })?;
        // check the prior states
        let prev_client_state = self.client_state_pre(client_id)?;

        let client = AnyClient::from_client_type(client_state.client_type());
        let (new_client_state, new_consensus_state) = client
            .check_header_and_update_state(
                self,
                client_id.clone(),
                prev_client_state,
                msg.header.clone(),
            )
            .map_err(|e| {
                Error::InvalidHeader(format!(
                    "The header is invalid: ID {}, {}",
                    client_id, e,
                ))
            })?;
        if new_client_state != client_state
            || new_consensus_state != consensus_state
        {
            return Err(Error::InvalidClient(
                "The updated client state or consensus state is unexpected"
                    .to_owned(),
            ));
        }

        let event = make_update_client_event(client_id, &msg);
        self.check_emitted_event(event)
            .map_err(|e| Error::IbcEvent(e.to_string()))
    }

    fn verify_upgrade_client(
        &self,
        client_id: &ClientId,
        msg: MsgUpgradeAnyClient,
    ) -> Result<()> {
        if msg.client_id != *client_id {
            return Err(Error::InvalidClient(format!(
                "The client ID is mismatched: {} in the tx data, {} in the key",
                msg.client_id, client_id,
            )));
        }

        // check the posterior states
        let client_state_post = ClientReader::client_state(self, client_id)
            .map_err(|_| {
                Error::InvalidClient(format!(
                    "The client state doesn't exist: ID {}",
                    client_id
                ))
            })?;
        let height = client_state_post.latest_height();
        let consensus_state_post =
            self.consensus_state(client_id, height).map_err(|_| {
                Error::InvalidClient(format!(
                    "The consensus state doesn't exist: ID {}, Height {}",
                    client_id, height
                ))
            })?;

        // verify the given states
        let client_type = self.client_type(client_id).map_err(|_| {
            Error::InvalidClient(format!(
                "The client type doesn't exist: ID {}",
                client_id
            ))
        })?;
        let client = AnyClient::from_client_type(client_type);
        match client.verify_upgrade_and_update_state(
            &msg.client_state,
            &msg.consensus_state,
            msg.proof_upgrade_client.clone(),
            msg.proof_upgrade_consensus_state.clone(),
        ) {
            Ok((new_client_state, new_consensus_state)) => {
                if new_client_state != client_state_post
                    || new_consensus_state != consensus_state_post
                {
                    return Err(Error::InvalidClient(
                        "The updated client state or consensus state is \
                         unexpected"
                            .to_owned(),
                    ));
                }
            }
            Err(e) => {
                return Err(Error::ProofVerificationFailure(e.to_string()));
            }
        }

        let event = make_upgrade_client_event(client_id, &msg);
        self.check_emitted_event(event)
            .map_err(|e| Error::IbcEvent(e.to_string()))
    }

    fn client_state_pre(&self, client_id: &ClientId) -> Result<AnyClientState> {
        let key = client_state_key(client_id);
        match self.ctx.read_pre(&key) {
            Ok(Some(value)) => AnyClientState::try_from_slice(&value[..])
                .map_err(|e| {
                    Error::InvalidClient(format!(
                        "Decoding the client state failed: ID {}, {}",
                        client_id, e
                    ))
                }),
            _ => Err(Error::InvalidClient(format!(
                "The prior client state doesn't exist: ID {}",
                client_id
            ))),
        }
    }

    pub(super) fn client_counter_pre(&self) -> Result<u64> {
        let key = client_counter_key();
        self.read_counter_pre(&key)
            .map_err(|e| Error::InvalidClient(e.to_string()))
    }
}

/// Load the posterior client state
impl<'a, DB, H, CA> ClientReader for Ibc<'a, DB, H, CA>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    fn client_type(&self, client_id: &ClientId) -> Ics02Result<ClientType> {
        let key = client_type_key(client_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => ClientType::try_from_slice(&value[..])
                .map_err(|_| Ics02Error::implementation_specific()),
            Ok(None) => Err(Ics02Error::client_not_found(client_id.clone())),
            Err(_) => Err(Ics02Error::implementation_specific()),
        }
    }

    fn client_state(
        &self,
        client_id: &ClientId,
    ) -> Ics02Result<AnyClientState> {
        let key = client_state_key(client_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => AnyClientState::try_from_slice(&value[..])
                .map_err(|_| Ics02Error::implementation_specific()),
            Ok(None) => Err(Ics02Error::client_not_found(client_id.clone())),
            Err(_) => Err(Ics02Error::implementation_specific()),
        }
    }

    fn consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics02Result<AnyConsensusState> {
        let key = consensus_state_key(client_id, height);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => AnyConsensusState::try_from_slice(&value[..])
                .map_err(|_| Ics02Error::implementation_specific()),
            Ok(None) => Err(Ics02Error::consensus_state_not_found(
                client_id.clone(),
                height,
            )),
            Err(_) => Err(Ics02Error::implementation_specific()),
        }
    }

    /// Search for the lowest consensus state higher than `height`.
    fn next_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics02Result<Option<AnyConsensusState>> {
        let mut h = height.increment();
        loop {
            match self.consensus_state(client_id, h) {
                Ok(cs) => return Ok(Some(cs)),
                Err(e)
                    if e.detail()
                        == Ics02Error::consensus_state_not_found(
                            client_id.clone(),
                            h,
                        )
                        .detail() =>
                {
                    h = h.increment()
                }
                _ => return Err(Ics02Error::implementation_specific()),
            }
        }
    }

    /// Search for the highest consensus state lower than `height`.
    fn prev_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics02Result<Option<AnyConsensusState>> {
        let mut h = match height.decrement() {
            Ok(prev) => prev,
            Err(_) => return Ok(None),
        };
        loop {
            match self.consensus_state(client_id, h) {
                Ok(cs) => return Ok(Some(cs)),
                Err(e)
                    if e.detail()
                        == Ics02Error::consensus_state_not_found(
                            client_id.clone(),
                            h,
                        )
                        .detail() =>
                {
                    h = match height.decrement() {
                        Ok(prev) => prev,
                        Err(_) => return Ok(None),
                    };
                }
                _ => return Err(Ics02Error::implementation_specific()),
            }
        }
    }

    fn client_counter(&self) -> Ics02Result<u64> {
        let key = client_counter_key();
        self.read_counter(&key)
            .map_err(|_| Ics02Error::implementation_specific())
    }
}

impl From<IbcDataError> for Error {
    fn from(err: IbcDataError) -> Self {
        Self::InvalidIbcData(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::DecodingTxData(err)
    }
}
