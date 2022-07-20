//! IBC validity predicate for client module
use std::convert::TryInto;
use std::str::FromStr;

use thiserror::Error;

use super::super::handler::{
    make_create_client_event, make_update_client_event,
    make_upgrade_client_event,
};
use super::super::storage::{
    client_counter_key, client_state_key, client_type_key,
    client_update_height_key, client_update_timestamp_key, consensus_height,
    consensus_state_key, consensus_state_prefix,
};
use super::{Ibc, StateChange};
use crate::ibc::clients::ics07_tendermint::consensus_state::ConsensusState as TmConsensusState;
use crate::ibc::core::ics02_client::client_consensus::{
    AnyConsensusState, ConsensusState,
};
use crate::ibc::core::ics02_client::client_def::{AnyClient, ClientDef};
use crate::ibc::core::ics02_client::client_state::AnyClientState;
use crate::ibc::core::ics02_client::client_type::ClientType;
use crate::ibc::core::ics02_client::context::ClientReader;
use crate::ibc::core::ics02_client::error::Error as Ics02Error;
use crate::ibc::core::ics02_client::height::Height;
use crate::ibc::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
use crate::ibc::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
use crate::ibc::core::ics02_client::msgs::ClientMsg;
use crate::ibc::core::ics04_channel::context::ChannelReader;
use crate::ibc::core::ics23_commitment::commitment::CommitmentRoot;
use crate::ibc::core::ics24_host::identifier::ClientId;
use crate::ibc::core::ics26_routing::msgs::Ics26Envelope;
use crate::ledger::native_vp::VpEnv;
use crate::ledger::storage::{self, StorageHasher};
use crate::tendermint_proto::Protobuf;
use crate::types::ibc::data::{Error as IbcDataError, IbcMessage};
use crate::types::storage::{BlockHeight, Key};
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
    #[error("Client update time error: {0}")]
    InvalidTimestamp(String),
    #[error("Client update height error: {0}")]
    InvalidHeight(String),
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

    fn get_client_update_time_change(
        &self,
        client_id: &ClientId,
    ) -> Result<StateChange> {
        let key = client_update_timestamp_key(client_id);
        let timestamp_change = self
            .get_state_change(&key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))?;
        let key = client_update_height_key(client_id);
        let height_change = self
            .get_state_change(&key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))?;
        // the time and height should be updated at once
        match (timestamp_change, height_change) {
            (StateChange::Created, StateChange::Created) => {
                Ok(StateChange::Created)
            }
            (StateChange::Updated, StateChange::Updated) => {
                let timestamp_pre =
                    self.client_update_time_pre(client_id).map_err(|e| {
                        Error::InvalidTimestamp(format!(
                            "Reading the prior client update time failed: {}",
                            e
                        ))
                    })?;
                let timestamp_post = self
                    .client_update_time(client_id, Height::default())
                    .map_err(|e| {
                        Error::InvalidTimestamp(format!(
                            "Reading the posterior client update time failed: \
                             {}",
                            e
                        ))
                    })?;
                if timestamp_post.nanoseconds() <= timestamp_pre.nanoseconds() {
                    return Err(Error::InvalidTimestamp(format!(
                        "The state change of the client update time is \
                         invalid: ID {}",
                        client_id
                    )));
                }
                let height_pre =
                    self.client_update_height_pre(client_id).map_err(|e| {
                        Error::InvalidHeight(format!(
                            "Reading the prior client update height failed: {}",
                            e
                        ))
                    })?;
                let height_post = self
                    .client_update_height(client_id, Height::default())
                    .map_err(|e| {
                        Error::InvalidTimestamp(format!(
                            "Reading the posterior client update height \
                             failed: {}",
                            e
                        ))
                    })?;
                if height_post <= height_pre {
                    return Err(Error::InvalidHeight(format!(
                        "The state change of the client update height is \
                         invalid: ID {}",
                        client_id
                    )));
                }
                Ok(StateChange::Updated)
            }
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the client update time and height are \
                 invalid: ID {}",
                client_id
            ))),
        }
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
        if self.get_client_update_time_change(client_id)?
            != StateChange::Created
        {
            return Err(Error::InvalidClient(format!(
                "The client update time or height are invalid: ID {}",
                client_id,
            )));
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
        if self.get_client_update_time_change(client_id)?
            != StateChange::Updated
        {
            return Err(Error::InvalidClient(format!(
                "The client update time and height are invalid: ID {}",
                client_id,
            )));
        }
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
        match self.ctx.read_bytes_pre(&key) {
            Ok(Some(value)) => {
                AnyClientState::decode_vec(&value).map_err(|e| {
                    Error::InvalidClient(format!(
                        "Decoding the client state failed: ID {}, {}",
                        client_id, e
                    ))
                })
            }
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
        match self.ctx.read_bytes_post(&key) {
            Ok(Some(value)) => {
                let type_str = std::str::from_utf8(&value)
                    .map_err(|_| Ics02Error::implementation_specific())?;
                ClientType::from_str(type_str)
                    .map_err(|_| Ics02Error::implementation_specific())
            }
            Ok(None) => Err(Ics02Error::client_not_found(client_id.clone())),
            Err(_) => Err(Ics02Error::implementation_specific()),
        }
    }

    fn client_state(
        &self,
        client_id: &ClientId,
    ) -> Ics02Result<AnyClientState> {
        let key = client_state_key(client_id);
        match self.ctx.read_bytes_post(&key) {
            Ok(Some(value)) => AnyClientState::decode_vec(&value)
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
        match self.ctx.read_bytes_post(&key) {
            Ok(Some(value)) => AnyConsensusState::decode_vec(&value)
                .map_err(|_| Ics02Error::implementation_specific()),
            Ok(None) => Err(Ics02Error::consensus_state_not_found(
                client_id.clone(),
                height,
            )),
            Err(_) => Err(Ics02Error::implementation_specific()),
        }
    }

    // Reimplement to avoid reading the posterior state
    fn maybe_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics02Result<Option<AnyConsensusState>> {
        let key = consensus_state_key(client_id, height);
        match self.ctx.read_bytes_pre(&key) {
            Ok(Some(value)) => {
                let cs = AnyConsensusState::decode_vec(&value)
                    .map_err(|_| Ics02Error::implementation_specific())?;
                Ok(Some(cs))
            }
            Ok(None) => Ok(None),
            Err(_) => Err(Ics02Error::implementation_specific()),
        }
    }

    /// Search for the lowest consensus state higher than `height`.
    fn next_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics02Result<Option<AnyConsensusState>> {
        let prefix = consensus_state_prefix(client_id);
        let mut iter = self
            .ctx
            .iter_prefix(&prefix)
            .map_err(|_| Ics02Error::implementation_specific())?;
        let mut lowest_height_value = None;
        while let Some((key, value)) = self
            .ctx
            .iter_pre_next(&mut iter)
            .map_err(|_| Ics02Error::implementation_specific())?
        {
            let key = Key::parse(&key)
                .map_err(|_| Ics02Error::implementation_specific())?;
            let consensus_height = consensus_height(&key)
                .map_err(|_| Ics02Error::implementation_specific())?;
            if consensus_height > height {
                lowest_height_value = match lowest_height_value {
                    Some((lowest, _)) if consensus_height < lowest => {
                        Some((consensus_height, value))
                    }
                    Some(_) => continue,
                    None => Some((consensus_height, value)),
                };
            }
        }
        match lowest_height_value {
            Some((_, value)) => {
                let cs = AnyConsensusState::decode_vec(&value)
                    .map_err(|_| Ics02Error::implementation_specific())?;
                Ok(Some(cs))
            }
            None => Ok(None),
        }
    }

    /// Search for the highest consensus state lower than `height`.
    fn prev_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics02Result<Option<AnyConsensusState>> {
        let prefix = consensus_state_prefix(client_id);
        let mut iter = self
            .ctx
            .iter_prefix(&prefix)
            .map_err(|_| Ics02Error::implementation_specific())?;
        let mut highest_height_value = None;
        while let Some((key, value)) = self
            .ctx
            .iter_pre_next(&mut iter)
            .map_err(|_| Ics02Error::implementation_specific())?
        {
            let key = Key::parse(&key)
                .map_err(|_| Ics02Error::implementation_specific())?;
            let consensus_height = consensus_height(&key)
                .map_err(|_| Ics02Error::implementation_specific())?;
            if consensus_height < height {
                highest_height_value = match highest_height_value {
                    Some((highest, _)) if consensus_height > highest => {
                        Some((consensus_height, value))
                    }
                    Some(_) => continue,
                    None => Some((consensus_height, value)),
                };
            }
        }
        match highest_height_value {
            Some((_, value)) => {
                let cs = AnyConsensusState::decode_vec(&value)
                    .map_err(|_| Ics02Error::implementation_specific())?;
                Ok(Some(cs))
            }
            None => Ok(None),
        }
    }

    fn host_height(&self) -> Height {
        let height = self.ctx.storage.get_block_height().0.0;
        // the revision number is always 0
        Height::new(0, height)
    }

    fn host_consensus_state(
        &self,
        height: Height,
    ) -> Ics02Result<AnyConsensusState> {
        let (header, gas) = self
            .ctx
            .storage
            .get_block_header(Some(BlockHeight(height.revision_height)))
            .map_err(|_| Ics02Error::implementation_specific())?;
        self.ctx
            .gas_meter
            .borrow_mut()
            .add(gas)
            .map_err(|_| Ics02Error::implementation_specific())?;
        match header {
            Some(h) => Ok(TmConsensusState {
                root: CommitmentRoot::from_bytes(h.hash.as_slice()),
                timestamp: h.time.try_into().unwrap(),
                next_validators_hash: h.next_validators_hash.into(),
            }
            .wrap_any()),
            None => Err(Ics02Error::missing_raw_header()),
        }
    }

    fn pending_host_consensus_state(&self) -> Ics02Result<AnyConsensusState> {
        let (block_height, gas) = self.ctx.storage.get_block_height();
        self.ctx
            .gas_meter
            .borrow_mut()
            .add(gas)
            .map_err(|_| Ics02Error::implementation_specific())?;
        let height = Height::new(0, block_height.0);
        ClientReader::host_consensus_state(self, height)
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
