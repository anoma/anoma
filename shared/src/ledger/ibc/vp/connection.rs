//! IBC validity predicate for connection module

use thiserror::Error;

use super::super::handler::{
    commitment_prefix, make_open_ack_connection_event,
    make_open_confirm_connection_event, make_open_init_connection_event,
    make_open_try_connection_event,
};
use super::super::storage::{
    connection_counter_key, connection_id, connection_key,
    is_connection_counter_key, Error as IbcStorageError,
};
use super::{Ibc, StateChange};
use crate::ibc::core::ics02_client::client_consensus::AnyConsensusState;
use crate::ibc::core::ics02_client::client_state::AnyClientState;
use crate::ibc::core::ics02_client::context::ClientReader;
use crate::ibc::core::ics02_client::height::Height;
use crate::ibc::core::ics03_connection::connection::{
    ConnectionEnd, Counterparty, State,
};
use crate::ibc::core::ics03_connection::context::ConnectionReader;
use crate::ibc::core::ics03_connection::error::Error as Ics03Error;
use crate::ibc::core::ics03_connection::handler::verify::verify_proofs;
use crate::ibc::core::ics03_connection::msgs::conn_open_ack::MsgConnectionOpenAck;
use crate::ibc::core::ics03_connection::msgs::conn_open_confirm::MsgConnectionOpenConfirm;
use crate::ibc::core::ics03_connection::msgs::conn_open_try::MsgConnectionOpenTry;
use crate::ibc::core::ics23_commitment::commitment::CommitmentPrefix;
use crate::ibc::core::ics24_host::identifier::{ClientId, ConnectionId};
use crate::ledger::storage::{self, StorageHasher};
use crate::tendermint_proto::Protobuf;
use crate::types::ibc::data::{Error as IbcDataError, IbcMessage};
use crate::types::storage::{BlockHeight, Epoch, Key};
use crate::vm::WasmCacheAccess;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("State change error: {0}")]
    InvalidStateChange(String),
    #[error("Client error: {0}")]
    InvalidClient(String),
    #[error("Connection error: {0}")]
    InvalidConnection(String),
    #[error("Version error: {0}")]
    InvalidVersion(String),
    #[error("Proof verification error: {0}")]
    ProofVerificationFailure(Ics03Error),
    #[error("Decoding TX data error: {0}")]
    DecodingTxData(std::io::Error),
    #[error("IBC data error: {0}")]
    InvalidIbcData(IbcDataError),
    #[error("IBC storage error: {0}")]
    IbcStorage(IbcStorageError),
    #[error("IBC event error: {0}")]
    IbcEvent(String),
}

/// IBC connection functions result
pub type Result<T> = std::result::Result<T, Error>;
/// ConnectionReader result
type Ics03Result<T> = core::result::Result<T, Ics03Error>;

impl<'a, DB, H, CA> Ibc<'a, DB, H, CA>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    pub(super) fn validate_connection(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        if is_connection_counter_key(key) {
            // the counter should be increased
            let counter = self.connection_counter().map_err(|e| {
                Error::InvalidConnection(format!(
                    "The connection counter doesn't exist: {}",
                    e
                ))
            })?;
            if self.connection_counter_pre()? < counter {
                return Ok(());
            } else {
                return Err(Error::InvalidConnection(
                    "The connection counter is invalid".to_owned(),
                ));
            }
        }

        let conn_id = connection_id(key)?;
        let conn = self.connection_end(&conn_id).map_err(|_| {
            Error::InvalidConnection(format!(
                "The connection doesn't exist: ID {}",
                conn_id
            ))
        })?;

        match self.get_connection_state_change(&conn_id)? {
            StateChange::Created => {
                self.validate_created_connection(&conn_id, conn, tx_data)
            }
            StateChange::Updated => {
                self.validate_updated_connection(&conn_id, conn, tx_data)
            }
            _ => Err(Error::InvalidStateChange(format!(
                "The state change of the connection is invalid: ID {}",
                conn_id
            ))),
        }
    }

    fn get_connection_state_change(
        &self,
        conn_id: &ConnectionId,
    ) -> Result<StateChange> {
        let key = connection_key(conn_id);
        self.get_state_change(&key)
            .map_err(|e| Error::InvalidStateChange(e.to_string()))
    }

    fn validate_created_connection(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        match conn.state() {
            State::Init => {
                let client_id = conn.client_id();
                ConnectionReader::client_state(self, client_id).map_err(
                    |_| {
                        Error::InvalidClient(format!(
                            "The client state for the connection doesn't \
                             exist: ID {}",
                            conn_id,
                        ))
                    },
                )?;
                let ibc_msg = IbcMessage::decode(tx_data)?;
                let msg = ibc_msg.msg_connection_open_init()?;
                let event = make_open_init_connection_event(conn_id, &msg);
                self.check_emitted_event(event)
                    .map_err(|e| Error::IbcEvent(e.to_string()))
            }
            State::TryOpen => {
                let ibc_msg = IbcMessage::decode(tx_data)?;
                let msg = ibc_msg.msg_connection_open_try()?;
                self.verify_connection_try_proof(conn, &msg)?;
                let event = make_open_try_connection_event(conn_id, &msg);
                self.check_emitted_event(event)
                    .map_err(|e| Error::IbcEvent(e.to_string()))
            }
            _ => Err(Error::InvalidConnection(format!(
                "The connection state is invalid: ID {}",
                conn_id
            ))),
        }
    }

    fn validate_updated_connection(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        match conn.state() {
            State::Open => {
                let prev_conn = self.connection_end_pre(conn_id)?;
                match prev_conn.state() {
                    State::Init => {
                        let ibc_msg = IbcMessage::decode(tx_data)?;
                        let msg = ibc_msg.msg_connection_open_ack()?;
                        self.verify_connection_ack_proof(conn_id, conn, &msg)?;
                        let event = make_open_ack_connection_event(&msg);
                        self.check_emitted_event(event)
                            .map_err(|e| Error::IbcEvent(e.to_string()))
                    }
                    State::TryOpen => {
                        let ibc_msg = IbcMessage::decode(tx_data)?;
                        let msg = ibc_msg.msg_connection_open_confirm()?;
                        self.verify_connection_confirm_proof(
                            conn_id, conn, &msg,
                        )?;
                        let event = make_open_confirm_connection_event(&msg);
                        self.check_emitted_event(event)
                            .map_err(|e| Error::IbcEvent(e.to_string()))
                    }
                    _ => Err(Error::InvalidStateChange(format!(
                        "The state change of connection is invalid: ID {}",
                        conn_id
                    ))),
                }
            }
            _ => Err(Error::InvalidConnection(format!(
                "The state of the connection is invalid: ID {}",
                conn_id
            ))),
        }
    }

    fn verify_connection_try_proof(
        &self,
        conn: ConnectionEnd,
        msg: &MsgConnectionOpenTry,
    ) -> Result<()> {
        let client_id = conn.client_id().clone();
        let counterpart_client_id = conn.counterparty().client_id().clone();
        // expected connection end
        let expected_conn = ConnectionEnd::new(
            State::Init,
            counterpart_client_id,
            Counterparty::new(client_id, None, self.commitment_prefix()),
            conn.versions().to_vec(),
            conn.delay_period(),
        );

        match verify_proofs(
            self,
            msg.client_state.clone(),
            msg.proofs.height(),
            &conn,
            &expected_conn,
            &msg.proofs,
        ) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e)),
        }
    }

    fn verify_connection_ack_proof(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        msg: &MsgConnectionOpenAck,
    ) -> Result<()> {
        // version check
        if !conn.versions().contains(&msg.version) {
            return Err(Error::InvalidVersion(
                "The version is unsupported".to_owned(),
            ));
        }

        // counterpart connection ID check
        match conn.counterparty().connection_id() {
            Some(counterpart_conn_id) => {
                if *counterpart_conn_id != msg.counterparty_connection_id {
                    return Err(Error::InvalidConnection(format!(
                        "The counterpart connection ID mismatched: ID {}",
                        counterpart_conn_id
                    )));
                }
            }
            None => {
                return Err(Error::InvalidConnection(format!(
                    "The connection doesn't have the counterpart connection \
                     ID: ID {}",
                    conn_id
                )));
            }
        }

        // expected counterpart connection
        let expected_conn = ConnectionEnd::new(
            State::TryOpen,
            conn.counterparty().client_id().clone(),
            Counterparty::new(
                conn.client_id().clone(),
                Some(conn_id.clone()),
                self.commitment_prefix(),
            ),
            conn.versions().to_vec(),
            conn.delay_period(),
        );

        match verify_proofs(
            self,
            msg.client_state.clone(),
            msg.proofs.height(),
            &conn,
            &expected_conn,
            &msg.proofs,
        ) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e)),
        }
    }

    fn verify_connection_confirm_proof(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        msg: &MsgConnectionOpenConfirm,
    ) -> Result<()> {
        // expected counterpart connection
        let expected_conn = ConnectionEnd::new(
            State::Open,
            conn.counterparty().client_id().clone(),
            Counterparty::new(
                conn.client_id().clone(),
                Some(conn_id.clone()),
                self.commitment_prefix(),
            ),
            conn.versions().to_vec(),
            conn.delay_period(),
        );

        match verify_proofs(
            self,
            None,
            msg.proofs.height(),
            &conn,
            &expected_conn,
            &msg.proofs,
        ) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e)),
        }
    }

    fn connection_end_pre(
        &self,
        conn_id: &ConnectionId,
    ) -> Result<ConnectionEnd> {
        let key = connection_key(conn_id);
        match self.ctx.read_pre(&key) {
            Ok(Some(value)) => ConnectionEnd::decode_vec(&value).map_err(|e| {
                Error::InvalidConnection(format!(
                    "Decoding the connection failed: {}",
                    e
                ))
            }),
            _ => Err(Error::InvalidConnection(format!(
                "Unable to get the previous connection: ID {}",
                conn_id
            ))),
        }
    }

    fn connection_counter_pre(&self) -> Result<u64> {
        let key = connection_counter_key();
        self.read_counter_pre(&key)
            .map_err(|e| Error::InvalidConnection(e.to_string()))
    }
}

impl<'a, DB, H, CA> ConnectionReader for Ibc<'a, DB, H, CA>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    fn connection_end(
        &self,
        conn_id: &ConnectionId,
    ) -> Ics03Result<ConnectionEnd> {
        let key = connection_key(conn_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => ConnectionEnd::decode_vec(&value)
                .map_err(|_| Ics03Error::implementation_specific()),
            Ok(None) => Err(Ics03Error::connection_not_found(conn_id.clone())),
            Err(_) => Err(Ics03Error::implementation_specific()),
        }
    }

    fn client_state(
        &self,
        client_id: &ClientId,
    ) -> Ics03Result<AnyClientState> {
        ClientReader::client_state(self, client_id)
            .map_err(Ics03Error::ics02_client)
    }

    fn host_current_height(&self) -> Height {
        self.host_height()
    }

    fn host_oldest_height(&self) -> Height {
        let epoch = Epoch::default().0;
        let height = BlockHeight::default().0;
        Height::new(epoch, height)
    }

    fn commitment_prefix(&self) -> CommitmentPrefix {
        commitment_prefix()
    }

    fn client_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Ics03Result<AnyConsensusState> {
        self.consensus_state(client_id, height)
            .map_err(Ics03Error::ics02_client)
    }

    fn host_consensus_state(
        &self,
        height: Height,
    ) -> Ics03Result<AnyConsensusState> {
        ClientReader::host_consensus_state(self, height)
            .map_err(Ics03Error::ics02_client)
    }

    fn connection_counter(&self) -> Ics03Result<u64> {
        let key = connection_counter_key();
        self.read_counter(&key)
            .map_err(|_| Ics03Error::implementation_specific())
    }
}

impl From<IbcStorageError> for Error {
    fn from(err: IbcStorageError) -> Self {
        Self::IbcStorage(err)
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
