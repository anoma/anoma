//! IBC validity predicate for connection module

use borsh::{BorshDeserialize, BorshSerialize};
use ibc::ics02_client::client_consensus::{AnyConsensusState, ConsensusState};
use ibc::ics02_client::client_state::AnyClientState;
use ibc::ics02_client::context::ClientReader;
use ibc::ics02_client::height::Height;
use ibc::ics03_connection::connection::{ConnectionEnd, Counterparty, State};
use ibc::ics03_connection::context::ConnectionReader;
use ibc::ics03_connection::error::Error as Ics03Error;
use ibc::ics03_connection::handler::verify::verify_proofs;
use ibc::ics07_tendermint::consensus_state::ConsensusState as TendermintConsensusState;
use ibc::ics23_commitment::commitment::CommitmentPrefix;
use ibc::ics24_host::identifier::{ClientId, ConnectionId};
use thiserror::Error;

use super::storage::{
    connection_counter_key, connection_id, connection_key,
    is_connection_counter_key, Error as IbcStorageError,
};
use super::{Ibc, StateChange};
use crate::ledger::storage::{self, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::ibc::{
    ConnectionOpenAckData, ConnectionOpenConfirmData, ConnectionOpenTryData,
    Error as IbcDataError,
};
use crate::types::storage::{BlockHeight, Epoch, Key, KeySeg};

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
}

/// IBC connection functions result
pub type Result<T> = std::result::Result<T, Error>;

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    pub(super) fn validate_connection(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<()> {
        if is_connection_counter_key(key) {
            // the counter should be increased
            if self.connection_counter_pre()? < self.connection_counter() {
                return Ok(());
            } else {
                return Err(Error::InvalidConnection(
                    "The connection counter is invalid".to_owned(),
                ));
            }
        }

        let conn_id = connection_id(key)?;
        let conn = self.connection_end(&conn_id).ok_or_else(|| {
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
                match ConnectionReader::client_state(self, client_id) {
                    Some(_) => Ok(()),
                    None => Err(Error::InvalidClient(format!(
                        "The client state for the connection doesn't exist: \
                         ID {}",
                        conn_id,
                    ))),
                }
            }
            State::TryOpen => self.verify_connection_try_proof(conn, tx_data),
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
                        self.verify_connection_ack_proof(conn_id, conn, tx_data)
                    }
                    State::TryOpen => self.verify_connection_confirm_proof(
                        conn_id, conn, tx_data,
                    ),
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
        tx_data: &[u8],
    ) -> Result<()> {
        let data = ConnectionOpenTryData::try_from_slice(tx_data)?;

        let client_id = conn.client_id().clone();
        let counterpart_client_id = conn.counterparty().client_id().clone();
        // expected connection end
        let expected_conn = ConnectionEnd::new(
            State::Init,
            counterpart_client_id,
            Counterparty::new(client_id, None, self.commitment_prefix()),
            conn.versions(),
            conn.delay_period(),
        );

        let proofs = data.proofs()?;
        match verify_proofs(
            self,
            Some(data.client_state),
            &conn,
            &expected_conn,
            &proofs,
        ) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e)),
        }
    }

    fn verify_connection_ack_proof(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        let data = ConnectionOpenAckData::try_from_slice(tx_data)?;

        // version check
        if !conn.versions().contains(&data.version) {
            return Err(Error::InvalidVersion(
                "The version is unsupported".to_owned(),
            ));
        }

        // counterpart connection ID check
        if let Some(counterpart_conn_id) = conn.counterparty().connection_id() {
            if *counterpart_conn_id != data.counterpart_conn_id {
                return Err(Error::InvalidConnection(format!(
                    "The counterpart connection ID mismatched: ID {}",
                    counterpart_conn_id
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
            conn.versions(),
            conn.delay_period(),
        );

        let proofs = data.proofs()?;
        match verify_proofs(
            self,
            Some(data.client_state),
            &conn,
            &expected_conn,
            &proofs,
        ) {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::ProofVerificationFailure(e)),
        }
    }

    fn verify_connection_confirm_proof(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<()> {
        let data = ConnectionOpenConfirmData::try_from_slice(tx_data)?;

        // expected counterpart connection
        let expected_conn = ConnectionEnd::new(
            State::Open,
            conn.counterparty().client_id().clone(),
            Counterparty::new(
                conn.client_id().clone(),
                Some(conn_id.clone()),
                self.commitment_prefix(),
            ),
            conn.versions(),
            conn.delay_period(),
        );

        let proofs = data.proofs()?;
        match verify_proofs(self, None, &conn, &expected_conn, &proofs) {
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
            Ok(Some(value)) => ConnectionEnd::try_from_slice(&value[..])
                .map_err(|e| {
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

impl<'a, DB, H> ConnectionReader for Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    fn connection_end(&self, conn_id: &ConnectionId) -> Option<ConnectionEnd> {
        let key = connection_key(conn_id);
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => ConnectionEnd::try_from_slice(&value[..]).ok(),
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn client_state(&self, client_id: &ClientId) -> Option<AnyClientState> {
        ClientReader::client_state(self, client_id)
    }

    fn host_current_height(&self) -> Height {
        let epoch = self.ctx.storage.get_current_epoch().0.0;
        let height = self.ctx.storage.get_block_height().0.0;
        Height::new(epoch, height)
    }

    fn host_oldest_height(&self) -> Height {
        let epoch = Epoch::default().0;
        let height = BlockHeight::default().0;
        Height::new(epoch, height)
    }

    fn commitment_prefix(&self) -> CommitmentPrefix {
        let addr = Address::Internal(InternalAddress::Ibc);
        let bytes = addr
            .raw()
            .try_to_vec()
            .expect("Encoding an address string shouldn't fail");
        CommitmentPrefix::from(bytes)
    }

    fn client_consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Option<AnyConsensusState> {
        self.consensus_state(client_id, height)
    }

    fn host_consensus_state(
        &self,
        _height: Height,
    ) -> Option<AnyConsensusState> {
        self.ctx
            .storage
            .get_block_header()
            .0
            .map(|h| TendermintConsensusState::from(h).wrap_any())
    }

    fn connection_counter(&self) -> u64 {
        let key = connection_counter_key();
        self.read_counter(&key)
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
