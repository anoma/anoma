//! IBC validity predicate for connection module

use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSerialize};
use ibc::ics02_client::client_consensus::{AnyConsensusState, ConsensusState};
use ibc::ics02_client::client_state::AnyClientState;
use ibc::ics02_client::context::ClientReader;
use ibc::ics02_client::height::Height;
use ibc::ics03_connection::connection::{ConnectionEnd, Counterparty, State};
use ibc::ics03_connection::context::ConnectionReader;
use ibc::ics03_connection::handler::verify::verify_proofs;
use ibc::ics07_tendermint::consensus_state::ConsensusState as TendermintConsensusState;
use ibc::ics23_commitment::commitment::CommitmentPrefix;
use ibc::ics24_host::identifier::{ClientId, ConnectionId};
use ibc::ics24_host::Path;
use tendermint_proto::Protobuf;

use super::{Error, Ibc, Result, StateChange};
use crate::ledger::storage::{self, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::ibc::{
    ConnectionOpenAckData, ConnectionOpenConfirmData, ConnectionOpenTryData,
    Error as IbcDataError,
};
use crate::types::storage::{BlockHeight, Epoch, Key, KeySeg};

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    pub(super) fn validate_connection(
        &self,
        key: &Key,
        tx_data: &[u8],
    ) -> Result<bool> {
        if key.is_ibc_connection_counter() {
            // the counter should be increased
            return Ok(
                self.connection_counter_pre() < self.connection_counter()
            );
        }

        let conn_id = Self::get_connection_id(key)?;
        let conn = match self.connection_end(&conn_id) {
            Some(c) => c,
            None => {
                tracing::info!(
                    "the connection end of ID {} doesn't exist",
                    conn_id
                );
                return Ok(false);
            }
        };

        match self.get_connection_state_change(&conn_id)? {
            StateChange::Created => {
                self.validate_created_connection(&conn_id, conn, tx_data)
            }
            StateChange::Updated => {
                self.validate_updated_connection(&conn_id, conn, tx_data)
            }
            _ => {
                tracing::info!(
                    "unexpected state change for an IBC connection: {}",
                    key
                );
                Ok(false)
            }
        }
    }

    /// Returns the connection ID after #IBC/connections
    fn get_connection_id(key: &Key) -> Result<ConnectionId> {
        match key.segments.get(2) {
            Some(id) => ConnectionId::from_str(&id.raw())
                .map_err(|e| Error::KeyError(e.to_string())),
            None => Err(Error::KeyError(format!(
                "The connection key doesn't have a connection ID: {}",
                key
            ))),
        }
    }

    fn get_connection_state_change(
        &self,
        conn_id: &ConnectionId,
    ) -> Result<StateChange> {
        let path = Path::Connections(conn_id.clone()).to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a client type failed");
        self.get_state_change(&key)
    }

    fn validate_created_connection(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<bool> {
        match conn.state() {
            State::Init => {
                let client_id = conn.client_id();
                match ConnectionReader::client_state(self, client_id) {
                    Some(_) => Ok(true),
                    None => {
                        tracing::info!(
                            "the client state corresponding to the connection \
                             ID {} doesn't exist",
                            conn_id,
                        );
                        Ok(false)
                    }
                }
            }
            State::TryOpen => self.verify_connection_try_proof(conn, tx_data),
            _ => {
                tracing::info!(
                    "the connection state of ID {} is invalid",
                    conn_id
                );
                Ok(false)
            }
        }
    }

    fn validate_updated_connection(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<bool> {
        match conn.state() {
            State::Open => {
                let prev_conn = match self.connection_end_pre(conn_id) {
                    Some(c) => c,
                    None => {
                        tracing::info!(
                            "the previous connection of ID {} doesn't exist",
                            conn_id
                        );
                        return Ok(false);
                    }
                };
                match prev_conn.state() {
                    State::Init => {
                        self.verify_connection_ack_proof(conn_id, conn, tx_data)
                    }
                    State::TryOpen => self.verify_connection_confirm_proof(
                        conn_id, conn, tx_data,
                    ),
                    _ => {
                        tracing::info!(
                            "the state change of connection ID {} was invalid",
                            conn_id
                        );
                        Ok(false)
                    }
                }
            }
            _ => {
                tracing::info!(
                    "the state of connection ID {} is invalid",
                    conn_id
                );
                Ok(false)
            }
        }
    }

    fn verify_connection_try_proof(
        &self,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<bool> {
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
            Some(data.client_state()?),
            &conn,
            &expected_conn,
            &proofs,
        ) {
            Ok(_) => Ok(true),
            Err(e) => {
                tracing::info!("proof verification failed: {}", e);
                Ok(false)
            }
        }
    }

    fn verify_connection_ack_proof(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<bool> {
        let data = ConnectionOpenAckData::try_from_slice(tx_data)?;

        // version check
        if conn.versions().contains(&data.version()?) {
            tracing::info!("unsupported version");
            return Ok(false);
        }

        // counterpart connection ID check
        if let Some(counterpart_conn_id) = conn.counterparty().connection_id() {
            if *counterpart_conn_id != data.counterpart_connection_id()? {
                tracing::info!("counterpart connection ID mismatched");
                return Ok(false);
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
            Some(data.client_state()?),
            &conn,
            &expected_conn,
            &proofs,
        ) {
            Ok(_) => Ok(true),
            Err(e) => {
                tracing::info!("proof verification failed: {}", e);
                Ok(false)
            }
        }
    }

    fn verify_connection_confirm_proof(
        &self,
        conn_id: &ConnectionId,
        conn: ConnectionEnd,
        tx_data: &[u8],
    ) -> Result<bool> {
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
            Ok(_) => Ok(true),
            Err(e) => {
                tracing::info!("proof verification failed: {}", e);
                Ok(false)
            }
        }
    }

    fn connection_end_pre(
        &self,
        conn_id: &ConnectionId,
    ) -> Option<ConnectionEnd> {
        let path = Path::Connections(conn_id.clone()).to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a connection end failed");
        match self.ctx.read_pre(&key) {
            Ok(Some(value)) => ConnectionEnd::decode_vec(&value).ok(),
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn connection_counter_pre(&self) -> u64 {
        let key = Key::ibc_connection_counter();
        match self.ctx.read_pre(&key) {
            Ok(Some(value)) => match storage::types::decode(&value) {
                Ok(c) => c,
                Err(e) => {
                    tracing::error!(
                        "decoding a connection counter failed: {}",
                        e
                    );
                    u64::MAX
                }
            },
            _ => {
                tracing::error!("connection counter should exist");
                unreachable!();
            }
        }
    }
}

impl<'a, DB, H> ConnectionReader for Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    fn connection_end(&self, conn_id: &ConnectionId) -> Option<ConnectionEnd> {
        let path = Path::Connections(conn_id.clone()).to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a connection end failed");
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => ConnectionEnd::decode_vec(&value).ok(),
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn client_state(&self, client_id: &ClientId) -> Option<AnyClientState> {
        ClientReader::client_state(self, client_id)
    }

    fn host_current_height(&self) -> Height {
        let epoch = self.ctx.storage.get_block_epoch().0.0;
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
        let key = Key::ibc_connection_counter();
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => match storage::types::decode(&value) {
                Ok(c) => c,
                Err(e) => {
                    tracing::error!(
                        "decoding a connection counter failed: {}",
                        e
                    );
                    u64::MIN
                }
            },
            _ => {
                tracing::error!("connection counter doesn't exist");
                unreachable!();
            }
        }
    }
}

impl From<IbcDataError> for Error {
    fn from(err: IbcDataError) -> Self {
        Self::IbcDataError(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::DecodingTxDataError(err)
    }
}
