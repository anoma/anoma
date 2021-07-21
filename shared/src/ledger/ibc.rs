//! IBC integration as a native validity predicate

use std::collections::HashSet;
use std::str::FromStr;

use borsh::BorshDeserialize;
use ibc::ics02_client::client_consensus::AnyConsensusState;
use ibc::ics02_client::client_def::{AnyClient, ClientDef};
use ibc::ics02_client::client_state::AnyClientState;
use ibc::ics02_client::client_type::ClientType;
use ibc::ics02_client::context::ClientReader;
use ibc::ics02_client::height::Height;
use ibc::ics24_host::identifier::ClientId;
use ibc::ics24_host::Path;
use tendermint_proto::Protobuf;
use thiserror::Error;

use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{Address, InternalAddress};
use crate::types::ibc::{ClientUpdateData, ClientUpgradeData};
use crate::types::storage::{Key, KeySeg};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
    #[error("Key error: {0}")]
    KeyError(String),
    #[error("Decoding TX data error: {0}")]
    DecodingTxDataError(std::io::Error),
    #[error("IBC data error: {0}")]
    IbcDataError(crate::types::ibc::Error),
}

/// IBC functions result
pub type Result<T> = std::result::Result<T, Error>;

/// IBC VP
pub struct Ibc<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

/// Initialize storage in the genesis block.
pub fn init_genesis_storage<DB, H>(storage: &mut Storage<DB, H>)
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    // the client counter
    let path = "clients/counter".to_owned();
    let key =
        Key::ibc_key(path).expect("Creating a key for a client counter failed");
    let value = crate::ledger::storage::types::encode(&0);
    storage
        .write(&key, value)
        .expect("Unable to write the initial client counter");
}

impl<'a, DB, H> NativeVp for Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::Ibc;

    fn validate_tx(
        &self,
        tx_data: &[u8],
        keys_changed: &HashSet<Key>,
        _verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        let mut clients = HashSet::new();

        for key in keys_changed {
            if !key.is_ibc_key() {
                continue;
            }

            let accepted = match Self::get_ibc_prefix(key) {
                IbcPrefix::Client => {
                    let client_id = Self::get_client_id(key)?;
                    if !clients.insert(client_id.clone()) {
                        // this client has been checked
                        continue;
                    }
                    match self.get_client_state_change(&client_id)? {
                        StateChange::Created => {
                            self.validate_created_client(&client_id)?
                        }
                        StateChange::Updated => {
                            self.validate_updated_client(&client_id, tx_data)?
                        }
                        _ => {
                            tracing::info!(
                                "unexpected state change for an IBC client: \
                                 key {}",
                                key
                            );
                            false
                        }
                    }
                }
                // TODO implement validations for modules
                IbcPrefix::Connection => false,
                IbcPrefix::Channel => false,
                IbcPrefix::Packet => false,
                IbcPrefix::Unknown => {
                    return Err(Error::KeyError(format!(
                        "Invalid IBC-related key: {}",
                        key
                    )));
                }
            };
            if !accepted {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

enum StateChange {
    Created,
    Updated,
    Deleted,
    NotExists,
}

enum IbcPrefix {
    Client,
    Connection,
    Channel,
    Packet,
    Unknown,
}

impl<'a, DB, H> Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    /// Returns the prefix after #IBC
    fn get_ibc_prefix(key: &Key) -> IbcPrefix {
        match key.segments.get(1) {
            Some(prefix) => match &*prefix.raw() {
                "clients" => IbcPrefix::Client,
                "connections" => IbcPrefix::Connection,
                "channelEnds" => IbcPrefix::Channel,
                "packets" => IbcPrefix::Packet,
                _ => IbcPrefix::Unknown,
            },
            None => IbcPrefix::Unknown,
        }
    }

    /// Returns the client ID after #IBC/clients
    fn get_client_id(key: &Key) -> Result<ClientId> {
        match key.segments.get(2) {
            Some(id) => ClientId::from_str(&id.raw())
                .map_err(|e| Error::KeyError(e.to_string())),
            None => Err(Error::KeyError(format!(
                "The client key doesn't have a client ID: {}",
                key
            ))),
        }
    }

    fn get_client_state_change(
        &self,
        client_id: &ClientId,
    ) -> Result<StateChange> {
        let path = Path::ClientState(client_id.clone()).to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a client type failed");
        if self.ctx.has_key_pre(&key)? {
            if self.ctx.has_key_post(&key)? {
                Ok(StateChange::Updated)
            } else {
                Ok(StateChange::Deleted)
            }
        } else if self.ctx.has_key_post(&key)? {
            Ok(StateChange::Created)
        } else {
            Ok(StateChange::NotExists)
        }
    }

    fn validate_created_client(&self, client_id: &ClientId) -> Result<bool> {
        let client_type = match self.client_type(client_id) {
            Some(t) => t,
            None => {
                tracing::info!(
                    "the client type of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        let client_state = match self.client_state(client_id) {
            Some(s) => s,
            None => {
                tracing::info!(
                    "the client state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        let height = client_state.latest_height();
        let consensus_state = match self.consensus_state(client_id, height) {
            Some(c) => c,
            None => {
                tracing::info!(
                    "the consensus state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        Ok(client_type == client_state.client_type()
            && client_type == consensus_state.client_type())
    }

    fn validate_updated_client(
        &self,
        client_id: &ClientId,
        tx_data: &[u8],
    ) -> Result<bool> {
        // check the type of data in tx_data
        match ClientUpdateData::try_from_slice(tx_data) {
            Ok(data) => {
                // "UpdateClient"
                self.verify_update_client(client_id, data)
            }
            Err(_) => match ClientUpgradeData::try_from_slice(tx_data) {
                Ok(data) => {
                    // "UpgradeClient"
                    self.verify_upgrade_client(client_id, data)
                }
                Err(e) => Err(Error::DecodingTxDataError(e)),
            },
        }
    }

    fn verify_update_client(
        &self,
        client_id: &ClientId,
        data: ClientUpdateData,
    ) -> Result<bool> {
        let id = data.client_id().map_err(Error::IbcDataError)?;
        if id != *client_id {
            tracing::info!(
                "the client ID is mismatched: {} in the tx data, {} in the key",
                id,
                client_id,
            );
            return Ok(false);
        }

        // check the posterior states
        let client_state = match self.client_state(client_id) {
            Some(s) => s,
            None => {
                tracing::info!(
                    "the client state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        let height = client_state.latest_height();
        let consensus_state = match self.consensus_state(client_id, height) {
            Some(s) => s,
            None => {
                tracing::info!(
                    "the consensus state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        // check the prior states
        let prev_client_state = match self.client_state_pre(client_id) {
            Some(s) => s,
            None => {
                tracing::info!(
                    "the prior client state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        let prev_consensus_state = match self
            .consensus_state_pre(client_id, prev_client_state.latest_height())
        {
            Some(s) => s,
            None => {
                tracing::info!(
                    "the prior consensus state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };

        let client = AnyClient::from_client_type(client_state.client_type());
        let headers = data.headers().map_err(Error::IbcDataError)?;
        let updated = headers.iter().try_fold(
            (prev_client_state, prev_consensus_state),
            |(new_client_state, _), header| {
                client.check_header_and_update_state(
                    new_client_state,
                    header.clone(),
                )
            },
        );
        match updated {
            Ok((new_client_state, new_consensus_state)) => Ok(new_client_state
                == client_state
                && new_consensus_state == consensus_state),
            Err(e) => {
                tracing::info!(
                    "a header is invalid for the client {}: {}",
                    client_id,
                    e,
                );
                Ok(false)
            }
        }
    }

    fn verify_upgrade_client(
        &self,
        client_id: &ClientId,
        data: ClientUpgradeData,
    ) -> Result<bool> {
        let id = data.client_id().map_err(Error::IbcDataError)?;
        if id != *client_id {
            tracing::info!(
                "the client ID is mismatched: {} in the tx data, {} in the key",
                id,
                client_id,
            );
            return Ok(false);
        }

        // check the posterior states
        let client_state = match self.client_state(client_id) {
            Some(s) => s,
            None => {
                tracing::info!(
                    "the client state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        let height = client_state.latest_height();
        let consensus_state = match self.consensus_state(client_id, height) {
            Some(s) => s,
            None => {
                tracing::info!(
                    "the consensus state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        // check the prior client state
        let pre_client_state = match self.client_state_pre(client_id) {
            Some(s) => s,
            None => {
                tracing::info!(
                    "the prior client state of ID {} doesn't exist",
                    client_id
                );
                return Ok(false);
            }
        };
        // get proofs
        let client_proof = data.proof_client().map_err(Error::IbcDataError)?;
        let consensus_proof =
            data.proof_consensus_state().map_err(Error::IbcDataError)?;

        let client = AnyClient::from_client_type(client_state.client_type());
        match client.verify_upgrade_and_update_state(
            &pre_client_state,
            &consensus_state,
            client_proof,
            consensus_proof,
        ) {
            Ok((new_client_state, new_consensus_state)) => Ok(new_client_state
                == client_state
                && new_consensus_state == consensus_state),
            Err(e) => {
                tracing::info!(
                    "the header is invalid for the client {}: {}",
                    client_id,
                    e
                );
                Ok(false)
            }
        }
    }

    fn client_state_pre(&self, client_id: &ClientId) -> Option<AnyClientState> {
        let path = Path::ClientState(client_id.clone()).to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a client state shouldn't fail");
        match self.ctx.read_pre(&key) {
            Ok(Some(value)) => AnyClientState::decode_vec(&value).ok(),
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn consensus_state_pre(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Option<AnyConsensusState> {
        let path = Path::ClientConsensusState {
            client_id: client_id.clone(),
            epoch: height.revision_number,
            height: height.revision_height,
        }
        .to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a consensus state shouldn't fail");
        match self.ctx.read_pre(&key) {
            Ok(Some(value)) => AnyConsensusState::decode_vec(&value).ok(),
            // returns None even if DB read fails
            _ => None,
        }
    }
}

/// Load the posterior client state
impl<'a, DB, H> ClientReader for Ibc<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    fn client_type(&self, client_id: &ClientId) -> Option<ClientType> {
        let path = Path::ClientType(client_id.clone()).to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a client type shouldn't fail");
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => {
                let s: String = storage::types::decode(&value).ok()?;
                Some(ClientType::from_str(&s).ok()?)
            }
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn client_state(&self, client_id: &ClientId) -> Option<AnyClientState> {
        let path = Path::ClientState(client_id.clone()).to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a client state shouldn't fail");
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => AnyClientState::decode_vec(&value).ok(),
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn consensus_state(
        &self,
        client_id: &ClientId,
        height: Height,
    ) -> Option<AnyConsensusState> {
        let path = Path::ClientConsensusState {
            client_id: client_id.clone(),
            epoch: height.revision_number,
            height: height.revision_height,
        }
        .to_string();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a consensus state shouldn't fail");
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => AnyConsensusState::decode_vec(&value).ok(),
            // returns None even if DB read fails
            _ => None,
        }
    }

    fn client_counter(&self) -> u64 {
        let path = "clients/counter".to_owned();
        let key = Key::ibc_key(path)
            .expect("Creating a key for a client counter failed");
        match self.ctx.read_post(&key) {
            Ok(Some(value)) => storage::types::decode(&value)
                .expect("converting a client counter shouldn't failed"),
            _ => {
                tracing::error!("client counter doesn't exist");
                unreachable!();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use borsh::ser::BorshSerialize;
    use ibc::ics02_client::client_consensus::ConsensusState;
    use ibc::ics02_client::client_state::ClientState;
    use ibc::ics02_client::header::AnyHeader;
    use ibc::mock::client_state::{MockClientState, MockConsensusState};
    use ibc::mock::header::MockHeader;
    use ibc::Height;
    use tendermint_proto::Protobuf;

    use super::*;
    use crate::ledger::gas::VpGasMeter;
    use crate::ledger::storage::testing::TestStorage;
    use crate::ledger::storage::write_log::WriteLog;
    use crate::proto::Tx;

    fn get_client_id() -> ClientId {
        ClientId::from_str("test_client").expect("Creating a client ID failed")
    }

    fn get_client_type_key() -> Key {
        let client_id = get_client_id();
        let path = Path::ClientType(client_id).to_string();
        Key::ibc_key(path).expect("Creating a key for a client type failed")
    }

    fn get_client_state_key() -> Key {
        let client_id = get_client_id();
        let path = Path::ClientState(client_id).to_string();
        Key::ibc_key(path).expect("Creating a key for a client state failed")
    }

    fn get_consensus_state_key(height: Height) -> Key {
        let path = Path::ClientConsensusState {
            client_id: get_client_id(),
            epoch: height.revision_number,
            height: height.revision_height,
        }
        .to_string();
        Key::ibc_key(path)
            .expect("Creating a key for a consensus state shouldn't fail")
    }

    fn insert_init_states(write_log: &mut WriteLog) {
        // insert a mock client type
        let client_type_key = get_client_type_key();
        let client_type = ClientType::Mock.as_str().to_owned();
        write_log
            .write(
                &client_type_key,
                crate::ledger::storage::types::encode(&client_type),
            )
            .expect("write failed");
        // insert a mock client state
        let client_state_key = get_client_state_key();
        let height = Height::new(1, 10);
        let header = MockHeader::new(height);
        let client_state = MockClientState(header).wrap_any();
        let bytes = client_state.encode_vec().expect("encoding failed");
        write_log
            .write(&client_state_key, bytes)
            .expect("write failed");
        // insert a mock consensus state
        let consensus_key = get_consensus_state_key(height);
        let consensus_state = MockConsensusState(header).wrap_any();
        let bytes = consensus_state.encode_vec().expect("encoding failed");
        write_log
            .write(&consensus_key, bytes)
            .expect("write failed");
        write_log.commit_tx();
    }

    #[test]
    fn test_create_client() {
        let storage = TestStorage::default();
        let mut write_log = WriteLog::default();
        insert_init_states(&mut write_log);

        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_client_state_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_create_client_fail() {
        let storage = TestStorage::default();
        let write_log = WriteLog::default();
        let tx_code = vec![];
        let tx_data = vec![];
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_client_state_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should return false because no state is stored
        assert!(
            !ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }

    #[test]
    fn test_update_client() {
        let mut storage = TestStorage::default();
        let mut write_log = WriteLog::default();
        insert_init_states(&mut write_log);
        write_log.commit_block(&mut storage).expect("commit failed");

        // update the client
        let client_id = get_client_id();
        let client_state_key = get_client_state_key();
        let height = Height::new(1, 11);
        let header = MockHeader::new(height);
        let client_state = MockClientState(header).wrap_any();
        let bytes = client_state.encode_vec().expect("encoding failed");
        write_log
            .write(&client_state_key, bytes)
            .expect("write failed");
        let consensus_key = get_consensus_state_key(height);
        let consensus_state = MockConsensusState(header).wrap_any();
        let bytes = consensus_state.encode_vec().expect("encoding failed");
        write_log
            .write(&consensus_key, bytes)
            .expect("write failed");
        write_log.commit_tx();

        let tx_code = vec![];
        let tx_data =
            ClientUpdateData::new(client_id, vec![AnyHeader::from(header)])
                .try_to_vec()
                .expect("encoding failed");
        let tx = Tx::new(tx_code, Some(tx_data.clone()));
        let gas_meter = VpGasMeter::new(0);
        let ctx = Ctx::new(&storage, &write_log, &tx, gas_meter);

        let mut keys_changed = HashSet::new();
        keys_changed.insert(get_client_state_key());

        let verifiers = HashSet::new();

        let ibc = Ibc { ctx };
        // this should return true because state has been stored
        assert!(
            ibc.validate_tx(&tx_data, &keys_changed, &verifiers)
                .expect("validation failed")
        );
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}
