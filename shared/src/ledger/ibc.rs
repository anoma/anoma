//! IBC integration as a native validity predicate

use std::collections::HashSet;

use crate::ledger::native_vp::{Ctx, NativeVp};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::ledger::vp_env::Result;
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{Key, DbKeySeg};

/// IBC VP
pub struct Ibc;

impl NativeVp for Ibc {
    const ADDR: InternalAddress = InternalAddress::Ibc;

    fn init_genesis_storage<DB, H>(_storage: &mut Storage<DB, H>)
    where
        DB: storage::DB + for<'iter> storage::DBIter<'iter>,
        H: StorageHasher,
    {
    }

    fn validate_tx<DB, H>(
        ctx: &mut Ctx<DB, H>,
        tx_data: &[u8],
        keys_changed: &HashSet<Key>,
        verifiers: &HashSet<Address>,
    ) -> Result<bool>
    where
        DB: storage::DB + for<'iter> storage::DBIter<'iter>,
        H: StorageHasher,
    {
        for key in keys_changed {
            if !is_ibc_key(key) {
                continue;
            }

            // clients
            if key.segments.contains(&DbKeySeg::StringSeg("clientState".to_owned())) {
                let client_id = get_client_id(key);
                if ctx.has_key_pre(key)? {
                    // update/upgrade the client
                    match get_header() {
                        Some(header) => {
                            let client_type = ctx.ibc_handler
                                .client_type(&client_id)
                                .ok_or_else(|| Kind::ClientNotFound(client_id.clone()))?;
                            let client_def = AnyClient::from_client_type(client_type);
                            client_def.check_header_and_update_state(client_state, header);
                        }
                        None => {
                            // upgrade the client
                        }
                    }
                } else {
                    // create a new client
                }
            }

            // connections
            if key.segments.contains(&StringSeg("connections".to_owned())) {
                let conn_id = get_connection_id(key);
                match ctx.read_pre(key)? {
                    Some(conn) => {
                        // the connection already exists
                        let client_id = conn.client_id();
                        if let Some(client_state) = ctx.ibc_handler.client_state(client_id) {
                        } else {
                            // the client doesn't exist
                            return Ok(false);
                        }
                    }
                    None => {
                        // ConnectionOpenInit
                        let conn: ConnectionEnd = decode(ctx.read_post(key)?);
                        let client_id = conn.client_id();
                        let client_key = get_client_state_key(client_id);
                        if !ctx.has_key_pre(&client_key)? || !conn.state_matches(&State::INIT) {
                            return Ok(false);
                        }
                    }
                }
            }

            // channels
        }
        Ok(true)
    }
}
