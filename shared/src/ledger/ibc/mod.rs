//! IBC integration

#[cfg(any(feature = "ibc-vp", feature = "ibc-vp-abci"))]
pub mod handler;
pub mod storage;
#[cfg(any(feature = "ibc-vp", feature = "ibc-vp-abci"))]
pub mod vp;

use borsh::BorshSerialize;
use storage::{
    capability_index_key, channel_counter_key, client_counter_key,
    connection_counter_key,
};

use crate::ledger::storage::{self as ledger_storage, Storage, StorageHasher};

/// Initialize storage in the genesis block.
pub fn init_genesis_storage<DB, H>(storage: &mut Storage<DB, H>)
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
{
    // the client counter
    let key = client_counter_key();
    let value = 0_u64.try_to_vec().unwrap();
    storage
        .write(&key, value)
        .expect("Unable to write the initial client counter");

    // the connection counter
    let key = connection_counter_key();
    let value = 0_u64.try_to_vec().unwrap();
    storage
        .write(&key, value)
        .expect("Unable to write the initial connection counter");

    // the channel counter
    let key = channel_counter_key();
    let value = 0_u64.try_to_vec().unwrap();
    storage
        .write(&key, value)
        .expect("Unable to write the initial channel counter");

    // the capability index
    let key = capability_index_key();
    let value = 0_u64.try_to_vec().unwrap();
    storage
        .write(&key, value)
        .expect("Unable to write the initial capability index");
}
