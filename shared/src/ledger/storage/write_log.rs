//! Write log is temporary storage for modifications performed by a transaction.
//! before they are committed to the ledger's storage.

use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{Address, EstablishedAddressGen};
use crate::types::ibc::IbcEvent;
use crate::types::storage::Key;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Storage error applying a write log: {0}")]
    StorageError(storage::Error),
    #[error("Trying to update a temporary value")]
    UpdateTemporaryValue,
    #[error(
        "Trying to update a validity predicate that a new account that's not \
         yet committed to storage"
    )]
    UpdateVpOfNewAccount,
    #[error("Trying to delete a validity predicate")]
    DeleteVp,
    #[error("Trying to write a temporary value after deleting")]
    WriteTempAfterDelete,
}

/// Result for functions that may fail
pub type Result<T> = std::result::Result<T, Error>;

/// A storage modification
#[derive(Clone, Debug)]
pub enum StorageModification {
    /// Write a new value
    Write {
        /// Value bytes
        value: Vec<u8>,
    },
    /// Delete an existing key-value
    Delete,
    /// Initialize a new account with established address and a given validity
    /// predicate. The key for `InitAccount` inside the [`WriteLog`] must point
    /// to its validity predicate.
    InitAccount {
        /// Validity predicate bytes
        vp: Vec<u8>,
    },
    /// Temporary value. This value will be never written to the storage. After
    /// writing a temporary value, it can't be mutated with normal write.
    Temp {
        /// Value bytes
        value: Vec<u8>,
    },
}

/// The write log storage
#[derive(Debug, Clone)]
pub struct WriteLog {
    /// The generator of established addresses
    address_gen: Option<EstablishedAddressGen>,
    /// All the storage modification accepted by validity predicates are stored
    /// in block write-log, before being committed to the storage
    block_write_log: HashMap<Key, StorageModification>,
    /// The storage modifications for the current transaction
    tx_write_log: HashMap<Key, StorageModification>,
    /// The IBC event for the current transaction
    ibc_event: Option<IbcEvent>,
}

impl Default for WriteLog {
    fn default() -> Self {
        Self {
            address_gen: None,
            block_write_log: HashMap::with_capacity(100_000),
            tx_write_log: HashMap::with_capacity(100),
            ibc_event: None,
        }
    }
}

impl WriteLog {
    /// Read a value at the given key and return the value and the gas cost,
    /// returns [`None`] if the key is not present in the write log
    pub fn read(&self, key: &Key) -> (Option<&StorageModification>, u64) {
        // try to read from tx write log first
        match self.tx_write_log.get(key).or_else(|| {
            // if not found, then try to read from block write log
            self.block_write_log.get(key)
        }) {
            Some(v) => {
                let gas = match v {
                    StorageModification::Write { ref value } => {
                        key.len() + value.len()
                    }
                    StorageModification::Delete => key.len(),
                    StorageModification::InitAccount { ref vp } => {
                        key.len() + vp.len()
                    }
                    StorageModification::Temp { ref value } => {
                        key.len() + value.len()
                    }
                };
                (Some(v), gas as _)
            }
            None => (None, key.len() as _),
        }
    }

    /// Write a key and a value and return the gas cost and the size difference
    /// Fails with [`Error::UpdateVpOfNewAccount`] when attempting to update a
    /// validity predicate of a new account that's not yet committed to storage.
    /// Fails with [`Error::UpdateTemporaryValue`] when attempting to update a
    /// temporary value.
    pub fn write(&mut self, key: &Key, value: Vec<u8>) -> Result<(u64, i64)> {
        let len = value.len();
        let gas = key.len() + len;
        let size_diff = match self
            .tx_write_log
            .insert(key.clone(), StorageModification::Write { value })
        {
            Some(prev) => match prev {
                StorageModification::Write { ref value } => {
                    len as i64 - value.len() as i64
                }
                StorageModification::Delete => len as i64,
                StorageModification::InitAccount { .. } => {
                    return Err(Error::UpdateVpOfNewAccount);
                }
                StorageModification::Temp { .. } => {
                    return Err(Error::UpdateTemporaryValue);
                }
            },
            // set just the length of the value because we don't know if
            // the previous value exists on the storage
            None => len as i64,
        };
        Ok((gas as _, size_diff))
    }

    /// Write a key and a value and return the gas cost and the size difference
    /// Fails with [`Error::UpdateVpOfNewAccount`] when attempting to update a
    /// validity predicate of a new account that's not yet committed to storage.
    /// Fails with [`Error::WriteTempAfterDelete`] when attempting to update a
    /// temporary value after deleting.
    pub fn write_temp(
        &mut self,
        key: &Key,
        value: Vec<u8>,
    ) -> Result<(u64, i64)> {
        let len = value.len();
        let gas = key.len() + len;
        let size_diff = match self
            .tx_write_log
            .insert(key.clone(), StorageModification::Temp { value })
        {
            Some(prev) => match prev {
                StorageModification::Write { ref value } => {
                    len as i64 - value.len() as i64
                }
                StorageModification::Delete => {
                    return Err(Error::WriteTempAfterDelete);
                }
                StorageModification::InitAccount { .. } => {
                    return Err(Error::UpdateVpOfNewAccount);
                }
                StorageModification::Temp { ref value } => {
                    len as i64 - value.len() as i64
                }
            },
            // set just the length of the value because we don't know if
            // the previous value exists on the storage
            None => len as i64,
        };
        Ok((gas as _, size_diff))
    }

    /// Delete a key and its value, and return the gas cost and the size
    /// difference.
    /// Fails with [`Error::DeleteVp`] for a validity predicate key, which are
    /// not possible to delete.
    pub fn delete(&mut self, key: &Key) -> Result<(u64, i64)> {
        if key.is_validity_predicate().is_some() {
            return Err(Error::DeleteVp);
        }
        let size_diff = match self
            .tx_write_log
            .insert(key.clone(), StorageModification::Delete)
        {
            Some(prev) => match prev {
                StorageModification::Write { ref value } => value.len() as i64,
                StorageModification::Delete => 0,
                StorageModification::InitAccount { .. } => {
                    return Err(Error::DeleteVp);
                }
                StorageModification::Temp { ref value } => value.len() as i64,
            },
            // set 0 because we don't know if the previous value exists on the
            // storage
            None => 0,
        };
        let gas = key.len() + size_diff as usize;
        Ok((gas as _, -size_diff))
    }

    /// Initialize a new account and return the gas cost.
    pub fn init_account(
        &mut self,
        storage_address_gen: &EstablishedAddressGen,
        vp: Vec<u8>,
    ) -> (Address, u64) {
        // If we've previously generated a new account, we use the local copy of
        // the generator. Otherwise, we create a new copy from the storage
        let address_gen =
            self.address_gen.get_or_insert(storage_address_gen.clone());
        let addr =
            address_gen.generate_address("TODO more randomness".as_bytes());
        let key = Key::validity_predicate(&addr);
        let gas = (key.len() + vp.len()) as _;
        self.tx_write_log
            .insert(key, StorageModification::InitAccount { vp });
        (addr, gas)
    }

    /// Set an IBC event and return the gas cost.
    pub fn set_ibc_event(&mut self, event: IbcEvent) -> u64 {
        let len = event
            .attributes
            .iter()
            .fold(0, |acc, (k, v)| acc + k.len() + v.len());
        self.ibc_event = Some(event);
        len as _
    }

    /// Get the storage keys changed and accounts keys initialized in the
    /// current transaction. The account keys point to the validity predicates
    /// of the newly created accounts.
    pub fn get_keys(&self) -> HashSet<Key> {
        self.tx_write_log.keys().cloned().collect()
    }

    /// Get the storage keys changed in the current transaction (left) and
    /// the addresses of accounts initialized in the current transaction
    /// (right). The first vector excludes keys of validity predicates of
    /// newly initialized accounts, but may include keys of other data
    /// written into newly initialized accounts.
    pub fn get_partitioned_keys(&self) -> (HashSet<&Key>, HashSet<&Address>) {
        use itertools::{Either, Itertools};
        self.tx_write_log.iter().partition_map(|(key, value)| {
            match (key.is_validity_predicate(), value) {
                (Some(address), StorageModification::InitAccount { .. }) => {
                    Either::Right(address)
                }
                _ => Either::Left(key),
            }
        })
    }

    /// Get the addresses of accounts initialized in the current transaction.
    pub fn get_initialized_accounts(&self) -> Vec<Address> {
        self.tx_write_log
            .iter()
            .filter_map(|(key, value)| {
                match (key.is_validity_predicate(), value) {
                    (
                        Some(address),
                        StorageModification::InitAccount { .. },
                    ) => Some(address.clone()),
                    _ => None,
                }
            })
            .collect()
    }

    /// Take the IBC event of the current transaction
    pub fn take_ibc_event(&mut self) -> Option<IbcEvent> {
        self.ibc_event.take()
    }

    /// Get the IBC event of the current transaction
    pub fn get_ibc_event(&self) -> Option<&IbcEvent> {
        self.ibc_event.as_ref()
    }

    /// Commit the current transaction's write log to the block when it's
    /// accepted by all the triggered validity predicates. Starts a new
    /// transaction write log.
    pub fn commit_tx(&mut self) {
        let tx_write_log = std::mem::replace(
            &mut self.tx_write_log,
            HashMap::with_capacity(100),
        );
        self.block_write_log.extend(tx_write_log);
    }

    /// Drop the current transaction's write log when it's declined by any of
    /// the triggered validity predicates. Starts a new transaction write log.
    pub fn drop_tx(&mut self) {
        self.tx_write_log.clear();
    }

    /// Commit the current block's write log to the storage. Starts a new block
    /// write log.
    pub fn commit_block<DB, H>(
        &mut self,
        storage: &mut Storage<DB, H>,
    ) -> Result<()>
    where
        DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
        H: StorageHasher,
    {
        let mut batch = Storage::<DB, H>::batch();
        for (key, entry) in self
            .block_write_log
            .iter()
            .filter(|(k, _)| k.is_updatable())
        {
            match entry {
                StorageModification::Write { value } => {
                    storage
                        .batch_write_subspace_val(
                            &mut batch,
                            key,
                            value.clone(),
                        )
                        .map_err(Error::StorageError)?;
                }
                StorageModification::Delete => {
                    storage
                        .batch_delete_subspace_val(&mut batch, key)
                        .map_err(Error::StorageError)?;
                }
                StorageModification::InitAccount { vp } => {
                    storage
                        .batch_write_subspace_val(&mut batch, key, vp.clone())
                        .map_err(Error::StorageError)?;
                }
                // temporary value isn't persisted
                StorageModification::Temp { .. } => {}
            }
        }
        storage.exec_batch(batch).map_err(Error::StorageError)?;
        if let Some(address_gen) = self.address_gen.take() {
            storage.address_gen = address_gen
        }
        self.block_write_log.clear();
        Ok(())
    }

    /// Get the storage keys that have been changed in the write log, grouped by
    /// their verifiers, whose VPs will be triggered by the changes.
    ///
    /// Note that some storage keys may comprise of multiple addresses, in which
    /// case every address will be the verifier of the key.
    pub fn verifiers_changed_keys(
        &self,
        verifiers_from_tx: &HashSet<Address>,
    ) -> HashMap<Address, HashSet<Key>> {
        let (changed_keys, initialized_accounts) = self.get_partitioned_keys();
        let mut verifiers =
            verifiers_from_tx
                .iter()
                .fold(HashMap::new(), |mut acc, addr| {
                    let changed_keys: HashSet<Key> =
                        changed_keys.iter().map(|&key| key.clone()).collect();
                    acc.insert(addr.clone(), changed_keys);
                    acc
                });

        // get changed keys grouped by the address
        for key in changed_keys {
            for addr in &key.find_addresses() {
                if verifiers_from_tx.contains(addr)
                    || initialized_accounts.contains(addr)
                {
                    // We can skip this when the address has been added from the
                    // Tx above, which associates with this address all the
                    // changed storage keys, even if their address is not
                    // included in the key.
                    // Also skip if it's an address of a newly initialized
                    // account, because anything can be written into an
                    // account's storage in the same tx in which it's
                    // initialized.
                    continue;
                }
                // Add the address as a verifier
                match verifiers.get_mut(addr) {
                    Some(keys) => {
                        keys.insert(key.clone());
                    }
                    None => {
                        let keys: HashSet<Key> =
                            vec![key.clone()].into_iter().collect();
                        verifiers.insert(addr.clone(), keys);
                    }
                }
            }
        }
        // The new accounts should be validated by every verifier's VP
        for initialized_account in initialized_accounts {
            for (_verifier, keys) in verifiers.iter_mut() {
                // The key for an initialized account points to its VP
                let vp_key = Key::validity_predicate(initialized_account);
                keys.insert(vp_key);
            }
        }
        verifiers
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use proptest::prelude::*;

    use super::*;
    use crate::types::address;

    #[test]
    fn test_crud_value() {
        let mut write_log = WriteLog::default();
        let key =
            Key::parse("key".to_owned()).expect("cannot parse the key string");

        // read a non-existing key
        let (value, gas) = write_log.read(&key);
        assert!(value.is_none());
        assert_eq!(gas, key.len() as u64);

        // delete a non-existing key
        let (gas, diff) = write_log.delete(&key).unwrap();
        assert_eq!(gas, key.len() as u64);
        assert_eq!(diff, 0);

        // insert a value
        let inserted = "inserted".as_bytes().to_vec();
        let (gas, diff) = write_log.write(&key, inserted.clone()).unwrap();
        assert_eq!(gas, (key.len() + inserted.len()) as u64);
        assert_eq!(diff, inserted.len() as i64);

        // read the value
        let (value, gas) = write_log.read(&key);
        match value.expect("no read value") {
            StorageModification::Write { value } => {
                assert_eq!(*value, inserted)
            }
            _ => panic!("unexpected read result"),
        }
        assert_eq!(gas, (key.len() + inserted.len()) as u64);

        // update the value
        let updated = "updated".as_bytes().to_vec();
        let (gas, diff) = write_log.write(&key, updated.clone()).unwrap();
        assert_eq!(gas, (key.len() + updated.len()) as u64);
        assert_eq!(diff, updated.len() as i64 - inserted.len() as i64);

        // delete the key
        let (gas, diff) = write_log.delete(&key).unwrap();
        assert_eq!(gas, (key.len() + updated.len()) as u64);
        assert_eq!(diff, -(updated.len() as i64));

        // delete the deleted key again
        let (gas, diff) = write_log.delete(&key).unwrap();
        assert_eq!(gas, key.len() as u64);
        assert_eq!(diff, 0);

        // read the deleted key
        let (value, gas) = write_log.read(&key);
        match &value.expect("no read value") {
            StorageModification::Delete => {}
            _ => panic!("unexpected result"),
        }
        assert_eq!(gas, key.len() as u64);

        // insert again
        let reinserted = "reinserted".as_bytes().to_vec();
        let (gas, diff) = write_log.write(&key, reinserted.clone()).unwrap();
        assert_eq!(gas, (key.len() + reinserted.len()) as u64);
        assert_eq!(diff, reinserted.len() as i64);
    }

    #[test]
    fn test_crud_account() {
        let mut write_log = WriteLog::default();
        let address_gen = EstablishedAddressGen::new("test");

        // init
        let init_vp = "initialized".as_bytes().to_vec();
        let (addr, gas) = write_log.init_account(&address_gen, init_vp.clone());
        let vp_key = Key::validity_predicate(&addr);
        assert_eq!(gas, (vp_key.len() + init_vp.len()) as u64);

        // read
        let (value, gas) = write_log.read(&vp_key);
        match value.expect("no read value") {
            StorageModification::InitAccount { vp } => assert_eq!(*vp, init_vp),
            _ => panic!("unexpected result"),
        }
        assert_eq!(gas, (vp_key.len() + init_vp.len()) as u64);

        // get all
        let (_changed_keys, init_accounts) = write_log.get_partitioned_keys();
        assert!(init_accounts.contains(&&addr));
        assert_eq!(init_accounts.len(), 1);
    }

    #[test]
    fn test_update_initialized_account_should_fail() {
        let mut write_log = WriteLog::default();
        let address_gen = EstablishedAddressGen::new("test");

        let init_vp = "initialized".as_bytes().to_vec();
        let (addr, _) = write_log.init_account(&address_gen, init_vp);
        let vp_key = Key::validity_predicate(&addr);

        // update should fail
        let updated_vp = "updated".as_bytes().to_vec();
        let result = write_log.write(&vp_key, updated_vp).unwrap_err();
        assert_matches!(result, Error::UpdateVpOfNewAccount);
    }

    #[test]
    fn test_delete_initialized_account_should_fail() {
        let mut write_log = WriteLog::default();
        let address_gen = EstablishedAddressGen::new("test");

        let init_vp = "initialized".as_bytes().to_vec();
        let (addr, _) = write_log.init_account(&address_gen, init_vp);
        let vp_key = Key::validity_predicate(&addr);

        // delete should fail
        let result = write_log.delete(&vp_key).unwrap_err();
        assert_matches!(result, Error::DeleteVp);
    }

    #[test]
    fn test_delete_vp_should_fail() {
        let mut write_log = WriteLog::default();
        let addr = address::testing::established_address_1();
        let vp_key = Key::validity_predicate(&addr);

        // delete should fail
        let result = write_log.delete(&vp_key).unwrap_err();
        assert_matches!(result, Error::DeleteVp);
    }

    #[test]
    fn test_commit() {
        let mut storage =
            crate::ledger::storage::testing::TestStorage::default();
        let mut write_log = WriteLog::default();
        let address_gen = EstablishedAddressGen::new("test");

        let key1 =
            Key::parse("key1".to_owned()).expect("cannot parse the key string");
        let key2 =
            Key::parse("key2".to_owned()).expect("cannot parse the key string");
        let key3 =
            Key::parse("key3".to_owned()).expect("cannot parse the key string");
        let key4 =
            Key::parse("key4".to_owned()).expect("cannot parse the key string");

        // initialize an account
        let vp1 = "vp1".as_bytes().to_vec();
        let (addr1, _) = write_log.init_account(&address_gen, vp1.clone());
        write_log.commit_tx();

        // write values
        let val1 = "val1".as_bytes().to_vec();
        write_log.write(&key1, val1.clone()).unwrap();
        write_log.write(&key2, val1.clone()).unwrap();
        write_log.write(&key3, val1.clone()).unwrap();
        write_log.write_temp(&key4, val1.clone()).unwrap();
        write_log.commit_tx();

        // these values are not written due to drop_tx
        let val2 = "val2".as_bytes().to_vec();
        write_log.write(&key1, val2.clone()).unwrap();
        write_log.write(&key2, val2.clone()).unwrap();
        write_log.write(&key3, val2).unwrap();
        write_log.drop_tx();

        // deletes and updates values
        let val3 = "val3".as_bytes().to_vec();
        write_log.delete(&key2).unwrap();
        write_log.write(&key3, val3.clone()).unwrap();
        write_log.commit_tx();

        // commit a block
        write_log.commit_block(&mut storage).expect("commit failed");

        let (vp, _gas) =
            storage.validity_predicate(&addr1).expect("vp read failed");
        assert_eq!(vp, Some(vp1));
        let (value, _) = storage.read(&key1).expect("read failed");
        assert_eq!(value.expect("no read value"), val1);
        let (value, _) = storage.read(&key2).expect("read failed");
        assert!(value.is_none());
        let (value, _) = storage.read(&key3).expect("read failed");
        assert_eq!(value.expect("no read value"), val3);
        let (value, _) = storage.read(&key4).expect("read failed");
        assert_eq!(value, None);
    }

    prop_compose! {
        fn arb_verifiers_changed_key_tx_all_key()
            (verifiers_from_tx in testing::arb_verifiers_from_tx())
            (tx_write_log in testing::arb_tx_write_log(verifiers_from_tx.clone()),
                verifiers_from_tx in Just(verifiers_from_tx))
        -> (HashSet<Address>, HashMap<Key, StorageModification>) {
            (verifiers_from_tx, tx_write_log)
        }
    }

    proptest! {
        /// Test [`WriteLog::verifiers_changed_keys`] that:
        /// 1. Every address from `verifiers_from_tx` is associated with all the
        ///    changed storage keys.
        /// 2. Every changed storage key is associated with all the addresses
        ///    included in the key except for the addresses of newly
        ///    initialized accounts.
        /// 3. Addresses of newly initialized accounts are not verifiers, so
        ///    that anything can be written into an account's storage in the
        ///    same tx in which it's initialized.
        /// 4. Every address is associated with all the newly initialized
        ///    accounts keys to their validity predicates.
        #[test]
        fn verifiers_changed_key_tx_all_key(
            (verifiers_from_tx, tx_write_log) in arb_verifiers_changed_key_tx_all_key(),
        ) {
            let write_log = WriteLog { tx_write_log, ..WriteLog::default() };
            let all_keys = write_log.get_keys();

            let verifiers_changed_keys = write_log.verifiers_changed_keys(&verifiers_from_tx);

            println!("verifiers_from_tx {:#?}", verifiers_from_tx);
            for verifier_from_tx in verifiers_from_tx {
                assert!(verifiers_changed_keys.contains_key(&verifier_from_tx));
                let keys = verifiers_changed_keys.get(&verifier_from_tx).unwrap().clone();
                // Test for 1.
                assert_eq!(&keys, &all_keys);
            }

            let (_changed_keys, initialized_accounts) = write_log.get_partitioned_keys();
            for (_verifier_addr, keys) in verifiers_changed_keys.iter() {
                for key in keys {
                    for addr_from_key in &key.find_addresses() {
                        if !initialized_accounts.contains(addr_from_key) {
                            let keys = verifiers_changed_keys.get(addr_from_key).unwrap();
                            // Test for 2.
                            assert!(keys.contains(key));
                        }
                    }
                }
            }

            println!("verifiers_changed_keys {:#?}", verifiers_changed_keys);
            println!("initialized_accounts {:#?}", initialized_accounts);
            for initialized_account in initialized_accounts {
                // Test for 3.
                assert!(!verifiers_changed_keys.contains_key(initialized_account));
                for (_verifier_addr, keys) in verifiers_changed_keys.iter() {
                    // Test for 4.
                    let vp_key = Key::validity_predicate(initialized_account);
                    assert!(keys.contains(&vp_key));
                }
            }
        }
    }
}

/// Helpers for testing with write log.
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use proptest::collection;
    use proptest::prelude::{any, prop_oneof, Just, Strategy};

    use super::*;
    use crate::types::address::testing::arb_address;
    use crate::types::storage::testing::arb_key;

    /// Generate an arbitrary tx write log of [`HashMap<Key,
    /// StorageModification>`].
    pub fn arb_tx_write_log(
        verifiers_from_tx: HashSet<Address>,
    ) -> impl Strategy<Value = HashMap<Key, StorageModification>> + 'static
    {
        arb_key().prop_flat_map(move |key| {
            // If the key is a validity predicate key and its owner is in the
            // verifier set, we must not generate `InitAccount` for it.
            let can_init_account = key
                .is_validity_predicate()
                .map(|owner| !verifiers_from_tx.contains(owner))
                .unwrap_or_default();
            collection::hash_map(
                Just(key),
                arb_storage_modification(can_init_account),
                0..100,
            )
        })
    }

    /// Generate arbitrary verifiers from tx of [`HashSet<Address>`].
    pub fn arb_verifiers_from_tx() -> impl Strategy<Value = HashSet<Address>> {
        collection::hash_set(arb_address(), 0..10)
    }

    /// Generate an arbitrary [`StorageModification`].
    pub fn arb_storage_modification(
        can_init_account: bool,
    ) -> impl Strategy<Value = StorageModification> {
        if can_init_account {
            prop_oneof![
                any::<Vec<u8>>()
                    .prop_map(|value| StorageModification::Write { value }),
                Just(StorageModification::Delete),
                any::<Vec<u8>>()
                    .prop_map(|vp| StorageModification::InitAccount { vp }),
                any::<Vec<u8>>()
                    .prop_map(|value| StorageModification::Temp { value }),
            ]
            .boxed()
        } else {
            prop_oneof![
                any::<Vec<u8>>()
                    .prop_map(|value| StorageModification::Write { value }),
                Just(StorageModification::Delete),
                any::<Vec<u8>>()
                    .prop_map(|value| StorageModification::Temp { value }),
            ]
            .boxed()
        }
    }
}
