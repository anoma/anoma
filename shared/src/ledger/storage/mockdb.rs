//! DB mock for testing

use std::cell::RefCell;
use std::collections::{btree_map, BTreeMap};
use std::ops::Bound::{Excluded, Included};
use std::path::Path;
use std::str::FromStr;

use super::merkle_tree::{MerkleTreeStoresRead, StoreType};
use super::{
    BlockStateRead, BlockStateWrite, DBIter, DBWriteBatch, Error, Result, DB,
};
use crate::ledger::storage::types::{self, KVBytes, PrefixIterator};
use crate::tendermint::block::Header;
use crate::tendermint_proto::Protobuf;
#[cfg(feature = "ferveo-tpke")]
use crate::types::storage::TxQueue;
use crate::types::storage::{
    BlockHeight, BlockResults, Key, KeySeg, KEY_SEGMENT_SEPARATOR,
};
use crate::types::time::DateTimeUtc;

/// An in-memory DB for testing.
#[derive(Debug, Default)]
pub struct MockDB(
    // The state is wrapped in `RefCell` to allow modifying it directly from
    // batch write method (which requires immutable self ref).
    RefCell<BTreeMap<String, Vec<u8>>>,
);

// The `MockDB` is not `Sync`, but we're sharing it across threads for reading
// only (for parallelized VP runs). In a different context, this may not be
// safe.
unsafe impl Sync for MockDB {}

/// An in-memory write batch is not needed as it just updates values in memory.
/// It's here to satisfy the storage interface.
#[derive(Debug, Default)]
pub struct MockDBWriteBatch;

impl DB for MockDB {
    /// There is no cache for MockDB
    type Cache = ();
    type WriteBatch = MockDBWriteBatch;

    fn open(_db_path: impl AsRef<Path>, _cache: Option<&Self::Cache>) -> Self {
        Self::default()
    }

    fn flush(&self, _wait: bool) -> Result<()> {
        Ok(())
    }

    fn read_last_block(&mut self) -> Result<Option<BlockStateRead>> {
        // Block height
        let height: BlockHeight = match self.0.borrow().get("height") {
            Some(bytes) => types::decode(bytes).map_err(Error::CodingError)?,
            None => return Ok(None),
        };
        // Block results
        let results_path = format!("results/{}", height.raw());
        let results: BlockResults =
            match self.0.borrow().get(results_path.as_str()) {
                Some(bytes) => {
                    types::decode(bytes).map_err(Error::CodingError)?
                }
                None => return Ok(None),
            };

        // Epoch start height and time
        let next_epoch_min_start_height: BlockHeight =
            match self.0.borrow().get("next_epoch_min_start_height") {
                Some(bytes) => {
                    types::decode(bytes).map_err(Error::CodingError)?
                }
                None => return Ok(None),
            };
        let next_epoch_min_start_time: DateTimeUtc =
            match self.0.borrow().get("next_epoch_min_start_time") {
                Some(bytes) => {
                    types::decode(bytes).map_err(Error::CodingError)?
                }
                None => return Ok(None),
            };
        #[cfg(feature = "ferveo-tpke")]
        let tx_queue: TxQueue = match self.0.borrow().get("tx_queue") {
            Some(bytes) => types::decode(bytes).map_err(Error::CodingError)?,
            None => return Ok(None),
        };

        // Load data at the height
        let prefix = format!("{}/", height.raw());
        let upper_prefix = format!("{}/", height.next_height().raw());
        let mut merkle_tree_stores = MerkleTreeStoresRead::default();
        let mut hash = None;
        let mut epoch = None;
        let mut pred_epochs = None;
        let mut address_gen = None;
        for (path, bytes) in self
            .0
            .borrow()
            .range((Included(prefix), Excluded(upper_prefix)))
        {
            let segments: Vec<&str> =
                path.split(KEY_SEGMENT_SEPARATOR).collect();
            match segments.get(1) {
                Some(prefix) => match *prefix {
                    "tree" => match segments.get(2) {
                        Some(s) => {
                            let st = StoreType::from_str(s)?;
                            match segments.get(3) {
                                Some(&"root") => merkle_tree_stores.set_root(
                                    &st,
                                    types::decode(bytes)
                                        .map_err(Error::CodingError)?,
                                ),
                                Some(&"store") => merkle_tree_stores.set_store(
                                    &st,
                                    types::decode(bytes)
                                        .map_err(Error::CodingError)?,
                                ),
                                _ => unknown_key_error(path)?,
                            }
                        }
                        None => unknown_key_error(path)?,
                    },
                    "header" => {
                        // the block header doesn't have to be restored
                    }
                    "hash" => {
                        hash = Some(
                            types::decode(bytes).map_err(Error::CodingError)?,
                        )
                    }
                    "epoch" => {
                        epoch = Some(
                            types::decode(bytes).map_err(Error::CodingError)?,
                        )
                    }
                    "pred_epochs" => {
                        pred_epochs = Some(
                            types::decode(bytes).map_err(Error::CodingError)?,
                        )
                    }
                    "address_gen" => {
                        address_gen = Some(
                            types::decode(bytes).map_err(Error::CodingError)?,
                        );
                    }
                    _ => unknown_key_error(path)?,
                },
                None => unknown_key_error(path)?,
            }
        }
        match (hash, epoch, pred_epochs, address_gen) {
            (Some(hash), Some(epoch), Some(pred_epochs), Some(address_gen)) => {
                Ok(Some(BlockStateRead {
                    merkle_tree_stores,
                    hash,
                    height,
                    epoch,
                    pred_epochs,
                    next_epoch_min_start_height,
                    next_epoch_min_start_time,
                    address_gen,
                    results,
                    #[cfg(feature = "ferveo-tpke")]
                    tx_queue,
                }))
            }
            _ => Err(Error::Temporary {
                error: "Essential data couldn't be read from the DB"
                    .to_string(),
            }),
        }
    }

    fn write_block(&mut self, state: BlockStateWrite) -> Result<()> {
        let BlockStateWrite {
            merkle_tree_stores,
            header,
            hash,
            height,
            epoch,
            pred_epochs,
            next_epoch_min_start_height,
            next_epoch_min_start_time,
            address_gen,
            results,
            #[cfg(feature = "ferveo-tpke")]
            tx_queue,
        }: BlockStateWrite = state;

        // Epoch start height and time
        self.0.borrow_mut().insert(
            "next_epoch_min_start_height".into(),
            types::encode(&next_epoch_min_start_height),
        );
        self.0.borrow_mut().insert(
            "next_epoch_min_start_time".into(),
            types::encode(&next_epoch_min_start_time),
        );
        #[cfg(feature = "ferveo-tpke")]
        {
            self.0
                .borrow_mut()
                .insert("tx_queue".into(), types::encode(&tx_queue));
        }

        let prefix_key = Key::from(height.to_db_key());
        // Merkle tree
        {
            let prefix_key = prefix_key
                .push(&"tree".to_owned())
                .map_err(Error::KeyError)?;
            for st in StoreType::iter() {
                let prefix_key = prefix_key
                    .push(&st.to_string())
                    .map_err(Error::KeyError)?;
                let root_key = prefix_key
                    .push(&"root".to_owned())
                    .map_err(Error::KeyError)?;
                self.0.borrow_mut().insert(
                    root_key.to_string(),
                    types::encode(merkle_tree_stores.root(st)),
                );
                let store_key = prefix_key
                    .push(&"store".to_owned())
                    .map_err(Error::KeyError)?;
                self.0.borrow_mut().insert(
                    store_key.to_string(),
                    types::encode(merkle_tree_stores.store(st)),
                );
            }
        }
        // Block header
        {
            if let Some(h) = header {
                let key = prefix_key
                    .push(&"header".to_owned())
                    .map_err(Error::KeyError)?;
                self.0.borrow_mut().insert(
                    key.to_string(),
                    h.encode_vec().expect("serialization failed"),
                );
            }
        }
        // Block hash
        {
            let key = prefix_key
                .push(&"hash".to_owned())
                .map_err(Error::KeyError)?;
            self.0
                .borrow_mut()
                .insert(key.to_string(), types::encode(&hash));
        }
        // Block epoch
        {
            let key = prefix_key
                .push(&"epoch".to_owned())
                .map_err(Error::KeyError)?;
            self.0
                .borrow_mut()
                .insert(key.to_string(), types::encode(&epoch));
        }
        // Predecessor block epochs
        {
            let key = prefix_key
                .push(&"pred_epochs".to_owned())
                .map_err(Error::KeyError)?;
            self.0
                .borrow_mut()
                .insert(key.to_string(), types::encode(&pred_epochs));
        }
        // Address gen
        {
            let key = prefix_key
                .push(&"address_gen".to_owned())
                .map_err(Error::KeyError)?;
            let value = &address_gen;
            self.0
                .borrow_mut()
                .insert(key.to_string(), types::encode(value));
        }
        self.0
            .borrow_mut()
            .insert("height".to_owned(), types::encode(&height));
        // Block results
        {
            let results_path = format!("results/{}", height.raw());
            self.0
                .borrow_mut()
                .insert(results_path, types::encode(&results));
        }
        Ok(())
    }

    fn read_block_header(&self, height: BlockHeight) -> Result<Option<Header>> {
        let prefix_key = Key::from(height.to_db_key());
        let key = prefix_key
            .push(&"header".to_owned())
            .map_err(Error::KeyError)?;
        let value = self.0.borrow().get(&key.to_string()).cloned();
        match value {
            Some(v) => Ok(Some(
                Header::decode_vec(&v).map_err(Error::ProtobufCodingError)?,
            )),
            None => Ok(None),
        }
    }

    fn read_subspace_val(&self, key: &Key) -> Result<Option<Vec<u8>>> {
        let key = Key::parse("subspace").map_err(Error::KeyError)?.join(key);
        Ok(self.0.borrow().get(&key.to_string()).cloned())
    }

    fn write_subspace_val(
        &mut self,
        _height: BlockHeight,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<i64> {
        let value = value.as_ref();
        let key = Key::parse("subspace").map_err(Error::KeyError)?.join(key);
        let current_len = value.len() as i64;
        Ok(
            match self
                .0
                .borrow_mut()
                .insert(key.to_string(), value.to_owned())
            {
                Some(prev_value) => current_len - prev_value.len() as i64,
                None => current_len,
            },
        )
    }

    fn delete_subspace_val(
        &mut self,
        _height: BlockHeight,
        key: &Key,
    ) -> Result<i64> {
        let key = Key::parse("subspace").map_err(Error::KeyError)?.join(key);
        Ok(match self.0.borrow_mut().remove(&key.to_string()) {
            Some(value) => value.len() as i64,
            None => 0,
        })
    }

    fn batch() -> Self::WriteBatch {
        MockDBWriteBatch
    }

    fn exec_batch(&mut self, _batch: Self::WriteBatch) -> Result<()> {
        // Nothing to do - in MockDB, batch writes are committed directly from
        // `batch_write_subspace_val` and `batch_delete_subspace_val`.
        Ok(())
    }

    fn batch_write_subspace_val(
        &self,
        _batch: &mut Self::WriteBatch,
        _height: BlockHeight,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<i64> {
        let value = value.as_ref();
        let key = Key::parse("subspace").map_err(Error::KeyError)?.join(key);
        let current_len = value.len() as i64;
        Ok(
            match self
                .0
                .borrow_mut()
                .insert(key.to_string(), value.to_owned())
            {
                Some(prev_value) => current_len - prev_value.len() as i64,
                None => current_len,
            },
        )
    }

    fn batch_delete_subspace_val(
        &self,
        _batch: &mut Self::WriteBatch,
        _height: BlockHeight,
        key: &Key,
    ) -> Result<i64> {
        let key = Key::parse("subspace").map_err(Error::KeyError)?.join(key);
        Ok(match self.0.borrow_mut().remove(&key.to_string()) {
            Some(value) => value.len() as i64,
            None => 0,
        })
    }
}

impl<'iter> DBIter<'iter> for MockDB {
    type PrefixIter = MockPrefixIterator;

    fn iter_prefix(&'iter self, prefix: &Key) -> MockPrefixIterator {
        let db_prefix = "subspace/".to_owned();
        let prefix = format!("{}{}", db_prefix, prefix);
        let iter = self.0.borrow().clone().into_iter();
        MockPrefixIterator::new(MockIterator { prefix, iter }, db_prefix)
    }

    fn iter_results(&'iter self) -> MockPrefixIterator {
        let db_prefix = "results/".to_owned();
        let prefix = "results".to_owned();
        let iter = self.0.borrow().clone().into_iter();
        MockPrefixIterator::new(MockIterator { prefix, iter }, db_prefix)
    }
}

/// A prefix iterator base for the [`MockPrefixIterator`].
#[derive(Debug)]
pub struct MockIterator {
    prefix: String,
    /// The concrete iterator
    pub iter: btree_map::IntoIter<String, Vec<u8>>,
}

/// A prefix iterator for the [`MockDB`].
pub type MockPrefixIterator = PrefixIterator<MockIterator>;

impl Iterator for MockIterator {
    type Item = KVBytes;

    fn next(&mut self) -> Option<Self::Item> {
        for (key, val) in &mut self.iter {
            if key.starts_with(&self.prefix) {
                return Some((
                    Box::from(key.as_bytes()),
                    Box::from(val.as_slice()),
                ));
            }
        }
        None
    }
}

impl Iterator for PrefixIterator<MockIterator> {
    type Item = (String, Vec<u8>, u64);

    /// Returns the next pair and the gas cost
    fn next(&mut self) -> Option<(String, Vec<u8>, u64)> {
        match self.iter.next() {
            Some((key, val)) => {
                let key = String::from_utf8(key.to_vec())
                    .expect("Cannot convert from bytes to key string");
                match key.strip_prefix(&self.db_prefix) {
                    Some(k) => {
                        let gas = k.len() + val.len();
                        Some((k.to_owned(), val.to_vec(), gas as _))
                    }
                    None => self.next(),
                }
            }
            None => None,
        }
    }
}

impl DBWriteBatch for MockDBWriteBatch {
    fn put<K, V>(&mut self, _key: K, _value: V)
    where
        K: AsRef<[u8]>,
        V: AsRef<[u8]>,
    {
        // Nothing to do - in MockDB, batch writes are committed directly from
        // `batch_write_subspace_val` and `batch_delete_subspace_val`.
    }

    fn delete<K: AsRef<[u8]>>(&mut self, _key: K) {
        // Nothing to do - in MockDB, batch writes are committed directly from
        // `batch_write_subspace_val` and `batch_delete_subspace_val`.
    }
}

fn unknown_key_error(key: &str) -> Result<()> {
    Err(Error::UnknownKey {
        key: key.to_owned(),
    })
}
