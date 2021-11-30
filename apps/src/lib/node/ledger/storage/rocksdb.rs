//! The persistent storage in RocksDB.
//!
//! The current storage tree is:
//! - `chain_id`
//! - `height`: the last committed block height
//! - `epoch_start_height`: block height at which the current epoch started
//! - `epoch_start_time`: block time at which the current epoch started
//! - `h`: for each block at height `h`:
//!   - `tree`: merkle tree
//!     - `root`: root hash
//!     - `store`: the tree's store
//!   - `hash`: block hash
//!   - `epoch`: block epoch
//!   - `subspace`: any byte data associated with accounts
//!   - `address_gen`: established address generator

use std::cmp::Ordering;
use std::collections::HashMap;
use std::env;
use std::path::Path;
use std::str::FromStr;

use anoma::ledger::storage::types::PrefixIterator;
use anoma::ledger::storage::{
    types, BlockStateRead, BlockStateWrite, DBIter, Error, Result, DB,
};
use anoma::types::storage::{BlockHeight, Key, KeySeg, KEY_SEGMENT_SEPARATOR};
use anoma::types::time::DateTimeUtc;
use rocksdb::{
    BlockBasedOptions, Direction, FlushOptions, IteratorMode, Options,
    ReadOptions, SliceTransform, WriteBatch, WriteOptions,
};

use crate::cli;

// TODO the DB schema will probably need some kind of versioning

/// Env. var to set a number of Rayon global worker threads
const ENV_VAR_ROCKSDB_COMPACTION_THREADS: &str =
    "ANOMA_ROCKSDB_COMPACTION_THREADS";

#[derive(Debug)]
pub struct RocksDB(rocksdb::DB);

/// Open RocksDB for the DB
pub fn open(path: impl AsRef<Path>) -> Result<RocksDB> {
    let logical_cores = num_cpus::get() as i32;
    let compaction_threads =
        if let Ok(num_str) = env::var(ENV_VAR_ROCKSDB_COMPACTION_THREADS) {
            match i32::from_str(&num_str) {
                Ok(num) if num > 0 => num,
                _ => {
                    eprintln!(
                        "Invalid env. var {} value: {}. Expecting a positive \
                         number.",
                        ENV_VAR_ROCKSDB_COMPACTION_THREADS, num_str
                    );
                    cli::safe_exit(1)
                }
            }
        } else {
            // If not set, default to quarter of logical CPUs count
            logical_cores / 4
        };
    tracing::debug!(
        "Using {} compactions threads for RocksDB.",
        compaction_threads
    );

    let mut cf_opts = Options::default();
    // ! recommended initial setup https://github.com/facebook/rocksdb/wiki/Setup-Options-and-Basic-Tuning#other-general-options
    cf_opts.set_level_compaction_dynamic_level_bytes(true);

    // This gives `compaction_threads` number to compaction threads and 1 thread
    // for flush background jobs: https://github.com/facebook/rocksdb/blob/17ce1ca48be53ba29138f92dafc9c853d9241377/options/options.cc#L622
    cf_opts.increase_parallelism(compaction_threads);

    cf_opts.set_bytes_per_sync(1048576);
    set_max_open_files(&mut cf_opts);

    cf_opts.set_compression_type(rocksdb::DBCompressionType::Zstd);
    cf_opts.set_compression_options(0, 0, 0, 1024 * 1024);
    // TODO the recommended default `options.compaction_pri =
    // kMinOverlappingRatio` doesn't seem to be available in Rust
    let mut table_opts = BlockBasedOptions::default();
    table_opts.set_block_size(16 * 1024);
    table_opts.set_cache_index_and_filter_blocks(true);
    table_opts.set_pin_l0_filter_and_index_blocks_in_cache(true);
    // latest format versions https://github.com/facebook/rocksdb/blob/d1c510baecc1aef758f91f786c4fbee3bc847a63/include/rocksdb/table.h#L394
    table_opts.set_format_version(5);
    cf_opts.set_block_based_table_factory(&table_opts);

    cf_opts.create_missing_column_families(true);
    cf_opts.create_if_missing(true);

    cf_opts.set_comparator("key_comparator", key_comparator);
    let extractor = SliceTransform::create_fixed_prefix(20);
    cf_opts.set_prefix_extractor(extractor);
    // TODO use column families
    rocksdb::DB::open_cf_descriptors(&cf_opts, path, vec![])
        .map(RocksDB)
        .map_err(|e| Error::DBError(e.into_string()))
}

/// A custom key comparator is used to sort keys by the height. In
/// lexicographical order, the height aren't ordered. For example, "11" is
/// before "2".
fn key_comparator(a: &[u8], b: &[u8]) -> Ordering {
    let a_str = &String::from_utf8(a.to_vec()).unwrap();
    let b_str = &String::from_utf8(b.to_vec()).unwrap();

    let a_vec: Vec<&str> = a_str.split('/').collect();
    let b_vec: Vec<&str> = b_str.split('/').collect();

    let result_a_h = a_vec[0].parse::<u64>();
    let result_b_h = b_vec[0].parse::<u64>();
    match (result_a_h, result_b_h) {
        (Ok(a_h), Ok(b_h)) => {
            if a_h == b_h {
                a_vec[1..].cmp(&b_vec[1..])
            } else {
                a_h.cmp(&b_h)
            }
        }
        _ => {
            // the key doesn't include the height
            a_str.cmp(b_str)
        }
    }
}

impl Drop for RocksDB {
    fn drop(&mut self) {
        self.flush().expect("flush failed");
    }
}

impl DB for RocksDB {
    fn open(db_path: impl AsRef<Path>) -> Self {
        open(db_path).expect("cannot open the DB")
    }

    fn flush(&self) -> Result<()> {
        let mut flush_opts = FlushOptions::default();
        flush_opts.set_wait(true);
        self.0
            .flush_opt(&flush_opts)
            .map_err(|e| Error::DBError(e.into_string()))
    }

    fn write_block(&mut self, state: BlockStateWrite) -> Result<()> {
        let mut batch = WriteBatch::default();
        let BlockStateWrite {
            root,
            store,
            hash,
            height,
            epoch,
            pred_epochs,
            next_epoch_min_start_height,
            next_epoch_min_start_time,
            address_gen,
        }: BlockStateWrite = state;

        // Epoch start height and time
        batch.put(
            "next_epoch_min_start_height",
            types::encode(&next_epoch_min_start_height),
        );
        batch.put(
            "next_epoch_min_start_time",
            types::encode(&next_epoch_min_start_time),
        );

        let prefix_key = Key::from(height.to_db_key());
        // Merkle tree
        {
            let prefix_key = prefix_key
                .push(&"tree".to_owned())
                .map_err(Error::KeyError)?;
            // Merkle root hash
            {
                let key = prefix_key
                    .push(&"root".to_owned())
                    .map_err(Error::KeyError)?;
                batch.put(key.to_string(), &root.as_slice());
            }
            // Tree's store
            {
                let key = prefix_key
                    .push(&"store".to_owned())
                    .map_err(Error::KeyError)?;
                batch.put(key.to_string(), types::encode(&store));
            }
        }
        // Block hash
        {
            let key = prefix_key
                .push(&"hash".to_owned())
                .map_err(Error::KeyError)?;
            batch.put(key.to_string(), types::encode(&hash));
        }
        // Block epoch
        {
            let key = prefix_key
                .push(&"epoch".to_owned())
                .map_err(Error::KeyError)?;
            batch.put(key.to_string(), types::encode(&epoch));
        }
        // Predecessor block epochs
        {
            let key = prefix_key
                .push(&"pred_epochs".to_owned())
                .map_err(Error::KeyError)?;
            batch.put(key.to_string(), types::encode(&pred_epochs));
        }
        // Address gen
        {
            let key = prefix_key
                .push(&"address_gen".to_owned())
                .map_err(Error::KeyError)?;
            batch.put(key.to_string(), types::encode(&address_gen));
        }
        let mut write_opts = WriteOptions::default();
        write_opts.disable_wal(true);
        self.0
            .write_opt(batch, &write_opts)
            .map_err(|e| Error::DBError(e.into_string()))?;

        // Block height - write after everything else is written
        // NOTE for async writes, we need to take care that all previous heights
        // are known when updating this
        self.0
            .put_opt("height", types::encode(&height), &write_opts)
            .map_err(|e| Error::DBError(e.into_string()))
    }

    fn read(&self, height: BlockHeight, key: &Key) -> Result<Option<Vec<u8>>> {
        let key = Key::from(height.to_db_key())
            .push(&"subspace".to_owned())
            .map_err(Error::KeyError)?
            .join(key);
        self.0
            .get(key.to_string())
            .map_err(|e| Error::DBError(e.into_string()))
    }

    fn write(
        &mut self,
        height: BlockHeight,
        key: &Key,
        value: Vec<u8>,
    ) -> Result<i64> {
        let key = Key::from(height.to_db_key())
            .push(&"subspace".to_owned())
            .map_err(Error::KeyError)?
            .join(key);
        let prev_len = self
            .0
            .get(key.to_string())
            .map_err(|e| Error::DBError(e.into_string()))?
            .map(|bytes| bytes.len())
            .unwrap_or_default();
        let size_diff = value.len() as i64 - prev_len as i64;
        self.0
            .put(key.to_string(), value)
            .map_err(|e| Error::DBError(e.into_string()))?;
        Ok(size_diff)
    }

    fn delete(&mut self, height: BlockHeight, key: &Key) -> Result<i64> {
        let key = Key::from(height.to_db_key())
            .push(&"subspace".to_owned())
            .map_err(Error::KeyError)?
            .join(key);
        let prev_len = self
            .0
            .get(key.to_string())
            .map_err(|e| Error::DBError(e.into_string()))?
            .map(|bytes| bytes.len() as i64)
            .unwrap_or_default();
        self.0
            .delete(key.to_string())
            .map_err(|e| Error::DBError(e.into_string()))?;
        Ok(prev_len)
    }

    fn read_last_block(&mut self) -> Result<Option<BlockStateRead>> {
        // Block height
        let height: BlockHeight;
        match self
            .0
            .get("height")
            .map_err(|e| Error::DBError(e.into_string()))?
        {
            Some(bytes) => {
                // TODO if there's an issue decoding this height, should we try
                // load its predecessor instead?
                height = types::decode(bytes).map_err(Error::CodingError)?;
            }
            None => return Ok(None),
        }

        // Epoch start height and time
        let next_epoch_min_start_height: BlockHeight = match self
            .0
            .get("next_epoch_min_start_height")
            .map_err(|e| Error::DBError(e.into_string()))?
        {
            Some(bytes) => types::decode(bytes).map_err(Error::CodingError)?,
            None => {
                tracing::error!(
                    "Couldn't load next epoch start height from the DB"
                );
                return Ok(None);
            }
        };
        let next_epoch_min_start_time: DateTimeUtc = match self
            .0
            .get("next_epoch_min_start_time")
            .map_err(|e| Error::DBError(e.into_string()))?
        {
            Some(bytes) => types::decode(bytes).map_err(Error::CodingError)?,
            None => {
                tracing::error!(
                    "Couldn't load next epoch start time from the DB"
                );
                return Ok(None);
            }
        };

        // Load data at the height
        let prefix = format!("{}/", height.raw());
        let mut read_opts = ReadOptions::default();
        read_opts.set_total_order_seek(false);
        let next_height_prefix = format!("{}/", height.next_height().raw());
        read_opts.set_iterate_upper_bound(next_height_prefix);
        let mut root = None;
        let mut store = None;
        let mut hash = None;
        let mut epoch = None;
        let mut pred_epochs = None;
        let mut address_gen = None;
        for (key, bytes) in self.0.iterator_opt(
            IteratorMode::From(prefix.as_bytes(), Direction::Forward),
            read_opts,
        ) {
            let path = &String::from_utf8((*key).to_vec()).map_err(|e| {
                Error::Temporary {
                    error: format!(
                        "Cannot convert path from utf8 bytes to string: {}",
                        e
                    ),
                }
            })?;
            let segments: Vec<&str> =
                path.split(KEY_SEGMENT_SEPARATOR).collect();
            match segments.get(1) {
                Some(prefix) => match *prefix {
                    "tree" => match segments.get(2) {
                        Some(smt) => match *smt {
                            "root" => {
                                root = Some(
                                    types::decode(bytes)
                                        .map_err(Error::CodingError)?,
                                )
                            }
                            "store" => {
                                store = Some(
                                    types::decode(bytes)
                                        .map_err(Error::CodingError)?,
                                )
                            }
                            _ => unknown_key_error(path)?,
                        },
                        None => unknown_key_error(path)?,
                    },
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
        match (root, store, hash, epoch, pred_epochs, address_gen) {
            (
                Some(root),
                Some(store),
                Some(hash),
                Some(epoch),
                Some(pred_epochs),
                Some(address_gen),
            ) => Ok(Some(BlockStateRead {
                root,
                store,
                hash,
                height,
                epoch,
                pred_epochs,
                next_epoch_min_start_height,
                next_epoch_min_start_time,
                address_gen,
            })),
            _ => Err(Error::Temporary {
                error: "Essential data couldn't be read from the DB"
                    .to_string(),
            }),
        }
    }
}

impl<'iter> DBIter<'iter> for RocksDB {
    type PrefixIter = PersistentPrefixIterator<'iter>;

    fn iter_prefix(
        &'iter self,
        height: BlockHeight,
        prefix: &Key,
    ) -> PersistentPrefixIterator<'iter> {
        let db_prefix = format!("{}/subspace/", height.raw());
        let prefix = format!("{}{}", db_prefix, prefix);

        let mut read_opts = ReadOptions::default();
        // don't use the prefix bloom filter
        read_opts.set_total_order_seek(true);
        let mut upper_prefix = prefix.clone().into_bytes();
        if let Some(last) = upper_prefix.pop() {
            upper_prefix.push(last + 1);
        }
        read_opts.set_iterate_upper_bound(upper_prefix);

        let iter = self.0.iterator_opt(
            IteratorMode::From(prefix.as_bytes(), Direction::Forward),
            read_opts,
        );
        PersistentPrefixIterator(PrefixIterator::new(iter, db_prefix))
    }
}

#[derive(Debug)]
pub struct PersistentPrefixIterator<'a>(
    PrefixIterator<rocksdb::DBIterator<'a>>,
);

impl<'a> Iterator for PersistentPrefixIterator<'a> {
    type Item = (String, Vec<u8>, u64);

    /// Returns the next pair and the gas cost
    fn next(&mut self) -> Option<(String, Vec<u8>, u64)> {
        match self.0.iter.next() {
            Some((key, val)) => {
                let key = String::from_utf8(key.to_vec())
                    .expect("Cannot convert from bytes to key string");
                match key.strip_prefix(&self.0.db_prefix) {
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

fn unknown_key_error(key: &str) -> Result<()> {
    Err(Error::UnknownKey {
        key: key.to_owned(),
    })
}

/// Try to increase NOFILE limit and set the `max_open_files` limit to it in
/// RocksDB options.
fn set_max_open_files(cf_opts: &mut rocksdb::Options) {
    #[cfg(unix)]
    imp::set_max_open_files(cf_opts);
    // Nothing to do on non-unix
    #[cfg(not(unix))]
    let _ = cf_opts;
}

#[cfg(unix)]
mod imp {
    use std::convert::TryInto;

    use rlimit::{Resource, Rlim};

    const DEFAULT_NOFILE_LIMIT: Rlim = Rlim::from_raw(16384);

    pub fn set_max_open_files(cf_opts: &mut rocksdb::Options) {
        let max_open_files = match increase_nofile_limit() {
            Ok(max_open_files) => Some(max_open_files),
            Err(err) => {
                tracing::error!("Failed to increase NOFILE limit: {}", err);
                None
            }
        };
        if let Some(max_open_files) =
            max_open_files.and_then(|max| max.as_raw().try_into().ok())
        {
            cf_opts.set_max_open_files(max_open_files);
        }
    }

    /// Try to increase NOFILE limit and return the current soft limit.
    fn increase_nofile_limit() -> std::io::Result<Rlim> {
        let (soft, hard) = Resource::NOFILE.get()?;
        tracing::debug!("Current NOFILE limit, soft={}, hard={}", soft, hard);

        let target = std::cmp::min(DEFAULT_NOFILE_LIMIT, hard);
        if soft >= target {
            tracing::debug!(
                "NOFILE limit already large enough, not attempting to increase"
            );
            Ok(soft)
        } else {
            tracing::debug!("Try to increase to {}", target);
            Resource::NOFILE.set(target, target)?;

            let (soft, hard) = Resource::NOFILE.get()?;
            tracing::debug!(
                "Increased NOFILE limit, soft={}, hard={}",
                soft,
                hard
            );
            Ok(soft)
        }
    }
}
