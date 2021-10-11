//! Ledger's state storage with key-value backed store and a merkle tree

#[cfg(any(test, feature = "testing"))]
pub mod mockdb;
pub mod types;
pub mod write_log;

use core::fmt::Debug;
use std::collections::HashMap;
use std::fmt::Display;

use sparse_merkle_tree::default_store::DefaultStore;
use sparse_merkle_tree::{SparseMerkleTree, H256};
use tendermint::block::Header;
use thiserror::Error;
use types::MerkleTree;

use super::parameters::Parameters;
use crate::bytes::ByteBuf;
use crate::ledger::gas::MIN_STORAGE_GAS;
use crate::ledger::parameters::{self, EpochDuration};
use crate::types::address::{Address, EstablishedAddressGen};
use crate::types::chain::{ChainId, CHAIN_ID_LENGTH};
use crate::types::storage::{
    BlockHash, BlockHeight, DbKeySeg, Epoch, Epochs, Key, BLOCK_HASH_LENGTH,
};
use crate::types::time::DateTimeUtc;

/// A result of a function that may fail
pub type Result<T> = std::result::Result<T, Error>;

/// The storage data
#[derive(Debug)]
pub struct Storage<D, H>
where
    D: DB + for<'iter> DBIter<'iter>,
    H: StorageHasher,
{
    /// The database for the storage
    pub db: D,
    /// The ID of the chain
    pub chain_id: ChainId,
    /// The storage for the current (yet to be committed) block
    pub block: BlockStorage<H>,
    /// The latest block header
    pub header: Option<Header>,
    /// The height of the committed block
    pub last_height: BlockHeight,
    /// The epoch of the committed block
    pub last_epoch: Epoch,
    /// Minimum block height at which the next epoch may start
    pub next_epoch_min_start_height: BlockHeight,
    /// Minimum block time at which the next epoch may start
    pub next_epoch_min_start_time: DateTimeUtc,
    /// The current established address generator
    pub address_gen: EstablishedAddressGen,
}

/// The block storage data
#[derive(Debug)]
pub struct BlockStorage<H: StorageHasher> {
    /// Merkle tree of all the other data in block storage
    pub tree: MerkleTree<H>,
    /// Hash of the block
    pub hash: BlockHash,
    /// Height of the block (i.e. the level)
    pub height: BlockHeight,
    /// Epoch of the block
    pub epoch: Epoch,
    /// Predecessor block epochs
    pub pred_epochs: Epochs,
    /// Accounts' subspaces storage for arbitrary key-values
    pub subspaces: HashMap<Key, Vec<u8>>,
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("TEMPORARY error: {error}")]
    Temporary { error: String },
    #[error("Found an unknown key: {key}")]
    UnknownKey { key: String },
    #[error("Storage key error {0}")]
    KeyError(crate::types::storage::Error),
    #[error("Coding error: {0}")]
    CodingError(types::Error),
    #[error("Merkle tree error: {0}")]
    MerkleTreeError(sparse_merkle_tree::error::Error),
    #[error("Merkle tree error: {0}")]
    DBError(String),
}

/// The block's state as stored in the database.
pub struct BlockState {
    /// Merkle tree root
    pub root: H256,
    /// Merkle tree store
    pub store: DefaultStore<H256>,
    /// Hash of the block
    pub hash: BlockHash,
    /// Height of the block
    pub height: BlockHeight,
    /// Epoch of the block
    pub epoch: Epoch,
    /// Predecessor block epochs
    pub pred_epochs: Epochs,
    /// Minimum block height at which the next epoch may start
    pub next_epoch_min_start_height: BlockHeight,
    /// Minimum block time at which the next epoch may start
    pub next_epoch_min_start_time: DateTimeUtc,
    /// Accounts' subspaces storage for arbitrary key-values
    pub subspaces: HashMap<Key, Vec<u8>>,
    /// Established address generator
    pub address_gen: EstablishedAddressGen,
}

/// A database backend.
pub trait DB: std::fmt::Debug {
    /// Flush data on the memory to persistent them
    fn flush(&self) -> Result<()>;

    /// Write a block
    fn write_block(&mut self, state: BlockState) -> Result<()>;

    /// Read the value with the given height and the key from the DB
    fn read(&self, height: BlockHeight, key: &Key) -> Result<Option<Vec<u8>>>;

    /// Read the last committed block
    fn read_last_block(&mut self) -> Result<Option<BlockState>>;
}

/// A database prefix iterator.
pub trait DBIter<'iter> {
    /// The concrete type of the iterator
    type PrefixIter: Debug + Iterator<Item = (String, Vec<u8>, u64)>;

    /// Read key value pairs with the given prefix from the DB
    fn iter_prefix(
        &'iter self,
        height: BlockHeight,
        prefix: &Key,
    ) -> Self::PrefixIter;
}

/// The root hash of the merkle tree as bytes
pub struct MerkleRoot(pub Vec<u8>);

impl Display for MerkleRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ByteBuf(&self.0))
    }
}

impl<D, H> Storage<D, H>
where
    D: DB + for<'iter> DBIter<'iter>,
    H: StorageHasher,
{
    /// Load the full state at the last committed height, if any. Returns the
    /// Merkle root hash and the height of the committed block.
    pub fn load_last_state(&mut self) -> Result<()> {
        if let Some(BlockState {
            root,
            store,
            hash,
            height,
            epoch,
            pred_epochs,
            next_epoch_min_start_height,
            next_epoch_min_start_time,
            subspaces,
            address_gen,
        }) = self.db.read_last_block()?
        {
            self.block.tree = MerkleTree(SparseMerkleTree::new(root, store));
            self.block.hash = hash;
            self.block.height = height;
            self.block.epoch = epoch;
            self.block.pred_epochs = pred_epochs;
            self.block.subspaces = subspaces;
            self.last_height = height;
            self.last_epoch = epoch;
            self.next_epoch_min_start_height = next_epoch_min_start_height;
            self.next_epoch_min_start_time = next_epoch_min_start_time;
            self.address_gen = address_gen;
            tracing::debug!("Loaded storage from DB");
        } else {
            tracing::info!("No state could be found");
        }
        Ok(())
    }

    /// Returns the Merkle root hash and the height of the committed block. If
    /// no block exists, returns None.
    pub fn get_state(&self) -> Option<(MerkleRoot, u64)> {
        if self.block.height.0 != 0 {
            Some((
                MerkleRoot(self.block.tree.0.root().as_slice().to_vec()),
                self.block.height.0,
            ))
        } else {
            None
        }
    }

    /// Persist the current block's state to the database
    pub fn commit(&mut self) -> Result<()> {
        let state = BlockState {
            root: *self.block.tree.0.root(),
            store: self.block.tree.0.store().clone(),
            hash: self.block.hash.clone(),
            height: self.block.height,
            epoch: self.block.epoch,
            pred_epochs: self.block.pred_epochs.clone(),
            next_epoch_min_start_height: self.next_epoch_min_start_height,
            next_epoch_min_start_time: self.next_epoch_min_start_time,
            subspaces: self.block.subspaces.clone(),
            address_gen: self.address_gen.clone(),
        };
        self.db.write_block(state)?;
        self.last_height = self.block.height;
        self.header = None;
        Ok(())
    }

    /// Find the root hash of the merkle tree
    pub fn merkle_root(&self) -> MerkleRoot {
        MerkleRoot(self.block.tree.0.root().as_slice().to_vec())
    }

    /// Update the merkle tree with a storage key-value.
    // TODO Enforce or check invariant (it should catch newly added storage
    // fields too) that every function that changes storage, except for data
    // from Tendermint's block header should call this function to update the
    // Merkle tree.
    fn update_tree(&mut self, key: H256, value: H256) -> Result<()> {
        self.block
            .tree
            .0
            .update(key, value)
            .map_err(Error::MerkleTreeError)?;
        Ok(())
    }

    /// Check if the given key is present in storage. Returns the result and the
    /// gas cost.
    pub fn has_key(&self, key: &Key) -> Result<(bool, u64)> {
        let gas = key.len();
        Ok((
            !self
                .block
                .tree
                .0
                .get(&H::hash_key(key))
                .map_err(Error::MerkleTreeError)?
                .is_zero(),
            gas as _,
        ))
    }

    /// Returns a value from the specified subspace and the gas cost
    pub fn read(&self, key: &Key) -> Result<(Option<Vec<u8>>, u64)> {
        tracing::debug!("storage read key {}", key,);
        let (present, gas) = self.has_key(key)?;
        if !present {
            return Ok((None, gas));
        }

        if let Some(v) = self.block.subspaces.get(key) {
            let gas = key.len() + v.len();
            return Ok((Some(v.to_vec()), gas as _));
        }

        match self.db.read(self.last_height, key)? {
            Some(v) => {
                let gas = key.len() + v.len();
                Ok((Some(v), gas as _))
            }
            None => Ok((None, key.len() as _)),
        }
    }

    /// Returns a prefix iterator and the gas cost
    pub fn iter_prefix(
        &self,
        prefix: &Key,
    ) -> (<D as DBIter<'_>>::PrefixIter, u64) {
        (
            self.db.iter_prefix(self.last_height, prefix),
            prefix.len() as _,
        )
    }

    /// Write a value to the specified subspace and returns the gas cost and the
    /// size difference
    pub fn write(&mut self, key: &Key, value: Vec<u8>) -> Result<(u64, i64)> {
        tracing::debug!("storage write key {}", key,);
        self.update_tree(H::hash_key(key), H::hash_value(&value))?;

        let len = value.len();
        let gas = key.len() + len;
        let size_diff = match self.block.subspaces.insert(key.clone(), value) {
            Some(prev) => len as i64 - prev.len() as i64,
            None => len as i64,
        };
        Ok((gas as _, size_diff))
    }

    /// Delete the specified subspace and returns the gas cost and the size
    /// difference
    pub fn delete(&mut self, key: &Key) -> Result<(u64, i64)> {
        let mut size_diff = 0;
        if self.has_key(key)?.0 {
            // update the merkle tree with a zero as a tombstone
            self.update_tree(H::hash_key(key), H256::zero())?;

            size_diff -= match self.block.subspaces.remove(key) {
                Some(prev) => prev.len() as i64,
                None => 0,
            };
        }
        let gas = key.len() + (-size_diff as usize);
        Ok((gas as _, size_diff))
    }

    /// Set the block header.
    /// The header is not in the Merkle tree as it's tracked by Tendermint.
    /// Hence, we don't update the tree when this is set.
    pub fn set_header(&mut self, header: Header) -> Result<()> {
        self.header = Some(header);
        Ok(())
    }

    /// Block data is in the Merkle tree as it's tracked by Tendermint in the
    /// block header. Hence, we don't update the tree when this is set.
    pub fn begin_block(
        &mut self,
        hash: BlockHash,
        height: BlockHeight,
    ) -> Result<()> {
        self.block.hash = hash;
        self.block.height = height;
        Ok(())
    }

    /// Get a validity predicate for the given account address and the gas cost
    /// for reading it.
    pub fn validity_predicate(
        &self,
        addr: &Address,
    ) -> Result<(Option<Vec<u8>>, u64)> {
        let key = Key::validity_predicate(addr);
        self.read(&key)
    }

    #[allow(dead_code)]
    /// Check if the given address exists on chain and return the gas cost.
    pub fn exists(&self, addr: &Address) -> Result<(bool, u64)> {
        let key = Key::validity_predicate(addr);
        self.has_key(&key)
    }

    /// Get the chain ID as a raw string
    pub fn get_chain_id(&self) -> (String, u64) {
        (self.chain_id.to_string(), CHAIN_ID_LENGTH as _)
    }

    /// Get the current (yet to be committed) block height
    pub fn get_block_height(&self) -> (BlockHeight, u64) {
        (self.block.height, MIN_STORAGE_GAS)
    }

    /// Get the current (yet to be committed) block hash
    pub fn get_block_hash(&self) -> (BlockHash, u64) {
        (self.block.hash.clone(), BLOCK_HASH_LENGTH as _)
    }

    /// Get the current (yet to be committed) block epoch
    pub fn get_current_epoch(&self) -> (Epoch, u64) {
        (self.block.epoch, MIN_STORAGE_GAS)
    }

    /// Get the epoch of the last committed block
    pub fn get_last_epoch(&self) -> (Epoch, u64) {
        (self.last_epoch, MIN_STORAGE_GAS)
    }

    /// Initialize the first epoch. The first epoch begins at genesis time.
    pub fn init_genesis_epoch(
        &mut self,
        initial_height: BlockHeight,
        genesis_time: DateTimeUtc,
        parameters: &Parameters,
    ) -> Result<()> {
        let EpochDuration {
            min_num_of_blocks,
            min_duration,
        } = parameters.epoch_duration;
        self.next_epoch_min_start_height = initial_height + min_num_of_blocks;
        self.next_epoch_min_start_time = genesis_time + min_duration;
        self.update_epoch_in_merkle_tree()
    }

    /// Get the block header
    pub fn get_block_header(&self) -> (Option<Header>, u64) {
        (self.header.clone(), MIN_STORAGE_GAS)
    }

    /// Initialize a new epoch when the current epoch is finished. Returns
    /// `true` on a new epoch.
    pub fn update_epoch(
        &mut self,
        height: BlockHeight,
        time: DateTimeUtc,
    ) -> Result<bool> {
        let (parameters, _gas) =
            parameters::read(self).expect("Couldn't read protocol parameters");

        // Check if the current epoch is over
        let new_epoch = height >= self.next_epoch_min_start_height
            && time >= self.next_epoch_min_start_time;
        if new_epoch {
            // Begin a new epoch
            self.block.epoch = self.block.epoch.next();
            self.last_epoch = self.last_epoch.next();
            debug_assert_eq!(self.block.epoch, self.last_epoch);
            let EpochDuration {
                min_num_of_blocks,
                min_duration,
            } = parameters.epoch_duration;
            self.next_epoch_min_start_height = height + min_num_of_blocks;
            self.next_epoch_min_start_time = time + min_duration;
            // TODO put this into PoS parameters and pass it to tendermint
            // `consensus_params` on `InitChain` and `EndBlock`
            let evidence_max_age_num_blocks: u64 = 100000;
            self.block
                .pred_epochs
                .new_epoch(height, evidence_max_age_num_blocks);
            tracing::info!("Began a new epoch {}", self.block.epoch);
        }
        self.update_epoch_in_merkle_tree()?;
        Ok(new_epoch)
    }

    /// Update the merkle tree with epoch data
    fn update_epoch_in_merkle_tree(&mut self) -> Result<()> {
        self.update_tree(
            H::hash_key(&Key {
                segments: vec![DbKeySeg::StringSeg(
                    "epoch_start_height".into(),
                )],
            }),
            H::hash_value(&types::encode(&self.next_epoch_min_start_height)),
        )?;
        self.update_tree(
            H::hash_key(&Key {
                segments: vec![DbKeySeg::StringSeg(
                    "epoch_start_height".into(),
                )],
            }),
            H::hash_value(&types::encode(&self.next_epoch_min_start_time)),
        )?;
        self.update_tree(
            H::hash_key(&Key {
                segments: vec![DbKeySeg::StringSeg("current_epoch".into())],
            }),
            H::hash_value(&types::encode(&self.block.epoch)),
        )
    }
}

/// The storage hasher used for the merkle tree.
pub trait StorageHasher: sparse_merkle_tree::traits::Hasher + Default {
    /// Hash a storage key
    fn hash_key(key: &Key) -> H256;
    /// Hash a storage value
    fn hash_value(value: impl AsRef<[u8]>) -> H256;
}

/// Helpers for testing components that depend on storage
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use std::convert::TryInto;

    use sha2::{Digest, Sha256};
    use sparse_merkle_tree::H256;

    use super::mockdb::MockDB;
    use super::*;

    /// The storage hasher used for the merkle tree.
    pub struct Sha256Hasher(Sha256);

    impl Default for Sha256Hasher {
        fn default() -> Self {
            Self(Sha256::default())
        }
    }

    impl sparse_merkle_tree::traits::Hasher for Sha256Hasher {
        fn write_h256(&mut self, h: &H256) {
            self.0.update(h.as_slice());
        }

        fn finish(self) -> H256 {
            let hash = self.0.finalize();
            let bytes: [u8; 32] = hash.as_slice().try_into().expect(
                "Sha256 output conversion to fixed array shouldn't fail",
            );
            bytes.into()
        }
    }

    impl StorageHasher for Sha256Hasher {
        fn hash_key(key: &Key) -> H256 {
            let mut hasher = Sha256::new();
            hasher.update(&types::encode(key));
            let hash = hasher.finalize();
            let bytes: [u8; 32] = hash.as_slice().try_into().expect(
                "Sha256 output conversion to fixed array shouldn't fail",
            );
            bytes.into()
        }

        fn hash_value(value: impl AsRef<[u8]>) -> H256 {
            let mut hasher = Sha256::new();
            hasher.update(value.as_ref());
            let hash = hasher.finalize();
            let bytes: [u8; 32] = hash.as_slice().try_into().expect(
                "Sha256 output conversion to fixed array shouldn't fail",
            );
            bytes.into()
        }
    }

    /// Storage with a mock DB for testing
    pub type TestStorage = Storage<MockDB, Sha256Hasher>;

    impl Default for TestStorage {
        fn default() -> Self {
            let chain_id = ChainId::default();
            let tree = MerkleTree::default();
            let subspaces = HashMap::new();
            let block = BlockStorage {
                tree,
                hash: BlockHash::default(),
                height: BlockHeight::default(),
                epoch: Epoch::default(),
                pred_epochs: Epochs::default(),
                subspaces,
            };
            Self {
                db: MockDB::default(),
                chain_id,
                block,
                header: None,
                last_height: BlockHeight(0),
                last_epoch: Epoch::default(),
                next_epoch_min_start_height: BlockHeight::default(),
                next_epoch_min_start_time: DateTimeUtc::now(),
                address_gen: EstablishedAddressGen::new(
                    "Test address generator seed",
                ),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use chrono::{TimeZone, Utc};
    use proptest::prelude::*;

    use super::testing::*;
    use super::*;
    use crate::ledger::parameters::Parameters;
    use crate::types::time::{self, Duration};

    prop_compose! {
        /// Setup test input data with arbitrary epoch duration, epoch start
        /// height and time, and a block height and time that are greater than
        /// the epoch start height and time, and the change to be applied to
        /// the epoch duration parameters.
        fn arb_and_epoch_duration_start_and_block()
        (
            start_height in 0..1000_u64,
            start_time in 0..10000_i64,
            min_num_of_blocks in 1..10_u64,
            min_duration in 1..100_i64,
        )
        (
            min_num_of_blocks in Just(min_num_of_blocks),
            min_duration in Just(min_duration),
            start_height in Just(start_height),
            start_time in Just(start_time),
            block_height in start_height + 1..(start_height + 2 * min_num_of_blocks as u64),
            block_time in start_time + 1..(start_time + 2 * min_duration),
            // Delta will be applied on the `min_num_of_blocks` parameter
            min_blocks_delta in -(min_num_of_blocks as i64 - 1)..5,
            // Delta will be applied on the `min_duration` parameter
            min_duration_delta in -(min_duration as i64 - 1)..50,
        ) -> (EpochDuration, BlockHeight, DateTimeUtc, BlockHeight, DateTimeUtc,
                i64, i64) {
            let epoch_duration = EpochDuration {
                min_num_of_blocks,
                min_duration: Duration::seconds(min_duration).into(),
            };
            (epoch_duration,
                BlockHeight(start_height), Utc.timestamp(start_time, 0).into(),
                BlockHeight(block_height), Utc.timestamp(block_time, 0).into(),
                min_blocks_delta, min_duration_delta)
        }
    }

    proptest! {
        /// Test that:
        /// 1. When the minimum blocks have been created since the epoch
        ///    start height and minimum time passed since the epoch start time,
        ///    a new epoch must start.
        /// 2. When the epoch duration parameters change, the current epoch's
        ///    duration doesn't change, but the next one does.
        #[test]
        fn update_epoch_after_its_duration(
            (epoch_duration, start_height, start_time, block_height, block_time,
            min_blocks_delta, min_duration_delta)
            in arb_and_epoch_duration_start_and_block())
        {
            let mut storage = TestStorage {
                next_epoch_min_start_height:
                    start_height + epoch_duration.min_num_of_blocks,
                next_epoch_min_start_time:
                    start_time + epoch_duration.min_duration,
                ..Default::default()
            };
            let mut parameters = Parameters {
                epoch_duration: epoch_duration.clone(),
            };
            parameters::init_genesis_storage(&mut storage, &parameters);

            let epoch_before = storage.last_epoch;
            assert_eq!(epoch_before, storage.block.epoch);

            // Try to apply the epoch update
            storage.update_epoch(block_height, block_time).unwrap();

            // Test for 1.
            if block_height.0 - start_height.0
                >= epoch_duration.min_num_of_blocks as u64
                && time::duration_passed(
                    block_time,
                    start_time,
                    epoch_duration.min_duration,
                )
            {
                assert_eq!(storage.block.epoch, epoch_before.next());
                assert_eq!(storage.last_epoch, epoch_before.next());
                assert_eq!(storage.next_epoch_min_start_height,
                    block_height + epoch_duration.min_num_of_blocks);
                assert_eq!(storage.next_epoch_min_start_time,
                    block_time + epoch_duration.min_duration);
                assert_eq!(storage.block.pred_epochs.get_epoch(block_height), Some(epoch_before.next()));
            } else {
                assert_eq!(storage.block.epoch, epoch_before);
                assert_eq!(storage.last_epoch, epoch_before);
                assert_eq!(storage.block.pred_epochs.get_epoch(block_height), Some(epoch_before));
            }

            // Update the epoch duration parameters
            parameters.epoch_duration.min_num_of_blocks =
                (parameters.epoch_duration.min_num_of_blocks as i64 + min_blocks_delta) as u64;
            let min_duration: i64 = parameters.epoch_duration.min_duration.0 as _;
            parameters.epoch_duration.min_duration =
                Duration::seconds(min_duration + min_duration_delta).into();
            parameters::update(&mut storage, &parameters).unwrap();

            // Test for 2.
            let epoch_before = storage.last_epoch;
            let height_of_update = storage.next_epoch_min_start_height.0 ;
            let time_of_update = storage.next_epoch_min_start_time;
            let height_before_update = BlockHeight(height_of_update - 1);
            let height_of_update = BlockHeight(height_of_update);
            let time_before_update = time_of_update - Duration::seconds(1);

            // No update should happen before both epoch duration conditions are
            // satisfied
            storage.update_epoch(height_before_update, time_before_update).unwrap();
            assert_eq!(storage.block.epoch, epoch_before);
            assert_eq!(storage.last_epoch, epoch_before);
            storage.update_epoch(height_of_update, time_before_update).unwrap();
            assert_eq!(storage.block.epoch, epoch_before);
            assert_eq!(storage.last_epoch, epoch_before);
            storage.update_epoch(height_before_update, time_of_update).unwrap();
            assert_eq!(storage.block.epoch, epoch_before);
            assert_eq!(storage.last_epoch, epoch_before);

            // Update should happen at this or after this height and time
            storage.update_epoch(height_of_update, time_of_update).unwrap();
            assert_eq!(storage.block.epoch, epoch_before.next());
            assert_eq!(storage.last_epoch, epoch_before.next());
            // The next epoch's minimum duration should change
            assert_eq!(storage.next_epoch_min_start_height,
                height_of_update + parameters.epoch_duration.min_num_of_blocks);
            assert_eq!(storage.next_epoch_min_start_time,
                time_of_update + parameters.epoch_duration.min_duration);
        }
    }
}
