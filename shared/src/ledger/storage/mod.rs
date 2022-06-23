//! Ledger's state storage with key-value backed store and a merkle tree

mod merkle_tree;
#[cfg(any(test, feature = "testing"))]
pub mod mockdb;
pub mod types;
pub mod write_log;

use core::fmt::Debug;
use std::collections::HashMap;

use borsh::{BorshDeserialize, BorshSerialize};
use ff::PrimeField;
use masp_primitives::asset_type::AssetType;
use masp_primitives::convert::AllowedConversion;
use masp_primitives::merkle_tree::FrozenCommitmentTree;
use masp_primitives::sapling::Node;
use masp_primitives::transaction::components::Amount;
use rayon::iter::{
    IndexedParallelIterator, IntoParallelIterator, ParallelIterator,
};
use rayon::prelude::ParallelSlice;
#[cfg(not(feature = "ABCI"))]
use tendermint::block::Header;
#[cfg(not(feature = "ABCI"))]
use tendermint::merkle::proof::Proof;
#[cfg(feature = "ABCI")]
use tendermint_stable::block::Header;
#[cfg(feature = "ABCI")]
use tendermint_stable::merkle::proof::Proof;
use thiserror::Error;

use super::parameters::Parameters;
use crate::ledger::gas::MIN_STORAGE_GAS;
use crate::ledger::parameters::{self, EpochDuration};
use crate::ledger::storage::merkle_tree::{
    Error as MerkleTreeError, MerkleRoot,
};
pub use crate::ledger::storage::merkle_tree::{
    MerkleTree, MerkleTreeStoresRead, MerkleTreeStoresWrite, Sha256Hasher,
    StorageHasher, StoreType,
};
use crate::types::address::{
    Address, EstablishedAddressGen, InternalAddress, *,
};
use crate::types::chain::{ChainId, CHAIN_ID_LENGTH};
#[cfg(feature = "ferveo-tpke")]
use crate::types::storage::TxQueue;
use crate::types::storage::{
    BlockHash, BlockHeight, Epoch, Epochs, Key, KeySeg, BLOCK_HASH_LENGTH,
};
use crate::types::time::DateTimeUtc;
use crate::types::token;

/// A result of a function that may fail
pub type Result<T> = std::result::Result<T, Error>;
/// A representation of the conversion state
#[derive(Debug, Default, BorshSerialize, BorshDeserialize)]
pub struct ConversionState {
    /// The tree currently containing all the conversions
    pub tree: FrozenCommitmentTree<Node>,
    /// Map assets to their latest conversion and position in Merkle tree
    pub assets: HashMap<AssetType, (Address, Epoch, AllowedConversion, usize)>,
}

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
    /// The currently saved conversion state
    pub conversion_state: ConversionState,
    /// Wrapper txs to be decrypted in the next block proposal
    #[cfg(feature = "ferveo-tpke")]
    pub tx_queue: TxQueue,
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
    MerkleTreeError(MerkleTreeError),
    #[error("Merkle tree error: {0}")]
    DBError(String),
}

/// The block's state as stored in the database.
pub struct BlockStateRead {
    /// Merkle tree stores
    pub merkle_tree_stores: MerkleTreeStoresRead,
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
    /// Established address generator
    pub address_gen: EstablishedAddressGen,
    /// Wrapper txs to be decrypted in the next block proposal
    #[cfg(feature = "ferveo-tpke")]
    pub tx_queue: TxQueue,
}

/// The block's state to write into the database.
pub struct BlockStateWrite<'a> {
    /// Merkle tree stores
    pub merkle_tree_stores: MerkleTreeStoresWrite<'a>,
    /// Hash of the block
    pub hash: &'a BlockHash,
    /// Height of the block
    pub height: BlockHeight,
    /// Epoch of the block
    pub epoch: Epoch,
    /// Predecessor block epochs
    pub pred_epochs: &'a Epochs,
    /// Minimum block height at which the next epoch may start
    pub next_epoch_min_start_height: BlockHeight,
    /// Minimum block time at which the next epoch may start
    pub next_epoch_min_start_time: DateTimeUtc,
    /// Established address generator
    pub address_gen: &'a EstablishedAddressGen,
    /// Wrapper txs to be decrypted in the next block proposal
    #[cfg(feature = "ferveo-tpke")]
    pub tx_queue: &'a TxQueue,
}

/// A database backend.
pub trait DB: std::fmt::Debug {
    /// A DB's cache
    type Cache;
    /// A handle for batch writes
    type WriteBatch: DBWriteBatch;

    /// Open the database from provided path
    fn open(
        db_path: impl AsRef<std::path::Path>,
        cache: Option<&Self::Cache>,
    ) -> Self;

    /// Flush data on the memory to persistent them
    fn flush(&self, wait: bool) -> Result<()>;

    /// Read the last committed block's metadata
    fn read_last_block(&mut self) -> Result<Option<BlockStateRead>>;

    /// Write block's metadata
    fn write_block(&mut self, state: BlockStateWrite) -> Result<()>;

    /// Read the latest value for account subspace key from the DB
    fn read_subspace_val(&self, key: &Key) -> Result<Option<Vec<u8>>>;

    /// Write the value with the given height and account subspace key to the
    /// DB. Returns the size difference from previous value, if any, or the
    /// size of the value otherwise.
    fn write_subspace_val(
        &mut self,
        height: BlockHeight,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<i64>;

    /// Delete the value with the given height and account subspace key from the
    /// DB. Returns the size of the removed value, if any, 0 if no previous
    /// value was found.
    fn delete_subspace_val(
        &mut self,
        height: BlockHeight,
        key: &Key,
    ) -> Result<i64>;

    /// Start write batch.
    fn batch() -> Self::WriteBatch;

    /// Execute write batch.
    fn exec_batch(&mut self, batch: Self::WriteBatch) -> Result<()>;

    /// Batch write the value with the given height and account subspace key to
    /// the DB. Returns the size difference from previous value, if any, or
    /// the size of the value otherwise.
    fn batch_write_subspace_val(
        &self,
        batch: &mut Self::WriteBatch,
        height: BlockHeight,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<i64>;

    /// Batch delete the value with the given height and account subspace key
    /// from the DB. Returns the size of the removed value, if any, 0 if no
    /// previous value was found.
    fn batch_delete_subspace_val(
        &self,
        batch: &mut Self::WriteBatch,
        height: BlockHeight,
        key: &Key,
    ) -> Result<i64>;
}

/// A database prefix iterator.
pub trait DBIter<'iter> {
    /// The concrete type of the iterator
    type PrefixIter: Debug + Iterator<Item = (String, Vec<u8>, u64)>;

    /// Read account subspace key value pairs with the given prefix from the DB
    fn iter_prefix(&'iter self, prefix: &Key) -> Self::PrefixIter;
}

/// Atomic batch write.
pub trait DBWriteBatch {
    /// Insert a value into the database under the given key.
    fn put<K, V>(&mut self, key: K, value: V)
    where
        K: AsRef<[u8]>,
        V: AsRef<[u8]>;

    /// Removes the database entry for key. Does nothing if the key was not
    /// found.
    fn delete<K: AsRef<[u8]>>(&mut self, key: K);
}

impl<D, H> Storage<D, H>
where
    D: DB + for<'iter> DBIter<'iter>,
    H: StorageHasher,
{
    /// open up a new instance of the storage given path to db and chain id
    pub fn open(
        db_path: impl AsRef<std::path::Path>,
        chain_id: ChainId,
        cache: Option<&D::Cache>,
    ) -> Self {
        let block = BlockStorage {
            tree: MerkleTree::default(),
            hash: BlockHash::default(),
            height: BlockHeight::default(),
            epoch: Epoch::default(),
            pred_epochs: Epochs::default(),
        };
        Storage::<D, H> {
            db: D::open(db_path, cache),
            chain_id,
            block,
            header: None,
            last_height: BlockHeight(0),
            last_epoch: Epoch::default(),
            next_epoch_min_start_height: BlockHeight::default(),
            next_epoch_min_start_time: DateTimeUtc::now(),
            address_gen: EstablishedAddressGen::new(
                "Privacy is a function of liberty.",
            ),
            conversion_state: ConversionState::default(),
            #[cfg(feature = "ferveo-tpke")]
            tx_queue: TxQueue::default(),
        }
    }

    /// Load the full state at the last committed height, if any. Returns the
    /// Merkle root hash and the height of the committed block.
    pub fn load_last_state(&mut self) -> Result<()> {
        if let Some(BlockStateRead {
            merkle_tree_stores,
            hash,
            height,
            epoch,
            pred_epochs,
            next_epoch_min_start_height,
            next_epoch_min_start_time,
            address_gen,
            #[cfg(feature = "ferveo-tpke")]
            tx_queue,
        }) = self.db.read_last_block()?
        {
            self.block.tree = MerkleTree::new(merkle_tree_stores);
            self.block.hash = hash;
            self.block.height = height;
            self.block.epoch = epoch;
            self.block.pred_epochs = pred_epochs;
            self.last_height = height;
            self.last_epoch = epoch;
            self.next_epoch_min_start_height = next_epoch_min_start_height;
            self.next_epoch_min_start_time = next_epoch_min_start_time;
            self.address_gen = address_gen;
            if self.last_epoch.0 > 1 {
                // The derived conversions will be placed in MASP address space
                let masp_addr = masp();
                let key_prefix: Key = masp_addr.to_db_key().into();
                // Load up the conversions currently being given as query
                // results
                let state_key = key_prefix
                    .push(&(token::CONVERSION_KEY_PREFIX.to_owned()))
                    .map_err(Error::KeyError)?;
                self.conversion_state = types::decode(
                    self.read(&state_key)
                        .expect("unable to read conversion state")
                        .0
                        .expect("unable to find conversion state"),
                )
                .expect("unable to decode conversion state")
            }
            #[cfg(feature = "ferveo-tpke")]
            {
                self.tx_queue = tx_queue;
            }
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
            Some((self.block.tree.root(), self.block.height.0))
        } else {
            None
        }
    }

    /// Persist the current block's state to the database
    pub fn commit(&mut self) -> Result<()> {
        let state = BlockStateWrite {
            merkle_tree_stores: self.block.tree.stores(),
            hash: &self.block.hash,
            height: self.block.height,
            epoch: self.block.epoch,
            pred_epochs: &self.block.pred_epochs,
            next_epoch_min_start_height: self.next_epoch_min_start_height,
            next_epoch_min_start_time: self.next_epoch_min_start_time,
            address_gen: &self.address_gen,
            #[cfg(feature = "ferveo-tpke")]
            tx_queue: &self.tx_queue,
        };
        self.db.write_block(state)?;
        self.last_height = self.block.height;
        self.header = None;
        Ok(())
    }

    /// Find the root hash of the merkle tree
    pub fn merkle_root(&self) -> MerkleRoot {
        self.block.tree.root()
    }

    /// Check if the given key is present in storage. Returns the result and the
    /// gas cost.
    pub fn has_key(&self, key: &Key) -> Result<(bool, u64)> {
        Ok((self.block.tree.has_key(key)?, key.len() as _))
    }

    /// Returns a value from the specified subspace and the gas cost
    pub fn read(&self, key: &Key) -> Result<(Option<Vec<u8>>, u64)> {
        tracing::debug!("storage read key {}", key);
        let (present, gas) = self.has_key(key)?;
        if !present {
            return Ok((None, gas));
        }

        match self.db.read_subspace_val(key)? {
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
        (self.db.iter_prefix(prefix), prefix.len() as _)
    }

    /// Write a value to the specified subspace and returns the gas cost and the
    /// size difference
    pub fn write(
        &mut self,
        key: &Key,
        value: impl AsRef<[u8]> + Clone,
    ) -> Result<(u64, i64)> {
        tracing::debug!("storage write key {}", key,);
        self.block.tree.update(key, value.clone())?;

        let len = value.as_ref().len();
        let gas = key.len() + len;
        let size_diff =
            self.db.write_subspace_val(self.last_height, key, value)?;
        Ok((gas as _, size_diff))
    }

    /// Delete the specified subspace and returns the gas cost and the size
    /// difference
    pub fn delete(&mut self, key: &Key) -> Result<(u64, i64)> {
        let mut deleted_bytes_len = 0;
        if self.has_key(key)?.0 {
            self.block.tree.delete(key)?;
            deleted_bytes_len =
                self.db.delete_subspace_val(self.last_height, key)?;
        }
        let gas = key.len() + deleted_bytes_len as usize;
        Ok((gas as _, deleted_bytes_len))
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

    /// Get the existence proof
    pub fn get_existence_proof(
        &self,
        key: &Key,
        value: Vec<u8>,
    ) -> Result<Proof> {
        Ok(self.block.tree.get_existence_proof(key, value)?)
    }

    /// Get the non-existence proof
    pub fn get_non_existence_proof(&self, key: &Key) -> Result<Proof> {
        Ok(self.block.tree.get_non_existence_proof(key)?)
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
            self.update_allowed_conversions()?;
        }
        self.update_epoch_in_merkle_tree()?;
        Ok(new_epoch)
    }

    /// Get the current conversions
    pub fn get_conversion_state(&self) -> &ConversionState {
        &self.conversion_state
    }

    /// Update the MASP's allowed conversions
    fn update_allowed_conversions(&mut self) -> Result<()> {
        // The derived conversions will be placed in MASP address space
        let masp_addr = masp();
        let key_prefix: Key = masp_addr.to_db_key().into();

        let masp_rewards = masp_rewards();
        // The total transparent value of the rewards being distributed
        let mut total_reward = token::Amount::from(0);

        // Construct MASP asset type for rewards. Always timestamp reward tokens
        // with the zeroth epoch to minimize the number of convert notes clients
        // have to use. This trick works under the assumption that reward tokens
        // from different epochs are exactly equivalent.
        let reward_asset_bytes = (xan(), 0u64)
            .try_to_vec()
            .expect("unable to serialize address and epoch");
        let reward_asset = AssetType::new(reward_asset_bytes.as_ref())
            .expect("unable to derive asset identifier");
        // Conversions from the previous to current asset for each address
        let mut current_convs = HashMap::<Address, AllowedConversion>::new();
        // Reward all tokens according to above reward rates
        for (addr, reward) in &masp_rewards {
            // Dispence a transparent reward in parallel to the shielded rewards
            let token_key = self.read(&token::balance_key(addr, &masp_addr));
            if let Ok((Some(addr_balance), _)) = token_key {
                // The reward for each reward.1 units of the current asset is
                // reward.0 units of the reward token
                let addr_bal: token::Amount =
                    types::decode(addr_balance).expect("invalid balance");
                // Since floor(a) + floor(b) <= floor(a+b), there will always be
                // enough rewards to reimburse users
                total_reward += token::Amount::from(
                    (u64::from(addr_bal) / reward.1) * reward.0
                );
            }
            // Construct MASP asset type with latest timestamp for this token
            let new_asset_bytes = (addr.clone(), self.last_epoch.0)
                .try_to_vec()
                .expect("unable to serialize address and epoch");
            let new_asset = AssetType::new(new_asset_bytes.as_ref())
                .expect("unable to derive asset identifier");

            // Provide an allowed conversion from previous timestamp
            // Construct MASP asset type with old timestamp
            let old_asset_bytes = (addr.clone(), self.last_epoch.0 - 1)
                .try_to_vec()
                .expect("unable to serialize address and epoch");
            let old_asset = AssetType::new(old_asset_bytes.as_ref())
                .expect("unable to derive asset identifier");
            // The -1 allows each instance of the old asset to be cancelled
            // out/replaced with the new asset
            current_convs.insert(
                addr.clone(),
                (Amount::from_pair(old_asset, -(reward.1 as i64)).unwrap()
                    + Amount::from_pair(new_asset, reward.1).unwrap()
                    + Amount::from_pair(reward_asset, reward.0).unwrap())
                .into(),
            );
            // Add a conversion from the previous asset type
            self.conversion_state.assets.insert(
                old_asset,
                (
                    addr.clone(),
                    self.last_epoch.prev(),
                    Amount::zero().into(),
                    0,
                ),
            );
        }

        // Try to distribute Merkle leaf updating as evenly as possible across
        // multiple cores
        let num_threads = rayon::current_num_threads();
        // Put assets into vector to enable computation batching
        let assets: Vec<_> = self
            .conversion_state
            .assets
            .values_mut()
            .enumerate()
            .collect();
        // ceil(assets.len() / num_threads)
        let notes_per_thread_max = (assets.len() - 1) / num_threads + 1;
        // floor(assets.len() / num_threads)
        let notes_per_thread_min = assets.len() / num_threads;
        // Now on each core, add the latest conversion to each conversion
        let conv_notes: Vec<Node> = assets
            .into_par_iter()
            .with_min_len(notes_per_thread_min)
            .with_max_len(notes_per_thread_max)
            .map(|(idx, (addr, _epoch, conv, pos))| {
                // Use transitivity to update conversion
                *conv += current_convs[addr].clone();
                // Update conversion position to leaf we are about to create
                *pos = idx;
                // The merkle tree need only provide the conversion commitment,
                // the remaining information is provided through the storage API
                Node::new(conv.cmu().to_repr())
            })
            .collect();

        // Update the MASP's transparent reward token balance to ensure that it
        // is sufficiently backed to redeem rewards
        let reward_key = token::balance_key(&xan(), &masp_addr);
        if let Ok((Some(addr_bal), _)) = self.read(&reward_key) {
            // If there is already a balance, then add to it
            let addr_bal: token::Amount =
                types::decode(addr_bal).expect("invalid balance");
            let new_bal = types::encode(&(addr_bal + total_reward));
            self.write(&reward_key, new_bal)
                .expect("unable to update MASP transparent balance");
        } else {
            // Otherwise the rewards form the entirity of the reward token
            // balance
            self.write(&reward_key, types::encode(&total_reward))
                .expect("unable to update MASP transparent balance");
        }
        // Try to distribute Merkle tree construction as evenly as possible
        // across multiple cores
        // Merkle trees must have exactly 2^n leaves to be mergeable
        let mut notes_per_thread_rounded = 1;
        while notes_per_thread_max > notes_per_thread_rounded * 4 {
            notes_per_thread_rounded *= 2;
        }
        // Make the sub-Merkle trees in parallel
        let tree_parts: Vec<_> = conv_notes
            .par_chunks(notes_per_thread_rounded)
            .map(FrozenCommitmentTree::new)
            .collect();
        // Convert conversion vector into tree so that Merkle paths can be
        // obtained
        self.conversion_state.tree = FrozenCommitmentTree::merge(&tree_parts);

        // Load up the conversions currently being given as query results
        let state_key = key_prefix
            .push(&(token::CONVERSION_KEY_PREFIX.to_owned()))
            .map_err(Error::KeyError)?;
        // Save the current conversion state in order to avoid computing
        // conversion commitments from scratch in the next epoch
        self.write(&state_key, types::encode(&self.conversion_state))
            .expect("unable to save current conversion state");
        Ok(())
    }

    /// Update the merkle tree with epoch data
    fn update_epoch_in_merkle_tree(&mut self) -> Result<()> {
        let key_prefix: Key =
            Address::Internal(InternalAddress::PoS).to_db_key().into();

        let key = key_prefix
            .push(&"epoch_start_height".to_string())
            .map_err(Error::KeyError)?;
        self.block
            .tree
            .update(&key, types::encode(&self.next_epoch_min_start_height))?;

        let key = key_prefix
            .push(&"epoch_start_time".to_string())
            .map_err(Error::KeyError)?;
        self.block
            .tree
            .update(&key, types::encode(&self.next_epoch_min_start_time))?;

        let key = key_prefix
            .push(&"current_epoch".to_string())
            .map_err(Error::KeyError)?;
        self.block
            .tree
            .update(&key, types::encode(&self.block.epoch))?;

        Ok(())
    }

    /// Start write batch.
    fn batch() -> D::WriteBatch {
        D::batch()
    }

    /// Execute write batch.
    fn exec_batch(&mut self, batch: D::WriteBatch) -> Result<()> {
        self.db.exec_batch(batch)
    }

    /// Batch write the value with the given height and account subspace key to
    /// the DB. Returns the size difference from previous value, if any, or
    /// the size of the value otherwise.
    fn batch_write_subspace_val(
        &mut self,
        batch: &mut D::WriteBatch,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<i64> {
        let value = value.as_ref();
        self.block.tree.update(key, value)?;
        self.db
            .batch_write_subspace_val(batch, self.block.height, key, value)
    }

    /// Batch delete the value with the given height and account subspace key
    /// from the DB. Returns the size of the removed value, if any, 0 if no
    /// previous value was found.
    fn batch_delete_subspace_val(
        &mut self,
        batch: &mut D::WriteBatch,
        key: &Key,
    ) -> Result<i64> {
        self.block.tree.delete(key)?;
        self.db
            .batch_delete_subspace_val(batch, self.block.height, key)
    }
}

impl From<MerkleTreeError> for Error {
    fn from(error: MerkleTreeError) -> Self {
        Self::MerkleTreeError(error)
    }
}

/// Helpers for testing components that depend on storage
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use merkle_tree::Sha256Hasher;

    use super::mockdb::MockDB;
    use super::*;

    /// Storage with a mock DB for testing
    pub type TestStorage = Storage<MockDB, Sha256Hasher>;

    impl Default for TestStorage {
        fn default() -> Self {
            let chain_id = ChainId::default();
            let tree = MerkleTree::default();
            let block = BlockStorage {
                tree,
                hash: BlockHash::default(),
                height: BlockHeight::default(),
                epoch: Epoch::default(),
                pred_epochs: Epochs::default(),
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
                conversion_state: ConversionState::default(),
                #[cfg(feature = "ferveo-tpke")]
                tx_queue: TxQueue::default(),
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
            max_expected_time_per_block in 1..100_i64,
        )
        (
            min_num_of_blocks in Just(min_num_of_blocks),
            min_duration in Just(min_duration),
            max_expected_time_per_block in Just(max_expected_time_per_block),
            start_height in Just(start_height),
            start_time in Just(start_time),
            block_height in start_height + 1..(start_height + 2 * min_num_of_blocks as u64),
            block_time in start_time + 1..(start_time + 2 * min_duration),
            // Delta will be applied on the `min_num_of_blocks` parameter
            min_blocks_delta in -(min_num_of_blocks as i64 - 1)..5,
            // Delta will be applied on the `min_duration` parameter
            min_duration_delta in -(min_duration as i64 - 1)..50,
            // Delta will be applied on the `max_expected_time_per_block` parameter
            max_time_per_block_delta in -(max_expected_time_per_block as i64 - 1)..50,
        ) -> (EpochDuration, i64, BlockHeight, DateTimeUtc, BlockHeight, DateTimeUtc,
                i64, i64, i64) {
            let epoch_duration = EpochDuration {
                min_num_of_blocks,
                min_duration: Duration::seconds(min_duration).into(),
            };
            (epoch_duration, max_expected_time_per_block,
                BlockHeight(start_height), Utc.timestamp(start_time, 0).into(),
                BlockHeight(block_height), Utc.timestamp(block_time, 0).into(),
                min_blocks_delta, min_duration_delta, max_time_per_block_delta)
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
            (epoch_duration, max_expected_time_per_block, start_height, start_time, block_height, block_time,
            min_blocks_delta, min_duration_delta, max_time_per_block_delta)
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
                max_expected_time_per_block: Duration::seconds(max_expected_time_per_block).into(),
                vp_whitelist: vec![],
                tx_whitelist: vec![]
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
            parameters.max_expected_time_per_block =
                Duration::seconds(max_expected_time_per_block + max_time_per_block_delta).into();
            parameters::update_max_expected_time_per_block_parameter(&mut storage, &parameters.max_expected_time_per_block).unwrap();
            parameters::update_epoch_parameter(&mut storage, &parameters.epoch_duration).unwrap();

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
