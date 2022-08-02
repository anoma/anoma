//! The common storage read trait is implemented in the storage, client RPC, tx
//! and VPs (both native and WASM).

use borsh::BorshDeserialize;

use crate::types::storage::{self, BlockHash, BlockHeight, Epoch};

/// Common storage read interface
pub trait StorageRead {
    /// Storage read prefix iterator
    type PrefixIter;

    /// Method's possible error.
    type Error;

    /// Storage read Borsh encoded value. It will try to read from the storage
    /// and decode it if found.
    fn read<T: BorshDeserialize>(
        &self,
        key: &storage::Key,
    ) -> Result<Option<T>, Self::Error>;

    /// Storage read raw bytes. It will try to read from the storage.
    fn read_bytes(
        &self,
        key: &storage::Key,
    ) -> Result<Option<Vec<u8>>, Self::Error>;

    /// Storage `has_key` in. It will try to read from the storage.
    fn has_key(&self, key: &storage::Key) -> Result<bool, Self::Error>;

    /// Storage prefix iterator. It will try to get an iterator from the
    /// storage.
    fn iter_prefix(
        &self,
        prefix: &storage::Key,
    ) -> Result<Self::PrefixIter, Self::Error>;

    /// Storage prefix iterator for. It will try to read from the storage.
    fn iter_next(
        &self,
        iter: &mut Self::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>, Self::Error>;

    /// Getting the chain ID.
    fn get_chain_id(&self) -> Result<String, Self::Error>;

    /// Getting the block height. The height is that of the block to which the
    /// current transaction is being applied.
    fn get_block_height(&self) -> Result<BlockHeight, Self::Error>;

    /// Getting the block hash. The height is that of the block to which the
    /// current transaction is being applied.
    fn get_block_hash(&self) -> Result<BlockHash, Self::Error>;

    /// Getting the block epoch. The epoch is that of the block to which the
    /// current transaction is being applied.
    fn get_block_epoch(&self) -> Result<Epoch, Self::Error>;
}
