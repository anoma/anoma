//! Transaction environment contains functions that can be called from
//! inside a tx.

use borsh::{BorshDeserialize, BorshSerialize};

use crate::types::address::Address;
use crate::types::ibc::IbcEvent;
use crate::types::storage::{self, BlockHash, BlockHeight, Epoch};
use crate::types::time::Rfc3339String;

/// Transaction host functions
pub trait TxEnv {
    /// Storage read prefix iterator
    type PrefixIter;

    /// Host functions possible errors, extensible with custom user errors.
    type Error;

    /// Storage read Borsh encoded value. It will try to read from the write log
    /// first and if no entry found then from the storage and then decode it if
    /// found.
    fn read<T: BorshDeserialize>(
        &self,
        key: &storage::Key,
    ) -> Result<Option<T>, Self::Error>;

    /// Storage read raw bytes. It will try to read from the write log first and
    /// if no entry found then from the storage.
    fn read_bytes(
        &self,
        key: &storage::Key,
    ) -> Result<Option<Vec<u8>>, Self::Error>;

    /// Check if the storage contains the given key. It will try
    /// to check the write log first and if no entry found then the storage.
    fn has_key(&self, key: &storage::Key) -> Result<bool, Self::Error>;

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

    /// Get time of the current block header as rfc 3339 string
    fn get_block_time(&self) -> Result<Rfc3339String, Self::Error>;

    /// Storage prefix iterator. It will try to get an iterator from the
    /// storage.
    fn iter_prefix(
        &self,
        prefix: &storage::Key,
    ) -> Result<Self::PrefixIter, Self::Error>;

    /// Storage prefix iterator next. It will try to read from the write log
    /// first and if no entry found then from the storage.
    fn iter_next(
        &self,
        iter: &mut Self::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>, Self::Error>;

    // --- MUTABLE ----

    /// Write a value to be encoded with Borsh at the given key to storage.
    fn write<T: BorshSerialize>(
        &mut self,
        key: &storage::Key,
        val: T,
    ) -> Result<(), Self::Error>;

    /// Write a value as bytes at the given key to storage.
    fn write_bytes(
        &mut self,
        key: &storage::Key,
        val: impl AsRef<[u8]>,
    ) -> Result<(), Self::Error>;

    /// Write a temporary value to be encoded with Borsh at the given key to
    /// storage.
    fn write_temp<T: BorshSerialize>(
        &mut self,
        key: &storage::Key,
        val: T,
    ) -> Result<(), Self::Error>;

    /// Write a temporary value as bytes at the given key to storage.
    fn write_bytes_temp(
        &mut self,
        key: &storage::Key,
        val: impl AsRef<[u8]>,
    ) -> Result<(), Self::Error>;

    /// Delete a value at the given key from storage.
    fn delete(&mut self, key: &storage::Key) -> Result<(), Self::Error>;

    /// Insert a verifier address. This address must exist on chain, otherwise
    /// the transaction will be rejected.
    ///
    /// Validity predicates of each verifier addresses inserted in the
    /// transaction will validate the transaction and will receive all the
    /// changed storage keys and initialized accounts in their inputs.
    fn insert_verifier(&mut self, addr: &Address) -> Result<(), Self::Error>;

    /// Initialize a new account generates a new established address and
    /// writes the given code as its validity predicate into the storage.
    fn init_account(
        &mut self,
        code: impl AsRef<[u8]>,
    ) -> Result<Address, Self::Error>;

    /// Update a validity predicate
    fn update_validity_predicate(
        &mut self,
        addr: &Address,
        code: impl AsRef<[u8]>,
    ) -> Result<(), Self::Error>;

    /// Emit an IBC event. There can be only one event per transaction. On
    /// multiple calls, only the last emitted event will be used.
    fn emit_ibc_event(&mut self, event: &IbcEvent) -> Result<(), Self::Error>;
}
