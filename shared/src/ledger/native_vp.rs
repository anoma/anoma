//! Native validity predicate interface associated with internal accounts such
//! as the PoS and IBC modules.
use std::cell::RefCell;
use std::collections::BTreeSet;

use thiserror::Error;

use crate::ledger::gas::VpGasMeter;
use crate::ledger::storage::write_log::WriteLog;
use crate::ledger::storage::{Storage, StorageHasher};
use crate::ledger::{storage, vp_env};
use crate::proto::Tx;
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{BlockHash, BlockHeight, Epoch, Key, TxIndex};
use crate::vm::prefix_iter::PrefixIterators;
use crate::vm::WasmCacheAccess;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Host context error: {0}")]
    ContextError(vp_env::RuntimeError),
}

/// Native VP function result
pub type Result<T> = std::result::Result<T, Error>;

/// A native VP module should implement its validation logic using this trait.
pub trait NativeVp {
    /// The address of this VP
    const ADDR: InternalAddress;

    /// Error type for the methods' results.
    type Error: std::error::Error;

    /// Run the validity predicate
    fn validate_tx(
        &self,
        tx_data: &[u8],
        keys_changed: &BTreeSet<Key>,
        verifiers: &BTreeSet<Address>,
    ) -> std::result::Result<bool, Self::Error>;
}

/// A validity predicate's host context.
///
/// This is similar to [`crate::vm::host_env::VpCtx`], but without the VM
/// wrapper types and `eval_runner` field. The references must not be changed
/// when [`Ctx`] is mutable.
#[derive(Debug)]
pub struct Ctx<'a, DB, H, CA>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
    CA: WasmCacheAccess,
{
    /// Storage prefix iterators.
    pub iterators: RefCell<PrefixIterators<'a, DB>>,
    /// VP gas meter.
    pub gas_meter: RefCell<VpGasMeter>,
    /// Read-only access to the storage.
    pub storage: &'a Storage<DB, H>,
    /// Read-only access to the write log.
    pub write_log: &'a WriteLog,
    /// The transaction code is used for signature verification
    pub tx: &'a Tx,
    /// The transaction index is used to obtain the shielded transaction's
    /// parent
    pub tx_index: &'a TxIndex,
    /// VP WASM compilation cache
    #[cfg(feature = "wasm-runtime")]
    pub vp_wasm_cache: crate::vm::wasm::VpCache<CA>,
    /// To avoid unused parameter without "wasm-runtime" feature
    #[cfg(not(feature = "wasm-runtime"))]
    pub cache_access: std::marker::PhantomData<CA>,
}

impl<'a, DB, H, CA> Ctx<'a, DB, H, CA>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    /// Initialize a new context for native VP call
    pub fn new(
        storage: &'a Storage<DB, H>,
        write_log: &'a WriteLog,
        tx: &'a Tx,
        tx_index: &'a TxIndex,
        gas_meter: VpGasMeter,
        #[cfg(feature = "wasm-runtime")]
        vp_wasm_cache: crate::vm::wasm::VpCache<CA>,
    ) -> Self {
        Self {
            iterators: RefCell::new(PrefixIterators::default()),
            gas_meter: RefCell::new(gas_meter),
            storage,
            write_log,
            tx,
            tx_index,
            #[cfg(feature = "wasm-runtime")]
            vp_wasm_cache,
            #[cfg(not(feature = "wasm-runtime"))]
            cache_access: std::marker::PhantomData,
        }
    }

    /// Add a gas cost incured in a validity predicate
    pub fn add_gas(&self, used_gas: u64) -> Result<()> {
        vp_env::add_gas(&mut *self.gas_meter.borrow_mut(), used_gas)
            .map_err(Error::ContextError)
    }

    /// Storage read prior state (before tx execution). It will try to read from
    /// the storage.
    pub fn read_pre(&self, key: &Key) -> Result<Option<Vec<u8>>> {
        vp_env::read_pre(
            &mut *self.gas_meter.borrow_mut(),
            self.storage,
            self.write_log,
            key,
        )
        .map_err(Error::ContextError)
    }

    /// Storage read posterior state (after tx execution). It will try to read
    /// from the write log first and if no entry found then from the
    /// storage.
    pub fn read_post(&self, key: &Key) -> Result<Option<Vec<u8>>> {
        vp_env::read_post(
            &mut *self.gas_meter.borrow_mut(),
            self.storage,
            self.write_log,
            key,
        )
        .map_err(Error::ContextError)
    }

    /// Storage read temporary state (after tx execution). It will try to read
    /// from only the write log.
    pub fn read_temp(&self, key: &Key) -> Result<Option<Vec<u8>>> {
        vp_env::read_temp(
            &mut *self.gas_meter.borrow_mut(),
            self.write_log,
            key,
        )
        .map_err(Error::ContextError)
    }

    /// Storage `has_key` in prior state (before tx execution). It will try to
    /// read from the storage.
    pub fn has_key_pre(&self, key: &Key) -> Result<bool> {
        vp_env::has_key_pre(
            &mut *self.gas_meter.borrow_mut(),
            self.storage,
            key,
        )
        .map_err(Error::ContextError)
    }

    /// Storage `has_key` in posterior state (after tx execution). It will try
    /// to check the write log first and if no entry found then the storage.
    pub fn has_key_post(&self, key: &Key) -> Result<bool> {
        vp_env::has_key_post(
            &mut *self.gas_meter.borrow_mut(),
            self.storage,
            self.write_log,
            key,
        )
        .map_err(Error::ContextError)
    }

    /// Getting the chain ID.
    pub fn get_chain_id(&self) -> Result<String> {
        vp_env::get_chain_id(&mut *self.gas_meter.borrow_mut(), self.storage)
            .map_err(Error::ContextError)
    }

    /// Getting the block height. The height is that of the block to which the
    /// current transaction is being applied.
    pub fn get_block_height(&self) -> Result<BlockHeight> {
        vp_env::get_block_height(
            &mut *self.gas_meter.borrow_mut(),
            self.storage,
        )
        .map_err(Error::ContextError)
    }

    /// Getting the block hash. The height is that of the block to which the
    /// current transaction is being applied.
    pub fn get_block_hash(&self) -> Result<BlockHash> {
        vp_env::get_block_hash(&mut *self.gas_meter.borrow_mut(), self.storage)
            .map_err(Error::ContextError)
    }

    /// Getting the block epoch. The epoch is that of the block to which the
    /// current transaction is being applied.
    pub fn get_block_epoch(&self) -> Result<Epoch> {
        vp_env::get_block_epoch(&mut *self.gas_meter.borrow_mut(), self.storage)
            .map_err(Error::ContextError)
    }

    /// Storage prefix iterator. It will try to get an iterator from the
    /// storage.
    pub fn iter_prefix(
        &self,
        prefix: &Key,
    ) -> Result<<DB as storage::DBIter<'a>>::PrefixIter> {
        vp_env::iter_prefix(
            &mut *self.gas_meter.borrow_mut(),
            self.storage,
            prefix,
        )
        .map_err(Error::ContextError)
    }

    /// Storage prefix iterator for prior state (before tx execution). It will
    /// try to read from the storage.
    pub fn iter_pre_next(
        &self,
        iter: &mut <DB as storage::DBIter<'_>>::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>> {
        vp_env::iter_pre_next::<DB>(&mut *self.gas_meter.borrow_mut(), iter)
            .map_err(Error::ContextError)
    }

    /// Storage prefix iterator next for posterior state (after tx execution).
    /// It will try to read from the write log first and if no entry found
    /// then from the storage.
    pub fn iter_post_next(
        &self,
        iter: &mut <DB as storage::DBIter<'_>>::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>> {
        vp_env::iter_post_next::<DB>(
            &mut *self.gas_meter.borrow_mut(),
            self.write_log,
            iter,
        )
        .map_err(Error::ContextError)
    }

    /// Evaluate a validity predicate with given data. The address, changed
    /// storage keys and verifiers will have the same values as the input to
    /// caller's validity predicate.
    ///
    /// If the execution fails for whatever reason, this will return `false`.
    /// Otherwise returns the result of evaluation.
    pub fn eval(
        &mut self,
        address: &Address,
        keys_changed: &BTreeSet<Key>,
        verifiers: &BTreeSet<Address>,
        vp_code: Vec<u8>,
        input_data: Vec<u8>,
    ) -> bool {
        #[cfg(feature = "wasm-runtime")]
        {
            use std::marker::PhantomData;

            use crate::vm::host_env::VpCtx;
            use crate::vm::wasm::run::VpEvalWasm;

            let eval_runner = VpEvalWasm {
                db: PhantomData,
                hasher: PhantomData,
                cache_access: PhantomData,
            };
            let mut iterators: PrefixIterators<'_, DB> =
                PrefixIterators::default();
            let mut result_buffer: Option<Vec<u8>> = None;

            let ctx = VpCtx::new(
                address,
                self.storage,
                self.write_log,
                &mut *self.gas_meter.borrow_mut(),
                self.tx,
                self.tx_index,
                &mut iterators,
                verifiers,
                &mut result_buffer,
                keys_changed,
                &eval_runner,
                &mut self.vp_wasm_cache,
            );
            match eval_runner.eval_native_result(ctx, vp_code, input_data) {
                Ok(result) => result,
                Err(err) => {
                    tracing::warn!(
                        "VP eval from a native VP failed with: {}",
                        err
                    );
                    false
                }
            }
        }

        #[cfg(not(feature = "wasm-runtime"))]
        {
            let _ = (address, keys_changed, verifiers, vp_code, input_data);
            unimplemented!(
                "The \"wasm-runtime\" feature must be enabled to use the \
                 `eval` function."
            )
        }
    }
}
