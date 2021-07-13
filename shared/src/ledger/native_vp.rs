//! Native validity predicate interface associated with internal accounts such
//! as the PoS and IBC modules.
use std::collections::HashSet;

use crate::ledger::gas::VpGasMeter;
use crate::ledger::ibc::Ibc;
use crate::ledger::pos::PoS;
use crate::ledger::storage::write_log::WriteLog;
use crate::ledger::storage::{Storage, StorageHasher};
use crate::ledger::vp_env::Result;
use crate::ledger::{storage, vp_env};
use crate::proto::Tx;
use crate::types::address::{Address, InternalAddress};
use crate::types::storage::{BlockHash, BlockHeight, Key};
use crate::vm::prefix_iter::PrefixIterators;

/// Initialize genesis storage for all the [`NativeVp`]s.
pub fn init_genesis_storage<DB, H>(storage: &mut Storage<DB, H>)
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    PoS::init_genesis_storage(storage);
    Ibc::init_genesis_storage(storage);
}

/// A native VP module should implement this module and add its initialization
/// call to [`init_genesis_storage`].
pub trait NativeVp {
    /// The address of this VP
    const ADDR: InternalAddress;

    /// Initialize storage in the genesis block
    fn init_genesis_storage<DB, H>(storage: &mut Storage<DB, H>)
    where
        DB: storage::DB + for<'iter> storage::DBIter<'iter>,
        H: StorageHasher;

    /// Run the validity predicate. This function can call methods on the
    /// [`Ctx`] argument to interact with the host structures.
    fn validate_tx<DB, H>(
        ctx: &mut Ctx<DB, H>,
        tx_data: &[u8],
        keys_changed: &HashSet<Key>,
        verifiers: &HashSet<Address>,
    ) -> Result<bool>
    where
        DB: storage::DB + for<'iter> storage::DBIter<'iter>,
        H: StorageHasher;
}

/// A validity predicate's host context.
///
/// This is similar to [`crate::vm::host_env::VpCtx`], but without the VM
/// wrapper types and `eval_runner` field. The references must not be changed
/// when [`Ctx`] is mutable.
#[derive(Debug)]
pub struct Ctx<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Storage prefix iterators.
    pub iterators: PrefixIterators<'a, DB>,
    /// VP gas meter.
    pub gas_meter: VpGasMeter,
    /// Read-only access to the storage.
    pub storage: &'a Storage<DB, H>,
    /// Read-only access to the write log.
    pub write_log: &'a WriteLog,
    /// The transaction code is used for signature verification
    pub tx: &'a Tx,
}

impl<'a, DB, H> Ctx<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    /// Initialize a new context for native VP call
    pub fn new(
        storage: &'a Storage<DB, H>,
        write_log: &'a WriteLog,
        tx: &'a Tx,
        gas_meter: VpGasMeter,
    ) -> Self {
        Self {
            iterators: PrefixIterators::default(),
            gas_meter,
            storage,
            write_log,
            tx,
        }
    }

    /// Add a gas cost incured in a validity predicate
    pub fn add_gas(&mut self, used_gas: u64) -> Result<()> {
        vp_env::add_gas(&mut self.gas_meter, used_gas)
    }

    /// Storage read prior state (before tx execution). It will try to read from
    /// the storage.
    pub fn read_pre(&mut self, key: &Key) -> Result<Option<Vec<u8>>> {
        vp_env::read_pre(&mut self.gas_meter, self.storage, key)
    }

    /// Storage read posterior state (after tx execution). It will try to read
    /// from the write log first and if no entry found then from the
    /// storage.
    pub fn read_post(&mut self, key: &Key) -> Result<Option<Vec<u8>>> {
        vp_env::read_post(
            &mut self.gas_meter,
            self.storage,
            self.write_log,
            key,
        )
    }

    /// Storage `has_key` in prior state (before tx execution). It will try to
    /// read from the storage.
    pub fn has_key_pre(&mut self, key: &Key) -> Result<bool> {
        vp_env::has_key_pre(&mut self.gas_meter, self.storage, key)
    }

    /// Storage `has_key` in posterior state (after tx execution). It will try
    /// to check the write log first and if no entry found then the storage.
    pub fn has_key_post(&mut self, key: &Key) -> Result<bool> {
        vp_env::has_key_post(
            &mut self.gas_meter,
            self.storage,
            self.write_log,
            key,
        )
    }

    /// Getting the chain ID.
    pub fn get_chain_id(&mut self) -> Result<String> {
        vp_env::get_chain_id(&mut self.gas_meter, self.storage)
    }

    /// Getting the block height. The height is that of the block to which the
    /// current transaction is being applied.
    pub fn get_block_height(&mut self) -> Result<BlockHeight> {
        vp_env::get_block_height(&mut self.gas_meter, self.storage)
    }

    /// Getting the block hash. The height is that of the block to which the
    /// current transaction is being applied.
    pub fn get_block_hash(&mut self) -> Result<BlockHash> {
        vp_env::get_block_hash(&mut self.gas_meter, self.storage)
    }

    /// Storage prefix iterator. It will try to get an iterator from the
    /// storage.
    pub fn iter_prefix(
        &mut self,
        prefix: &Key,
    ) -> Result<<DB as storage::DBIter<'a>>::PrefixIter> {
        vp_env::iter_prefix(&mut self.gas_meter, self.storage, prefix)
    }

    /// Storage prefix iterator for prior state (before tx execution). It will
    /// try to read from the storage.
    pub fn iter_pre_next(
        &mut self,
        iter: &mut <DB as storage::DBIter<'_>>::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>> {
        vp_env::iter_pre_next::<DB>(&mut self.gas_meter, iter)
    }

    /// Storage prefix iterator next for posterior state (after tx execution).
    /// It will try to read from the write log first and if no entry found
    /// then from the storage.
    pub fn iter_post_next(
        &mut self,
        iter: &mut <DB as storage::DBIter<'_>>::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>> {
        vp_env::iter_post_next::<DB>(&mut self.gas_meter, self.write_log, iter)
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
        keys_changed: &HashSet<Key>,
        verifiers: &HashSet<Address>,
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
            };
            let mut iterators: PrefixIterators<'_, DB> =
                PrefixIterators::default();
            let mut result_buffer: Option<Vec<u8>> = None;

            let ctx = VpCtx::new(
                address,
                self.storage,
                self.write_log,
                &mut self.gas_meter,
                self.tx,
                &mut iterators,
                verifiers,
                &mut result_buffer,
                keys_changed,
                &eval_runner,
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
