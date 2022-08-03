//! This crate contains library code for transaction WASM. Most of the code is
//! re-exported from the `anoma_vm_env` crate.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

mod error;
pub mod governance;
pub mod ibc;
pub mod intent;
pub mod nft;
pub mod proof_of_stake;
pub mod token;

use core::slice;
use std::marker::PhantomData;

pub use anoma::ledger::governance::storage as gov_storage;
pub use anoma::ledger::parameters::storage as parameters_storage;
pub use anoma::ledger::storage::types::encode;
use anoma::ledger::storage_api;
pub use anoma::ledger::storage_api::StorageRead;
pub use anoma::ledger::treasury::storage as treasury_storage;
pub use anoma::ledger::tx_env::TxEnv;
pub use anoma::proto::{Signed, SignedTxData};
pub use anoma::types::address::Address;
use anoma::types::chain::CHAIN_ID_LENGTH;
use anoma::types::internal::HostEnvResult;
use anoma::types::storage::{BlockHash, BlockHeight, Epoch, BLOCK_HASH_LENGTH};
use anoma::types::time::Rfc3339String;
pub use anoma::types::*;
pub use anoma_macros::transaction;
use anoma_vm_env::tx::*;
use anoma_vm_env::{read_from_buffer, read_key_val_bytes_from_buffer};
pub use borsh::{BorshDeserialize, BorshSerialize};
pub use error::*;

pub use crate::ibc::IbcActions;
pub use crate::proof_of_stake::{PosRead, PosWrite};

/// Log a string. The message will be printed at the `tracing::Level::Info`.
pub fn log_string<T: AsRef<str>>(msg: T) {
    let msg = msg.as_ref();
    unsafe {
        anoma_tx_log_string(msg.as_ptr() as _, msg.len() as _);
    }
}

/// Format and log a string in a debug build.
///
/// In WASM target debug build, the message will be printed at the
/// `tracing::Level::Info` when executed in the VM. An optimized build will
/// omit any `debug_log!` statements unless `-C debug-assertions` is passed to
/// the compiler.
///
/// In non-WASM target, the message is simply printed out to stdout.
#[macro_export]
macro_rules! debug_log {
    ($($arg:tt)*) => {{
        (
            if cfg!(target_arch = "wasm32") {
                if cfg!(debug_assertions)
                {
                    log_string(format!($($arg)*));
                }
            } else {
                println!($($arg)*);
            }
        )
    }};
}

/// Execution context provides access to the host environment functions
pub struct Ctx(());

impl Ctx {
    /// Create a host context. The context on WASM side is only provided by
    /// the VM once its being executed (in here it's implicit). But
    /// because we want to have interface identical with the native
    /// VPs, in which the context is explicit, in here we're just
    /// using an empty `Ctx` to "fake" it.
    ///
    /// # Safety
    ///
    /// When using `#[transaction]` macro from `anoma_macros`,
    /// the constructor should not be called from transactions and validity
    /// predicates implementation directly - they receive `&Self` as
    /// an argument provided by the macro that wrap the low-level WASM
    /// interface with Rust native types.
    ///
    /// Otherwise, this should only be called once to initialize this "fake"
    /// context in order to benefit from type-safety of the host environment
    /// methods implemented on the context.
    #[allow(clippy::new_without_default)]
    pub const unsafe fn new() -> Self {
        Self(())
    }
}

/// Transaction result
pub type TxResult = EnvResult<()>;

#[derive(Debug)]
pub struct KeyValIterator<T>(pub u64, pub PhantomData<T>);

impl StorageRead for Ctx {
    type PrefixIter = KeyValIterator<(String, Vec<u8>)>;

    fn read<T: BorshDeserialize>(
        &self,
        key: &anoma::types::storage::Key,
    ) -> Result<Option<T>, storage_api::Error> {
        let key = key.to_string();
        let read_result =
            unsafe { anoma_tx_read(key.as_ptr() as _, key.len() as _) };
        Ok(read_from_buffer(read_result, anoma_tx_result_buffer)
            .and_then(|t| T::try_from_slice(&t[..]).ok()))
    }

    fn read_bytes(
        &self,
        key: &anoma::types::storage::Key,
    ) -> Result<Option<Vec<u8>>, storage_api::Error> {
        let key = key.to_string();
        let read_result =
            unsafe { anoma_tx_read(key.as_ptr() as _, key.len() as _) };
        Ok(read_from_buffer(read_result, anoma_tx_result_buffer))
    }

    fn has_key(
        &self,
        key: &anoma::types::storage::Key,
    ) -> Result<bool, storage_api::Error> {
        let key = key.to_string();
        let found =
            unsafe { anoma_tx_has_key(key.as_ptr() as _, key.len() as _) };
        Ok(HostEnvResult::is_success(found))
    }

    fn get_chain_id(&self) -> Result<String, storage_api::Error> {
        let result = Vec::with_capacity(CHAIN_ID_LENGTH);
        unsafe {
            anoma_tx_get_chain_id(result.as_ptr() as _);
        }
        let slice =
            unsafe { slice::from_raw_parts(result.as_ptr(), CHAIN_ID_LENGTH) };
        Ok(String::from_utf8(slice.to_vec())
            .expect("Cannot convert the ID string"))
    }

    fn get_block_height(
        &self,
    ) -> Result<anoma::types::storage::BlockHeight, storage_api::Error> {
        Ok(BlockHeight(unsafe { anoma_tx_get_block_height() }))
    }

    fn get_block_hash(
        &self,
    ) -> Result<anoma::types::storage::BlockHash, storage_api::Error> {
        let result = Vec::with_capacity(BLOCK_HASH_LENGTH);
        unsafe {
            anoma_tx_get_block_hash(result.as_ptr() as _);
        }
        let slice = unsafe {
            slice::from_raw_parts(result.as_ptr(), BLOCK_HASH_LENGTH)
        };
        Ok(BlockHash::try_from(slice).expect("Cannot convert the hash"))
    }

    fn get_block_epoch(
        &self,
    ) -> Result<anoma::types::storage::Epoch, storage_api::Error> {
        Ok(Epoch(unsafe { anoma_tx_get_block_epoch() }))
    }

    fn iter_prefix(
        &self,
        prefix: &anoma::types::storage::Key,
    ) -> Result<Self::PrefixIter, storage_api::Error> {
        let prefix = prefix.to_string();
        let iter_id = unsafe {
            anoma_tx_iter_prefix(prefix.as_ptr() as _, prefix.len() as _)
        };
        Ok(KeyValIterator(iter_id, PhantomData))
    }

    fn iter_next(
        &self,
        iter: &mut Self::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>, storage_api::Error> {
        let read_result = unsafe { anoma_tx_iter_next(iter.0) };
        Ok(read_key_val_bytes_from_buffer(
            read_result,
            anoma_tx_result_buffer,
        ))
    }
}

impl TxEnv for Ctx {
    type Error = Error;

    fn get_block_time(&self) -> Result<time::Rfc3339String, Error> {
        let read_result = unsafe { anoma_tx_get_block_time() };
        let time_value = read_from_buffer(read_result, anoma_tx_result_buffer)
            .expect("The block time should exist");
        Ok(Rfc3339String(
            String::try_from_slice(&time_value[..])
                .expect("The conversion shouldn't fail"),
        ))
    }

    fn write<T: BorshSerialize>(
        &mut self,
        key: &anoma::types::storage::Key,
        val: T,
    ) -> storage_api::Result<()> {
        let buf = val.try_to_vec().unwrap();
        self.write_bytes(key, buf)
    }

    fn write_bytes(
        &mut self,
        key: &anoma::types::storage::Key,
        val: impl AsRef<[u8]>,
    ) -> storage_api::Result<()> {
        let key = key.to_string();
        unsafe {
            anoma_tx_write(
                key.as_ptr() as _,
                key.len() as _,
                val.as_ref().as_ptr() as _,
                val.as_ref().len() as _,
            )
        };
        Ok(())
    }

    fn write_temp<T: BorshSerialize>(
        &mut self,
        key: &anoma::types::storage::Key,
        val: T,
    ) -> Result<(), Error> {
        let buf = val.try_to_vec().unwrap();
        self.write_bytes_temp(key, buf)
    }

    fn write_bytes_temp(
        &mut self,
        key: &anoma::types::storage::Key,
        val: impl AsRef<[u8]>,
    ) -> Result<(), Error> {
        let key = key.to_string();
        unsafe {
            anoma_tx_write_temp(
                key.as_ptr() as _,
                key.len() as _,
                val.as_ref().as_ptr() as _,
                val.as_ref().len() as _,
            )
        };
        Ok(())
    }

    fn delete(
        &mut self,
        key: &anoma::types::storage::Key,
    ) -> storage_api::Result<()> {
        let key = key.to_string();
        unsafe { anoma_tx_delete(key.as_ptr() as _, key.len() as _) };
        Ok(())
    }

    fn insert_verifier(&mut self, addr: &Address) -> Result<(), Error> {
        let addr = addr.encode();
        unsafe { anoma_tx_insert_verifier(addr.as_ptr() as _, addr.len() as _) }
        Ok(())
    }

    fn init_account(
        &mut self,
        code: impl AsRef<[u8]>,
    ) -> Result<Address, Error> {
        let code = code.as_ref();
        let result = Vec::with_capacity(address::ESTABLISHED_ADDRESS_BYTES_LEN);
        unsafe {
            anoma_tx_init_account(
                code.as_ptr() as _,
                code.len() as _,
                result.as_ptr() as _,
            )
        };
        let slice = unsafe {
            slice::from_raw_parts(
                result.as_ptr(),
                address::ESTABLISHED_ADDRESS_BYTES_LEN,
            )
        };
        Ok(Address::try_from_slice(slice)
            .expect("Decoding address created by the ledger shouldn't fail"))
    }

    fn update_validity_predicate(
        &mut self,
        addr: &Address,
        code: impl AsRef<[u8]>,
    ) -> Result<(), Error> {
        let addr = addr.encode();
        let code = code.as_ref();
        unsafe {
            anoma_tx_update_validity_predicate(
                addr.as_ptr() as _,
                addr.len() as _,
                code.as_ptr() as _,
                code.len() as _,
            )
        };
        Ok(())
    }

    fn emit_ibc_event(&mut self, event: &ibc::IbcEvent) -> Result<(), Error> {
        let event = BorshSerialize::try_to_vec(event).unwrap();
        unsafe {
            anoma_tx_emit_ibc_event(event.as_ptr() as _, event.len() as _)
        };
        Ok(())
    }
}
