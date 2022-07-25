//! This crate contains library code for validity predicate WASM. Most of the
//! code is re-exported from the `anoma_vm_env` crate.

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

mod error;
pub mod intent;
pub mod key;
pub mod nft;
pub mod token;

// used in the VP input
use core::convert::AsRef;
use core::slice;
pub use std::collections::{BTreeSet, HashSet};
use std::convert::TryFrom;
use std::marker::PhantomData;

pub use anoma::ledger::governance::storage as gov_storage;
pub use anoma::ledger::read::StorageRead;
pub use anoma::ledger::vp_env::VpEnv;
pub use anoma::ledger::{parameters, pos as proof_of_stake};
pub use anoma::proto::{Signed, SignedTxData};
pub use anoma::types::address::Address;
use anoma::types::chain::CHAIN_ID_LENGTH;
use anoma::types::hash::{Hash, HASH_LENGTH};
use anoma::types::internal::HostEnvResult;
use anoma::types::key::*;
use anoma::types::storage::{BlockHash, BlockHeight, Epoch, BLOCK_HASH_LENGTH};
pub use anoma::types::*;
pub use anoma_macros::validity_predicate;
use anoma_vm_env::vp::*;
use anoma_vm_env::{read_from_buffer, read_key_val_bytes_from_buffer};
pub use borsh::{BorshDeserialize, BorshSerialize};
pub use error::*;
pub use sha2::{Digest, Sha256, Sha384, Sha512};

pub fn sha256(bytes: &[u8]) -> Hash {
    let digest = Sha256::digest(bytes);
    Hash(*digest.as_ref())
}

pub fn is_tx_whitelisted(ctx: &Ctx) -> VpResult {
    let tx_hash = ctx.get_tx_code_hash()?;
    let key = parameters::storage::get_tx_whitelist_storage_key();
    let whitelist: Vec<String> = ctx.read_pre(&key)?.unwrap_or_default();
    // if whitelist is empty, allow any transaction
    Ok(whitelist.is_empty() || whitelist.contains(&tx_hash.to_string()))
}

pub fn is_vp_whitelisted(ctx: &Ctx, vp_bytes: &[u8]) -> VpResult {
    let vp_hash = sha256(vp_bytes);
    let key = parameters::storage::get_vp_whitelist_storage_key();
    let whitelist: Vec<String> = ctx.read_pre(&key)?.unwrap_or_default();
    // if whitelist is empty, allow any transaction
    Ok(whitelist.is_empty() || whitelist.contains(&vp_hash.to_string()))
}

/// Log a string. The message will be printed at the `tracing::Level::Info`.
pub fn log_string<T: AsRef<str>>(msg: T) {
    let msg = msg.as_ref();
    unsafe {
        anoma_vp_log_string(msg.as_ptr() as _, msg.len() as _);
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

#[derive(Debug)]
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
    /// When using `#[validity_predicate]` macro from `anoma_macros`,
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

/// Read access to the prior storage (state before tx execution) via
/// [`trait@StorageRead`].
#[derive(Debug)]
pub struct CtxPreStorageRead<'a> {
    ctx: &'a Ctx,
}

/// Read access to the posterior storage (state after tx execution) via
/// [`trait@StorageRead`].
#[derive(Debug)]
pub struct CtxPostStorageRead<'a> {
    ctx: &'a Ctx,
}

/// Validity predicate result
pub type VpResult = EnvResult<bool>;

/// Accept a transaction
pub fn accept() -> VpResult {
    Ok(true)
}

/// Reject a transaction
pub fn reject() -> VpResult {
    Ok(false)
}

#[derive(Debug)]
pub struct KeyValIterator<T>(pub u64, pub PhantomData<T>);

impl VpEnv for Ctx {
    type Error = Error;
    type PrefixIter = KeyValIterator<(String, Vec<u8>)>;

    fn read_pre<T: BorshDeserialize>(
        &self,
        key: &storage::Key,
    ) -> Result<Option<T>, Self::Error> {
        let key = key.to_string();
        let read_result =
            unsafe { anoma_vp_read_pre(key.as_ptr() as _, key.len() as _) };
        Ok(read_from_buffer(read_result, anoma_vp_result_buffer)
            .and_then(|bytes| T::try_from_slice(&bytes[..]).ok()))
    }

    fn read_bytes_pre(
        &self,
        key: &storage::Key,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        let key = key.to_string();
        let read_result =
            unsafe { anoma_vp_read_pre(key.as_ptr() as _, key.len() as _) };
        Ok(read_from_buffer(read_result, anoma_vp_result_buffer))
    }

    fn read_post<T: BorshDeserialize>(
        &self,
        key: &storage::Key,
    ) -> Result<Option<T>, Self::Error> {
        let key = key.to_string();
        let read_result =
            unsafe { anoma_vp_read_post(key.as_ptr() as _, key.len() as _) };
        Ok(read_from_buffer(read_result, anoma_vp_result_buffer)
            .and_then(|bytes| T::try_from_slice(&bytes[..]).ok()))
    }

    fn read_bytes_post(
        &self,
        key: &storage::Key,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        let key = key.to_string();
        let read_result =
            unsafe { anoma_vp_read_post(key.as_ptr() as _, key.len() as _) };
        Ok(read_from_buffer(read_result, anoma_vp_result_buffer))
    }

    fn read_temp<T: BorshDeserialize>(
        &self,
        key: &storage::Key,
    ) -> Result<Option<T>, Self::Error> {
        let key = key.to_string();
        let read_result =
            unsafe { anoma_vp_read_temp(key.as_ptr() as _, key.len() as _) };
        Ok(read_from_buffer(read_result, anoma_vp_result_buffer)
            .and_then(|t| T::try_from_slice(&t[..]).ok()))
    }

    fn read_bytes_temp(
        &self,
        key: &storage::Key,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        let key = key.to_string();
        let read_result =
            unsafe { anoma_vp_read_temp(key.as_ptr() as _, key.len() as _) };
        Ok(read_from_buffer(read_result, anoma_vp_result_buffer))
    }

    fn has_key_pre(&self, key: &storage::Key) -> Result<bool, Self::Error> {
        let key = key.to_string();
        let found =
            unsafe { anoma_vp_has_key_pre(key.as_ptr() as _, key.len() as _) };
        Ok(HostEnvResult::is_success(found))
    }

    fn has_key_post(&self, key: &storage::Key) -> Result<bool, Self::Error> {
        let key = key.to_string();
        let found =
            unsafe { anoma_vp_has_key_post(key.as_ptr() as _, key.len() as _) };
        Ok(HostEnvResult::is_success(found))
    }

    fn get_chain_id(&self) -> Result<String, Self::Error> {
        let result = Vec::with_capacity(CHAIN_ID_LENGTH);
        unsafe {
            anoma_vp_get_chain_id(result.as_ptr() as _);
        }
        let slice =
            unsafe { slice::from_raw_parts(result.as_ptr(), CHAIN_ID_LENGTH) };
        Ok(String::from_utf8(slice.to_vec())
            .expect("Cannot convert the ID string"))
    }

    fn get_block_height(&self) -> Result<BlockHeight, Self::Error> {
        Ok(BlockHeight(unsafe { anoma_vp_get_block_height() }))
    }

    fn get_block_hash(&self) -> Result<BlockHash, Self::Error> {
        let result = Vec::with_capacity(BLOCK_HASH_LENGTH);
        unsafe {
            anoma_vp_get_block_hash(result.as_ptr() as _);
        }
        let slice = unsafe {
            slice::from_raw_parts(result.as_ptr(), BLOCK_HASH_LENGTH)
        };
        Ok(BlockHash::try_from(slice).expect("Cannot convert the hash"))
    }

    fn get_block_epoch(&self) -> Result<Epoch, Self::Error> {
        Ok(Epoch(unsafe { anoma_vp_get_block_epoch() }))
    }

    fn iter_prefix(
        &self,
        prefix: &storage::Key,
    ) -> Result<Self::PrefixIter, Self::Error> {
        let prefix = prefix.to_string();
        let iter_id = unsafe {
            anoma_vp_iter_prefix(prefix.as_ptr() as _, prefix.len() as _)
        };
        Ok(KeyValIterator(iter_id, PhantomData))
    }

    fn iter_pre_next(
        &self,
        iter: &mut Self::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>, Self::Error> {
        let read_result = unsafe { anoma_vp_iter_pre_next(iter.0) };
        Ok(read_key_val_bytes_from_buffer(
            read_result,
            anoma_vp_result_buffer,
        ))
    }

    fn iter_post_next(
        &self,
        iter: &mut Self::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>, Self::Error> {
        let read_result = unsafe { anoma_vp_iter_post_next(iter.0) };
        Ok(read_key_val_bytes_from_buffer(
            read_result,
            anoma_vp_result_buffer,
        ))
    }

    fn eval(
        &self,
        vp_code: Vec<u8>,
        input_data: Vec<u8>,
    ) -> Result<bool, Self::Error> {
        let result = unsafe {
            anoma_vp_eval(
                vp_code.as_ptr() as _,
                vp_code.len() as _,
                input_data.as_ptr() as _,
                input_data.len() as _,
            )
        };
        Ok(HostEnvResult::is_success(result))
    }

    fn verify_tx_signature(
        &self,
        pk: &common::PublicKey,
        sig: &common::Signature,
    ) -> Result<bool, Self::Error> {
        let pk = BorshSerialize::try_to_vec(pk).unwrap();
        let sig = BorshSerialize::try_to_vec(sig).unwrap();
        let valid = unsafe {
            anoma_vp_verify_tx_signature(
                pk.as_ptr() as _,
                pk.len() as _,
                sig.as_ptr() as _,
                sig.len() as _,
            )
        };
        Ok(HostEnvResult::is_success(valid))
    }

    fn get_tx_code_hash(&self) -> Result<Hash, Self::Error> {
        let result = Vec::with_capacity(HASH_LENGTH);
        unsafe {
            anoma_vp_get_tx_code_hash(result.as_ptr() as _);
        }
        let slice =
            unsafe { slice::from_raw_parts(result.as_ptr(), HASH_LENGTH) };
        Ok(Hash::try_from(slice).expect("Cannot convert the hash"))
    }
}

impl StorageRead for CtxPreStorageRead<'_> {
    type Error = Error;
    type PrefixIter = KeyValIterator<(String, Vec<u8>)>;

    fn read<T: BorshDeserialize>(
        &self,
        key: &storage::Key,
    ) -> Result<Option<T>, Self::Error> {
        self.ctx.read_pre(key)
    }

    fn read_bytes(
        &self,
        key: &storage::Key,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        self.ctx.read_bytes_pre(key)
    }

    fn has_key(&self, key: &storage::Key) -> Result<bool, Self::Error> {
        self.ctx.has_key_pre(key)
    }

    fn iter_prefix(
        &self,
        prefix: &storage::Key,
    ) -> Result<Self::PrefixIter, Self::Error> {
        self.ctx.iter_prefix(prefix)
    }

    fn iter_next(
        &self,
        iter: &mut Self::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>, Self::Error> {
        self.ctx.iter_pre_next(iter)
    }

    fn get_chain_id(&self) -> Result<String, Self::Error> {
        self.ctx.get_chain_id()
    }

    fn get_block_height(&self) -> Result<BlockHeight, Self::Error> {
        self.ctx.get_block_height()
    }

    fn get_block_hash(&self) -> Result<BlockHash, Self::Error> {
        self.ctx.get_block_hash()
    }

    fn get_block_epoch(&self) -> Result<Epoch, Self::Error> {
        self.ctx.get_block_epoch()
    }
}

impl StorageRead for CtxPostStorageRead<'_> {
    type Error = Error;
    type PrefixIter = KeyValIterator<(String, Vec<u8>)>;

    fn read<T: BorshDeserialize>(
        &self,
        key: &storage::Key,
    ) -> Result<Option<T>, Self::Error> {
        self.ctx.read_post(key)
    }

    fn read_bytes(
        &self,
        key: &storage::Key,
    ) -> Result<Option<Vec<u8>>, Self::Error> {
        self.ctx.read_bytes_post(key)
    }

    fn has_key(&self, key: &storage::Key) -> Result<bool, Self::Error> {
        self.ctx.has_key_post(key)
    }

    fn iter_prefix(
        &self,
        prefix: &storage::Key,
    ) -> Result<Self::PrefixIter, Self::Error> {
        self.ctx.iter_prefix(prefix)
    }

    fn iter_next(
        &self,
        iter: &mut Self::PrefixIter,
    ) -> Result<Option<(String, Vec<u8>)>, Self::Error> {
        self.ctx.iter_post_next(iter)
    }

    fn get_chain_id(&self) -> Result<String, Self::Error> {
        self.ctx.get_chain_id()
    }

    fn get_block_height(&self) -> Result<BlockHeight, Self::Error> {
        self.ctx.get_block_height()
    }

    fn get_block_hash(&self) -> Result<BlockHash, Self::Error> {
        self.ctx.get_block_hash()
    }

    fn get_block_epoch(&self) -> Result<Epoch, Self::Error> {
        self.ctx.get_block_epoch()
    }
}
