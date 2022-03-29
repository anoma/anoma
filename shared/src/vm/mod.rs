//! Virtual machine modules for running transactions, validity predicates,
//! matchmaker and matchmaker's filter.

use std::ffi::c_void;
use std::marker::PhantomData;
use std::slice;

use wasmparser::{Validator, WasmFeatures};

pub mod host_env;
pub mod memory;
pub mod prefix_iter;
pub mod types;
#[cfg(feature = "wasm-runtime")]
pub mod wasm;
use thiserror::Error;

const UNTRUSTED_WASM_FEATURES: WasmFeatures = WasmFeatures {
    reference_types: false,
    multi_value: false,
    bulk_memory: false,
    module_linking: false,
    simd: false,
    threads: false,
    tail_call: false,
    deterministic_only: true,
    multi_memory: false,
    exceptions: false,
    memory64: false,
    mutable_global: false,
    saturating_float_to_int: false,
    sign_extension: false,
    relaxed_simd: false,
    extended_const: false,
};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum WasmValidationError {
    #[error(
        "Invalid WASM using forbidden features: {0}. Expected: \
         {UNTRUSTED_WASM_FEATURES:?}"
    )]
    ForbiddenWasmFeatures(wasmparser::BinaryReaderError),
}

/// WASM Cache access level, used to limit dry-ran transactions to read-only
/// cache access.
pub trait WasmCacheAccess: Clone + std::fmt::Debug + Default {
    /// Is access read/write?
    fn is_read_write() -> bool;
}

/// Regular read/write caches access
#[derive(Debug, Clone, Default)]
pub struct WasmCacheRwAccess;
impl WasmCacheAccess for WasmCacheRwAccess {
    fn is_read_write() -> bool {
        true
    }
}

/// Restricted read-only access for dry-ran transactions
#[derive(Debug, Clone, Default)]
pub struct WasmCacheRoAccess;

impl WasmCacheAccess for WasmCacheRoAccess {
    fn is_read_write() -> bool {
        false
    }
}

/// This is used to attach the Ledger's host structures to wasm environment,
/// which is used for implementing some host calls. It wraps an immutable
/// reference, so the access is thread-safe, but because of the unsafe
/// reference conversion, care must be taken that while this reference is
/// borrowed, no other process can modify it.
#[derive(Clone)]
pub struct HostRef<'a, T: 'a> {
    data: *const c_void,
    phantom: PhantomData<&'a T>,
}
unsafe impl<T> Send for HostRef<'_, T> {}
unsafe impl<T> Sync for HostRef<'_, T> {}

impl<'a, T: 'a> HostRef<'a, &T> {
    /// Wrap a reference for VM environment.
    ///
    /// # Safety
    ///
    /// Because this is unsafe, care must be taken that while this reference
    /// is borrowed, no other process can modify it.
    pub unsafe fn new(host_structure: &T) -> Self {
        Self {
            data: host_structure as *const T as *const c_void,
            phantom: PhantomData,
        }
    }

    /// Get a reference from VM environment.
    ///
    /// # Safety
    ///
    /// Because this is unsafe, care must be taken that while this reference
    /// is borrowed, no other process can modify it.
    pub unsafe fn get(&self) -> &'a T {
        &*(self.data as *const T)
    }
}

/// This is used to attach the Ledger's host structures to wasm environment,
/// which is used for implementing some host calls. It wraps an immutable
/// slice, so the access is thread-safe, but because of the unsafe slice
/// conversion, care must be taken that while this slice is borrowed, no other
/// process can modify it.
#[derive(Clone)]
pub struct HostSlice<'a, T: 'a> {
    data: *const c_void,
    len: usize,
    phantom: PhantomData<&'a T>,
}
unsafe impl<T> Send for HostSlice<'_, T> {}
unsafe impl<T> Sync for HostSlice<'_, T> {}

impl<'a, T: 'a> HostSlice<'a, &[T]> {
    /// Wrap a slice for VM environment.
    ///
    /// # Safety
    ///
    /// Because this is unsafe, care must be taken that while this slice is
    /// borrowed, no other process can modify it.
    pub unsafe fn new(host_structure: &[T]) -> Self {
        Self {
            data: host_structure as *const [T] as *const c_void,
            len: host_structure.len(),
            phantom: PhantomData,
        }
    }

    /// Get a slice from VM environment.
    ///
    /// # Safety
    ///
    /// Because this is unsafe, care must be taken that while this slice is
    /// borrowed, no other process can modify it.
    pub unsafe fn get(&self) -> &'a [T] {
        slice::from_raw_parts(self.data as *const T, self.len)
    }
}

/// This is used to attach the Ledger's host structures to wasm environment,
/// which is used for implementing some host calls. Because it's mutable, it's
/// not thread-safe. Also, care must be taken that while this reference is
/// borrowed, no other process can read or modify it.
#[derive(Clone)]
pub struct MutHostRef<'a, T: 'a> {
    data: *mut c_void,
    phantom: PhantomData<&'a T>,
}
unsafe impl<T> Send for MutHostRef<'_, T> {}
unsafe impl<T> Sync for MutHostRef<'_, T> {}

impl<'a, T: 'a> MutHostRef<'a, &T> {
    /// Wrap a mutable reference for VM environment.
    ///
    /// # Safety
    ///
    /// This is not thread-safe. Also, because this is unsafe, care must be
    /// taken that while this reference is borrowed, no other process can read
    /// or modify it.
    pub unsafe fn new(host_structure: &mut T) -> Self {
        Self {
            data: host_structure as *mut T as *mut c_void,
            phantom: PhantomData,
        }
    }

    /// Get a mutable reference from VM environment.
    ///
    /// # Safety
    ///
    /// This is not thread-safe. Also, because this is unsafe, care must be
    /// taken that while this reference is borrowed, no other process can read
    /// or modify it.
    pub unsafe fn get(&self) -> &'a mut T {
        &mut *(self.data as *mut T)
    }
}

/// This is used to attach the Ledger's host structures to wasm environment,
/// which is used for implementing some host calls. It wraps an mutable
/// slice, so the access is thread-safe, but because of the unsafe slice
/// conversion, care must be taken that while this slice is borrowed, no other
/// process can modify it.
#[derive(Clone)]
pub struct MutHostSlice<'a, T: 'a> {
    data: *mut c_void,
    len: usize,
    phantom: PhantomData<&'a T>,
}
unsafe impl<T> Send for MutHostSlice<'_, T> {}
unsafe impl<T> Sync for MutHostSlice<'_, T> {}

impl<'a, T: 'a> MutHostSlice<'a, &[T]> {
    /// Wrap a slice for VM environment.
    ///
    /// # Safety
    ///
    /// Because this is unsafe, care must be taken that while this slice is
    /// borrowed, no other process can modify it.
    #[allow(dead_code)]
    pub unsafe fn new(host_structure: &mut [T]) -> Self {
        Self {
            data: host_structure as *mut [T] as *mut c_void,
            len: host_structure.len(),
            phantom: PhantomData,
        }
    }

    /// Get a slice from VM environment.
    ///
    /// # Safety
    ///
    /// Because this is unsafe, care must be taken that while this slice is
    /// borrowed, no other process can modify it.
    pub unsafe fn get(&self) -> &'a mut [T] {
        slice::from_raw_parts_mut(self.data as *mut T, self.len)
    }
}

/// Validate an untrusted wasm code with restrictions that we place such code
/// (e.g. transaction and validity predicates)
pub fn validate_untrusted_wasm(
    wasm_code: impl AsRef<[u8]>,
) -> Result<(), WasmValidationError> {
    let mut validator = Validator::new();
    validator.wasm_features(UNTRUSTED_WASM_FEATURES);
    validator
        .validate_all(wasm_code.as_ref())
        .map_err(WasmValidationError::ForbiddenWasmFeatures)
}
