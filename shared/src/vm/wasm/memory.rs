//! Wasm memory is used for bi-directionally passing data between the host and a
//! wasm instance.

use std::ptr::NonNull;
use std::str::Utf8Error;
use std::sync::Arc;

use borsh::BorshSerialize;
use loupe::MemoryUsage;
use thiserror::Error;
use wasmer::{
    vm, BaseTunables, HostEnvInitError, LazyInit, Memory, MemoryError,
    MemoryType, Pages, TableType, Target, Tunables,
};
use wasmer_vm::{
    MemoryStyle, TableStyle, VMMemoryDefinition, VMTableDefinition,
};

use crate::vm::memory::VmMemory;
use crate::vm::types::VpInput;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed initializing the memory: {0}")]
    InitMemoryError(wasmer::MemoryError),
    #[error("Memory ouf of bounds: {0}")]
    MemoryOutOfBounds(wasmer::MemoryError),
    #[error("Encoding error: {0}")]
    EncodingError(std::io::Error),
    #[error("Memory is not initialized")]
    UninitializedMemory,
    #[error("Invalid utf8 string read from memory")]
    InvalidUtf8String(Utf8Error),
}

/// Result of a function that may fail
pub type Result<T> = std::result::Result<T, Error>;

// The bounds are set in number of pages, the actual size is multiplied by
// `wasmer::WASM_PAGE_SIZE = 64kiB`.
// TODO set bounds to accommodate for wasm env size
/// Initial pages in tx memory
pub const TX_MEMORY_INIT_PAGES: u32 = 100; // 6.4 MiB
/// Mamixmum pages in tx memory
pub const TX_MEMORY_MAX_PAGES: u32 = 200; // 12.8 MiB
/// Initial pages in VP memory
pub const VP_MEMORY_INIT_PAGES: u32 = 100; // 6.4 MiB
/// Mamixmum pages in VP memory
pub const VP_MEMORY_MAX_PAGES: u32 = 200; // 12.8 MiB
/// Initial pages in matchmaker memory
pub const MATCHMAKER_MEMORY_INIT_PAGES: u32 = 400; // 25.6 MiB
/// Initial pages in matchmaker filter memory
pub const FILTER_MEMORY_INIT_PAGES: u32 = 100; // 6.4 MiB

/// Prepare memory for instantiating a transaction module
pub fn prepare_tx_memory(store: &wasmer::Store) -> Result<wasmer::Memory> {
    let mem_type = wasmer::MemoryType::new(
        TX_MEMORY_INIT_PAGES,
        Some(TX_MEMORY_MAX_PAGES),
        false,
    );
    Memory::new(store, mem_type).map_err(Error::InitMemoryError)
}

/// Prepare memory for instantiating a validity predicate module
pub fn prepare_vp_memory(store: &wasmer::Store) -> Result<wasmer::Memory> {
    let mem_type = wasmer::MemoryType::new(
        VP_MEMORY_INIT_PAGES,
        Some(VP_MEMORY_MAX_PAGES),
        false,
    );
    let memory =
        Memory::new(store, mem_type).map_err(Error::InitMemoryError)?;
    Ok(memory)
}

/// Prepare memory for instantiating a matchmaker module
pub fn prepare_matchmaker_memory(
    store: &wasmer::Store,
) -> Result<wasmer::Memory> {
    let mem_type =
        wasmer::MemoryType::new(MATCHMAKER_MEMORY_INIT_PAGES, None, false);
    Memory::new(store, mem_type).map_err(Error::InitMemoryError)
}

/// Prepare memory for instantiating a filter module
pub fn prepare_filter_memory(store: &wasmer::Store) -> Result<wasmer::Memory> {
    let mem_type =
        wasmer::MemoryType::new(FILTER_MEMORY_INIT_PAGES, None, false);
    Memory::new(store, mem_type).map_err(Error::InitMemoryError)
}

/// Input data for transaction wasm call
pub struct TxCallInput {
    /// Raw pointer to the data
    pub tx_data_ptr: u64,
    /// Length of the data
    pub tx_data_len: u64,
}

/// Write transaction inputs into wasm memory
pub fn write_tx_inputs(
    memory: &wasmer::Memory,
    tx_data_bytes: impl AsRef<[u8]>,
) -> Result<TxCallInput> {
    let tx_data_ptr = 0;
    let tx_data_len = tx_data_bytes.as_ref().len() as _;

    write_memory_bytes(memory, tx_data_ptr, tx_data_bytes)?;

    Ok(TxCallInput {
        tx_data_ptr,
        tx_data_len,
    })
}

/// Input data for validity predicate wasm call
#[derive(Clone, Debug)]
pub struct VpCallInput {
    /// Pointer to the address
    pub addr_ptr: u64,
    /// Length of the address
    pub addr_len: u64,
    /// Pointer to the data
    pub data_ptr: u64,
    /// Length of the data
    pub data_len: u64,
    /// Pointer to the serialized changed keys
    pub keys_changed_ptr: u64,
    /// Length of the serialized changed keys
    pub keys_changed_len: u64,
    /// Pointer to the serialized verifiers
    pub verifiers_ptr: u64,
    /// Length of the serialized verifiers
    pub verifiers_len: u64,
}

/// Write validity predicate inputs into wasm memory
pub fn write_vp_inputs(
    memory: &wasmer::Memory,
    VpInput {
        addr,
        data,
        keys_changed,
        verifiers,
    }: VpInput,
) -> Result<VpCallInput> {
    let addr_ptr = 0;
    let addr_bytes = addr.try_to_vec().map_err(Error::EncodingError)?;
    let addr_len = addr_bytes.len() as _;

    let data_ptr = addr_ptr + addr_len;
    let data_len = data.len() as _;

    let keys_changed_bytes =
        keys_changed.try_to_vec().map_err(Error::EncodingError)?;
    let keys_changed_ptr = data_ptr + data_len;
    let keys_changed_len = keys_changed_bytes.len() as _;

    let verifiers_bytes =
        verifiers.try_to_vec().map_err(Error::EncodingError)?;
    let verifiers_ptr = keys_changed_ptr + keys_changed_len;
    let verifiers_len = verifiers_bytes.len() as _;

    let bytes = [
        &addr_bytes[..],
        data,
        &keys_changed_bytes[..],
        &verifiers_bytes[..],
    ]
    .concat();
    write_memory_bytes(memory, addr_ptr, bytes)?;

    Ok(VpCallInput {
        addr_ptr,
        addr_len,
        data_ptr,
        data_len,
        keys_changed_ptr,
        keys_changed_len,
        verifiers_ptr,
        verifiers_len,
    })
}

/// Input data for matchmaker wasm call
pub struct MatchmakerCallInput {
    /// Pointer to the data
    pub data_ptr: u64,
    /// Length of the data
    pub data_len: u64,
    /// Pointer to the intent ID
    pub intent_id_ptr: u64,
    /// Length of the intent ID
    pub intent_id_len: u64,
    /// Pointer to the intent data
    pub intent_data_ptr: u64,
    /// Length of the intent data
    pub intent_data_len: u64,
}

/// Write matchmaker inputs into wasm memory
pub fn write_matchmaker_inputs(
    memory: &wasmer::Memory,
    data: impl AsRef<[u8]>,
    intent_id: impl AsRef<[u8]>,
    intent_data: impl AsRef<[u8]>,
) -> Result<MatchmakerCallInput> {
    let data_ptr = 0;
    let data_len = data.as_ref().len() as _;

    let intent_id_ptr = data_ptr + data_len;
    let intent_id_len = intent_id.as_ref().len() as _;

    let intent_data_ptr = intent_id_ptr + intent_id_len;
    let intent_data_len = intent_data.as_ref().len() as _;

    write_memory_bytes(memory, data_ptr, data)?;
    write_memory_bytes(memory, intent_id_ptr, intent_id)?;
    write_memory_bytes(memory, intent_data_ptr, intent_data)?;

    Ok(MatchmakerCallInput {
        data_ptr,
        data_len,
        intent_id_ptr,
        intent_id_len,
        intent_data_ptr,
        intent_data_len,
    })
}

/// Input data for matchmaker filter wasm call
pub struct FilterCallInput {
    /// Pointer to the intent data
    pub intent_data_ptr: u64,
    /// Length of the intent data
    pub intent_data_len: u64,
}

/// Write matchmaker filter inputs into wasm memory
pub fn write_filter_inputs(
    memory: &wasmer::Memory,
    intent_data: impl AsRef<[u8]>,
) -> Result<FilterCallInput> {
    let intent_data_ptr = 0;
    let intent_data_len = intent_data.as_ref().len() as _;

    tracing::info!("write_data_inputs of len {}", intent_data_len);
    write_memory_bytes(memory, intent_data_ptr, intent_data)?;

    Ok(FilterCallInput {
        intent_data_ptr,
        intent_data_len,
    })
}

/// Check that the given offset and length fits into the memory bounds. If not,
/// it will try to grow the memory.
fn check_bounds(memory: &Memory, offset: u64, len: usize) -> Result<()> {
    tracing::debug!(
        "check_bounds pages {}, data_size {}, offset + len {}",
        memory.size().0,
        memory.data_size(),
        offset + len as u64
    );
    if memory.data_size() < offset + len as u64 {
        let cur_pages = memory.size().0;
        let capacity = cur_pages as usize * wasmer::WASM_PAGE_SIZE;
        let missing = offset as usize + len - capacity;
        // Ceiling division
        let req_pages = ((missing + wasmer::WASM_PAGE_SIZE - 1)
            / wasmer::WASM_PAGE_SIZE) as u32;
        tracing::info!("trying to grow memory by {} pages", req_pages);
        memory.grow(req_pages).map_err(Error::MemoryOutOfBounds)?;
    }
    Ok(())
}

/// Read bytes from memory at the given offset and length
fn read_memory_bytes(
    memory: &Memory,
    offset: u64,
    len: usize,
) -> Result<Vec<u8>> {
    check_bounds(memory, offset, len)?;
    let offset = offset as usize;
    let vec: Vec<_> = memory.view()[offset..(offset + len)]
        .iter()
        .map(|cell| cell.get())
        .collect();
    Ok(vec)
}

/// Write bytes into memory at the given offset
fn write_memory_bytes(
    memory: &Memory,
    offset: u64,
    bytes: impl AsRef<[u8]>,
) -> Result<()> {
    let slice = bytes.as_ref();
    let len = slice.len();
    check_bounds(memory, offset, len as _)?;
    let offset = offset as usize;
    memory.view()[offset..(offset + len)]
        .iter()
        .zip(slice.iter())
        .for_each(|(cell, v)| cell.set(*v));
    Ok(())
}

/// The wasm memory
#[derive(Debug, Clone, Default)]
pub struct WasmMemory {
    inner: LazyInit<wasmer::Memory>,
}

impl WasmMemory {
    /// Initialize the memory from the given exports, used to implement
    /// [`wasmer::WasmerEnv`].
    pub fn init_env_memory(
        &mut self,
        exports: &wasmer::Exports,
    ) -> std::result::Result<(), HostEnvInitError> {
        let memory = exports.get_memory("memory")?;
        if !self.inner.initialize(memory.clone()) {
            tracing::error!("wasm memory is already initialized");
        }
        Ok(())
    }
}

impl VmMemory for WasmMemory {
    type Error = Error;

    /// Read bytes from memory at the given offset and length, return the bytes
    /// and the gas cost
    fn read_bytes(&self, offset: u64, len: usize) -> Result<(Vec<u8>, u64)> {
        let memory = self.inner.get_ref().ok_or(Error::UninitializedMemory)?;
        let bytes = read_memory_bytes(memory, offset, len)?;
        let gas = bytes.len();
        Ok((bytes, gas as _))
    }

    /// Write bytes into memory at the given offset and return the gas cost
    fn write_bytes(&self, offset: u64, bytes: impl AsRef<[u8]>) -> Result<u64> {
        let gas = bytes.as_ref().len();
        let memory = self.inner.get_ref().ok_or(Error::UninitializedMemory)?;
        write_memory_bytes(memory, offset, bytes)?;
        Ok(gas as _)
    }

    /// Read string from memory at the given offset and bytes length, and return
    /// the gas cost
    fn read_string(&self, offset: u64, len: usize) -> Result<(String, u64)> {
        let (bytes, gas) = self.read_bytes(offset, len)?;
        let string = std::str::from_utf8(&bytes)
            .map_err(Error::InvalidUtf8String)?
            .to_string();
        Ok((string, gas as _))
    }

    /// Write string into memory at the given offset and return the gas cost
    #[allow(dead_code)]
    fn write_string(&self, offset: u64, string: String) -> Result<u64> {
        self.write_bytes(offset, string.as_bytes())
    }
}

/// A custom [`Tunables`] to set a WASM memory limits.
///
/// Adapted from <https://github.com/wasmerio/wasmer/blob/29d7b4a5f1c401d9a1e95086ed85878c8407ec16/examples/tunables_limit_memory.rs>.
#[derive(MemoryUsage)]
pub struct Limit<T: Tunables> {
    /// The maximum a linear memory is allowed to be (in Wasm pages, 64 KiB
    /// each). Since Wasmer ensures there is only none or one memory, this
    /// is practically an upper limit for the guest memory.
    limit: Pages,
    /// The base implementation we delegate all the logic to
    base: T,
}

/// A [`Limit`] with memory limit setup for validity predicate WASM
/// execution.
pub fn vp_limit() -> Limit<BaseTunables> {
    let base = BaseTunables::for_target(&Target::default());
    let limit = Pages(VP_MEMORY_MAX_PAGES);
    Limit { limit, base }
}
/// A [`Limit`] with memory limit setup for transaction WASM execution.
pub fn tx_limit() -> Limit<BaseTunables> {
    let base = BaseTunables::for_target(&Target::default());
    let limit = Pages(TX_MEMORY_MAX_PAGES);
    Limit { limit, base }
}

impl<T: Tunables> Limit<T> {
    /// Takes an input memory type as requested by the guest and sets
    /// a maximum if missing. The resulting memory type is final if
    /// valid. However, this can produce invalid types, such that
    /// validate_memory must be called before creating the memory.
    fn adjust_memory(&self, requested: &MemoryType) -> MemoryType {
        let mut adjusted = *requested;
        if requested.maximum.is_none() {
            adjusted.maximum = Some(self.limit);
        }
        adjusted
    }

    /// Ensures the a given memory type does not exceed the memory limit.
    /// Call this after adjusting the memory.
    fn validate_memory(
        &self,
        ty: &MemoryType,
    ) -> std::result::Result<(), MemoryError> {
        if ty.minimum > self.limit {
            return Err(MemoryError::Generic(
                "Minimum exceeds the allowed memory limit".to_string(),
            ));
        }

        if let Some(max) = ty.maximum {
            if max > self.limit {
                return Err(MemoryError::Generic(
                    "Maximum exceeds the allowed memory limit".to_string(),
                ));
            }
        } else {
            return Err(MemoryError::Generic("Maximum unset".to_string()));
        }

        Ok(())
    }
}

impl<T: Tunables> Tunables for Limit<T> {
    /// Construct a `MemoryStyle` for the provided `MemoryType`
    ///
    /// Delegated to base.
    fn memory_style(&self, memory: &MemoryType) -> MemoryStyle {
        let adjusted = self.adjust_memory(memory);
        self.base.memory_style(&adjusted)
    }

    /// Construct a `TableStyle` for the provided `TableType`
    ///
    /// Delegated to base.
    fn table_style(&self, table: &TableType) -> TableStyle {
        self.base.table_style(table)
    }

    /// Create a memory owned by the host given a [`MemoryType`] and a
    /// [`MemoryStyle`].
    ///
    /// The requested memory type is validated, adjusted to the limited and then
    /// passed to base.
    fn create_host_memory(
        &self,
        ty: &MemoryType,
        style: &MemoryStyle,
    ) -> std::result::Result<Arc<dyn vm::Memory>, MemoryError> {
        let adjusted = self.adjust_memory(ty);
        self.validate_memory(&adjusted)?;
        self.base.create_host_memory(&adjusted, style)
    }

    /// Create a memory owned by the VM given a [`MemoryType`] and a
    /// [`MemoryStyle`].
    ///
    /// Delegated to base.
    unsafe fn create_vm_memory(
        &self,
        ty: &MemoryType,
        style: &MemoryStyle,
        vm_definition_location: NonNull<VMMemoryDefinition>,
    ) -> std::result::Result<Arc<dyn vm::Memory>, MemoryError> {
        let adjusted = self.adjust_memory(ty);
        self.validate_memory(&adjusted)?;
        self.base
            .create_vm_memory(&adjusted, style, vm_definition_location)
    }

    /// Create a table owned by the host given a [`TableType`] and a
    /// [`TableStyle`].
    ///
    /// Delegated to base.
    fn create_host_table(
        &self,
        ty: &TableType,
        style: &TableStyle,
    ) -> std::result::Result<Arc<dyn vm::Table>, String> {
        self.base.create_host_table(ty, style)
    }

    /// Create a table owned by the VM given a [`TableType`] and a
    /// [`TableStyle`].
    ///
    /// Delegated to base.
    unsafe fn create_vm_table(
        &self,
        ty: &TableType,
        style: &TableStyle,
        vm_definition_location: NonNull<VMTableDefinition>,
    ) -> std::result::Result<Arc<dyn vm::Table>, String> {
        self.base.create_vm_table(ty, style, vm_definition_location)
    }
}

#[cfg(test)]
pub mod tests {
    use wasmer::{
        wat2wasm, BaseTunables, Cranelift, Instance, Module, Store, Target,
    };

    use super::*;

    #[test]
    fn test_wasm_tunables_limit_memory() {
        // A Wasm module with one exported memory (min: 7 pages, max: unset)
        let wat = br#"(module (memory 7) (export "memory" (memory 0)))"#;

        // Alternatively: A Wasm module with one exported memory (min: 7 pages,
        // max: 80 pages) let wat = br#"(module (memory 7 80) (export
        // "memory" (memory 0)))"#;

        let wasm_bytes = wat2wasm(wat).unwrap();

        // Any compiler and any engine do the job here
        let compiler = Cranelift::default();
        let engine = wasmer_engine_universal::Universal::new(compiler).engine();

        let base = BaseTunables::for_target(&Target::default());
        let limit = Pages(24);
        let tunables = Limit { limit, base };

        // Create a store, that holds the engine and our custom tunables
        let store = Store::new_with_tunables(&engine, tunables);

        println!("Compiling module...");
        let module = Module::new(&store, wasm_bytes).unwrap();

        println!("Instantiating module...");
        let import_object = wasmer::imports! {};

        // Now at this point, our custom tunables are used
        let instance = Instance::new(&module, &import_object).unwrap();

        // Check what happened
        let mut memories: Vec<Memory> = instance
            .exports
            .iter()
            .memories()
            .map(|pair| pair.1.clone())
            .collect();
        assert_eq!(memories.len(), 1);

        let first_memory = memories.pop().unwrap();
        println!("Memory of this instance: {:?}", first_memory);
        assert_eq!(first_memory.ty().maximum.unwrap(), limit);
    }
}
