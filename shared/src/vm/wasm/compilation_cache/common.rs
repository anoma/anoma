//! WASM compilation cache.
//! The cache is backed by in-memory LRU cache with configurable size
//! limit and a file system cache of compiled modules (either to dynamic libs
//! compiled via the `dylib` module, or serialized modules compiled via the
//! `universal` module).

use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use std::thread::sleep;
use std::time::Duration;
use std::{cmp, fs};

use clru::{CLruCache, CLruCacheConfig, WeightScale};
use wasmer::{Module, Store};
use wasmer_cache::{FileSystemCache, Hash};

use crate::vm::wasm::run::untrusted_wasm_store;
use crate::vm::wasm::{self, memory};
use crate::vm::{WasmCacheAccess, WasmCacheRoAccess};

/// The size of the [`struct@Hash`]
const HASH_BYTES: usize = 32;

/// Cache handle. Thread-safe.
#[derive(Debug, Clone)]
pub struct Cache<N: CacheName, A: WasmCacheAccess> {
    /// Cached files directory
    dir: PathBuf,
    /// Compilation progress
    progress: Arc<RwLock<HashMap<Hash, Compilation>>>,
    /// In-memory LRU cache of compiled modules
    in_memory: Arc<RwLock<MemoryCache>>,
    /// The cache's name
    name: PhantomData<N>,
    /// Cache access level
    access: PhantomData<A>,
}

/// This trait is used to give names to different caches
pub trait CacheName: Clone + std::fmt::Debug {
    /// Get the name of the cache
    fn name() -> &'static str;
}

/// In-memory LRU cache of compiled modules
type MemoryCache = CLruCache<Hash, Module, RandomState, ModuleCacheScale>;

/// Compilation progress
#[derive(Debug)]
enum Compilation {
    Compiling,
    Done,
}

/// Configures the cache scale of modules that limits the maximum capacity
/// of the cache (CLruCache::len + CLruCache::weight <= CLruCache::capacity).
#[derive(Debug)]
struct ModuleCacheScale;

impl WeightScale<Hash, Module> for ModuleCacheScale {
    fn weight(&self, key: &Hash, value: &Module) -> usize {
        // We only want to limit the max memory size, not the number of
        // elements, so we use the size of the module as its scale
        // and subtract 1 from it to negate the increment of the cache length.

        let size = loupe::size_of_val(&value) + HASH_BYTES;
        tracing::debug!(
            "WASM module hash {}, size including the hash {}",
            key.to_string(),
            size
        );
        cmp::max(1, size) - 1
    }
}

impl<N: CacheName, A: WasmCacheAccess> Cache<N, A> {
    /// Create a wasm in-memory cache with a given size limit and a file
    /// system cache.
    ///
    /// # Panics
    /// The `max_bytes` must be non-zero.
    pub fn new(dir: impl Into<PathBuf>, max_bytes: usize) -> Self {
        let cache = CLruCache::with_config(
            CLruCacheConfig::new(NonZeroUsize::new(max_bytes).unwrap())
                .with_scale(ModuleCacheScale),
        );
        let in_memory = Arc::new(RwLock::new(cache));
        let dir = dir.into();
        fs::create_dir_all(&dir)
            .expect("Couldn't create the wasm cache directory");
        Self {
            dir,
            progress: Default::default(),
            in_memory,
            name: Default::default(),
            access: Default::default(),
        }
    }

    /// Get a WASM module from LRU cache, from a file or compile it and cache
    /// it. If the cache access is set to [`crate::vm::WasmCacheRwAccess`], it
    /// updates the position in the LRU cache. Otherwise, the compiled
    /// module will not be be cached, if it's not already.
    pub fn fetch_or_compile(
        &mut self,
        code: impl AsRef<[u8]>,
    ) -> Result<(Module, Store), wasm::run::Error> {
        if A::is_read_write() {
            self.get_or_compile(code)
        } else {
            self.peek_or_compile(code)
        }
    }

    /// Get a WASM module from LRU cache, from a file or compile it and cache
    /// it. Updates the position in the LRU cache.
    fn get_or_compile(
        &mut self,
        code: impl AsRef<[u8]>,
    ) -> Result<(Module, Store), wasm::run::Error> {
        let hash = hash_of_code(&code);

        let mut in_memory = self.in_memory.write().unwrap();
        if let Some(module) = in_memory.get(&hash) {
            tracing::info!(
                "{} found {} in cache.",
                N::name(),
                hash.to_string()
            );
            return Ok((module.clone(), store()));
        }
        drop(in_memory);

        let mut iter = 0;
        loop {
            let mut progress = self.progress.write().unwrap();
            match progress.get(&hash) {
                Some(Compilation::Done) => {
                    drop(progress);
                    let mut in_memory = self.in_memory.write().unwrap();
                    if let Some(module) = in_memory.get(&hash) {
                        tracing::info!(
                            "{} found {} in memory cache.",
                            N::name(),
                            hash.to_string()
                        );
                        return Ok((module.clone(), store()));
                    }

                    let (module, store) = file_load_module(&self.dir, &hash);
                    tracing::info!(
                        "{} found {} in file cache.",
                        N::name(),
                        hash.to_string()
                    );
                    // Put into cache, ignore result if it's full
                    let _ = in_memory.put_with_weight(hash, module.clone());

                    return Ok((module, store));
                }
                Some(Compilation::Compiling) => {
                    drop(progress);
                    tracing::info!(
                        "Waiting for {} {} ...",
                        N::name(),
                        hash.to_string()
                    );
                    exponential_backoff(iter);
                    iter += 1;
                    continue;
                }
                None => {
                    progress.insert(hash, Compilation::Compiling);
                    drop(progress);

                    let (module, store) =
                        if module_file_exists(&self.dir, &hash) {
                            tracing::info!(
                                "Loading {} {} from file.",
                                N::name(),
                                hash.to_string()
                            );
                            file_load_module(&self.dir, &hash)
                        } else {
                            tracing::info!(
                                "Compiling {} {}.",
                                N::name(),
                                hash.to_string()
                            );

                            match wasm::run::prepare_wasm_code(code) {
                                Ok(code) => match compile(code) {
                                    Ok((module, store)) => {
                                        // Write the file
                                        file_write_module(
                                            &self.dir, &module, &hash,
                                        );

                                        (module, store)
                                    }
                                    Err(err) => {
                                        let mut progress =
                                            self.progress.write().unwrap();
                                        tracing::info!(
                                            "Failed to compile WASM {} with {}",
                                            hash.to_string(),
                                            err
                                        );
                                        progress.remove(&hash);
                                        drop(progress);
                                        return Err(err);
                                    }
                                },
                                Err(err) => {
                                    let mut progress =
                                        self.progress.write().unwrap();
                                    tracing::info!(
                                        "Failed to prepare WASM {} with {}",
                                        hash.to_string(),
                                        err
                                    );
                                    progress.remove(&hash);
                                    drop(progress);
                                    return Err(err);
                                }
                            }
                        };

                    // Update progress
                    let mut progress = self.progress.write().unwrap();
                    progress.insert(hash, Compilation::Done);

                    // Put into cache, ignore the result (fails if the module
                    // cannot fit into the cache)
                    let mut in_memory = self.in_memory.write().unwrap();
                    let _ = in_memory.put_with_weight(hash, module.clone());

                    return Ok((module, store));
                }
            }
        }
    }

    /// Peak-only is used for dry-ran txs (and VPs that the tx triggers).
    /// It doesn't update the in-memory cache or persist the compiled modules to
    /// files.
    fn peek_or_compile(
        &self,
        code: impl AsRef<[u8]>,
    ) -> Result<(Module, Store), wasm::run::Error> {
        let hash = hash_of_code(&code);

        let in_memory = self.in_memory.read().unwrap();
        if let Some(module) = in_memory.peek(&hash) {
            tracing::info!(
                "{} found {} in cache.",
                N::name(),
                hash.to_string()
            );
            return Ok((module.clone(), store()));
        }
        drop(in_memory);

        let mut iter = 0;
        loop {
            let progress = self.progress.read().unwrap();
            match progress.get(&hash) {
                Some(Compilation::Done) => {
                    drop(progress);
                    let in_memory = self.in_memory.read().unwrap();
                    if let Some(module) = in_memory.peek(&hash) {
                        tracing::info!(
                            "{} found {} in memory cache.",
                            N::name(),
                            hash.to_string()
                        );
                        return Ok((module.clone(), store()));
                    }

                    let (module, store) = file_load_module(&self.dir, &hash);
                    tracing::info!(
                        "{} found {} in file cache.",
                        N::name(),
                        hash.to_string()
                    );
                    return Ok((module, store));
                }
                Some(Compilation::Compiling) => {
                    drop(progress);
                    tracing::info!(
                        "Waiting for {} {} ...",
                        N::name(),
                        hash.to_string()
                    );
                    exponential_backoff(iter);
                    iter += 1;
                    continue;
                }
                None => {
                    drop(progress);

                    return if module_file_exists(&self.dir, &hash) {
                        tracing::info!(
                            "Loading {} {} from file.",
                            N::name(),
                            hash.to_string()
                        );
                        Ok(file_load_module(&self.dir, &hash))
                    } else {
                        tracing::info!(
                            "Compiling {} {}.",
                            N::name(),
                            hash.to_string()
                        );
                        let code = wasm::run::prepare_wasm_code(code)?;
                        compile(code)
                    };
                }
            }
        }
    }

    /// Pre-compile a WASM module to a file. The compilation runs in a new OS
    /// thread and the function returns immediately.
    pub fn pre_compile(&mut self, code: impl AsRef<[u8]>) {
        if A::is_read_write() {
            let hash = hash_of_code(&code);
            let mut progress = self.progress.write().unwrap();
            match progress.get(&hash) {
                Some(_) => {
                    // Already known, do nothing
                }
                None => {
                    if module_file_exists(&self.dir, &hash) {
                        progress.insert(hash, Compilation::Done);
                        return;
                    }
                    progress.insert(hash, Compilation::Compiling);
                    drop(progress);
                    let progress = self.progress.clone();
                    let code = code.as_ref().to_vec();
                    let dir = self.dir.clone();
                    std::thread::spawn(move || {
                        tracing::info!("Compiling {}.", hash.to_string());

                        let (_module, _store) =
                            match wasm::run::prepare_wasm_code(code) {
                                Ok(code) => match compile(code) {
                                    Ok((module, store)) => {
                                        let mut progress =
                                            progress.write().unwrap();
                                        progress
                                            .insert(hash, Compilation::Done);
                                        file_write_module(&dir, &module, &hash);
                                        (module, store)
                                    }
                                    Err(err) => {
                                        let mut progress =
                                            progress.write().unwrap();
                                        tracing::info!(
                                            "Failed to compile WASM {} with {}",
                                            hash.to_string(),
                                            err
                                        );
                                        progress.remove(&hash);
                                        return Err(err);
                                    }
                                },
                                Err(err) => {
                                    let mut progress =
                                        progress.write().unwrap();
                                    tracing::info!(
                                        "Failed to prepare WASM {} with {}",
                                        hash.to_string(),
                                        err
                                    );
                                    progress.remove(&hash);
                                    return Err(err);
                                }
                            };

                        let res: Result<(), wasm::run::Error> = Ok(());
                        res
                    });
                }
            }
        }
    }

    /// Get a read-only cache handle.
    pub fn read_only(&self) -> Cache<N, WasmCacheRoAccess> {
        Cache {
            dir: self.dir.clone(),
            progress: self.progress.clone(),
            in_memory: self.in_memory.clone(),
            name: Default::default(),
            access: Default::default(),
        }
    }
}

fn exponential_backoff(iteration: u64) {
    sleep(Duration::from_millis(u64::pow(2, iteration as u32) * 10))
}

fn hash_of_code(code: impl AsRef<[u8]>) -> Hash {
    Hash::generate(code.as_ref())
}

fn hash_to_store_dir(hash: &Hash) -> PathBuf {
    PathBuf::from("vp_wasm_cache").join(hash.to_string())
}

fn compile(
    code: impl AsRef<[u8]>,
) -> Result<(Module, Store), wasm::run::Error> {
    // There's an issue with dylib compiler on mac in linker and on linux
    // with the dylib's store loading the dylib from a file, so we're caching a
    // module serialized to bytes instead for now.
    universal::compile(code).map_err(wasm::run::Error::CompileError)
}

fn file_ext() -> &'static str {
    // This has to be using the file_ext matching the compilation method in the
    // `fn compile`
    universal::FILE_EXT
}

fn store() -> Store {
    // This has to be using the store matching the compilation method in the
    // `fn compile`
    universal::store()
}

fn file_write_module(dir: impl AsRef<Path>, module: &Module, hash: &Hash) {
    use wasmer_cache::Cache;
    let mut fs_cache = fs_cache(dir, hash);
    fs_cache.store(*hash, module).unwrap();
}

fn file_load_module(dir: impl AsRef<Path>, hash: &Hash) -> (Module, Store) {
    use wasmer_cache::Cache;
    let fs_cache = fs_cache(dir, hash);
    let store = store();
    let module = unsafe { fs_cache.load(&store, *hash) }.unwrap();
    (module, store)
}

fn fs_cache(dir: impl AsRef<Path>, hash: &Hash) -> FileSystemCache {
    let path = dir.as_ref().join(hash_to_store_dir(hash));
    let mut fs_cache = FileSystemCache::new(path).unwrap();
    fs_cache.set_cache_extension(Some(file_ext()));
    fs_cache
}

fn module_file_exists(dir: impl AsRef<Path>, hash: &Hash) -> bool {
    let file = dir.as_ref().join(hash_to_store_dir(hash)).join(format!(
        "{}.{}",
        hash.to_string(),
        file_ext()
    ));
    file.exists()
}

/// A universal engine compilation. The module can be serialized to/from bytes.
mod universal {
    use super::*;

    #[allow(dead_code)]
    pub const FILE_EXT: &str = "bin";

    /// Compile wasm with a universal engine.
    #[allow(dead_code)]
    pub fn compile(
        code: impl AsRef<[u8]>,
    ) -> Result<(Module, Store), wasmer::CompileError> {
        let store = store();
        let module = Module::new(&store, code.as_ref())?;
        Ok((module, store))
    }

    /// Universal WASM store
    #[allow(dead_code)]
    pub fn store() -> Store {
        untrusted_wasm_store(memory::vp_limit())
    }
}

/// A dynamic library engine compilation.
mod dylib {
    use super::*;

    #[allow(dead_code)]
    #[cfg(windows)]
    pub const FILE_EXT: &str = "dll";
    #[allow(dead_code)]
    #[cfg(all(not(unix), target_os = "macos"))]
    pub const FILE_EXT: &str = "dylib";
    #[allow(dead_code)]
    #[cfg(all(unix, not(target_os = "macos")))]
    pub const FILE_EXT: &str = "so";

    /// Compile wasm to a dynamic library
    #[allow(dead_code)]
    pub fn compile(
        code: impl AsRef<[u8]>,
    ) -> Result<(Module, Store), wasmer::CompileError> {
        let store = store();
        let module = Module::new(&store, code.as_ref())?;
        Ok((module, store))
    }

    /// Dylib WASM store
    #[allow(dead_code)]
    pub fn store() -> Store {
        let compiler = wasmer_compiler_singlepass::Singlepass::default();
        let engine = wasmer_engine_dylib::Dylib::new(compiler).engine();
        Store::new_with_tunables(&engine, memory::vp_limit())
    }
}

/// Testing helpers
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use tempfile::{tempdir, TempDir};

    use super::*;
    use crate::vm::WasmCacheRwAccess;

    /// Cache with a temp dir for testing
    pub fn cache<N: CacheName>() -> (Cache<N, WasmCacheRwAccess>, TempDir) {
        let dir = tempdir().unwrap();
        let cache = Cache::new(
            dir.path(),
            50 * 1024 * 1024, // 50 MiB
        );
        (cache, dir)
    }
}

#[cfg(test)]
mod test {
    use std::cmp::max;

    use byte_unit::Byte;
    use tempfile::{tempdir, TempDir};
    use test_log::test;

    use super::*;
    use crate::vm::WasmCacheRwAccess;

    const TX_NO_OP: &str = "../wasm_for_tests/tx_no_op.wasm";
    const TX_READ_STORAGE_KEY: &str =
        "../wasm_for_tests/tx_read_storage_key.wasm";
    const VP_ALWAYS_TRUE: &str = "../wasm_for_tests/vp_always_true.wasm";
    const VP_EVAL: &str = "../wasm_for_tests/vp_eval.wasm";

    #[test]
    fn test_fetch_or_compile_valid_wasm() {
        // Load some WASMs and find their hashes and in-memory size
        let tx_read_storage_key = load_wasm(TX_READ_STORAGE_KEY);
        let tx_no_op = load_wasm(TX_NO_OP);

        // Create a new cache with the limit set to
        // `max(tx_read_storage_key.size, tx_no_op.size) + 1`
        {
            let max_bytes = max(tx_read_storage_key.size, tx_no_op.size) + 1;
            println!(
                "Using cache with max_bytes {} ({})",
                Byte::from_bytes(max_bytes as u128).get_appropriate_unit(true),
                max_bytes
            );
            let (mut cache, _tmp_dir) = cache(max_bytes);

            // Fetch `tx_read_storage_key`
            {
                let (_module, _store) =
                    cache.fetch_or_compile(&tx_read_storage_key.code).unwrap();

                let in_memory = cache.in_memory.read().unwrap();
                assert_matches!(
                    in_memory.peek(&tx_read_storage_key.hash),
                    Some(_),
                    "The module must be in memory"
                );

                let progress = cache.progress.read().unwrap();
                assert_matches!(
                    progress.get(&tx_read_storage_key.hash),
                    Some(Compilation::Done),
                    "The progress must be updated"
                );

                assert!(
                    module_file_exists(&cache.dir, &tx_read_storage_key.hash),
                    "The file must be written"
                );
            }

            // Fetch `tx_no_op`. Fetching another module should get us over the
            // limit, so the previous one should be popped from the cache
            {
                let (_module, _store) =
                    cache.fetch_or_compile(&tx_no_op.code).unwrap();

                let in_memory = cache.in_memory.read().unwrap();
                assert_matches!(
                    in_memory.peek(&tx_no_op.hash),
                    Some(_),
                    "The module must be in memory"
                );

                let progress = cache.progress.read().unwrap();
                assert_matches!(
                    progress.get(&tx_no_op.hash),
                    Some(Compilation::Done),
                    "The progress must be updated"
                );

                assert!(
                    module_file_exists(&cache.dir, &tx_no_op.hash),
                    "The file must be written"
                );

                // The previous module's file should still exist
                assert!(
                    module_file_exists(&cache.dir, &tx_read_storage_key.hash),
                    "The file must be written"
                );
                // But it should not be in-memory
                assert_matches!(
                    in_memory.peek(&tx_read_storage_key.hash),
                    None,
                    "The module should have been popped from memory"
                );
            }

            // Reset the in-memory cache and progress and fetch
            // `tx_read_storage_key` again, this time it should get loaded
            // from file
            let in_memory_cache = CLruCache::with_config(
                CLruCacheConfig::new(NonZeroUsize::new(max_bytes).unwrap())
                    .with_scale(ModuleCacheScale),
            );
            let in_memory = Arc::new(RwLock::new(in_memory_cache));
            cache.in_memory = in_memory;
            cache.progress = Default::default();
            {
                let (_module, _store) =
                    cache.fetch_or_compile(&tx_read_storage_key.code).unwrap();

                let in_memory = cache.in_memory.read().unwrap();
                assert_matches!(
                    in_memory.peek(&tx_read_storage_key.hash),
                    Some(_),
                    "The module must be in memory"
                );

                let progress = cache.progress.read().unwrap();
                assert_matches!(
                    progress.get(&tx_read_storage_key.hash),
                    Some(Compilation::Done),
                    "The progress must be updated"
                );

                assert!(
                    module_file_exists(&cache.dir, &tx_read_storage_key.hash),
                    "The file must be written"
                );

                // The previous module's file should still exist
                assert!(
                    module_file_exists(&cache.dir, &tx_no_op.hash),
                    "The file must be written"
                );
                // But it should not be in-memory
                assert_matches!(
                    in_memory.peek(&tx_no_op.hash),
                    None,
                    "The module should have been popped from memory"
                );
            }

            // Fetch `tx_read_storage_key` again, now it should be in-memory
            {
                let (_module, _store) =
                    cache.fetch_or_compile(&tx_read_storage_key.code).unwrap();

                let in_memory = cache.in_memory.read().unwrap();
                assert_matches!(
                    in_memory.peek(&tx_read_storage_key.hash),
                    Some(_),
                    "The module must be in memory"
                );

                let progress = cache.progress.read().unwrap();
                assert_matches!(
                    progress.get(&tx_read_storage_key.hash),
                    Some(Compilation::Done),
                    "The progress must be updated"
                );

                assert!(
                    module_file_exists(&cache.dir, &tx_read_storage_key.hash),
                    "The file must be written"
                );

                // The previous module's file should still exist
                assert!(
                    module_file_exists(&cache.dir, &tx_no_op.hash),
                    "The file must be written"
                );
                // But it should not be in-memory
                assert_matches!(
                    in_memory.peek(&tx_no_op.hash),
                    None,
                    "The module should have been popped from memory"
                );
            }

            // Fetch `tx_no_op` with read/only access
            {
                let mut cache = cache.read_only();

                // Fetching with read-only should not modify the in-memory cache
                let (_module, _store) =
                    cache.fetch_or_compile(&tx_no_op.code).unwrap();

                let in_memory = cache.in_memory.read().unwrap();
                assert_matches!(
                    in_memory.peek(&tx_no_op.hash),
                    None,
                    "The module should not be added back to in-memory cache"
                );

                let in_memory = cache.in_memory.read().unwrap();
                assert_matches!(
                    in_memory.peek(&tx_read_storage_key.hash),
                    Some(_),
                    "The previous module must still be in memory"
                );
            }
        }
    }

    #[test]
    fn test_fetch_or_compile_invalid_wasm() {
        // Some random bytes
        let invalid_wasm = vec![1_u8, 0, 8, 10, 6, 1];
        let hash = hash_of_code(&invalid_wasm);
        let (mut cache, _) = testing::cache::<TestCache>();

        // Try to compile it
        let error = cache
            .fetch_or_compile(&invalid_wasm)
            .expect_err("Compilation should fail");
        println!("Error: {}", error);

        let in_memory = cache.in_memory.read().unwrap();
        assert_matches!(
            in_memory.peek(&hash),
            None,
            "There should be no entry for this hash in memory"
        );

        let progress = cache.progress.read().unwrap();
        assert_matches!(progress.get(&hash), None, "Any progress is removed");

        assert!(
            !module_file_exists(&cache.dir, &hash),
            "The file must not be written"
        );
    }

    #[test]
    fn test_pre_compile_valid_wasm() {
        // Load some WASMs and find their hashes and in-memory size
        let vp_always_true = load_wasm(VP_ALWAYS_TRUE);
        let vp_eval = load_wasm(VP_EVAL);

        // Create a new cache with the limit set to
        // `max(vp_always_true.size, vp_eval.size) + 1 + extra_bytes`
        {
            let max_bytes = max(vp_always_true.size, vp_eval.size) + 1;
            println!(
                "Using cache with max_bytes {} ({})",
                Byte::from_bytes(max_bytes as u128).get_appropriate_unit(true),
                max_bytes
            );
            let (mut cache, _tmp_dir) = cache(max_bytes);

            // Pre-compile `vp_always_true`
            {
                cache.pre_compile(&vp_always_true.code);

                let progress = cache.progress.read().unwrap();
                assert_matches!(
                    progress.get(&vp_always_true.hash),
                    Some(Compilation::Done | Compilation::Compiling),
                    "The progress must be updated"
                );
            }

            // Now fetch it to wait for it finish compilation
            {
                let (_module, _store) =
                    cache.fetch_or_compile(&vp_always_true.code).unwrap();

                let in_memory = cache.in_memory.read().unwrap();
                assert_matches!(
                    in_memory.peek(&vp_always_true.hash),
                    Some(_),
                    "The module must be in memory"
                );

                let progress = cache.progress.read().unwrap();
                assert_matches!(
                    progress.get(&vp_always_true.hash),
                    Some(Compilation::Done),
                    "The progress must be updated"
                );

                assert!(
                    module_file_exists(&cache.dir, &vp_always_true.hash),
                    "The file must be written"
                );
            }

            // Pre-compile `vp_eval`. Pre-compiling another module should get us
            // over the limit, so the previous one should be popped
            // from the cache
            {
                cache.pre_compile(&vp_eval.code);

                let progress = cache.progress.read().unwrap();
                assert_matches!(
                    progress.get(&vp_eval.hash),
                    Some(Compilation::Done | Compilation::Compiling),
                    "The progress must be updated"
                );
            }

            // Now fetch it to wait for it finish compilation
            {
                let (_module, _store) =
                    cache.fetch_or_compile(&vp_eval.code).unwrap();

                let in_memory = cache.in_memory.read().unwrap();
                assert_matches!(
                    in_memory.peek(&vp_eval.hash),
                    Some(_),
                    "The module must be in memory"
                );

                assert!(
                    module_file_exists(&cache.dir, &vp_eval.hash),
                    "The file must be written"
                );

                // The previous module's file should still exist
                assert!(
                    module_file_exists(&cache.dir, &vp_always_true.hash),
                    "The file must be written"
                );
                // But it should not be in-memory
                assert_matches!(
                    in_memory.peek(&vp_always_true.hash),
                    None,
                    "The module should have been popped from memory"
                );
            }
        }
    }

    #[test]
    fn test_pre_compile_invalid_wasm() {
        // Some random bytes
        let invalid_wasm = vec![1_u8];
        let hash = hash_of_code(&invalid_wasm);
        let (mut cache, _) = testing::cache::<TestCache>();

        // Try to pre-compile it
        {
            cache.pre_compile(&invalid_wasm);
            let progress = cache.progress.read().unwrap();
            assert_matches!(
                progress.get(&hash),
                Some(Compilation::Done | Compilation::Compiling) | None,
                "The progress must be updated"
            );
        }

        // Now fetch it to wait for it finish compilation
        {
            let error = cache
                .fetch_or_compile(&invalid_wasm)
                .expect_err("Compilation should fail");
            println!("Error: {}", error);

            let in_memory = cache.in_memory.read().unwrap();
            assert_matches!(
                in_memory.peek(&hash),
                None,
                "There should be no entry for this hash in memory"
            );

            let progress = cache.progress.read().unwrap();
            assert_matches!(
                progress.get(&hash),
                None,
                "Any progress is removed"
            );

            assert!(
                !module_file_exists(&cache.dir, &hash),
                "The file must not be written"
            );
        }
    }

    /// Get the WASM code bytes, its hash and find the compiled module's size
    fn load_wasm(file: impl AsRef<str>) -> WasmWithMeta {
        // When `WeightScale` calls `loupe::size_of_val` in the cache, for some
        // reason it returns 8 bytes more than the same call in here.
        let extra_bytes = 8;

        let file = file.as_ref();
        let code = fs::read(file).unwrap();
        let hash = hash_of_code(&code);
        // Find the size of the compiled module
        let size = {
            let (mut cache, _tmp_dir) = cache(
                // No in-memory cache needed, but must be non-zero
                1,
            );
            let (module, _store) = cache.fetch_or_compile(&code).unwrap();
            loupe::size_of_val(&module) + HASH_BYTES + extra_bytes
        };
        println!(
            "Compiled module {} size including the hash: {} ({})",
            file,
            Byte::from_bytes(size as u128).get_appropriate_unit(true),
            size,
        );
        WasmWithMeta { code, hash, size }
    }

    /// A test helper for loading WASM and finding its hash and size
    #[derive(Clone, Debug)]
    struct WasmWithMeta {
        code: Vec<u8>,
        hash: Hash,
        /// Compiled module's in-memory size
        size: usize,
    }

    /// A `CacheName` implementation for unit tests
    #[derive(Clone, Debug)]
    struct TestCache;
    impl CacheName for TestCache {
        fn name() -> &'static str {
            "test"
        }
    }

    /// A cache with a temp dir for unit tests
    fn cache(
        max_bytes: usize,
    ) -> (Cache<TestCache, WasmCacheRwAccess>, TempDir) {
        let dir = tempdir().unwrap();
        let cache = Cache::new(dir.path(), max_bytes);
        (cache, dir)
    }
}
