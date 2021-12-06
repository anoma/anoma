//! VP Wasm compilation cache

use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::thread::sleep;
use std::time::Duration;
use std::{cmp, fs};

use clru::{CLruCache, CLruCacheConfig, WeightScale};
use wasmer::{Module, Store};
use wasmer_cache::{Cache, FileSystemCache, Hash};

use crate::vm::wasm::memory;
use crate::vm::wasm::run::untrusted_wasm_store;

/// VP cache handle allows concurrent access to the cache
#[derive(Debug, Clone)]
pub struct VpCache {
    /// Cached files directory
    dir: PathBuf,
    /// Compilation progress
    progress: Arc<Mutex<HashMap<Hash, Compilation>>>,
    /// In-memory LRU cache of compiled VP modules
    in_memory: Arc<Mutex<MemoryCache>>,
}

/// In-memory LRU cache of compiled VP modules
type MemoryCache =
    CLruCache<Hash, (Module, Store), RandomState, VpModuleCacheScale>;

impl VpCache {
    /// Create a VP wasm in-memory cache with a given size limit and a file
    /// system cache.
    pub fn new(dir: impl Into<PathBuf>, max_bytes: usize) -> Self {
        let cache = CLruCache::with_config(
            CLruCacheConfig::new(NonZeroUsize::new(max_bytes).unwrap())
                .with_scale(VpModuleCacheScale),
        );
        let modules = Arc::new(Mutex::new(cache));
        Self {
            dir: dir.into(),
            progress: Default::default(),
            in_memory: modules,
        }
    }
}

#[derive(Debug)]
enum Compilation {
    Compiling,
    Done,
}

/// Configures the cache scale of VP modules that limits the maximum capacity
/// of the cache (CLruCache::len + CLruCache::weight <= CLruCache::capacity).
#[derive(Debug)]
struct VpModuleCacheScale;

impl WeightScale<Hash, (Module, Store)> for VpModuleCacheScale {
    fn weight(&self, _key: &Hash, value: &(Module, Store)) -> usize {
        // We only want to limit the max memory size, not the number of
        // elements, so we use the size of the module and the store as its scale
        // and subtract 1 from it to negate the increment of the cache
        // length.
        cmp::max(
            1,
            loupe::size_of_val(&value.0) + loupe::size_of_val(&value.1),
        ) - 1
    }
}

impl VpCache {
    /// Fetch a WASM module from LRU cache, from a file or compile it and cache
    /// it. Updates the position in the LRU cache.
    pub fn fetch_or_compile(
        &mut self,
        code: impl AsRef<[u8]>,
    ) -> Result<(Module, Store), super::run::Error> {
        let hash = hash_of_code(&code);

        let mut in_memory = self.in_memory.lock().unwrap();
        if let Some((module, store)) = in_memory.get(&hash) {
            tracing::info!("Found {} in cache.", hash.to_string());
            return Ok((module.clone(), store.clone()));
        }
        drop(in_memory);

        let mut iter = 0;
        loop {
            let mut progress = self.progress.lock().unwrap();
            match progress.get(&hash) {
                Some(Compilation::Done) => {
                    drop(progress);
                    tracing::info!("Found {} in cache.", hash.to_string());
                    let mut in_memory = self.in_memory.lock().unwrap();
                    if let Some((module, store)) = in_memory.get(&hash) {
                        tracing::info!("Found {} in cache.", hash.to_string());
                        return Ok((module.clone(), store.clone()));
                    }

                    let (module, store) = file_load_module(&self.dir, &hash);
                    // Put into cache, ignore result if it's full
                    let _ = in_memory
                        .put_with_weight(hash, (module.clone(), store.clone()));

                    return Ok((module, store));
                }
                Some(Compilation::Compiling) => {
                    drop(progress);
                    tracing::info!("Waiting for {} ...", hash.to_string());
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
                                "Loading {} from file.",
                                hash.to_string()
                            );
                            file_load_module(&self.dir, &hash)
                        } else {
                            tracing::info!("Compiling {}.", hash.to_string());
                            let code = super::run::prepare_wasm_code(code)?;
                            let (module, store) = compile(code)?;
                            file_write_module(&self.dir, &module, &hash);
                            (module, store)
                        };

                    let mut progress = self.progress.lock().unwrap();
                    progress.insert(hash, Compilation::Done);

                    let mut in_memory = self.in_memory.lock().unwrap();
                    // Put into cache, ignore result if it's full
                    let _ = in_memory
                        .put_with_weight(hash, (module.clone(), store.clone()));

                    return Ok((module, store));
                }
            }
        }
    }

    /// Pre-compile a WASM module to a file. The compilation runs in a new OS
    /// thread and the function returns immediately.
    pub fn pre_compile(&mut self, code: impl AsRef<[u8]>) {
        let hash = hash_of_code(&code);
        let mut progress = self.progress.lock().unwrap();
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
                    let code = super::run::prepare_wasm_code(code)?;
                    let (module, _store) = compile(code)?;
                    file_write_module(dir, &module, &hash);
                    let mut progress = progress.lock().unwrap();
                    progress.insert(hash, Compilation::Done);
                    let res: Result<(), super::run::Error> = Ok(());
                    res
                });
            }
        }
    }
}

fn exponential_backoff(iteration: u64) {
    sleep(Duration::from_millis((2 ^ iteration) * 10))
}

fn hash_of_code(code: impl AsRef<[u8]>) -> Hash {
    Hash::generate(code.as_ref())
}

fn hash_to_store_dir(hash: &Hash) -> PathBuf {
    PathBuf::from("vp_wasm_cache").join(hash.to_string())
}

fn compile(
    code: impl AsRef<[u8]>,
) -> Result<(Module, Store), super::run::Error> {
    #[cfg(target_os = "macos")]
    {
        // There's an issue with dylib compiler on mac, so we're caching a
        // module serialized to bytes instead.
        universal::compile(code).map_err(super::run::Error::CompileError)
    }
    #[cfg(not(target_os = "macos"))]
    {
        dylib::compile(code).map_err(super::run::Error::CompileError)
    }
}

fn file_ext() -> &'static str {
    // This has to be using the file_ext matching the compilation method in the
    // `fn compile`
    #[cfg(target_os = "macos")]
    {
        universal::FILE_EXT
    }
    #[cfg(not(target_os = "macos"))]
    {
        dylib::FILE_EXT
    }
}

fn file_write_module(dir: impl AsRef<Path>, module: &Module, hash: &Hash) {
    fs::create_dir_all(&dir)
        .expect("Couldn't create the VP wasm cache directory");
    let mut fs_cache = fs_cache(dir, hash);
    fs_cache.store(*hash, module).unwrap();
}

fn file_load_module(dir: impl AsRef<Path>, hash: &Hash) -> (Module, Store) {
    let fs_cache = fs_cache(dir, hash);
    let store = untrusted_wasm_store(memory::vp_limit());
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

    pub const FILE_EXT: &str = "bin";

    /// Compile wasm with a universal engine.
    #[allow(dead_code)]
    pub fn compile(
        code: impl AsRef<[u8]>,
    ) -> Result<(Module, Store), wasmer::CompileError> {
        let store = untrusted_wasm_store(memory::vp_limit());
        let module = Module::new(&store, code.as_ref())?;
        Ok((module, store))
    }
}

/// A dynamic library engine compilation.
mod dylib {
    use super::*;

    #[cfg(windows)]
    pub const FILE_EXT: &str = "dll";
    #[cfg(all(not(unix), target_os = "macos"))]
    pub const FILE_EXT: &str = "dylib";
    #[cfg(all(unix, not(target_os = "macos")))]
    pub const FILE_EXT: &str = "so";

    /// Compile wasm to a dynamic library
    #[allow(dead_code)]
    pub fn compile(
        code: impl AsRef<[u8]>,
    ) -> Result<(Module, Store), wasmer::CompileError> {
        let compiler = wasmer_compiler_singlepass::Singlepass::default();
        let engine = wasmer_engine_dylib::Dylib::new(compiler).engine();
        // let store = Store::new_with_tunables(&engine, memory::vp_limit());
        let store = Store::new(&engine);
        let module = Module::new(&store, code.as_ref())?;
        Ok((module, store))
    }
}

/// Testing helpers
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use tempfile::{tempdir, TempDir};

    use super::*;

    /// VP Cache with a temp dir for testing
    pub fn vp_cache() -> (VpCache, TempDir) {
        let dir = tempdir().unwrap();
        let cache = VpCache::new(
            dir.path(),
            50 * 1024 * 1024, // 50 MiB
        );
        (cache, dir)
    }
}
