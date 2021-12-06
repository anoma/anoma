//! VP WASM compilation cache

use super::common::{Cache, CacheName};

/// VP WASM compilation cache handle. Thread-safe.
pub type VpCache<A> = Cache<Name, A>;

/// VP cache name
#[derive(Debug, Clone)]
pub struct Name;

impl CacheName for Name {
    fn name() -> &'static str {
        "VP"
    }
}
