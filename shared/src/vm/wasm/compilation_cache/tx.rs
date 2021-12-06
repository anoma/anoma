//! Transaction WASM compilation cache

use super::common::{Cache, CacheName};

/// Tx WASM compilation cache handle. Thread-safe.
pub type TxCache<A> = Cache<Name, A>;

/// Tx cache name
#[derive(Debug, Clone)]
pub struct Name;

impl CacheName for Name {
    fn name() -> &'static str {
        "Tx"
    }
}
