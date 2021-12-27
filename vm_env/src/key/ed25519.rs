use anoma::types::address::Address;
use anoma::types::key::ed25519;

/// Vp imports and functions.
pub mod vp {
    pub use anoma::types::key::ed25519::*;

    use super::*;
    use crate::imports::vp;

    /// Get the public key associated with the given address. Panics if not
    /// found.
    pub fn get(owner: &Address) -> Option<PublicKey> {
        let key = ed25519::pk_key(owner).to_string();
        vp::read_pre(&key)
    }

    /// Get the public protocol key associated with the given address
    /// if it exists.
    pub fn get_protocol(owner: &Address) -> Option<PublicKey> {
        let key = ed25519::protocol_pk_key(owner).to_string();
        if vp::has_key_pre(&key) {
            vp::read_pre(&key)
        } else {
            None
        }
    }
}
