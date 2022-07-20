//! Cryptographic signature keys

use anoma::types::address::Address;
pub use anoma::types::key::*;

use super::*;

/// Get the public key associated with the given address. Panics if not
/// found.
pub fn get(ctx: &Ctx, owner: &Address) -> EnvResult<Option<common::PublicKey>> {
    let key = pk_key(owner);
    ctx.read_pre(&key)
}
