//! A VP for a fungible token. Enforces that the total supply is unchanged in a
//! transaction that moves balance(s).

use anoma_vp_prelude::*;

#[validity_predicate]
fn validate_tx(
    ctx: &Ctx,
    _tx_data: Vec<u8>,
    addr: Address,
    keys_changed: BTreeSet<storage::Key>,
    verifiers: BTreeSet<Address>,
) -> VpResult {
    debug_log!(
        "validate_tx called with token addr: {}, key_changed: {:?}, \
         verifiers: {:?}",
        addr,
        keys_changed,
        verifiers
    );

    if !is_tx_whitelisted(ctx)? {
        return reject();
    }

    for key in keys_changed.iter() {
        if key.is_validity_predicate().is_some() {
            let vp: Vec<u8> = ctx.read_bytes_post(key)?.unwrap();
            if !is_vp_whitelisted(ctx, &vp)? {
                return reject();
            }
        }
    }

    token::vp(ctx, &addr, &keys_changed, &verifiers)
}
