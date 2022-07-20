use std::collections::HashSet;

use anoma::proto::Signed;
use anoma::types::intent;
pub use anoma::types::intent::*;
use anoma::types::key::*;

use super::*;
pub fn invalidate_exchange(
    ctx: &mut Ctx,
    intent: &Signed<Exchange>,
) -> TxResult {
    let key = intent::invalid_intent_key(&intent.data.addr);
    let mut invalid_intent: HashSet<common::Signature> =
        ctx.read(&key)?.unwrap_or_default();
    invalid_intent.insert(intent.sig.clone());
    ctx.write(&key, &invalid_intent)
}
