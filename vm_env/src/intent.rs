use std::collections::HashSet;

use anoma_shared::types::intent;
use anoma_shared::types::key::ed25519::{Signature, Signed};

/// Tx imports and functions.
pub mod tx {
    pub use anoma_shared::types::intent::*;

    use super::*;
    pub fn invalidate_intent(intent: &Signed<Intent>) {
        use crate::imports::tx;
        let key = intent::invalid_intent_key(&intent.data.addr);
        let mut invalid_intent: HashSet<Signature> =
            tx::read(&key.to_string()).unwrap_or_default();
        invalid_intent.insert(intent.sig.clone());
        tx::write(&key.to_string(), &invalid_intent)
    }
}

/// Vp imports and functions.
pub mod vp {
    pub use anoma_shared::types::intent::*;

    use super::*;

    pub fn vp(intent: &Signed<Intent>) -> bool {
        use crate::imports::vp;
        let key = intent::invalid_intent_key(&intent.data.addr);

        let invalid_intent_pre: HashSet<Signature> =
            vp::read_pre(&key.to_string()).unwrap_or_default();
        let invalid_intent_post: HashSet<Signature> =
            vp::read_post(&key.to_string()).unwrap_or_default();
        !invalid_intent_pre.contains(&intent.sig)
            && invalid_intent_post.contains(&intent.sig)
    }
}
