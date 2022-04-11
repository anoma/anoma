//! Parameters storage
use super::ADDRESS;
use crate::types::storage::{DbKeySeg, Key};

const EPOCH_DURATION_KEY: &str = "epoch_duration";
const VP_WHITELIST_KEY: &str = "vp_whitelist";
const TX_WHITELIST_KEY: &str = "tx_whitelist";
const MAX_EXPECTED_TIME_PER_BLOCK_KEY: &str = "max_expected_time_per_block";

/// Storage key used for epoch parameter.
pub fn get_epoch_storage_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ADDRESS),
            DbKeySeg::StringSeg(EPOCH_DURATION_KEY.to_string()),
        ],
    }
}

/// Storage key used for vp whitelist parameter.
pub fn get_vp_whitelist_storage_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ADDRESS),
            DbKeySeg::StringSeg(VP_WHITELIST_KEY.to_string()),
        ],
    }
}

/// Storage key used for tx whitelist parameter.
pub fn get_tx_whitelist_storage_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ADDRESS),
            DbKeySeg::StringSeg(TX_WHITELIST_KEY.to_string()),
        ],
    }
}

/// Storage key used for tx whitelist parameter.
pub fn get_max_expected_time_per_block_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ADDRESS),
            DbKeySeg::StringSeg(MAX_EXPECTED_TIME_PER_BLOCK_KEY.to_string()),
        ],
    }
}
