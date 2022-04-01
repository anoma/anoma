//! Parameters storage
use super::ADDRESS;
use crate::types::storage::{DbKeySeg, Key};

const EPOCH_DURATION_KEY: &str = "epoch_duration";
const VP_WHITELIST_KEY: &str = "vp_whitelist";
const TX_WHITELIST_KEY: &str = "tx_whitelist";
const MAX_EXPECTED_TIME_PER_BLOCK_KEY: &str = "max_expected_time_per_block";

/// Returns if the key is a parameter key.
pub fn is_parameter_key(key: &Key) -> bool {
    matches!(&key.segments[0], DbKeySeg::AddressSeg(addr) if addr == &ADDRESS)
}

/// Returns if the key is a protocol parameter key.
pub fn is_protocol_parameter_key(key: &Key) -> bool {
    is_epoch_storage_key(key)
        || is_max_expected_time_per_block_key(key)
        || is_tx_whitelist_key(key)
        || is_vp_whitelist_key(key)
}

/// Returns if the key is an epoch storage key.
pub fn is_epoch_storage_key(key: &Key) -> bool {
    matches!(&key.segments[..], [
        DbKeySeg::AddressSeg(addr),
        DbKeySeg::StringSeg(epoch_duration),
    ] if addr == &ADDRESS && epoch_duration == EPOCH_DURATION_KEY)
}

/// Returns if the key is the max_expected_time_per_block key.
pub fn is_max_expected_time_per_block_key(key: &Key) -> bool {
    matches!(&key.segments[..], [
        DbKeySeg::AddressSeg(addr),
        DbKeySeg::StringSeg(max_expected_time_per_block),
    ] if addr == &ADDRESS && max_expected_time_per_block == MAX_EXPECTED_TIME_PER_BLOCK_KEY)
}

/// Returns if the key is the tx_whitelist key.
pub fn is_tx_whitelist_key(key: &Key) -> bool {
    matches!(&key.segments[..], [
        DbKeySeg::AddressSeg(addr),
        DbKeySeg::StringSeg(tx_whitelist),
    ] if addr == &ADDRESS && tx_whitelist == TX_WHITELIST_KEY)
}

/// Returns if the key is the vp_whitelist key.
pub fn is_vp_whitelist_key(key: &Key) -> bool {
    matches!(&key.segments[..], [
        DbKeySeg::AddressSeg(addr),
        DbKeySeg::StringSeg(vp_whitelist),
    ] if addr == &ADDRESS && vp_whitelist == VP_WHITELIST_KEY)
}

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
