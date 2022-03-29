use super::ADDRESS;
use crate::types::storage::{DbKeySeg, Key, KeySeg};

const MAX_TRANSFERABLE_FUND_KEY: &str = "max_fund";

/// Check if a key is a treasury key
pub fn is_treasury_key(key: &Key) -> bool {
    matches!(&key.segments[0], DbKeySeg::AddressSeg(addr) if addr == &ADDRESS)
}

/// Check if key is max funds transfer key
pub fn is_max_funds_transfer_key(key: &Key) -> bool {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(max_fund)]
            if addr == &ADDRESS && max_fund == MAX_TRANSFERABLE_FUND_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Get key of max funds transfer parameter
pub fn get_max_transferable_fund_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&MAX_TRANSFERABLE_FUND_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}
