use super::ADDRESS;
use crate::types::storage::{Key, KeySeg};

const QUEUE_STORAGE_KEY: &str = "queue";

pub fn queue_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&QUEUE_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}
