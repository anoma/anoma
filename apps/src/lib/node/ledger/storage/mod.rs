//! The storage module handles both the current state in-memory and the stored
//! state in DB.

mod rocksdb;

use std::collections::HashMap;
use std::fmt;
use std::path::Path;

use anoma::ledger::storage::types::MerkleTree;
use anoma::ledger::storage::{types, BlockStorage, Storage, StorageHasher};
use anoma::types::address::EstablishedAddressGen;
use anoma::types::chain::ChainId;
use anoma::types::storage::{BlockHash, BlockHeight, Epoch, Epochs, Key};
use anoma::types::time::DateTimeUtc;
use blake2b_rs::{Blake2b, Blake2bBuilder};
use sparse_merkle_tree::blake2b::Blake2bHasher;
use sparse_merkle_tree::traits::Hasher;
use sparse_merkle_tree::H256;

pub struct PersistentStorageHasher(Blake2bHasher);

pub type PersistentDB = rocksdb::RocksDB;

pub type PersistentStorage = Storage<PersistentDB, PersistentStorageHasher>;

pub fn open(db_path: impl AsRef<Path>, chain_id: ChainId) -> PersistentStorage {
    let block = BlockStorage {
        tree: MerkleTree::default(),
        hash: BlockHash::default(),
        height: BlockHeight::default(),
        epoch: Epoch::default(),
        pred_epochs: Epochs::default(),
        subspaces: HashMap::default(),
    };
    PersistentStorage {
        db: rocksdb::open(db_path).expect("cannot open the DB"),
        chain_id,
        block,
        header: None,
        last_height: BlockHeight(0),
        last_epoch: Epoch::default(),
        next_epoch_min_start_height: BlockHeight::default(),
        next_epoch_min_start_time: DateTimeUtc::now(),
        address_gen: EstablishedAddressGen::new(
            "Privacy is a function of liberty.",
        ),
    }
}

impl Default for PersistentStorageHasher {
    fn default() -> Self {
        Self(Blake2bHasher::default())
    }
}

impl Hasher for PersistentStorageHasher {
    fn write_h256(&mut self, h: &H256) {
        self.0.write_h256(h)
    }

    fn finish(self) -> H256 {
        self.0.finish()
    }
}

impl StorageHasher for PersistentStorageHasher {
    fn hash_key(key: &Key) -> H256 {
        let mut buf = [0u8; 32];
        let mut hasher = new_blake2b();
        hasher.update(&types::encode(key));
        hasher.finalize(&mut buf);
        buf.into()
    }

    fn hash_value(value: impl AsRef<[u8]>) -> H256 {
        let mut buf = [0u8; 32];
        let mut hasher = new_blake2b();
        hasher.update(value.as_ref());
        hasher.finalize(&mut buf);
        buf.into()
    }
}

impl fmt::Debug for PersistentStorageHasher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PersistentStorageHasher")
    }
}

fn new_blake2b() -> Blake2b {
    Blake2bBuilder::new(32).personal(b"anoma storage").build()
}

#[cfg(test)]
mod tests {
    use anoma::ledger::storage::types;
    use tempfile::TempDir;

    use super::*;

    #[test]
    fn test_crud_value() {
        let db_path =
            TempDir::new().expect("Unable to create a temporary DB directory");
        let mut storage = open(db_path.path(), ChainId::default());
        let key =
            Key::parse("key".to_owned()).expect("cannot parse the key string");
        let value: u64 = 1;
        let value_bytes = types::encode(&value);
        let value_bytes_len = value_bytes.len();

        // before insertion
        let (result, gas) = storage.has_key(&key).expect("has_key failed");
        assert!(!result);
        assert_eq!(gas, key.len() as u64);
        let (result, gas) = storage.read(&key).expect("read failed");
        assert_eq!(result, None);
        assert_eq!(gas, key.len() as u64);

        // insert
        storage.write(&key, value_bytes).expect("write failed");

        // read
        let (result, gas) = storage.has_key(&key).expect("has_key failed");
        assert!(result);
        assert_eq!(gas, key.len() as u64);
        let (result, gas) = storage.read(&key).expect("read failed");
        let read_value: u64 =
            types::decode(&result.expect("value doesn't exist"))
                .expect("decoding failed");
        assert_eq!(read_value, value);
        assert_eq!(gas, key.len() as u64 + value_bytes_len as u64);

        // delete
        storage.delete(&key).expect("delete failed");

        // read again
        let (result, _) = storage.has_key(&key).expect("has_key failed");
        assert!(!result);
        let (result, _) = storage.read(&key).expect("read failed");
        assert_eq!(result, None);
    }

    #[test]
    fn test_commit_block() {
        let db_path =
            TempDir::new().expect("Unable to create a temporary DB directory");
        let mut storage = open(db_path.path(), ChainId::default());
        storage
            .begin_block(BlockHash::default(), BlockHeight(100))
            .expect("begin_block failed");
        let key =
            Key::parse("key".to_owned()).expect("cannot parse the key string");
        let value: u64 = 1;
        let value_bytes = types::encode(&value);

        // insert and commit
        storage
            .write(&key, value_bytes.clone())
            .expect("write failed");
        storage.commit().expect("commit failed");

        // save the last state and drop the storage
        let root = storage.merkle_root().0;
        let hash = storage.get_block_hash().0;
        let address_gen = storage.address_gen.clone();
        drop(storage);

        // load the last state
        let mut storage = open(db_path.path(), ChainId::default());
        storage
            .load_last_state()
            .expect("loading the last state failed");
        let (loaded_root, height) =
            storage.get_state().expect("no block exists");
        assert_eq!(loaded_root.0, root);
        assert_eq!(height, 100);
        assert_eq!(storage.get_block_hash().0, hash);
        assert_eq!(storage.address_gen, address_gen);
        let (val, _) = storage.read(&key).expect("read failed");
        assert_eq!(val.expect("no value"), value_bytes);
    }

    #[test]
    fn test_iter() {
        let db_path =
            TempDir::new().expect("Unable to create a temporary DB directory");
        let mut storage = open(db_path.path(), ChainId::default());
        storage
            .begin_block(BlockHash::default(), BlockHeight(100))
            .expect("begin_block failed");

        let mut expected = Vec::new();
        let prefix = Key::parse("prefix".to_owned())
            .expect("cannot parse the key string");
        for i in (0..9).rev() {
            let key = prefix
                .push(&format!("{}", i))
                .expect("cannot push the key segment");
            let value_bytes = types::encode(&(i as u64));
            // insert
            storage
                .write(&key, value_bytes.clone())
                .expect("write failed");
            expected.push((key.to_string(), value_bytes));
        }
        storage.commit().expect("commit failed");

        let (iter, gas) = storage.iter_prefix(&prefix);
        assert_eq!(gas, prefix.len() as u64);
        for (k, v, gas) in iter {
            match expected.pop() {
                Some((expected_key, expected_val)) => {
                    assert_eq!(k, expected_key);
                    assert_eq!(v, expected_val);
                    let expected_gas = expected_key.len() + expected_val.len();
                    assert_eq!(gas, expected_gas as u64);
                }
                None => panic!("read a pair though no expected pair"),
            }
        }
    }

    #[test]
    fn test_validity_predicate() {
        let db_path =
            TempDir::new().expect("Unable to create a temporary DB directory");
        let mut storage = open(db_path.path(), ChainId::default());
        storage
            .begin_block(BlockHash::default(), BlockHeight(100))
            .expect("begin_block failed");

        let addr = storage.address_gen.generate_address("test".as_bytes());
        let key = Key::validity_predicate(&addr);

        // not exist
        let (vp, gas) =
            storage.validity_predicate(&addr).expect("VP load failed");
        assert_eq!(vp, None);
        assert_eq!(gas, key.len() as u64);

        // insert
        let vp1 = "vp1".as_bytes().to_vec();
        storage.write(&key, vp1.clone()).expect("write failed");

        // check
        let (vp, gas) =
            storage.validity_predicate(&addr).expect("VP load failed");
        assert_eq!(vp.expect("no VP"), vp1);
        assert_eq!(gas, (key.len() + vp1.len()) as u64);
    }
}
