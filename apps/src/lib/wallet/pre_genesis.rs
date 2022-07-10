use std::fs;
use std::path::{Path, PathBuf};

use anoma::types::key::common;
use ark_serialize::{Read, Write};
use file_lock::{FileLock, FileOptions};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::wallet;
use crate::wallet::{store, StoredKeypair};

/// Validator pre-genesis wallet file name
const VALIDATOR_FILE_NAME: &str = "wallet.toml";

#[derive(Error, Debug)]
pub enum ReadError {
    #[error("Failed decoding the wallet store: {0}")]
    Decode(toml::de::Error),
    #[error("Failed to read the wallet store from {0}: {1}")]
    ReadWallet(String, String),
    #[error("Failed to write the wallet store: {0}")]
    StoreNewWallet(String),
    #[error("Failed to decode a key: {0}")]
    Decryption(wallet::keys::DecryptionError),
}

/// Get the path to the validator pre-genesis wallet store.
pub fn validator_file_name(store_dir: impl AsRef<Path>) -> PathBuf {
    store_dir.as_ref().join(VALIDATOR_FILE_NAME)
}

/// Validator pre-genesis wallet includes all the required keys for genesis
/// setup and a cache of decrypted keys.
pub struct ValidatorWallet {
    /// The wallet store that can be written/read to/from TOML
    pub store: ValidatorStore,
    /// Cryptographic keypair for validator account key
    pub account_key: common::SecretKey,
    /// Cryptographic keypair for consensus key
    pub consensus_key: common::SecretKey,
    /// Cryptographic keypair for rewards key
    pub rewards_key: common::SecretKey,
    /// Cryptographic keypair for Tendermint node key
    pub tendermint_node_key: common::SecretKey,
}

/// Validator pre-genesis wallet store includes all the required keys for
/// genesis setup.
#[derive(Serialize, Deserialize, Debug)]
pub struct ValidatorStore {
    /// Cryptographic keypair for validator account key
    pub account_key: wallet::StoredKeypair<common::SecretKey>,
    /// Cryptographic keypair for consensus key
    pub consensus_key: wallet::StoredKeypair<common::SecretKey>,
    /// Cryptographic keypair for rewards key
    pub rewards_key: wallet::StoredKeypair<common::SecretKey>,
    /// Cryptographic keypair for Tendermint node key
    pub tendermint_node_key: wallet::StoredKeypair<common::SecretKey>,
    /// Special validator keys
    pub validator_keys: wallet::ValidatorKeys,
}

impl ValidatorWallet {
    /// Generate a new [`ValidatorWallet`] with required pre-genesis keys and
    /// store it as TOML at the given path.
    pub fn gen_and_store(
        unsafe_dont_encrypt: bool,
        store_dir: &Path,
    ) -> std::io::Result<Self> {
        let validator = Self::gen(unsafe_dont_encrypt);
        let data = validator.store.encode();
        let wallet_path = validator_file_name(store_dir);
        // Make sure the dir exists
        let wallet_dir = wallet_path.parent().unwrap();
        fs::create_dir_all(wallet_dir)?;
        // Write the file
        let options =
            FileOptions::new().create(true).write(true).truncate(true);
        let mut filelock =
            FileLock::lock(wallet_path.to_str().unwrap(), true, options)?;
        filelock.file.write_all(&data)?;
        Ok(validator)
    }

    /// Try to load and decrypt keys, if encrypted, in a [`ValidatorWallet`]
    /// from a TOML file.
    pub fn load(store_dir: &Path) -> Result<Self, ReadError> {
        let wallet_file = validator_file_name(store_dir);
        match FileLock::lock(
            wallet_file.to_str().unwrap(),
            true,
            FileOptions::new().read(true).write(false),
        ) {
            Ok(mut filelock) => {
                let mut store = Vec::<u8>::new();
                filelock.file.read_to_end(&mut store).map_err(|err| {
                    ReadError::ReadWallet(
                        store_dir.to_str().unwrap().into(),
                        err.to_string(),
                    )
                })?;
                let store =
                    ValidatorStore::decode(store).map_err(ReadError::Decode)?;

                let password = if store.account_key.is_encrypted()
                    || store.consensus_key.is_encrypted()
                    || store.rewards_key.is_encrypted()
                    || store.account_key.is_encrypted()
                {
                    Some(wallet::read_password("Enter decryption password: "))
                } else {
                    None
                };

                let account_key =
                    store.account_key.get(true, password.clone())?;
                let consensus_key =
                    store.consensus_key.get(true, password.clone())?;
                let rewards_key =
                    store.rewards_key.get(true, password.clone())?;
                let tendermint_node_key =
                    store.tendermint_node_key.get(true, password)?;

                Ok(Self {
                    store,
                    account_key,
                    consensus_key,
                    rewards_key,
                    tendermint_node_key,
                })
            }
            Err(err) => Err(ReadError::ReadWallet(
                wallet_file.to_string_lossy().into_owned(),
                err.to_string(),
            )),
        }
    }

    /// Generate a new [`Validator`] with required pre-genesis keys. Will prompt
    /// for password when `!unsafe_dont_encrypt`.
    fn gen(unsafe_dont_encrypt: bool) -> Self {
        let password = wallet::read_and_confirm_pwd(unsafe_dont_encrypt);
        let (account_key, account_sk) = gen_key_to_store(&password);
        let (consensus_key, consensus_sk) = gen_key_to_store(&password);
        let (rewards_key, rewards_sk) = gen_key_to_store(&password);
        let (tendermint_node_key, tendermint_node_sk) =
            gen_key_to_store(&password);
        let validator_keys = store::Store::gen_validator_keys(None);
        let store = ValidatorStore {
            account_key,
            consensus_key,
            rewards_key,
            tendermint_node_key,
            validator_keys,
        };
        Self {
            store,
            account_key: account_sk,
            consensus_key: consensus_sk,
            rewards_key: rewards_sk,
            tendermint_node_key: tendermint_node_sk,
        }
    }
}

impl ValidatorStore {
    /// Decode from TOML string bytes
    pub fn decode(data: Vec<u8>) -> Result<Self, toml::de::Error> {
        toml::from_slice(&data)
    }

    /// Encode in TOML string bytes
    pub fn encode(&self) -> Vec<u8> {
        toml::to_vec(self).expect(
            "Serializing of validator pre-genesis wallet shouldn't fail",
        )
    }
}

fn gen_key_to_store(
    password: &Option<String>,
) -> (StoredKeypair<common::SecretKey>, common::SecretKey) {
    let sk = store::gen_sk();
    StoredKeypair::new(sk, password.clone())
}

impl From<wallet::keys::DecryptionError> for ReadError {
    fn from(err: wallet::keys::DecryptionError) -> Self {
        ReadError::Decryption(err)
    }
}
