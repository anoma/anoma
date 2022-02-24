pub mod defaults;
mod keys;
mod store;

use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use anoma::types::address::Address;
use anoma::types::key::*;
pub use store::wallet_file;
use thiserror::Error;

pub use self::keys::{DecryptionError, StoredKeypair};
use self::store::{Alias, Store};
use crate::cli;
use crate::config::genesis::genesis_config::GenesisConfig;
use crate::std::fs;

#[derive(Debug)]
pub struct Wallet {
    store_dir: PathBuf,
    store: Store,
    decrypted_key_cache: HashMap<Alias, Rc<common::SecretKey>>,
}

#[derive(Error, Debug)]
pub enum FindKeyError {
    #[error("No matching key found")]
    KeyNotFound,
    #[error("{0}")]
    KeyDecryptionError(keys::DecryptionError),
}

impl Wallet {
    /// Load a wallet from the store file or create a new wallet without any
    /// keys or addresses.
    pub fn load_or_new(store_dir: &Path) -> Self {
        let store = Store::load_or_new(store_dir).unwrap_or_else(|err| {
            eprintln!("Unable to load the wallet: {}", err);
            cli::safe_exit(1)
        });
        Self {
            store_dir: store_dir.to_path_buf(),
            store,
            decrypted_key_cache: HashMap::default(),
        }
    }

    /// Load a wallet from the store file or create a new one with the default
    /// addresses loaded from the genesis file, if not found.
    pub fn load_or_new_from_genesis(
        store_dir: &Path,
        load_genesis: impl FnOnce() -> GenesisConfig,
    ) -> Self {
        let store = Store::load_or_new_from_genesis(store_dir, load_genesis)
            .unwrap_or_else(|err| {
                eprintln!("Unable to load the wallet: {}", err);
                cli::safe_exit(1)
            });
        Self {
            store_dir: store_dir.to_path_buf(),
            store,
            decrypted_key_cache: HashMap::default(),
        }
    }

    /// Add addresses from a genesis configuration.
    pub fn add_genesis_addresses(&mut self, genesis: GenesisConfig) {
        self.store.add_genesis_addresses(genesis)
    }

    /// Save the wallet store to a file.
    pub fn save(&self) -> std::io::Result<()> {
        self.store.save(&self.store_dir)
    }

    /// Generate a new keypair and derive an implicit address from its public
    /// and insert them into the store with the provided alias. If none
    /// provided, the alias will be the public key hash. If the key is to be
    /// encrypted, will prompt for password from stdin. Stores the key in
    /// decrypted key cache and returns the alias of the key and a
    /// reference-counting pointer to the key.
    pub fn gen_key(
        &mut self,
        alias: Option<String>,
        unsafe_dont_encrypt: bool,
    ) -> (String, Rc<common::SecretKey>) {
        let password = if unsafe_dont_encrypt {
            println!("Warning: The keypair will NOT be encrypted.");
            None
        } else {
            Some(read_password("Enter your encryption password: "))
        };
        // Bis repetita for confirmation.
        let pwd = if unsafe_dont_encrypt {
            None
        } else {
            Some(read_password(
                "To confirm, please enter the same encryption password once \
                 more: ",
            ))
        };
        if pwd != password {
            eprintln!("Your two inputs do not match!");
            cli::safe_exit(1)
        }
        let (alias, key) = self.store.gen_key(alias, password);
        // Cache the newly added key
        self.decrypted_key_cache.insert(alias.clone(), key.clone());
        (alias, key)
    }

    /// Find the stored key by an alias, a public key hash or a public key.
    /// If the key is encrypted, will prompt for password from stdin.
    /// Any keys that are decrypted are stored in and read from a cache to avoid
    /// prompting for password multiple times.
    pub fn find_key(
        &mut self,
        alias_pkh_or_pk: impl AsRef<str>,
    ) -> Result<Rc<common::SecretKey>, FindKeyError> {
        // Try cache first
        if let Some(cached_key) =
            self.decrypted_key_cache.get(alias_pkh_or_pk.as_ref())
        {
            return Ok(cached_key.clone());
        }
        // If not cached, look-up in store
        let stored_key = self
            .store
            .find_key(alias_pkh_or_pk.as_ref())
            .ok_or(FindKeyError::KeyNotFound)?;
        Self::decrypt_stored_key(
            &mut self.decrypted_key_cache,
            stored_key,
            alias_pkh_or_pk,
        )
    }

    /// Find the stored key by a public key.
    /// If the key is encrypted, will prompt for password from stdin.
    /// Any keys that are decrypted are stored in and read from a cache to avoid
    /// prompting for password multiple times.
    pub fn find_key_by_pk(
        &mut self,
        pk: &common::PublicKey,
    ) -> Result<Rc<common::SecretKey>, FindKeyError> {
        // Try to look-up alias for the given pk. Otherwise, use the PKH string.
        let pkh: PublicKeyHash = pk.into();
        let alias = self
            .store
            .find_alias_by_pkh(&pkh)
            .unwrap_or_else(|| pkh.to_string());
        // Try read cache
        if let Some(cached_key) = self.decrypted_key_cache.get(&alias) {
            return Ok(cached_key.clone());
        }
        // Look-up from store
        let stored_key = self
            .store
            .find_key_by_pk(pk)
            .ok_or(FindKeyError::KeyNotFound)?;
        Self::decrypt_stored_key(
            &mut self.decrypted_key_cache,
            stored_key,
            alias,
        )
    }

    /// Find the stored key by a public key hash.
    /// If the key is encrypted, will prompt for password from stdin.
    /// Any keys that are decrypted are stored in and read from a cache to avoid
    /// prompting for password multiple times.
    pub fn find_key_by_pkh(
        &mut self,
        pkh: &PublicKeyHash,
    ) -> Result<Rc<common::SecretKey>, FindKeyError> {
        // Try to look-up alias for the given pk. Otherwise, use the PKH string.
        let alias = self
            .store
            .find_alias_by_pkh(pkh)
            .unwrap_or_else(|| pkh.to_string());
        // Try read cache
        if let Some(cached_key) = self.decrypted_key_cache.get(&alias) {
            return Ok(cached_key.clone());
        }
        // Look-up from store
        let stored_key = self
            .store
            .find_key_by_pkh(pkh)
            .ok_or(FindKeyError::KeyNotFound)?;
        Self::decrypt_stored_key(
            &mut self.decrypted_key_cache,
            stored_key,
            alias,
        )
    }

    /// Decrypt stored key, if it's not stored un-encrypted.
    /// If a given storage key needs to be decrypted, prompt for password from
    /// stdin and if successfully decrypted, store it in a cache.
    fn decrypt_stored_key(
        decrypted_key_cache: &mut HashMap<String, Rc<common::SecretKey>>,
        stored_key: &StoredKeypair,
        alias_pkh_or_pk: impl AsRef<str>,
    ) -> Result<Rc<common::SecretKey>, FindKeyError> {
        match stored_key {
            StoredKeypair::Encrypted(encrypted) => {
                let password = read_password("Enter decryption password: ");
                let key = encrypted
                    .decrypt(password)
                    .map_err(FindKeyError::KeyDecryptionError)?;
                let alias = alias_pkh_or_pk.as_ref().to_owned();
                decrypted_key_cache.insert(alias.clone(), Rc::new(key));
                decrypted_key_cache
                    .get(&alias)
                    .cloned()
                    .ok_or(FindKeyError::KeyNotFound)
            }
            StoredKeypair::Raw(raw) => Ok(raw.clone()),
        }
    }

    /// Get all known keys by their alias, paired with PKH, if known.
    pub fn get_keys(
        &self,
    ) -> HashMap<Alias, (&StoredKeypair, Option<&PublicKeyHash>)> {
        self.store.get_keys()
    }

    /// Find the stored address by an alias.
    pub fn find_address(&self, alias: impl AsRef<str>) -> Option<&Address> {
        self.store.find_address(alias)
    }

    /// Get all known addresses by their alias, paired with PKH, if known.
    pub fn get_addresses(&self) -> &HashMap<Alias, Address> {
        self.store.get_addresses()
    }

    /// Add a new address with the given alias. If the alias is already used,
    /// will ask whether the existing alias should be replaced, a different
    /// alias is desired, or the alias creation should be cancelled. Return
    /// the chosen alias if the address has been added, otherwise return
    /// nothing.
    pub fn add_address(
        &mut self,
        alias: Alias,
        address: Address,
    ) -> Option<Alias> {
        self.store.insert_address(alias, address)
    }

    /// Insert a new key with the given alias. If the alias is already used,
    /// will prompt for overwrite confirmation.
    pub fn insert_keypair(
        &mut self,
        alias: Alias,
        keypair: StoredKeypair,
        pkh: PublicKeyHash,
    ) -> Option<Alias> {
        self.store.insert_keypair(alias, keypair, pkh)
    }
}

/// Read the password for encryption/decryption from the file/env/stdin. Panics
/// if all options are empty/invalid.
fn read_password(prompt_msg: &str) -> String {
    let pwd = match env::var("ANOMA_WALLET_PASSWORD_FILE") {
        Ok(path) => fs::read_to_string(path)
            .expect("Something went wrong reading the file"),
        Err(_) => match env::var("ANOMA_WALLET_PASSWORD") {
            Ok(password) => password,
            Err(_) => rpassword::read_password_from_tty(Some(prompt_msg))
                .unwrap_or_default(),
        },
    };
    if pwd.is_empty() {
        eprintln!("Password cannot be empty");
        cli::safe_exit(1)
    }
    pwd
}
