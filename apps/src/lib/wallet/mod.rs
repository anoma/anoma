pub mod defaults;
mod keys;
mod store;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use anoma::types::address::Address;
use anoma::types::key::ed25519::{Keypair, PublicKey, PublicKeyHash};
use thiserror::Error;

pub use self::keys::{DecryptionError, StoredKeypair};
use self::store::{Alias, Store};
use crate::cli;

#[derive(Debug)]
pub struct Wallet {
    base_dir: PathBuf,
    store: Store,
    decrypted_key_cache: HashMap<Alias, Rc<Keypair>>,
}

#[derive(Error, Debug)]
pub enum FindKeyError {
    #[error("No matching key found")]
    KeyNotFound,
    #[error("{0}")]
    KeyDecryptionError(keys::DecryptionError),
}

impl Wallet {
    /// Load a wallet from the store file or create a new one with the default
    /// keys and addresses if not found.
    pub fn load_or_new(base_dir: &Path) -> Self {
        let store = Store::load_or_new(base_dir).unwrap_or_else(|err| {
            eprintln!("Unable to load the wallet: {}", err);
            cli::safe_exit(1)
        });
        Self {
            base_dir: base_dir.to_path_buf(),
            store,
            decrypted_key_cache: HashMap::default(),
        }
    }

    /// Save the wallet store to a file.
    pub fn save(&self) -> std::io::Result<()> {
        self.store.save(&self.base_dir)
    }

    /// Generate a new keypair and derive an implicit address from its public
    /// and insert them into the store with the provided alias. If none
    /// provided, the alias will be the public key hash. If the key is to be
    /// encrypted, will prompt for password from stdin. Returns the alias of
    /// the key.
    pub fn gen_key(
        &mut self,
        alias: Option<String>,
        unsafe_dont_encrypt: bool,
    ) -> String {
        let password = if unsafe_dont_encrypt {
            println!("Warning: The keypair will NOT be encrypted.");
            None
        } else {
            Some(read_password("Enter encryption password: "))
        };
        self.store.gen_key(alias, password)
    }

    /// Find the stored key by an alias, a public key hash or a public key.
    /// If the key is encrypted, will prompt for password from stdin.
    /// Any keys that are decrypted are stored in and read from a cache to avoid
    /// prompting for password multiple times.
    pub fn find_key(
        &mut self,
        alias_pkh_or_pk: impl AsRef<str>,
    ) -> Result<Rc<Keypair>, FindKeyError> {
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
        pk: &PublicKey,
    ) -> Result<Rc<Keypair>, FindKeyError> {
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
    ) -> Result<Rc<Keypair>, FindKeyError> {
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
        decrypted_key_cache: &mut HashMap<String, Rc<Keypair>>,
        stored_key: &StoredKeypair,
        alias_pkh_or_pk: impl AsRef<str>,
    ) -> Result<Rc<Keypair>, FindKeyError> {
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
    /// will prompt for overwrite confirmation, which when declined, the address
    /// won't be added. Return `true` if the address has been added.
    pub fn add_address(&mut self, alias: String, address: Address) -> bool {
        self.store.insert_address(alias, address)
    }
}

/// Read the password for encryption/decryption from the stdin. Panics if the
/// input is an empty string.
fn read_password(prompt_msg: &str) -> String {
    let pwd =
        rpassword::read_password_from_tty(Some(prompt_msg)).unwrap_or_default();
    if pwd.is_empty() {
        eprintln!("Password cannot be empty");
        cli::safe_exit(1)
    }
    pwd
}
