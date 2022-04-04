use std::collections::HashMap;
use std::fs;
use std::io::prelude::*;
use std::io::{self, ErrorKind, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str::FromStr;

use anoma::types::address::{Address, ImplicitAddress};
use anoma::types::key::dkg_session_keys::DkgKeypair;
use anoma::types::key::*;
use anoma::types::transaction::EllipticCurve;
use ark_std::rand::prelude::*;
use ark_std::rand::SeedableRng;
use file_lock::{FileLock, FileOptions};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use anoma::types::masp::{PaymentAddress, ExtendedSpendingKey, FullViewingKey};

use super::alias::Alias;
use super::keys::StoredKeypair;
use crate::cli;
use crate::config::genesis::genesis_config::GenesisConfig;

/// Special keys for a validator
#[derive(Serialize, Deserialize, Debug)]
pub struct ValidatorKeys {
    /// Special keypair for signing protocol txs
    pub protocol_keypair: common::SecretKey,
    /// Special session keypair needed by validators for participating
    /// in the DKG protocol
    pub dkg_keypair: Option<DkgKeypair>,
}

impl ValidatorKeys {
    /// Get the protocol keypair
    pub fn get_protocol_keypair(&self) -> &common::SecretKey {
        &self.protocol_keypair
    }
}

/// Special data associated with a validator
#[derive(Serialize, Deserialize, Debug)]
pub struct ValidatorData {
    /// The address associated to a validator
    pub address: Address,
    /// special keys for a validator
    pub keys: ValidatorKeys,
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Store {
    /// Known viewing keys
    view_keys: HashMap<Alias, FullViewingKey>,
    /// Known spending keys
    spend_keys: HashMap<Alias, ExtendedSpendingKey>,
    /// Known payment addresses
    payment_addrs: HashMap<Alias, PaymentAddress>,
    /// Cryptographic keypairs
    keys: HashMap<Alias, StoredKeypair>,
    /// Anoma address book
    addresses: HashMap<Alias, Address>,
    /// Known mappings of public key hashes to their aliases in the `keys`
    /// field. Used for look-up by a public key.
    pkhs: HashMap<PublicKeyHash, Alias>,
    /// Special keys if the wallet belongs to a validator
    pub(crate) validator_data: Option<ValidatorData>,
}

#[derive(Error, Debug)]
pub enum LoadStoreError {
    #[error("Failed decoding the wallet store: {0}")]
    Decode(toml::de::Error),
    #[error("Failed to read the wallet store from {0}: {1}")]
    ReadWallet(String, String),
    #[error("Failed to write the wallet store: {0}")]
    StoreNewWallet(String),
}

impl Store {
    #[cfg(not(feature = "dev"))]
    fn new(genesis: GenesisConfig) -> Self {
        let mut store = Self::default();
        store.add_genesis_addresses(genesis);
        store
    }

    #[cfg(feature = "dev")]
    fn new() -> Self {
        let mut store = Self::default();
        // Pre-load the default keys without encryption
        let no_password = None;
        for (alias, keypair) in super::defaults::keys() {
            let pkh: PublicKeyHash = (&keypair.ref_to()).into();
            store.keys.insert(
                alias.clone(),
                StoredKeypair::new(keypair, no_password.clone()).0,
            );
            store.pkhs.insert(pkh, alias);
        }
        store
            .addresses
            .extend(super::defaults::addresses().into_iter());
        store
    }

    /// Add addresses from a genesis configuration.
    pub fn add_genesis_addresses(&mut self, genesis: GenesisConfig) {
        self.addresses.extend(
            super::defaults::addresses_from_genesis(genesis).into_iter(),
        );
    }

    /// Save the wallet store to a file.
    pub fn save(&self, store_dir: &Path) -> std::io::Result<()> {
        let data = self.encode();
        let wallet_path = wallet_file(store_dir);
        // Make sure the dir exists
        let wallet_dir = wallet_path.parent().unwrap();
        fs::create_dir_all(wallet_dir)?;
        // Write the file
        let options =
            FileOptions::new().create(true).write(true).truncate(true);
        let mut filelock =
            FileLock::lock(wallet_path.to_str().unwrap(), true, options)?;
        filelock.file.write_all(&data)
    }

    /// Load the store file or create a new one without any keys or addresses.
    pub fn load_or_new(store_dir: &Path) -> Result<Self, LoadStoreError> {
        Self::load_or_new_aux(store_dir, || {
            let store = Self::default();
            store.save(store_dir).map_err(|err| {
                LoadStoreError::StoreNewWallet(err.to_string())
            })?;
            Ok(store)
        })
    }

    /// Load the store file or create a new one with the default addresses from
    /// the genesis file, if not found.
    pub fn load_or_new_from_genesis(
        store_dir: &Path,
        load_genesis: impl FnOnce() -> GenesisConfig,
    ) -> Result<Self, LoadStoreError> {
        Self::load_or_new_aux(store_dir, || {
            #[cfg(not(feature = "dev"))]
            let store = Self::new(load_genesis());
            #[cfg(feature = "dev")]
            let store = {
                // The function is unused in dev
                let _ = load_genesis;
                Self::new()
            };
            store.save(store_dir).map_err(|err| {
                LoadStoreError::StoreNewWallet(err.to_string())
            })?;
            Ok(store)
        })
    }

    /// Load the store file or create a new with the provided function.
    fn load_or_new_aux(
        store_dir: &Path,
        new_store: impl FnOnce() -> Result<Self, LoadStoreError>,
    ) -> Result<Self, LoadStoreError> {
        let wallet_file = wallet_file(store_dir);
        match FileLock::lock(
            wallet_file.to_str().unwrap(),
            true,
            FileOptions::new().read(true).write(false),
        ) {
            Ok(mut filelock) => {
                let mut store = Vec::<u8>::new();
                filelock.file.read_to_end(&mut store).map_err(|err| {
                    LoadStoreError::ReadWallet(
                        store_dir.to_str().unwrap().into(),
                        err.to_string(),
                    )
                })?;
                Store::decode(store).map_err(LoadStoreError::Decode)
            }
            Err(err) => match err.kind() {
                ErrorKind::NotFound => {
                    println!(
                        "No wallet found at {:?}. Creating a new one.",
                        wallet_file
                    );
                    new_store()
                }
                _ => Err(LoadStoreError::ReadWallet(
                    wallet_file.to_string_lossy().into_owned(),
                    err.to_string(),
                )),
            },
        }
    }

    /// Find the stored key by an alias, a public key hash or a public key.
    pub fn find_key(
        &self,
        alias_pkh_or_pk: impl AsRef<str>,
    ) -> Option<&StoredKeypair> {
        let alias_pkh_or_pk = alias_pkh_or_pk.as_ref();
        // Try to find by alias
        self.keys
            .get(&alias_pkh_or_pk.into())
            // Try to find by PKH
            .or_else(|| {
                let pkh = PublicKeyHash::from_str(alias_pkh_or_pk).ok()?;
                self.find_key_by_pkh(&pkh)
            })
            // Try to find by PK
            .or_else(|| {
                let pk = common::PublicKey::from_str(alias_pkh_or_pk).ok()?;
                self.find_key_by_pk(&pk)
            })
    }

    pub fn find_spending_key(
        &self,
        alias: impl AsRef<str>,
    ) -> Option<&ExtendedSpendingKey> {
        self.spend_keys.get(&alias.into())
    }

    pub fn find_viewing_key(
        &self,
        alias: impl AsRef<str>,
    ) -> Option<&FullViewingKey> {
        self.view_keys.get(&alias.into())
    }

    pub fn find_payment_addr(
        &self,
        alias: impl AsRef<str>,
    ) -> Option<&PaymentAddress> {
        self.payment_addrs.get(&alias.into())
    }

    /// Find the stored key by a public key.
    pub fn find_key_by_pk(
        &self,
        pk: &common::PublicKey,
    ) -> Option<&StoredKeypair> {
        let pkh = PublicKeyHash::from(pk);
        self.find_key_by_pkh(&pkh)
    }

    /// Find the stored key by a public key hash.
    pub fn find_key_by_pkh(
        &self,
        pkh: &PublicKeyHash,
    ) -> Option<&StoredKeypair> {
        let alias = self.pkhs.get(pkh)?;
        self.keys.get(alias)
    }

    /// Find the stored alias for a public key hash.
    pub fn find_alias_by_pkh(&self, pkh: &PublicKeyHash) -> Option<Alias> {
        self.pkhs.get(pkh).cloned()
    }

    /// Find the stored address by an alias.
    pub fn find_address(&self, alias: impl AsRef<str>) -> Option<&Address> {
        self.addresses.get(&alias.into())
    }

    /// Get all known keys by their alias, paired with PKH, if known.
    pub fn get_keys(
        &self,
    ) -> HashMap<Alias, (&StoredKeypair, Option<&PublicKeyHash>)> {
        let mut keys: HashMap<Alias, (&StoredKeypair, Option<&PublicKeyHash>)> =
            self.pkhs
                .iter()
                .filter_map(|(pkh, alias)| {
                    let key = &self.keys.get(alias)?;
                    Some((alias.clone(), (*key, Some(pkh))))
                })
                .collect();
        self.keys.iter().for_each(|(alias, key)| {
            if !keys.contains_key(alias) {
                keys.insert(alias.clone(), (key, None));
            }
        });
        keys
    }

    /// Get all known addresses by their alias, paired with PKH, if known.
    pub fn get_addresses(&self) -> &HashMap<Alias, Address> {
        &self.addresses
    }

    /// Get all known payment addresses by their alias.
    pub fn get_payment_addrs(&self) -> &HashMap<Alias, PaymentAddress> {
        &self.payment_addrs
    }

    /// Get all known viewing keys by their alias.
    pub fn get_viewing_keys(&self) -> &HashMap<Alias, FullViewingKey> {
        &self.view_keys
    }

    fn generate_keypair() -> common::SecretKey {
        use rand::rngs::OsRng;
        let mut csprng = OsRng {};
        ed25519::SigScheme::generate(&mut csprng)
            .try_to_sk()
            .unwrap()
    }

    fn generate_spending_key() -> ExtendedSpendingKey {
        use rand::rngs::OsRng;
        let mut spend_key = [0; 32];
        OsRng.fill_bytes(&mut spend_key);
        masp_primitives::zip32::ExtendedSpendingKey::master(spend_key.as_ref()).into()
    }

    /// Generate a new keypair and insert it into the store with the provided
    /// alias. If none provided, the alias will be the public key hash.
    /// If no password is provided, the keypair will be stored raw without
    /// encryption. Returns the alias of the key and a reference-counting
    /// pointer to the key.
    pub fn gen_key(
        &mut self,
        alias: Option<String>,
        password: Option<String>,
    ) -> (Alias, Rc<common::SecretKey>) {
        let keypair = Self::generate_keypair();
        let pkh: PublicKeyHash = PublicKeyHash::from(&keypair.ref_to());
        let (keypair_to_store, raw_keypair) =
            StoredKeypair::new(keypair, password);
        let address = Address::Implicit(ImplicitAddress(pkh.clone()));
        let alias: Alias = alias.unwrap_or_else(|| pkh.clone().into()).into();
        if self
            .insert_keypair(alias.clone(), keypair_to_store, pkh)
            .is_none()
        {
            eprintln!("Action cancelled, no changes persisted.");
            cli::safe_exit(1);
        }
        if self.insert_address(alias.clone(), address).is_none() {
            eprintln!("Action cancelled, no changes persisted.");
            cli::safe_exit(1);
        }
        (alias, raw_keypair)
    }

    pub fn gen_spending_key(
        &mut self,
        alias: String,
    ) -> (Alias, ExtendedSpendingKey) {
        let spendkey = Self::generate_spending_key();
        let alias = Alias::from(alias);
        if self
            .insert_spending_key(alias.clone(), spendkey)
            .is_none()
        {
            eprintln!("Action cancelled, no changes persisted.");
            cli::safe_exit(1);
        }
        (alias, spendkey)
    }

    /// Generate keypair for signing protocol txs and for the DKG
    /// A protocol keypair may be optionally provided
    ///
    /// Note that this removes the validator data.
    pub fn gen_validator_keys(
        protocol_keypair: Option<common::SecretKey>,
    ) -> ValidatorKeys {
        let protocol_keypair =
            protocol_keypair.unwrap_or_else(Self::generate_keypair);
        let dkg_keypair = ferveo_common::Keypair::<EllipticCurve>::new(
            &mut StdRng::from_entropy(),
        );
        ValidatorKeys {
            protocol_keypair,
            dkg_keypair: Some(dkg_keypair.into()),
        }
    }

    /// Add validator data to the store
    pub fn add_validator_data(
        &mut self,
        address: Address,
        keys: ValidatorKeys,
    ) {
        self.validator_data = Some(ValidatorData { address, keys });
    }

    /// Returns the validator data, if it exists
    pub fn get_validator_data(&self) -> Option<&ValidatorData> {
        self.validator_data.as_ref()
    }

    /// Returns the validator data, if it exists
    pub fn validator_data(self) -> Option<ValidatorData> {
        self.validator_data
    }

    /// Insert a new key with the given alias. If the alias is already used,
    /// will prompt for overwrite/reselection confirmation. If declined, then
    /// keypair is not inserted and nothing is returned, otherwise selected
    /// alias is returned.
    pub(super) fn insert_keypair(
        &mut self,
        alias: Alias,
        keypair: StoredKeypair,
        pkh: PublicKeyHash,
    ) -> Option<Alias> {
        if alias.is_empty() {
            println!(
                "Empty alias given, defaulting to {}.",
                alias = Into::<Alias>::into(pkh.to_string())
            );
        }
        if self.keys.contains_key(&alias) {
            match show_overwrite_confirmation(&alias, "a key") {
                ConfirmationResponse::Replace => {}
                ConfirmationResponse::Reselect(new_alias) => {
                    return self.insert_keypair(new_alias, keypair, pkh);
                }
                ConfirmationResponse::Skip => return None,
            }
        }
        self.keys.insert(alias.clone(), keypair);
        self.pkhs.insert(pkh, alias.clone());
        Some(alias)
    }

    pub fn insert_spending_key(
        &mut self,
        alias: Alias,
        spendkey: ExtendedSpendingKey,
    ) -> Option<Alias> {
        if alias.is_empty() {
            eprintln!("Empty alias given.");
            return None;
        }
        if self.spend_keys.contains_key(&alias) {
            match show_overwrite_confirmation(&alias, "a spending key") {
                ConfirmationResponse::Replace => {}
                ConfirmationResponse::Reselect(new_alias) => {
                    return self.insert_spending_key(new_alias, spendkey);
                }
                ConfirmationResponse::Skip => return None,
            }
        }
        self.spend_keys.insert(alias.clone(), spendkey);
        Some(alias)
    }

    pub fn insert_viewing_key(
        &mut self,
        alias: Alias,
        viewkey: FullViewingKey,
    ) -> Option<Alias> {
        if alias.is_empty() {
            eprintln!("Empty alias given.");
            return None;
        }
        if self.view_keys.contains_key(&alias) {
            match show_overwrite_confirmation(&alias, "a viewing key") {
                ConfirmationResponse::Replace => {}
                ConfirmationResponse::Reselect(new_alias) => {
                    return self.insert_viewing_key(new_alias, viewkey);
                }
                ConfirmationResponse::Skip => return None,
            }
        }
        self.view_keys.insert(alias.clone(), viewkey);
        Some(alias)
    }

    pub fn insert_payment_addr(
        &mut self,
        alias: Alias,
        payment_addr: PaymentAddress,
    ) -> Option<Alias> {
        if alias.is_empty() {
            eprintln!("Empty alias given.");
            return None;
        }
        if self.payment_addrs.contains_key(&alias) {
            match show_overwrite_confirmation(&alias, "a payment address") {
                ConfirmationResponse::Replace => {}
                ConfirmationResponse::Reselect(new_alias) => {
                    return self.insert_payment_addr(new_alias, payment_addr);
                }
                ConfirmationResponse::Skip => return None,
            }
        }
        self.payment_addrs.insert(alias.clone(), payment_addr);
        Some(alias)
    }

    /// Insert a new address with the given alias. If the alias is already used,
    /// will prompt for overwrite/reselection confirmation, which when declined,
    /// the address won't be added. Return the selected alias if the address has
    /// been added.
    pub fn insert_address(
        &mut self,
        alias: Alias,
        address: Address,
    ) -> Option<Alias> {
        if alias.is_empty() {
            println!(
                "Empty alias given, defaulting to {}.",
                alias = address.encode()
            );
        }
        if self.addresses.contains_key(&alias) {
            match show_overwrite_confirmation(&alias, "an address") {
                ConfirmationResponse::Replace => {}
                ConfirmationResponse::Reselect(new_alias) => {
                    return self.insert_address(new_alias, address);
                }
                ConfirmationResponse::Skip => return None,
            }
        }
        self.addresses.insert(alias.clone(), address);
        Some(alias)
    }

    fn decode(data: Vec<u8>) -> Result<Self, toml::de::Error> {
        toml::from_slice(&data)
    }

    fn encode(&self) -> Vec<u8> {
        toml::to_vec(self).expect("Serializing of store shouldn't fail")
    }
}

enum ConfirmationResponse {
    Replace,
    Reselect(Alias),
    Skip,
}

/// The given alias has been selected but conflicts with another alias in
/// the store. Offer the user to either replace existing mapping, alter the
/// chosen alias to a name of their chosing, or cancel the aliasing.

fn show_overwrite_confirmation(
    alias: &Alias,
    alias_for: &str,
) -> ConfirmationResponse {
    print!(
        "You're trying to create an alias \"{}\" that already exists for {} \
         in your store.\nWould you like to replace it? \
         s(k)ip/re(p)lace/re(s)elect: ",
        alias, alias_for
    );
    io::stdout().flush().unwrap();

    let mut buffer = String::new();
    // Get the user to select between 3 choices
    match io::stdin().read_line(&mut buffer) {
        Ok(size) if size > 0 => {
            // Isolate the single character representing the choice
            let byte = buffer.chars().next().unwrap();
            buffer.clear();
            match byte {
                'p' | 'P' => return ConfirmationResponse::Replace,
                's' | 'S' => {
                    // In the case of reselection, elicit new alias
                    print!("Please enter a different alias: ");
                    io::stdout().flush().unwrap();
                    if io::stdin().read_line(&mut buffer).is_ok() {
                        return ConfirmationResponse::Reselect(
                            buffer.trim().into(),
                        );
                    }
                }
                'k' | 'K' => return ConfirmationResponse::Skip,
                // Input is senseless fall through to repeat prompt
                _ => {}
            };
        }
        _ => {}
    }
    // Input is senseless fall through to repeat prompt
    println!("Invalid option, try again.");
    show_overwrite_confirmation(alias, alias_for)
}

/// Wallet file name
const FILE_NAME: &str = "wallet.toml";

/// Get the path to the wallet store.
pub fn wallet_file(store_dir: impl AsRef<Path>) -> PathBuf {
    store_dir.as_ref().join(FILE_NAME)
}

#[cfg(all(test, feature = "dev"))]
mod test_wallet {
    use super::*;

    #[test]
    fn test_toml_roundtrip() {
        let mut store = Store::new();
        let validator_keys = Store::gen_validator_keys(None);
        store.add_validator_data(
            Address::decode("atest1v4ehgw36x3prswzxggunzv6pxqmnvdj9xvcyzvpsggeyvs3cg9qnywf589qnwvfsg5erg3fkl09rg5").unwrap(),
            validator_keys
        );
        let data = store.encode();
        let _ = Store::decode(data).expect("Test failed");
    }
}
