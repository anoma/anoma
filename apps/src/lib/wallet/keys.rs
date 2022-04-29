//! Cryptographic keys for digital signatures support for the wallet.

use std::fmt::Display;
use std::rc::Rc;
use std::str::FromStr;

use anoma::types::key::*;
use borsh::{BorshDeserialize, BorshSerialize};
use orion::{aead, kdf};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::read_password;

const ENCRYPTED_KEY_PREFIX: &str = "encrypted:";
const UNENCRYPTED_KEY_PREFIX: &str = "unencrypted:";

/// A keypair stored in a wallet
#[derive(Debug)]
pub enum StoredKeypair {
    /// An encrypted keypair
    Encrypted(EncryptedKeypair),
    /// An raw (unencrypted) keypair
    Raw(
        // Wrapped in `Rc` to avoid reference lifetimes when we borrow the key
        Rc<common::SecretKey>,
    ),
}

impl Serialize for StoredKeypair {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // String encoded, because toml doesn't support enums
        match self {
            StoredKeypair::Encrypted(encrypted) => {
                let keypair_string =
                    format!("{}{}", ENCRYPTED_KEY_PREFIX, encrypted);
                serde::Serialize::serialize(&keypair_string, serializer)
            }
            StoredKeypair::Raw(raw) => {
                let keypair_string =
                    format!("{}{}", UNENCRYPTED_KEY_PREFIX, raw);
                serde::Serialize::serialize(&keypair_string, serializer)
            }
        }
    }
}

impl<'de> Deserialize<'de> for StoredKeypair {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let keypair_string: String =
            serde::Deserialize::deserialize(deserializer)
                .map_err(|err| {
                    DeserializeStoredKeypairError::InvalidStoredKeypairString(
                        err.to_string(),
                    )
                })
                .map_err(D::Error::custom)?;
        if let Some(raw) = keypair_string.strip_prefix(UNENCRYPTED_KEY_PREFIX) {
            FromStr::from_str(raw)
                .map(|keypair| Self::Raw(Rc::new(keypair)))
                .map_err(|err| {
                    DeserializeStoredKeypairError::InvalidStoredKeypairString(
                        err.to_string(),
                    )
                })
                .map_err(D::Error::custom)
        } else if let Some(encrypted) =
            keypair_string.strip_prefix(ENCRYPTED_KEY_PREFIX)
        {
            FromStr::from_str(encrypted)
                .map(Self::Encrypted)
                .map_err(|err| {
                    DeserializeStoredKeypairError::InvalidStoredKeypairString(
                        err.to_string(),
                    )
                })
                .map_err(D::Error::custom)
        } else {
            Err(DeserializeStoredKeypairError::MissingPrefix)
                .map_err(D::Error::custom)
        }
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum DeserializeStoredKeypairError {
    #[error("The stored keypair is not valid: {0}")]
    InvalidStoredKeypairString(String),
    #[error("The stored keypair is missing a prefix")]
    MissingPrefix,
}

/// An encrypted keypair stored in a wallet
#[derive(Debug)]
pub struct EncryptedKeypair(Vec<u8>);

impl Display for EncryptedKeypair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hex::encode(&self.0))
    }
}

impl FromStr for EncryptedKeypair {
    type Err = hex::FromHexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        hex::decode(s).map(Self)
    }
}

#[allow(missing_docs)]
#[derive(Debug, Error)]
pub enum DecryptionError {
    #[error("Unexpected encryption salt")]
    BadSalt,
    #[error("Unable to decrypt the keypair. Is the password correct?")]
    DecryptionError,
    #[error("Unable to deserialize the keypair")]
    DeserializingError,
    #[error("Asked not to decrypt")]
    NotDecrypting,
}

impl StoredKeypair {
    /// Construct a keypair for storage. If no password is provided, the keypair
    /// will be stored raw without encryption. Returns the key for storing and a
    /// reference-counting point to the raw key.
    pub fn new(
        keypair: common::SecretKey,
        password: Option<String>,
    ) -> (Self, Rc<common::SecretKey>) {
        match password {
            Some(password) => {
                let keypair = Rc::new(keypair);
                (
                    Self::Encrypted(EncryptedKeypair::new(&keypair, password)),
                    keypair,
                )
            }
            None => {
                let keypair = Rc::new(keypair);
                (Self::Raw(keypair.clone()), keypair)
            }
        }
    }

    /// Get a raw keypair from a stored keypair. If the keypair is encrypted and
    /// no password is provided in the argument, a password will be prompted
    /// from stdin.
    pub fn get(
        &self,
        decrypt: bool,
        password: Option<String>,
    ) -> Result<Rc<common::SecretKey>, DecryptionError> {
        match self {
            StoredKeypair::Encrypted(encrypted_keypair) => {
                if decrypt {
                    let password = password.unwrap_or_else(|| {
                        read_password("Enter decryption password: ")
                    });
                    let key = encrypted_keypair.decrypt(password)?;
                    Ok(Rc::new(key))
                } else {
                    Err(DecryptionError::NotDecrypting)
                }
            }
            StoredKeypair::Raw(keypair) => Ok(keypair.clone()),
        }
    }

    pub fn is_encrypted(&self) -> bool {
        match self {
            StoredKeypair::Encrypted(_) => true,
            StoredKeypair::Raw(_) => false,
        }
    }
}

impl EncryptedKeypair {
    /// Encrypt a keypair and store it with its salt.
    pub fn new(keypair: &common::SecretKey, password: String) -> Self {
        let salt = encryption_salt();
        let encryption_key = encryption_key(&salt, password);

        let data = keypair
            .try_to_vec()
            .expect("Serializing keypair shouldn't fail");

        let encrypted_keypair = aead::seal(&encryption_key, &data)
            .expect("Encryption of data shouldn't fail");

        let encrypted_data = [salt.as_ref(), &encrypted_keypair].concat();

        Self(encrypted_data)
    }

    /// Decrypt an encrypted keypair
    pub fn decrypt(
        &self,
        password: String,
    ) -> Result<common::SecretKey, DecryptionError> {
        let salt_len = encryption_salt().len();
        let (raw_salt, cipher) = self.0.split_at(salt_len);

        let salt = kdf::Salt::from_slice(raw_salt)
            .map_err(|_| DecryptionError::BadSalt)?;

        let encryption_key = encryption_key(&salt, password);

        let decrypted_data = aead::open(&encryption_key, cipher)
            .map_err(|_| DecryptionError::DecryptionError)?;

        common::SecretKey::try_from_slice(&decrypted_data)
            .map_err(|_| DecryptionError::DeserializingError)
    }
}

/// Keypair encryption salt
fn encryption_salt() -> kdf::Salt {
    kdf::Salt::default()
}

/// Make encryption secret key from a password.
fn encryption_key(salt: &kdf::Salt, password: String) -> kdf::SecretKey {
    kdf::Password::from_slice(password.as_bytes())
        .and_then(|password| kdf::derive_key(&password, salt, 3, 1 << 16, 32))
        .expect("Generation of encryption secret key shouldn't fail")
}
