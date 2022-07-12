//! Cryptographic keys for digital signatures support for the wallet.

use std::fmt::Display;
use std::marker::PhantomData;
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSerialize};
use orion::{aead, kdf};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::read_password;

const ENCRYPTED_KEY_PREFIX: &str = "encrypted:";
const UNENCRYPTED_KEY_PREFIX: &str = "unencrypted:";

/// A keypair stored in a wallet
#[derive(Debug)]
pub enum StoredKeypair<T: BorshSerialize + BorshDeserialize + Display + FromStr>
where
    <T as FromStr>::Err: Display,
{
    /// An encrypted keypair
    Encrypted(EncryptedKeypair<T>),
    /// An raw (unencrypted) keypair
    Raw(T),
}

impl<T: BorshSerialize + BorshDeserialize + Display + FromStr> Serialize
    for StoredKeypair<T>
where
    <T as FromStr>::Err: Display,
{
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

impl<'de, T: BorshSerialize + BorshDeserialize + Display + FromStr>
    Deserialize<'de> for StoredKeypair<T>
where
    <T as FromStr>::Err: Display,
{
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
                .map(|keypair| Self::Raw(keypair))
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
pub struct EncryptedKeypair<T: BorshSerialize + BorshDeserialize>(
    Vec<u8>,
    PhantomData<T>,
);

impl<T: BorshSerialize + BorshDeserialize> Display for EncryptedKeypair<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hex::encode(&self.0))
    }
}

impl<T: BorshSerialize + BorshDeserialize> FromStr for EncryptedKeypair<T> {
    type Err = hex::FromHexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        hex::decode(s).map(|x| Self(x, PhantomData))
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

impl<T: BorshSerialize + BorshDeserialize + Display + FromStr + Clone>
    StoredKeypair<T>
where
    <T as FromStr>::Err: Display,
{
    /// Construct a keypair for storage. If no password is provided, the keypair
    /// will be stored raw without encryption. Returns the key for storing and a
    /// reference-counting point to the raw key.
    pub fn new(keypair: T, password: Option<String>) -> (Self, T) {
        match password {
            Some(password) => (
                Self::Encrypted(EncryptedKeypair::new(&keypair, password)),
                keypair,
            ),
            None => (Self::Raw(keypair.clone()), keypair),
        }
    }

    /// Get a raw keypair from a stored keypair. If the keypair is encrypted and
    /// no password is provided in the argument, a password will be prompted
    /// from stdin.
    pub fn get(
        &self,
        decrypt: bool,
        password: Option<String>,
    ) -> Result<T, DecryptionError> {
        match self {
            StoredKeypair::Encrypted(encrypted_keypair) => {
                if decrypt {
                    let password = password.unwrap_or_else(|| {
                        read_password("Enter decryption password: ")
                    });
                    let key = encrypted_keypair.decrypt(password)?;
                    Ok(key)
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

impl<T: BorshSerialize + BorshDeserialize> EncryptedKeypair<T> {
    /// Encrypt a keypair and store it with its salt.
    pub fn new(keypair: &T, password: String) -> Self {
        let salt = encryption_salt();
        let encryption_key = encryption_key(&salt, password);

        let data = keypair
            .try_to_vec()
            .expect("Serializing keypair shouldn't fail");

        let encrypted_keypair = aead::seal(&encryption_key, &data)
            .expect("Encryption of data shouldn't fail");

        let encrypted_data = [salt.as_ref(), &encrypted_keypair].concat();

        Self(encrypted_data, PhantomData)
    }

    /// Decrypt an encrypted keypair
    pub fn decrypt(&self, password: String) -> Result<T, DecryptionError> {
        let salt_len = encryption_salt().len();
        let (raw_salt, cipher) = self.0.split_at(salt_len);

        let salt = kdf::Salt::from_slice(raw_salt)
            .map_err(|_| DecryptionError::BadSalt)?;

        let encryption_key = encryption_key(&salt, password);

        let decrypted_data = aead::open(&encryption_key, cipher)
            .map_err(|_| DecryptionError::DecryptionError)?;

        T::try_from_slice(&decrypted_data)
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
