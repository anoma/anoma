//! Cryptographic keys
/// Elliptic curve keys for the DKG
pub mod dkg_session_keys;

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
#[cfg(feature = "rand")]
use rand::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use super::address::Address;
use super::storage::{self, DbKeySeg, Key, KeySeg};
use crate::types::address;

pub mod common;
pub mod ed25519;

const PK_STORAGE_KEY: &str = "public_key";
const PROTOCOL_PK_STORAGE_KEY: &str = "protocol_public_key";

/// Obtain a storage key for user's public key.
pub fn pk_key(owner: &Address) -> storage::Key {
    Key::from(owner.to_db_key())
        .push(&PK_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Check if the given storage key is a public key. If it is, returns the owner.
pub fn is_pk_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(owner), DbKeySeg::StringSeg(key)]
            if key == PK_STORAGE_KEY =>
        {
            Some(owner)
        }
        _ => None,
    }
}

/// Obtain a storage key for user's protocol public key.
pub fn protocol_pk_key(owner: &Address) -> storage::Key {
    Key::from(owner.to_db_key())
        .push(&PROTOCOL_PK_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Check if the given storage key is a public key. If it is, returns the owner.
pub fn is_protocol_pk_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(owner), DbKeySeg::StringSeg(key)]
            if key == PROTOCOL_PK_STORAGE_KEY =>
        {
            Some(owner)
        }
        _ => None,
    }
}

/// Represents an error in signature verification
#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum VerifySigError {
    #[error("Signature verification failed: {0}")]
    SigVerifyError(String),
    #[error("Signature verification failed to encode the data: {0}")]
    DataEncodingError(std::io::Error),
    #[error("Transaction doesn't have any data with a signature.")]
    MissingData,
    #[error("Signature belongs to a different scheme from the public key.")]
    MismatchedScheme,
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum ParsePublicKeyError {
    #[error("Invalid public key hex: {0}")]
    InvalidHex(hex::FromHexError),
    #[error("Invalid public key encoding: {0}")]
    InvalidEncoding(std::io::Error),
    #[error("Parsed public key does not belong to desired scheme")]
    MismatchedScheme,
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum ParseSignatureError {
    #[error("Invalid signature hex: {0}")]
    InvalidHex(hex::FromHexError),
    #[error("Invalid signature encoding: {0}")]
    InvalidEncoding(std::io::Error),
    #[error("Parsed signature does not belong to desired scheme")]
    MismatchedScheme,
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum ParseSecretKeyError {
    #[error("Invalid secret key hex: {0}")]
    InvalidHex(hex::FromHexError),
    #[error("Invalid secret key encoding: {0}")]
    InvalidEncoding(std::io::Error),
    #[error("Parsed secret key does not belong to desired scheme")]
    MismatchedScheme,
}

/// A value-to-value conversion that consumes the input value.

pub trait RefTo<T> {
    /// Performs the conversion.
    fn ref_to(&self) -> T;
}

/// Simple and safe type conversions that may fail in a controlled
/// way under some circumstances.

pub trait TryFromRef<T: ?Sized>: Sized {
    /// The type returned in the event of a conversion error.
    type Error;
    /// Performs the conversion.
    fn try_from_ref(value: &T) -> Result<Self, Self::Error>;
}

/// Type capturing signature scheme IDs
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum SchemeType {
    /// Type identifier for Ed25519-consensus
    Ed25519Consensus,
    /// Type identifier for Common
    Common,
}

/// Represents a signature

pub trait Signature:
    Hash + PartialOrd + Serialize + BorshSerialize + BorshDeserialize
{
    /// The scheme type of this implementation
    const TYPE: SchemeType;
    /// Convert from one Signature type to another
    fn try_from_sig<PK: Signature>(
        pk: &PK,
    ) -> Result<Self, ParseSignatureError> {
        if PK::TYPE == Self::TYPE {
            let sig_arr = pk.try_to_vec().unwrap();
            let res = Self::try_from_slice(sig_arr.as_ref());
            res.map_err(ParseSignatureError::InvalidEncoding)
        } else {
            Err(ParseSignatureError::MismatchedScheme)
        }
    }
    /// Convert from self to another SecretKey type
    fn try_to_sig<PK: Signature>(&self) -> Result<PK, ParseSignatureError> {
        PK::try_from_sig(self)
    }
}

/// Represents a public key

pub trait PublicKey:
    BorshSerialize
    + BorshDeserialize
    + Ord
    + Clone
    + Display
    + Debug
    + PartialOrd
    + FromStr<Err = ParsePublicKeyError>
    + Hash
    + Send
    + Sync
{
    /// The scheme type of this implementation
    const TYPE: SchemeType;
    /// Convert from one PublicKey type to another
    fn try_from_pk<PK: PublicKey>(
        pk: &PK,
    ) -> Result<Self, ParsePublicKeyError> {
        if Self::TYPE == PK::TYPE {
            let pk_arr = pk.try_to_vec().unwrap();
            let res = Self::try_from_slice(pk_arr.as_ref());
            res.map_err(ParsePublicKeyError::InvalidEncoding)
        } else {
            Err(ParsePublicKeyError::MismatchedScheme)
        }
    }
    /// Convert from self to another SecretKey type
    fn try_to_pk<PK: PublicKey>(&self) -> Result<PK, ParsePublicKeyError> {
        PK::try_from_pk(self)
    }
}

/// Represents a secret key

pub trait SecretKey:
    BorshSerialize
    + BorshDeserialize
    + Display
    + Debug
    + RefTo<Self::PublicKey>
    + FromStr<Err = ParseSecretKeyError>
    + Clone
    + Sync
    + Send
{
    /// The scheme type of this implementation
    const TYPE: SchemeType;
    /// Represents the public part of this keypair
    type PublicKey: PublicKey;
    /// Convert from one SecretKey type to self
    fn try_from_sk<SK: SecretKey>(
        sk: &SK,
    ) -> Result<Self, ParseSecretKeyError> {
        if SK::TYPE == Self::TYPE {
            let sk_vec = sk.try_to_vec().unwrap();
            let res = Self::try_from_slice(sk_vec.as_ref());
            res.map_err(ParseSecretKeyError::InvalidEncoding)
        } else {
            Err(ParseSecretKeyError::MismatchedScheme)
        }
    }
    /// Convert from self to another SecretKey type
    fn try_to_sk<SK: SecretKey>(&self) -> Result<SK, ParseSecretKeyError> {
        SK::try_from_sk(self)
    }
}

/// Represents a digital signature scheme. More precisely this trait captures
/// the concepts of public keys, private keys, and signatures as well as
/// the algorithms over these concepts to generate keys, sign messages, and
/// verify signatures.

pub trait SigScheme: Eq + Ord + Debug + Serialize + Default {
    /// Represents the signature for this scheme
    type Signature: 'static + Signature;
    /// Represents the public key for this scheme
    type PublicKey: 'static + PublicKey;
    /// Represents the secret key for this scheme
    type SecretKey: 'static + SecretKey;
    /// The scheme type of this implementation
    const TYPE: SchemeType;
    /// Generate a keypair.
    #[cfg(feature = "rand")]
    fn generate<R>(csprng: &mut R) -> Self::SecretKey
    where
        R: CryptoRng + RngCore;
    /// Sign the data with a key.
    fn sign(
        keypair: &Self::SecretKey,
        data: impl AsRef<[u8]>,
    ) -> Self::Signature;
    /// Check that the public key matches the signature on the given data.
    fn verify_signature<T: BorshSerialize + BorshDeserialize>(
        pk: &Self::PublicKey,
        data: &T,
        sig: &Self::Signature,
    ) -> Result<(), VerifySigError>;
    /// Check that the public key matches the signature on the given raw data.
    fn verify_signature_raw(
        pk: &Self::PublicKey,
        data: &[u8],
        sig: &Self::Signature,
    ) -> Result<(), VerifySigError>;
}

/// Ed25519 public key hash
#[derive(
    Debug,
    Clone,
    BorshSerialize,
    BorshDeserialize,
    BorshSchema,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[serde(transparent)]
pub struct PublicKeyHash(pub(crate) String);

const PKH_HASH_LEN: usize = address::HASH_LEN;

impl From<PublicKeyHash> for String {
    fn from(pkh: PublicKeyHash) -> Self {
        pkh.0
    }
}

impl Display for PublicKeyHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for PublicKeyHash {
    type Err = PkhFromStringError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != PKH_HASH_LEN {
            return Err(Self::Err::UnexpectedLen(s.len()));
        }
        Ok(Self(s.to_owned()))
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum PkhFromStringError {
    #[error("Wrong PKH len. Expected {PKH_HASH_LEN}, got {0}")]
    UnexpectedLen(usize),
}

impl<PK: PublicKey> From<&PK> for PublicKeyHash {
    fn from(pk: &PK) -> Self {
        let pk_bytes =
            pk.try_to_vec().expect("Public key encoding shouldn't fail");
        let mut hasher = Sha256::new();
        hasher.update(pk_bytes);
        // hex of the first 40 chars of the hash
        PublicKeyHash(format!(
            "{:.width$X}",
            hasher.finalize(),
            width = PKH_HASH_LEN
        ))
    }
}

/// Helpers for testing with keys.
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use borsh::BorshDeserialize;
    use proptest::prelude::*;
    use rand::prelude::{StdRng, ThreadRng};
    use rand::{thread_rng, SeedableRng};

    use super::SigScheme;
    use crate::types::key::*;

    /// A keypair for tests
    pub fn keypair_1() -> <common::SigScheme as SigScheme>::SecretKey {
        // generated from `cargo test gen_keypair -- --nocapture`
        let bytes = [
            33, 82, 91, 186, 100, 168, 220, 158, 185, 140, 63, 172, 3, 88, 52,
            113, 94, 30, 213, 84, 175, 184, 235, 169, 70, 175, 36, 252, 45,
            190, 138, 79,
        ];
        ed25519::SecretKey::try_from_slice(bytes.as_ref())
            .unwrap()
            .try_to_sk()
            .unwrap()
    }

    /// A keypair for tests
    pub fn keypair_2() -> <common::SigScheme as SigScheme>::SecretKey {
        // generated from `cargo test gen_keypair -- --nocapture`
        let bytes = [
            27, 238, 157, 32, 131, 242, 184, 142, 146, 189, 24, 249, 68, 165,
            205, 71, 213, 158, 25, 253, 52, 217, 87, 52, 171, 225, 110, 131,
            238, 58, 94, 56,
        ];
        ed25519::SecretKey::try_from_slice(bytes.as_ref())
            .unwrap()
            .try_to_sk()
            .unwrap()
    }

    /// Generate an arbitrary [`super::SecretKey`].
    pub fn arb_keypair<S: SigScheme>() -> impl Strategy<Value = S::SecretKey> {
        any::<[u8; 32]>().prop_map(move |seed| {
            let mut rng = StdRng::from_seed(seed);
            S::generate(&mut rng)
        })
    }

    /// Generate a new random [`super::SecretKey`].
    pub fn gen_keypair<S: SigScheme>() -> S::SecretKey {
        let mut rng: ThreadRng = thread_rng();
        S::generate(&mut rng)
    }
}

#[cfg(test)]
macro_rules! sigscheme_test {
    ($name:ident, $type:ty) => {
        pub mod $name {
            use super::*;

            /// Run `cargo test gen_keypair -- --nocapture` to generate a
            /// keypair.
            #[test]
            fn gen_keypair0() {
                use rand::prelude::ThreadRng;
                use rand::thread_rng;

                let mut rng: ThreadRng = thread_rng();
                let keypair = <$type>::generate(&mut rng);
                println!(
                    "keypair {:?}",
                    keypair.try_to_vec().unwrap().as_slice()
                );
            }
            /// Run `cargo test gen_keypair -- --nocapture` to generate a
            /// new keypair.
            #[test]
            fn gen_keypair1() {
                let secret_key = testing::gen_keypair::<$type>();
                let public_key = secret_key.ref_to();
                println!("Public key: {}", public_key);
                println!("Secret key: {}", secret_key);
            }
        }
    };
}

#[cfg(test)]
sigscheme_test! {ed25519_test, ed25519::SigScheme}
