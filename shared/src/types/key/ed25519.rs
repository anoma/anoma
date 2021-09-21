//! Ed25519 keys and related functionality

use std::convert::TryInto;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::io::{ErrorKind, Write};
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSerialize};
pub use ed25519_dalek::SignatureError;
use ed25519_dalek::{ExpandedSecretKey, Signer, Verifier};
#[cfg(feature = "rand")]
use rand::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use crate::proto::Tx;
use crate::types::address::{self, Address};
use crate::types::storage::{DbKeySeg, Key, KeySeg};

const SIGNATURE_LEN: usize = ed25519_dalek::SIGNATURE_LENGTH;

/// Ed25519 public key
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PublicKey(ed25519_dalek::PublicKey);

/// Ed25519 secret key
#[derive(Debug, Serialize, Deserialize)]
pub struct SecretKey(ed25519_dalek::SecretKey);

/// Ed25519 keypair
#[derive(Debug, Serialize, Deserialize, BorshSerialize, BorshDeserialize)]
pub struct Keypair {
    /// Secret key
    pub secret: SecretKey,
    /// Public key
    pub public: PublicKey,
}

/// Ed25519 signature
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Signature(ed25519_dalek::Signature);

/// Ed25519 public key hash
#[derive(
    Debug,
    Clone,
    BorshSerialize,
    BorshDeserialize,
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
const PK_STORAGE_KEY: &str = "ed25519_pk";

/// Obtain a storage key for user's public key.
pub fn pk_key(owner: &Address) -> Key {
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

/// Sign the data with a key.
pub fn sign(keypair: &Keypair, data: impl AsRef<[u8]>) -> Signature {
    keypair.sign(data.as_ref())
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum VerifySigError {
    #[error("Signature verification failed: {0}")]
    SigError(SignatureError),
    #[error("Signature verification failed to encode the data: {0}")]
    EncodingError(std::io::Error),
    #[error("Transaction doesn't have any data with a signature.")]
    MissingData,
}

/// Check that the public key matches the signature on the given data.
pub fn verify_signature<T: BorshSerialize + BorshDeserialize>(
    pk: &PublicKey,
    data: &T,
    sig: &Signature,
) -> Result<(), VerifySigError> {
    let bytes = data.try_to_vec().map_err(VerifySigError::EncodingError)?;
    pk.0.verify_strict(&bytes, &sig.0)
        .map_err(VerifySigError::SigError)
}

/// Check that the public key matches the signature on the given raw data.
pub fn verify_signature_raw(
    pk: &PublicKey,
    data: &[u8],
    sig: &Signature,
) -> Result<(), VerifySigError> {
    pk.0.verify_strict(data, &sig.0)
        .map_err(VerifySigError::SigError)
}

/// This can be used to sign an arbitrary tx. The signature is produced and
/// verified on the tx data concatenated with the tx code, however the tx code
/// itself is not part of this structure.
///
/// Because the signature is not checked by the ledger, we don't inline it into
/// the `Tx` type directly. Instead, the signature is attached to the `tx.data`,
/// which is can then be checked by a validity predicate wasm.
#[derive(Clone, Debug, BorshSerialize, BorshDeserialize)]
pub struct SignedTxData {
    /// The original tx data bytes, if any
    pub data: Option<Vec<u8>>,
    /// The signature is produced on the tx data concatenated with the tx code
    /// and the timestamp.
    pub sig: Signature,
}

/// Sign a transaction using [`SignedTxData`].
pub fn sign_tx(keypair: &Keypair, tx: Tx) -> Tx {
    let to_sign = tx.to_bytes();
    let sig = sign(keypair, &to_sign);
    let signed = SignedTxData { data: tx.data, sig }
        .try_to_vec()
        .expect("Encoding transaction data shouldn't fail");
    Tx {
        code: tx.code,
        data: Some(signed),
        timestamp: tx.timestamp,
    }
}

/// Verify that the transaction has been signed by the secret key
/// counterpart of the given public key.
pub fn verify_tx_sig(
    pk: &PublicKey,
    tx: &Tx,
    sig: &Signature,
) -> Result<(), VerifySigError> {
    // Try to get the transaction data from decoded `SignedTxData`
    let tx_data = tx.data.clone().ok_or(VerifySigError::MissingData)?;
    let signed_tx_data = SignedTxData::try_from_slice(&tx_data[..])
        .expect("Decoding transaction data shouldn't fail");
    let data = signed_tx_data.data;
    let tx = Tx {
        code: tx.code.clone(),
        data,
        timestamp: tx.timestamp,
    };
    let signed_data = tx.to_bytes();
    verify_signature_raw(pk, &signed_data, sig)
}

/// A generic signed data wrapper for Borsh encode-able data.
#[derive(
    Clone, Debug, BorshSerialize, BorshDeserialize, Serialize, Deserialize,
)]
pub struct Signed<T: BorshSerialize + BorshDeserialize> {
    /// Arbitrary data to be signed
    pub data: T,
    /// The signature of the data
    pub sig: Signature,
}

impl Keypair {
    /// Convert this keypair to bytes.
    pub fn to_bytes(&self) -> [u8; 64] {
        let mut bytes: [u8; ed25519_dalek::KEYPAIR_LENGTH] =
            [0u8; ed25519_dalek::KEYPAIR_LENGTH];

        bytes[..ed25519_dalek::SECRET_KEY_LENGTH]
            .copy_from_slice(self.secret.0.as_bytes());
        bytes[ed25519_dalek::SECRET_KEY_LENGTH..]
            .copy_from_slice(self.public.0.as_bytes());
        bytes
    }

    /// Generate an ed25519 keypair.
    /// Wrapper for [`ed25519_dalek::Keypair::generate`].
    #[cfg(feature = "rand")]
    pub fn generate<R>(csprng: &mut R) -> Keypair
    where
        R: CryptoRng + RngCore,
    {
        ed25519_dalek::Keypair::generate(csprng).into()
    }

    /// Construct a `Keypair` from the bytes of a `PublicKey` and `SecretKey`.
    /// Wrapper for [`ed25519_dalek::Keypair::from_bytes`].
    pub fn from_bytes(bytes: &[u8]) -> Result<Keypair, SignatureError> {
        let keypair = ed25519_dalek::Keypair::from_bytes(bytes)?;
        Ok(keypair.into())
    }
}

impl<T> PartialEq for Signed<T>
where
    T: BorshSerialize + BorshDeserialize + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data && self.sig == other.sig
    }
}

impl<T> Eq for Signed<T> where
    T: BorshSerialize + BorshDeserialize + Eq + PartialEq
{
}

impl<T> Hash for Signed<T>
where
    T: BorshSerialize + BorshDeserialize + Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data.hash(state);
        self.sig.hash(state);
    }
}

impl<T> PartialOrd for Signed<T>
where
    T: BorshSerialize + BorshDeserialize + PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.data.partial_cmp(&other.data)
    }
}

impl<T> Signed<T>
where
    T: BorshSerialize + BorshDeserialize,
{
    /// Initialize a new signed data.
    pub fn new(keypair: &Keypair, data: T) -> Self {
        let to_sign = data
            .try_to_vec()
            .expect("Encoding data for signing shouldn't fail");
        let sig = sign(keypair, &to_sign);
        Self { data, sig }
    }

    /// Verify that the data has been signed by the secret key
    /// counterpart of the given public key.
    pub fn verify(&self, pk: &PublicKey) -> Result<(), VerifySigError> {
        let bytes = self
            .data
            .try_to_vec()
            .expect("Encoding data for verifying signature shouldn't fail");
        verify_signature_raw(pk, &bytes, &self.sig)
    }
}

impl BorshDeserialize for PublicKey {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        // deserialize the bytes first
        let bytes: Vec<u8> =
            BorshDeserialize::deserialize(buf).map_err(|e| {
                std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Error decoding ed25519 public key: {}", e),
                )
            })?;
        ed25519_dalek::PublicKey::from_bytes(&bytes)
            .map(PublicKey)
            .map_err(|e| {
                std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Error decoding ed25519 public key: {}", e),
                )
            })
    }
}

impl BorshSerialize for PublicKey {
    fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        // We need to turn the key to bytes first..
        let vec = self.0.as_bytes().to_vec();
        // .. and then encode them with Borsh
        let bytes = vec
            .try_to_vec()
            .expect("Public key bytes encoding shouldn't fail");
        writer.write_all(&bytes)
    }
}

impl BorshDeserialize for SecretKey {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        // deserialize the bytes first
        let bytes: Vec<u8> =
            BorshDeserialize::deserialize(buf).map_err(|e| {
                std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Error decoding ed25519 secret key: {}", e),
                )
            })?;
        ed25519_dalek::SecretKey::from_bytes(&bytes)
            .map(SecretKey)
            .map_err(|e| {
                std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Error decoding ed25519 secret key: {}", e),
                )
            })
    }
}

impl BorshSerialize for SecretKey {
    fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        // We need to turn the key to bytes first..
        let vec = self.0.as_bytes().to_vec();
        // .. and then encode them with Borsh
        let bytes = vec
            .try_to_vec()
            .expect("Secret key bytes encoding shouldn't fail");
        writer.write_all(&bytes)
    }
}

impl BorshDeserialize for Signature {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        // deserialize the bytes first
        let bytes: Vec<u8> =
            BorshDeserialize::deserialize(buf).map_err(|e| {
                std::io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Error decoding ed25519 signature: {}", e),
                )
            })?;
        // convert them to an expected size array
        let bytes: [u8; SIGNATURE_LEN] = bytes[..].try_into().map_err(|e| {
            std::io::Error::new(
                ErrorKind::InvalidInput,
                format!("Error decoding ed25519 signature: {}", e),
            )
        })?;
        Ok(Signature(ed25519_dalek::Signature::new(bytes)))
    }
}

impl BorshSerialize for Signature {
    fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        // We need to turn the signature to bytes first..
        let vec = self.0.to_bytes().to_vec();
        // .. and then encode them with Borsh
        let bytes = vec
            .try_to_vec()
            .expect("Signature bytes encoding shouldn't fail");
        writer.write_all(&bytes)
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for PublicKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.try_to_vec()
            .expect("Encoding public key shouldn't fail")
            .hash(state);
    }
}

impl PartialOrd for PublicKey {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.try_to_vec()
            .expect("Encoding public key shouldn't fail")
            .partial_cmp(
                &other
                    .try_to_vec()
                    .expect("Encoding public key shouldn't fail"),
            )
    }
}

impl Display for PublicKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vec = self
            .try_to_vec()
            .expect("Encoding public key shouldn't fail");
        write!(f, "{}", hex::encode(&vec))
    }
}

impl FromStr for PublicKey {
    type Err = ParsePublicKeyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let vec = hex::decode(s).map_err(ParsePublicKeyError::InvalidHex)?;
        BorshDeserialize::try_from_slice(&vec)
            .map_err(ParsePublicKeyError::InvalidEncoding)
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum ParsePublicKeyError {
    #[error("Invalid public key hex: {0}")]
    InvalidHex(hex::FromHexError),
    #[error("Invalid public key encoding: {0}")]
    InvalidEncoding(std::io::Error),
}

impl Display for SecretKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vec = self
            .try_to_vec()
            .expect("Encoding secret key shouldn't fail");
        write!(f, "{}", hex::encode(&vec))
    }
}

impl FromStr for SecretKey {
    type Err = ParseSecretKeyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let vec = hex::decode(s).map_err(ParseSecretKeyError::InvalidHex)?;
        BorshDeserialize::try_from_slice(&vec)
            .map_err(ParseSecretKeyError::InvalidEncoding)
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum ParseSecretKeyError {
    #[error("Invalid secret key hex: {0}")]
    InvalidHex(hex::FromHexError),
    #[error("Invalid secret key encoding: {0}")]
    InvalidEncoding(std::io::Error),
}

impl Display for Keypair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vec = self.try_to_vec().expect("Encoding keypair shouldn't fail");
        write!(f, "{}", hex::encode(&vec))
    }
}

impl FromStr for Keypair {
    type Err = ParseKeypairError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let vec = hex::decode(s).map_err(ParseKeypairError::InvalidHex)?;
        BorshDeserialize::try_from_slice(&vec)
            .map_err(ParseKeypairError::InvalidEncoding)
    }
}

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum ParseKeypairError {
    #[error("Invalid keypair hex: {0}")]
    InvalidHex(hex::FromHexError),
    #[error("Invalid keypair encoding: {0}")]
    InvalidEncoding(std::io::Error),
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Signature {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.try_to_vec()
            .expect("Encoding signature for hash shouldn't fail")
            .hash(state);
    }
}

impl PartialOrd for Signature {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.try_to_vec()
            .expect("Encoding signature shouldn't fail")
            .partial_cmp(
                &other
                    .try_to_vec()
                    .expect("Encoding signature shouldn't fail"),
            )
    }
}

impl From<ed25519_dalek::PublicKey> for PublicKey {
    fn from(pk: ed25519_dalek::PublicKey) -> Self {
        Self(pk)
    }
}

impl From<PublicKey> for ed25519_dalek::PublicKey {
    fn from(pk: PublicKey) -> Self {
        pk.0
    }
}

impl PublicKeyHash {
    fn from_public_key(pk: &PublicKey) -> Self {
        let pk_bytes =
            pk.try_to_vec().expect("Public key encoding shouldn't fail");
        let mut hasher = Sha256::new();
        hasher.update(pk_bytes);
        // hex of the first 40 chars of the hash
        Self(format!(
            "{:.width$X}",
            hasher.finalize(),
            width = PKH_HASH_LEN
        ))
    }
}

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

impl From<PublicKey> for PublicKeyHash {
    fn from(pk: PublicKey) -> Self {
        Self::from_public_key(&pk)
    }
}

impl From<&PublicKey> for PublicKeyHash {
    fn from(pk: &PublicKey) -> Self {
        Self::from_public_key(pk)
    }
}

impl From<ed25519_dalek::SecretKey> for SecretKey {
    fn from(sk: ed25519_dalek::SecretKey) -> Self {
        Self(sk)
    }
}

impl From<SecretKey> for ed25519_dalek::SecretKey {
    fn from(sk: SecretKey) -> Self {
        sk.0
    }
}

impl From<ed25519_dalek::Signature> for Signature {
    fn from(sig: ed25519_dalek::Signature) -> Self {
        Self(sig)
    }
}

impl From<Signature> for ed25519_dalek::Signature {
    fn from(sig: Signature) -> Self {
        sig.0
    }
}

impl From<ed25519_dalek::Keypair> for Keypair {
    fn from(keypair: ed25519_dalek::Keypair) -> Self {
        Self {
            secret: keypair.secret.into(),
            public: keypair.public.into(),
        }
    }
}

impl From<Keypair> for ed25519_dalek::Keypair {
    fn from(keypair: Keypair) -> Self {
        Self {
            secret: keypair.secret.into(),
            public: keypair.public.into(),
        }
    }
}

impl Signer<Signature> for Keypair {
    /// Sign a message with this keypair's secret key.
    fn try_sign(&self, message: &[u8]) -> Result<Signature, SignatureError> {
        let expanded: ExpandedSecretKey = (&self.secret.0).into();
        Ok(expanded.sign(message, &self.public.0).into())
    }
}

impl Verifier<Signature> for Keypair {
    /// Verify a signature on a message with this keypair's public key.
    fn verify(
        &self,
        message: &[u8],
        signature: &Signature,
    ) -> Result<(), SignatureError> {
        self.public.0.verify(message, &signature.0)
    }
}

impl Verifier<Signature> for PublicKey {
    /// Verify a signature on a message with this keypair's public key.
    ///
    /// # Return
    ///
    /// Returns `Ok(())` if the signature is valid, and `Err` otherwise.
    fn verify(
        &self,
        message: &[u8],
        signature: &Signature,
    ) -> Result<(), SignatureError> {
        self.0.verify(message, &signature.0)
    }
}

impl AsRef<[u8]> for Signature {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl ed25519_dalek::ed25519::signature::Signature for Signature {
    fn from_bytes(
        bytes: &[u8],
    ) -> Result<Self, ed25519_dalek::ed25519::signature::Error> {
        let sig: ed25519_dalek::Signature = bytes.try_into()?;
        Ok(sig.into())
    }
}

/// Run `cargo test gen_keypair -- --nocapture` to generate a keypair.
#[cfg(test)]
#[test]
fn gen_keypair() {
    use rand::prelude::ThreadRng;
    use rand::thread_rng;

    let mut rng: ThreadRng = thread_rng();
    let keypair = Keypair::generate(&mut rng);
    println!("keypair {:?}", keypair.to_bytes());
}

/// Helpers for testing with keys.
#[cfg(any(test, feature = "testing"))]
pub mod testing {
    use proptest::prelude::*;
    use rand::prelude::StdRng;
    use rand::SeedableRng;

    use super::*;

    /// A keypair for tests
    pub fn keypair_1() -> Keypair {
        // generated from `cargo test gen_keypair -- --nocapture`
        let bytes = [
            33, 82, 91, 186, 100, 168, 220, 158, 185, 140, 63, 172, 3, 88, 52,
            113, 94, 30, 213, 84, 175, 184, 235, 169, 70, 175, 36, 252, 45,
            190, 138, 79, 210, 187, 198, 90, 69, 83, 156, 77, 199, 63, 208, 63,
            137, 102, 22, 229, 110, 195, 38, 174, 142, 127, 157, 224, 139, 212,
            239, 204, 58, 80, 108, 184,
        ];
        ed25519_dalek::Keypair::from_bytes(&bytes).unwrap().into()
    }

    /// A keypair for tests
    pub fn keypair_2() -> Keypair {
        // generated from `cargo test gen_keypair -- --nocapture`
        let bytes = [
            27, 238, 157, 32, 131, 242, 184, 142, 146, 189, 24, 249, 68, 165,
            205, 71, 213, 158, 25, 253, 52, 217, 87, 52, 171, 225, 110, 131,
            238, 58, 94, 56, 218, 133, 189, 80, 14, 157, 68, 124, 151, 37, 127,
            173, 117, 91, 248, 234, 34, 13, 77, 148, 10, 75, 30, 191, 172, 85,
            175, 8, 36, 233, 18, 203,
        ];
        ed25519_dalek::Keypair::from_bytes(&bytes).unwrap().into()
    }

    /// Generate an arbitrary [`Keypair`].
    pub fn arb_keypair() -> impl Strategy<Value = Keypair> {
        any::<[u8; 32]>().prop_map(|seed| {
            let mut rng = StdRng::from_seed(seed);
            ed25519_dalek::Keypair::generate(&mut rng).into()
        })
    }
}

#[cfg(test)]
pub mod tests {
    use rand::prelude::ThreadRng;
    use rand::thread_rng;

    use super::*;

    /// Run `cargo test gen_keypair -- --nocapture` to generate a
    /// new keypair.
    #[test]
    fn gen_keypair() {
        let mut rng: ThreadRng = thread_rng();
        let keypair = Keypair::generate(&mut rng);
        let public_key: PublicKey = keypair.public;
        let secret_key: SecretKey = keypair.secret;
        println!("Public key: {}", public_key);
        println!("Secret key: {}", secret_key);
    }
}
