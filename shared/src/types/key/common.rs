//! Cryptographic keys

use std::fmt::Display;
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSerialize};
#[cfg(feature = "rand")]
use rand::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};

use super::{
    ed25519, ParsePublicKeyError, ParseSecretKeyError, ParseSignatureError,
    RefTo, SchemeType, SigScheme as SigSchemeTrait, VerifySigError,
};

/// Public key
#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Serialize,
    Deserialize,
    BorshSerialize,
    BorshDeserialize,
)]
pub enum PublicKey {
    /// Encapsulate Ed25519 public keys
    Ed25519(ed25519::PublicKey),
}

impl super::PublicKey for PublicKey {
    const TYPE: SchemeType = SigScheme::TYPE;

    fn try_from_pk<PK: super::PublicKey>(
        pk: &PK,
    ) -> Result<Self, ParsePublicKeyError> {
        if PK::TYPE == Self::TYPE {
            Self::try_from_slice(pk.try_to_vec().unwrap().as_slice())
                .map_err(ParsePublicKeyError::InvalidEncoding)
        } else if PK::TYPE == ed25519::PublicKey::TYPE {
            Ok(Self::Ed25519(
                ed25519::PublicKey::try_from_slice(
                    pk.try_to_vec().unwrap().as_slice(),
                )
                .map_err(ParsePublicKeyError::InvalidEncoding)?,
            ))
        } else {
            Err(ParsePublicKeyError::MismatchedScheme)
        }
    }
}

impl Display for PublicKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", hex::encode(&self.try_to_vec().unwrap()))
    }
}

impl FromStr for PublicKey {
    type Err = ParsePublicKeyError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        let vec = hex::decode(str).map_err(ParsePublicKeyError::InvalidHex)?;
        Self::try_from_slice(vec.as_slice())
            .map_err(ParsePublicKeyError::InvalidEncoding)
    }
}

/// Secret key
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
#[allow(clippy::large_enum_variant)]
pub enum SecretKey {
    /// Encapsulate Ed25519 secret keys
    Ed25519(ed25519::SecretKey),
}

impl super::SecretKey for SecretKey {
    type PublicKey = PublicKey;

    const TYPE: SchemeType = SigScheme::TYPE;

    fn try_from_sk<PK: super::SecretKey>(
        pk: &PK,
    ) -> Result<Self, ParseSecretKeyError> {
        if PK::TYPE == Self::TYPE {
            Self::try_from_slice(pk.try_to_vec().unwrap().as_ref())
                .map_err(ParseSecretKeyError::InvalidEncoding)
        } else if PK::TYPE == ed25519::SecretKey::TYPE {
            Ok(Self::Ed25519(
                ed25519::SecretKey::try_from_slice(
                    pk.try_to_vec().unwrap().as_ref(),
                )
                .map_err(ParseSecretKeyError::InvalidEncoding)?,
            ))
        } else {
            Err(ParseSecretKeyError::MismatchedScheme)
        }
    }
}

impl RefTo<PublicKey> for SecretKey {
    fn ref_to(&self) -> PublicKey {
        match self {
            SecretKey::Ed25519(sk) => PublicKey::Ed25519(sk.ref_to()),
        }
    }
}

impl Display for SecretKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", hex::encode(&self.try_to_vec().unwrap()))
    }
}

impl FromStr for SecretKey {
    type Err = ParseSecretKeyError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        let vec = hex::decode(str).map_err(ParseSecretKeyError::InvalidHex)?;
        Self::try_from_slice(vec.as_slice())
            .map_err(ParseSecretKeyError::InvalidEncoding)
    }
}

/// Signature
#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    PartialOrd,
    Hash,
    Serialize,
    Deserialize,
    BorshSerialize,
    BorshDeserialize,
)]
pub enum Signature {
    /// Encapsulate Ed25519 signatures
    Ed25519(ed25519::Signature),
}

impl super::Signature for Signature {
    const TYPE: SchemeType = SigScheme::TYPE;

    fn try_from_sig<PK: super::Signature>(
        pk: &PK,
    ) -> Result<Self, ParseSignatureError> {
        if PK::TYPE == Self::TYPE {
            Self::try_from_slice(pk.try_to_vec().unwrap().as_slice())
                .map_err(ParseSignatureError::InvalidEncoding)
        } else if PK::TYPE == ed25519::Signature::TYPE {
            Ok(Self::Ed25519(
                ed25519::Signature::try_from_slice(
                    pk.try_to_vec().unwrap().as_slice(),
                )
                .map_err(ParseSignatureError::InvalidEncoding)?,
            ))
        } else {
            Err(ParseSignatureError::MismatchedScheme)
        }
    }
}

/// An implementation of the common signature scheme
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
    Default,
)]
pub struct SigScheme;

impl super::SigScheme for SigScheme {
    type PublicKey = PublicKey;
    type SecretKey = SecretKey;
    type Signature = Signature;

    const TYPE: SchemeType = SchemeType::Common;

    #[cfg(feature = "rand")]
    fn generate<R>(_csprng: &mut R) -> SecretKey
    where
        R: CryptoRng + RngCore,
    {
        panic!(
            "Cannot generate common signing scheme. Must convert from \
             alternative scheme."
        );
    }

    fn sign(keypair: &SecretKey, data: impl AsRef<[u8]>) -> Self::Signature {
        match keypair {
            SecretKey::Ed25519(kp) => {
                Signature::Ed25519(ed25519::SigScheme::sign(kp, data))
            }
        }
    }

    fn verify_signature<T: BorshSerialize + BorshDeserialize>(
        pk: &Self::PublicKey,
        data: &T,
        sig: &Self::Signature,
    ) -> Result<(), VerifySigError> {
        match (pk, sig) {
            (PublicKey::Ed25519(pk), Signature::Ed25519(sig)) => {
                ed25519::SigScheme::verify_signature(pk, data, sig)
            } // _ => Err(VerifySigError::MismatchedScheme),
        }
    }

    fn verify_signature_raw(
        pk: &Self::PublicKey,
        data: &[u8],
        sig: &Self::Signature,
    ) -> Result<(), VerifySigError> {
        match (pk, sig) {
            (PublicKey::Ed25519(pk), Signature::Ed25519(sig)) => {
                ed25519::SigScheme::verify_signature_raw(pk, data, sig)
            } // _ => Err(VerifySigError::MismatchedScheme),
        }
    }
}
