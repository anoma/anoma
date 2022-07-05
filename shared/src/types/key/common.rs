//! Cryptographic keys

use std::fmt::Display;
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
#[cfg(feature = "rand")]
use rand::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};

use super::{
    ed25519, secp256k1, ParsePublicKeyError, ParseSecretKeyError,
    ParseSignatureError, RefTo, SchemeType, SigScheme as SigSchemeTrait,
    VerifySigError,
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
    BorshSchema,
)]
pub enum PublicKey {
    /// Encapsulate Ed25519 public keys
    Ed25519(ed25519::PublicKey),
    /// Encapsulate Secp256k1 public keys
    Secp256k1(secp256k1::PublicKey),
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
        } else if PK::TYPE == secp256k1::PublicKey::TYPE {
            Ok(Self::Secp256k1(
                secp256k1::PublicKey::try_from_slice(
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
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize, BorshSchema)]
#[allow(clippy::large_enum_variant)]
pub enum SecretKey {
    /// Encapsulate Ed25519 secret keys
    Ed25519(ed25519::SecretKey),
    /// Encapsulate Secp256k1 secret keys
    Secp256k1(secp256k1::SecretKey),
}

impl Serialize for SecretKey {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // String encoded, because toml doesn't support enums
        let prefix = match self {
            SecretKey::Ed25519(_) => "ED25519_SK_PREFIX",
            SecretKey::Secp256k1(_) => "SECP256K1_SK_PREFIX",
        };
        let keypair_string = format!("{}{}", prefix, self);
        Serialize::serialize(&keypair_string, serializer)
    }
}

impl<'de> Deserialize<'de> for SecretKey {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let keypair_string: String =
            serde::Deserialize::deserialize(deserializer)
                .map_err(D::Error::custom)?;
        if let Some(raw) = keypair_string.strip_prefix("ED25519_SK_PREFIX") {
            SecretKey::from_str(raw).map_err(D::Error::custom)
        } else if let Some(raw) =
            keypair_string.strip_prefix("SECP256K1_SK_PREFIX")
        {
            SecretKey::from_str(raw).map_err(D::Error::custom)
        } else {
            Err(D::Error::custom(
                "Could not deserialize SecretKey do to invalid prefix",
            ))
        }
    }
}

impl super::SecretKey for SecretKey {
    type PublicKey = PublicKey;

    const TYPE: SchemeType = SigScheme::TYPE;

    fn try_from_sk<SK: super::SecretKey>(
        sk: &SK,
    ) -> Result<Self, ParseSecretKeyError> {
        if SK::TYPE == Self::TYPE {
            Self::try_from_slice(sk.try_to_vec().unwrap().as_ref())
                .map_err(ParseSecretKeyError::InvalidEncoding)
        } else if SK::TYPE == ed25519::SecretKey::TYPE {
            Ok(Self::Ed25519(
                ed25519::SecretKey::try_from_slice(
                    sk.try_to_vec().unwrap().as_ref(),
                )
                .map_err(ParseSecretKeyError::InvalidEncoding)?,
            ))
        } else if SK::TYPE == secp256k1::SecretKey::TYPE {
            Ok(Self::Secp256k1(
                secp256k1::SecretKey::try_from_slice(
                    sk.try_to_vec().unwrap().as_ref(),
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
            SecretKey::Secp256k1(sk) => PublicKey::Secp256k1(sk.ref_to()),
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
    BorshSchema,
)]
pub enum Signature {
    /// Encapsulate Ed25519 signatures
    Ed25519(ed25519::Signature),
    /// Encapsulate Secp256k1 signatures
    Secp256k1(secp256k1::Signature),
}

impl super::Signature for Signature {
    const TYPE: SchemeType = SigScheme::TYPE;

    fn try_from_sig<SIG: super::Signature>(
        sig: &SIG,
    ) -> Result<Self, ParseSignatureError> {
        if SIG::TYPE == Self::TYPE {
            Self::try_from_slice(sig.try_to_vec().unwrap().as_slice())
                .map_err(ParseSignatureError::InvalidEncoding)
        } else if SIG::TYPE == ed25519::Signature::TYPE {
            Ok(Self::Ed25519(
                ed25519::Signature::try_from_slice(
                    sig.try_to_vec().unwrap().as_slice(),
                )
                .map_err(ParseSignatureError::InvalidEncoding)?,
            ))
        } else if SIG::TYPE == secp256k1::Signature::TYPE {
            Ok(Self::Secp256k1(
                secp256k1::Signature::try_from_slice(
                    sig.try_to_vec().unwrap().as_slice(),
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
    BorshSchema,
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
            SecretKey::Secp256k1(kp) => {
                Signature::Secp256k1(secp256k1::SigScheme::sign(kp, data))
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
            }
            (PublicKey::Secp256k1(pk), Signature::Secp256k1(sig)) => {
                secp256k1::SigScheme::verify_signature(pk, data, sig)
            }
            _ => Err(VerifySigError::MismatchedScheme),
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
            }
            (PublicKey::Secp256k1(pk), Signature::Secp256k1(sig)) => {
                secp256k1::SigScheme::verify_signature_raw(pk, data, sig)
            }
            _ => Err(VerifySigError::MismatchedScheme),
        }
    }
}
