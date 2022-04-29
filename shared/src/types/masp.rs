//! MASP types

use std::fmt::Display;
use std::str::FromStr;
use std::io::{Error, ErrorKind};

use bech32::{FromBase32, ToBase32};
use borsh::{BorshSerialize, BorshDeserialize};

use crate::types::address::{Address, DecodeError, BECH32M_VARIANT, masp};

pub use bellman::groth16::PreparedVerifyingKey;
pub use bls12_381::Bls12;
use group::GroupEncoding;
pub use masp_primitives::primitives::Note;
pub use masp_primitives::redjubjub::PublicKey;
pub use masp_proofs::sapling::SaplingVerificationContext;

/// human-readable part of Bech32m encoded address
// TODO use "a" for live network
const EXT_FULL_VIEWING_KEY_HRP: &str = "xfvktest";
const PAYMENT_ADDRESS_HRP: &str = "patest";
const EXT_SPENDING_KEY_HRP: &str = "xsktest";

/// Wrapper for masp_primitive's FullViewingKey
#[derive(Clone, Debug, Copy)]
pub struct ExtendedViewingKey(masp_primitives::zip32::ExtendedFullViewingKey);

impl Display for ExtendedViewingKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut bytes = [0; 169];
        self.0.write(&mut bytes[..])
            .expect("should be able to serialize an ExtendedFullViewingKey");
        let encoded = bech32::encode(
            EXT_FULL_VIEWING_KEY_HRP,
            bytes.to_base32(),
            BECH32M_VARIANT,
        )
        .unwrap_or_else(|_| {
            panic!(
                "The human-readable part {} should never cause a failure",
                EXT_FULL_VIEWING_KEY_HRP
            )
        });
        write!(f, "{encoded}")
    }
}

impl FromStr for ExtendedViewingKey {
    type Err = DecodeError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (prefix, base32, variant) =
            bech32::decode(string).map_err(DecodeError::DecodeBech32)?;
        if prefix != EXT_FULL_VIEWING_KEY_HRP {
            return Err(DecodeError::UnexpectedBech32Prefix(
                prefix,
                EXT_FULL_VIEWING_KEY_HRP.into(),
            ));
        }
        match variant {
            BECH32M_VARIANT => {}
            _ => return Err(DecodeError::UnexpectedBech32Variant(variant)),
        }
        let bytes: Vec<u8> = FromBase32::from_base32(&base32)
            .map_err(DecodeError::DecodeBase32)?;
        masp_primitives::zip32::ExtendedFullViewingKey::read(&mut &bytes[..])
            .map_err(DecodeError::InvalidInnerEncoding)
            .map(Self)
    }
}

impl From<ExtendedViewingKey> for masp_primitives::zip32::ExtendedFullViewingKey {
    fn from(key: ExtendedViewingKey) -> Self {
        key.0
    }
}

impl From<masp_primitives::zip32::ExtendedFullViewingKey> for ExtendedViewingKey {
    fn from(key: masp_primitives::zip32::ExtendedFullViewingKey) -> Self {
        Self(key)
    }
}

impl serde::Serialize for ExtendedViewingKey {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let encoded = self.to_string();
        serde::Serialize::serialize(&encoded, serializer)
    }
}

impl<'de> serde::Deserialize<'de> for ExtendedViewingKey {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let encoded: String = serde::Deserialize::deserialize(deserializer)?;
        Self::from_str(&encoded).map_err(D::Error::custom)
    }
}

/// Wrapper for masp_primitive's PaymentAddress
#[derive(Clone, Debug, Copy, PartialOrd, Ord, Eq, PartialEq)]
pub struct PaymentAddress(masp_primitives::primitives::PaymentAddress);

impl From<PaymentAddress> for masp_primitives::primitives::PaymentAddress {
    fn from(addr: PaymentAddress) -> Self {
        addr.0
    }
}

impl From<masp_primitives::primitives::PaymentAddress> for PaymentAddress {
    fn from(addr: masp_primitives::primitives::PaymentAddress) -> Self {
        Self(addr)
    }
}

impl Display for PaymentAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = self.0.to_bytes();
        let encoded = bech32::encode(
            PAYMENT_ADDRESS_HRP,
            bytes.to_base32(),
            BECH32M_VARIANT,
        )
        .unwrap_or_else(|_| {
            panic!(
                "The human-readable part {} should never cause a failure",
                PAYMENT_ADDRESS_HRP
            )
        });
        write!(f, "{encoded}")
    }
}

impl FromStr for PaymentAddress {
    type Err = DecodeError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (prefix, base32, variant) =
            bech32::decode(string).map_err(DecodeError::DecodeBech32)?;
        if prefix != PAYMENT_ADDRESS_HRP {
            return Err(DecodeError::UnexpectedBech32Prefix(
                prefix,
                PAYMENT_ADDRESS_HRP.into(),
            ));
        }
        match variant {
            BECH32M_VARIANT => {}
            _ => return Err(DecodeError::UnexpectedBech32Variant(variant)),
        }
        let addr_len_err = |_| DecodeError::InvalidInnerEncoding(
            Error::new(ErrorKind::InvalidData, "expected 43 bytes for the payment address")
        );
        let addr_data_err = || DecodeError::InvalidInnerEncoding(
            Error::new(ErrorKind::InvalidData, "invalid payment address provided")
        );
        let bytes: Vec<u8> = FromBase32::from_base32(&base32)
            .map_err(DecodeError::DecodeBase32)?;
        masp_primitives::primitives::PaymentAddress::from_bytes(
            &bytes.try_into().map_err(addr_len_err)?
        ).ok_or_else(addr_data_err).map(Self)
    }
}

impl serde::Serialize for PaymentAddress {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let encoded = self.to_string();
        serde::Serialize::serialize(&encoded, serializer)
    }
}

impl<'de> serde::Deserialize<'de> for PaymentAddress {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let encoded: String = serde::Deserialize::deserialize(deserializer)?;
        Self::from_str(&encoded).map_err(D::Error::custom)
    }
}

/// Wrapper for masp_primitive's ExtendedSpendingKey
#[derive(Clone, Debug, Copy, BorshSerialize, BorshDeserialize)]
pub struct ExtendedSpendingKey(masp_primitives::zip32::ExtendedSpendingKey);

impl Display for ExtendedSpendingKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut bytes = [0; 169];
        self.0.write(&mut &mut bytes[..])
            .expect("should be able to serialize an ExtendedSpendingKey");
        let encoded = bech32::encode(
            EXT_SPENDING_KEY_HRP,
            bytes.to_base32(),
            BECH32M_VARIANT,
        )
        .unwrap_or_else(|_| {
            panic!(
                "The human-readable part {} should never cause a failure",
                EXT_SPENDING_KEY_HRP
            )
        });
        write!(f, "{encoded}")
    }
}

impl FromStr for ExtendedSpendingKey {
    type Err = DecodeError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (prefix, base32, variant) =
            bech32::decode(string).map_err(DecodeError::DecodeBech32)?;
        if prefix != EXT_SPENDING_KEY_HRP {
            return Err(DecodeError::UnexpectedBech32Prefix(
                prefix,
                EXT_SPENDING_KEY_HRP.into(),
            ));
        }
        match variant {
            BECH32M_VARIANT => {}
            _ => return Err(DecodeError::UnexpectedBech32Variant(variant)),
        }
        let bytes: Vec<u8> = FromBase32::from_base32(&base32)
            .map_err(DecodeError::DecodeBase32)?;
        masp_primitives::zip32::ExtendedSpendingKey::read(&mut &bytes[..])
            .map_err(DecodeError::InvalidInnerEncoding)
            .map(Self)
    }
}

impl From<ExtendedSpendingKey> for masp_primitives::zip32::ExtendedSpendingKey {
    fn from(key: ExtendedSpendingKey) -> Self {
        key.0
    }
}

impl From<masp_primitives::zip32::ExtendedSpendingKey> for ExtendedSpendingKey {
    fn from(key: masp_primitives::zip32::ExtendedSpendingKey) -> Self {
        Self(key)
    }
}

impl serde::Serialize for ExtendedSpendingKey {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let encoded = self.to_string();
        serde::Serialize::serialize(&encoded, serializer)
    }
}

impl<'de> serde::Deserialize<'de> for ExtendedSpendingKey {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let encoded: String = serde::Deserialize::deserialize(deserializer)?;
        Self::from_str(&encoded).map_err(D::Error::custom)
    }
}

/// Represents a source of funds for a transfer
#[derive(Debug, Clone)]
pub enum TransferSource {
    /// A transfer coming from a transparent address
    Address(Address),
    /// A transfer coming from a shielded address
    ExtendedSpendingKey(ExtendedSpendingKey),
}

impl TransferSource {
    /// Get the transparent address that this source would effectively draw from
    pub fn effective_address(&self) -> Address {
        match self {
            Self::Address(x) => x.clone(),
            // An ExtendedSpendingKey for a source effectively means that
            // assets will be drawn from the MASP
            Self::ExtendedSpendingKey(_) => masp(),
        }
    }
    /// Get the contained ExtendedSpendingKey contained, if any
    pub fn spending_key(&self) -> Option<ExtendedSpendingKey> {
        match self {
            Self::ExtendedSpendingKey(x) => Some(*x),
            _ => None,
        }
    }
}

/// Represents a target for the funds of a transfer
#[derive(Debug, Clone)]
pub enum TransferTarget {
    /// A transfer going to a transparent address
    Address(Address),
    /// A transfer going to a shielded address
    PaymentAddress(PaymentAddress),
}

impl TransferTarget {
    /// Get the transparent address that this target would effectively go to
    pub fn effective_address(&self) -> Address {
        match self {
            Self::Address(x) => x.clone(),
            // An ExtendedSpendingKey for a source effectively means that
            // assets will be drawn from the MASP
            Self::PaymentAddress(_) => masp(),
        }
    }
    /// Get the contained PaymentAddress, if any
    pub fn payment_address(&self) -> Option<PaymentAddress> {
        match self {
            Self::PaymentAddress(x) => Some(*x),
            _ => None,
        }
    }
    /// Get the contained Address, if any
    pub fn address(&self) -> Option<Address> {
        match self {
            Self::Address(x) => Some(x.clone()),
            _ => None,
        }
    }
}

/// Represents the owner of arbitrary funds
#[derive(Debug, Clone)]
pub enum BalanceOwner {
    /// A balance stored at a transparent address
    Address(Address),
    /// A balance stored at a shielded address
    FullViewingKey(ExtendedViewingKey),
}

impl BalanceOwner {
    /// Get the contained Address, if any
    pub fn address(&self) -> Option<Address> {
        match self {
            Self::Address(x) => Some(x.clone()),
            _ => None,
        }
    }

    /// Get the contained FullViewingKey, if any
    pub fn full_viewing_key(&self) -> Option<ExtendedViewingKey> {
        match self {
            Self::FullViewingKey(x) => Some(*x),
            _ => None,
        }
    }
}

/// Represents any MASP value
#[derive(Debug, Clone)]
pub enum MaspValue {
    /// A MASP PaymentAddress
    PaymentAddress(PaymentAddress),
    /// A MASP ExtendedSpendingKey
    ExtendedSpendingKey(ExtendedSpendingKey),
    /// A MASP FullViewingKey
    FullViewingKey(ExtendedViewingKey),
}

impl FromStr for MaspValue {
    type Err = DecodeError;
    
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Try to decode this value first as a PaymentAddress, then as an
        // ExtendedSpendingKey, then as FullViewingKey
        PaymentAddress::from_str(s).map(Self::PaymentAddress)
            .or_else(|_err| ExtendedSpendingKey::from_str(s)
                     .map(Self::ExtendedSpendingKey))
            .or_else(|_err| ExtendedViewingKey::from_str(s)
                     .map(Self::FullViewingKey))
    }
}

/// An encrypted note on the Merkle tree
pub struct EncryptedNote {
    /// Note ciphertext
    // TODO: fixed size
    pub c_enc: Vec<u8>,
    /// Viewing key recovery information
    // TODO: fixed size
    pub c_out: Vec<u8>,
}

/// Spend description
pub struct SpendDescription {
    /// Value commitment to amount of the asset in the note being spent
    pub cv: jubjub::ExtendedPoint,
    /// Last block's commitment tree root
    pub anchor: bls12_381::Scalar,
    /// Nullifier for the note being spent
    pub nullifier: [u8; 32],
    /// Re-randomized version of the spend authorization key
    pub rk: masp_primitives::redjubjub::PublicKey,
    /// Spend authorization signature
    pub spend_auth_sig: masp_primitives::redjubjub::Signature,
    /// Signature hash
    pub sighash_value: [u8; 32],
    /// Zero-knowledge proof of the note and proof-authorizing key
    pub zkproof: bellman::groth16::Proof<bls12_381::Bls12>,
}

#[derive(BorshDeserialize, BorshSerialize)]
#[allow(missing_docs)]
pub struct SpendDescriptionBytes {
    pub cv: [u8; 32],
    pub anchor: [u8; 32],
    pub nullifier: [u8; 32],
    pub rk: [u8; 32],
    pub spend_auth_sig: Vec<u8>,
    pub sighash_value: [u8; 32],
    pub zkproof: Vec<u8>,
}

impl From<SpendDescription> for SpendDescriptionBytes {
    fn from(s: SpendDescription) -> Self {
        let mut sig = Vec::new();
        s.spend_auth_sig.write(&mut sig).unwrap();
        let mut proof = Vec::new();
        s.zkproof.write(&mut proof).unwrap();
        SpendDescriptionBytes {
            cv: s.cv.to_bytes(),
            anchor: s.anchor.to_bytes(),
            nullifier: s.nullifier.clone(),
            rk: s.rk.0.to_bytes(),
            spend_auth_sig: sig,
            sighash_value: s.sighash_value.clone(),
            zkproof: proof,
        }
    }
}

impl From<SpendDescriptionBytes> for SpendDescription {
    fn from(s: SpendDescriptionBytes) -> Self {
        let sig = masp_primitives::redjubjub::Signature::read(
            s.spend_auth_sig.as_slice(),
        )
        .unwrap();
        let proof =
            bellman::groth16::Proof::read(s.zkproof.as_slice()).unwrap();
        SpendDescription {
            cv: jubjub::ExtendedPoint::from_bytes(&s.cv).unwrap(),
            anchor: bls12_381::Scalar::from_bytes(&s.anchor).unwrap(),
            nullifier: s.nullifier.clone(),
            rk: PublicKey(jubjub::ExtendedPoint::from_bytes(&s.rk).unwrap()),
            spend_auth_sig: sig,
            sighash_value: s.sighash_value.clone(),
            zkproof: proof,
        }
    }
}

/// Output description
pub struct OutputDescription {
    /// Value commitment to amount of the asset in the note being created
    pub cv: jubjub::ExtendedPoint,
    /// Derived commitment tree location for the output note
    pub cmu: bls12_381::Scalar,
    /// Note encryption public key
    pub epk: jubjub::ExtendedPoint,
    /// Encrypted note ciphertext
    // TODO: c_enc: [u8; masp_primitives::note_encryption::ENC_CIPHERTEXT_SIZE],
    pub c_enc: Vec<u8>,
    /// Encrypted note key recovery ciphertext
    // TODO: c_out: [u8; masp_primitives::note_encryption::OUT_CIPHERTEXT_SIZE],
    pub c_out: Vec<u8>,
    /// Zero-knowledge proof of the commitment tree location
    pub zkproof: bellman::groth16::Proof<bls12_381::Bls12>,
}

#[derive(BorshDeserialize, BorshSerialize)]
#[allow(missing_docs)]
pub struct OutputDescriptionBytes {
    pub cv: [u8; 32],
    pub cmu: [u8; 32],
    pub epk: [u8; 32],
    pub c_enc: Vec<u8>,
    pub c_out: Vec<u8>,
    pub zkproof: Vec<u8>,
}

impl From<OutputDescription> for OutputDescriptionBytes {
    fn from(o: OutputDescription) -> Self {
        let mut proof = Vec::new();
        o.zkproof.write(&mut proof).unwrap();
        OutputDescriptionBytes {
            cv: o.cv.to_bytes(),
            cmu: o.cmu.to_bytes(),
            epk: o.epk.to_bytes(),
            c_enc: o.c_enc.clone(),
            c_out: o.c_out.clone(),
            zkproof: proof,
        }
    }
}

impl From<OutputDescriptionBytes> for OutputDescription {
    fn from(o: OutputDescriptionBytes) -> Self {
        let proof =
            bellman::groth16::Proof::read(o.zkproof.as_slice()).unwrap();
        OutputDescription {
            cv: jubjub::ExtendedPoint::from_bytes(&o.cv).unwrap(),
            cmu: bls12_381::Scalar::from_bytes(&o.cmu).unwrap(),
            epk: jubjub::ExtendedPoint::from_bytes(&o.epk).unwrap(),
            c_enc: o.c_enc.clone(),
            c_out: o.c_out.clone(),
            zkproof: proof,
        }
    }
}

/// Shielded transaction
pub struct ShieldedTransaction {
    /// Spend descriptions
    pub spends: Vec<SpendDescription>,
    /// Output descriptions
    pub outputs: Vec<OutputDescription>,
    /// Per-asset value balance
    pub assets_and_values: Vec<(masp_primitives::asset_type::AssetType, i64)>,
    /// Binding signature
    pub binding_sig: masp_primitives::redjubjub::Signature,
    /// Binding signature hash
    pub sighash_value: [u8; 32],
}

#[derive(BorshDeserialize,BorshSerialize)]
#[allow(missing_docs)]
pub struct ShieldedTransactionBytes {
    pub spends: Vec<SpendDescriptionBytes>,
    pub outputs: Vec<OutputDescriptionBytes>,
    pub assets_and_values: Vec<([u8; 32], i64)>,
    pub binding_sig: Vec<u8>,
    pub sighash_value: [u8; 32],
}

impl From<ShieldedTransaction> for ShieldedTransactionBytes {
    fn from(tx: ShieldedTransaction) -> Self {
        let mut sig = Vec::new();
        tx.binding_sig.write(&mut sig).unwrap();
        ShieldedTransactionBytes {
            spends: tx.spends.into_iter().map(|s| s.into()).collect(),
            outputs: tx.outputs.into_iter().map(|s| s.into()).collect(),
            assets_and_values: tx
                .assets_and_values
                .into_iter()
                .map(|(asset, value)| {
                    (asset.get_identifier().to_owned(), value)
                })
                .collect(),
            binding_sig: sig,
            sighash_value: tx.sighash_value.clone(),
        }
    }
}

impl From<ShieldedTransactionBytes> for ShieldedTransaction {
    fn from(tx: ShieldedTransactionBytes) -> Self {
        let sig = masp_primitives::redjubjub::Signature::read(
            tx.binding_sig.as_slice(),
        )
        .unwrap();
        ShieldedTransaction {
            spends: tx.spends.into_iter().map(|s| s.into()).collect(),
            outputs: tx.outputs.into_iter().map(|s| s.into()).collect(),
            assets_and_values: tx
                .assets_and_values
                .into_iter()
                .map(|(asset, value)| {
                    (
                        masp_primitives::asset_type::AssetType::from_identifier(
                            &asset,
                        ).unwrap(),
                        value,
                    )
                })
                .collect(),
            binding_sig: sig,
            sighash_value: tx.sighash_value.clone(),
        }
    }
}
