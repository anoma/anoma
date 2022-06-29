//! MASP types

use std::fmt::Display;
use std::io::{Error, ErrorKind};
use std::str::FromStr;

use bech32::{FromBase32, ToBase32};
use borsh::{BorshDeserialize, BorshSerialize};
use sha2::{Digest, Sha256};

use crate::types::address::{
    masp, Address, DecodeError, BECH32M_VARIANT, HASH_LEN,
};

/// human-readable part of Bech32m encoded address
// TODO use "a" for live network
const EXT_FULL_VIEWING_KEY_HRP: &str = "xfvktest";
const PAYMENT_ADDRESS_HRP: &str = "patest";
const PINNED_PAYMENT_ADDRESS_HRP: &str = "ppatest";
const EXT_SPENDING_KEY_HRP: &str = "xsktest";

/// Wrapper for masp_primitive's FullViewingKey
#[derive(
    Clone,
    Debug,
    Copy,
    Hash,
    BorshSerialize,
    BorshDeserialize,
    Eq,
    PartialEq,
    PartialOrd,
    Ord,
)]
pub struct ExtendedViewingKey(masp_primitives::zip32::ExtendedFullViewingKey);

impl Display for ExtendedViewingKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut bytes = [0; 169];
        self.0
            .write(&mut bytes[..])
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

impl From<ExtendedViewingKey>
    for masp_primitives::zip32::ExtendedFullViewingKey
{
    fn from(key: ExtendedViewingKey) -> Self {
        key.0
    }
}

impl From<masp_primitives::zip32::ExtendedFullViewingKey>
    for ExtendedViewingKey
{
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
#[derive(
    Clone,
    Debug,
    Copy,
    PartialOrd,
    Ord,
    Eq,
    PartialEq,
    Hash,
    BorshSerialize,
    BorshDeserialize,
)]
pub struct PaymentAddress(masp_primitives::primitives::PaymentAddress, bool);

impl PaymentAddress {
    /// Turn this PaymentAddress into a pinned/unpinned one
    pub fn pinned(self, pin: bool) -> PaymentAddress {
        PaymentAddress(self.0, pin)
    }

    /// Determine whether this PaymentAddress is pinned
    pub fn is_pinned(&self) -> bool {
        self.1
    }

    /// Hash this payment address
    pub fn hash(&self) -> String {
        let bytes = (self.0, self.1)
            .try_to_vec()
            .expect("Payment address encoding shouldn't fail");
        let mut hasher = Sha256::new();
        hasher.update(bytes);
        // hex of the first 40 chars of the hash
        format!("{:.width$X}", hasher.finalize(), width = HASH_LEN)
    }
}

impl From<PaymentAddress> for masp_primitives::primitives::PaymentAddress {
    fn from(addr: PaymentAddress) -> Self {
        addr.0
    }
}

impl From<masp_primitives::primitives::PaymentAddress> for PaymentAddress {
    fn from(addr: masp_primitives::primitives::PaymentAddress) -> Self {
        Self(addr, false)
    }
}

impl Display for PaymentAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = self.0.to_bytes();
        let hrp = if self.1 {
            PINNED_PAYMENT_ADDRESS_HRP
        } else {
            PAYMENT_ADDRESS_HRP
        };
        let encoded = bech32::encode(hrp, bytes.to_base32(), BECH32M_VARIANT)
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
        let pinned = if prefix == PAYMENT_ADDRESS_HRP {
            false
        } else if prefix == PINNED_PAYMENT_ADDRESS_HRP {
            true
        } else {
            return Err(DecodeError::UnexpectedBech32Prefix(
                prefix,
                PAYMENT_ADDRESS_HRP.into(),
            ));
        };
        match variant {
            BECH32M_VARIANT => {}
            _ => return Err(DecodeError::UnexpectedBech32Variant(variant)),
        }
        let addr_len_err = |_| {
            DecodeError::InvalidInnerEncoding(Error::new(
                ErrorKind::InvalidData,
                "expected 43 bytes for the payment address",
            ))
        };
        let addr_data_err = || {
            DecodeError::InvalidInnerEncoding(Error::new(
                ErrorKind::InvalidData,
                "invalid payment address provided",
            ))
        };
        let bytes: Vec<u8> = FromBase32::from_base32(&base32)
            .map_err(DecodeError::DecodeBase32)?;
        masp_primitives::primitives::PaymentAddress::from_bytes(
            &bytes.try_into().map_err(addr_len_err)?,
        )
        .ok_or_else(addr_data_err)
        .map(|x| Self(x, pinned))
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
        self.0
            .write(&mut &mut bytes[..])
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
#[allow(clippy::large_enum_variant)]
#[derive(
    Debug,
    Clone,
    Hash,
    BorshSerialize,
    BorshDeserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
)]
pub enum BalanceOwner {
    /// A balance stored at a transparent address
    Address(Address),
    /// A balance stored at a shielded address
    FullViewingKey(ExtendedViewingKey),
    /// A balance stored at a payment address
    PaymentAddress(PaymentAddress),
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

    /// Get the contained PaymentAddress, if any
    pub fn payment_address(&self) -> Option<PaymentAddress> {
        match self {
            Self::PaymentAddress(x) => Some(*x),
            _ => None,
        }
    }
}

/// Represents any MASP value
#[allow(clippy::large_enum_variant)]
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
        PaymentAddress::from_str(s)
            .map(Self::PaymentAddress)
            .or_else(|_err| {
                ExtendedSpendingKey::from_str(s).map(Self::ExtendedSpendingKey)
            })
            .or_else(|_err| {
                ExtendedViewingKey::from_str(s).map(Self::FullViewingKey)
            })
    }
}
