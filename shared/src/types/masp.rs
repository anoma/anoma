//! MASP types

use std::fmt::Display;
use std::str::FromStr;
use std::io::{Error, ErrorKind};

use bech32::{FromBase32, ToBase32};

use crate::types::address::{DecodeError, BECH32M_VARIANT};

/// human-readable part of Bech32m encoded address
// TODO use "a" for live network
const FULL_VIEWING_KEY_HRP: &str = "fvktest";
const PAYMENT_ADDRESS_HRP: &str = "patest";
const EXTENDED_SPENDING_KEY_HRP: &str = "esktest";

/// Wrapper for masp_primitive's FullViewingKey
pub struct FullViewingKey(masp_primitives::keys::FullViewingKey);

impl Display for FullViewingKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = self.0.to_bytes();
        let encoded = bech32::encode(
            FULL_VIEWING_KEY_HRP,
            bytes.to_base32(),
            BECH32M_VARIANT,
        )
        .unwrap_or_else(|_| {
            panic!(
                "The human-readable part {} should never cause a failure",
                FULL_VIEWING_KEY_HRP
            )
        });
        writeln!(f, "{encoded}")
    }
}

impl FromStr for FullViewingKey {
    type Err = DecodeError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (prefix, base32, variant) =
            bech32::decode(string).map_err(DecodeError::DecodeBech32)?;
        if prefix != FULL_VIEWING_KEY_HRP {
            return Err(DecodeError::UnexpectedBech32Prefix(
                prefix,
                FULL_VIEWING_KEY_HRP.into(),
            ));
        }
        match variant {
            BECH32M_VARIANT => {}
            _ => return Err(DecodeError::UnexpectedBech32Variant(variant)),
        }
        let bytes: Vec<u8> = FromBase32::from_base32(&base32)
            .map_err(DecodeError::DecodeBase32)?;
        masp_primitives::keys::FullViewingKey::read(&mut &bytes[..])
            .map_err(DecodeError::InvalidInnerEncoding)
            .map(Self)
    }
}

impl From<FullViewingKey> for masp_primitives::keys::FullViewingKey {
    fn from(key: FullViewingKey) -> Self {
        key.0
    }
}

impl From<masp_primitives::keys::FullViewingKey> for FullViewingKey {
    fn from(key: masp_primitives::keys::FullViewingKey) -> Self {
        Self(key)
    }
}

/// Wrapper for masp_primitive's PaymentAddress
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
        writeln!(f, "{encoded}")
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

/// Wrapper for masp_primitive's ExtendedSpendingKey
pub struct ExtendedSpendingKey(masp_primitives::zip32::ExtendedSpendingKey);

impl Display for ExtendedSpendingKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut bytes = [0; 169];
        self.0.write(&mut &mut bytes[..])
            .expect("should be able to serialize an ExtendedSpendingKey");
        let encoded = bech32::encode(
            EXTENDED_SPENDING_KEY_HRP,
            bytes.to_base32(),
            BECH32M_VARIANT,
        )
        .unwrap_or_else(|_| {
            panic!(
                "The human-readable part {} should never cause a failure",
                EXTENDED_SPENDING_KEY_HRP
            )
        });
        writeln!(f, "{encoded}")
    }
}

impl FromStr for ExtendedSpendingKey {
    type Err = DecodeError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let (prefix, base32, variant) =
            bech32::decode(string).map_err(DecodeError::DecodeBech32)?;
        if prefix != EXTENDED_SPENDING_KEY_HRP {
            return Err(DecodeError::UnexpectedBech32Prefix(
                prefix,
                EXTENDED_SPENDING_KEY_HRP.into(),
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
