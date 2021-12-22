use crate::types::address::Address;
use crate::types::storage::{DbKeySeg, Key, KeySeg};

#[cfg(feature = "ferveo-tpke")]
mod dkg_keys {
    use std::fmt::Display;
    use std::io::{Error, ErrorKind};
    use std::str::FromStr;

    use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
    use borsh::{BorshDeserialize, BorshSerialize};
    use serde::{Deserialize, Serialize};

    use crate::types::key::ed25519::ParsePublicKeyError;
    use crate::types::transaction::EllipticCurve;

    /// A keypair used in the DKG protocol
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct DkgKeypair(ferveo_common::Keypair<EllipticCurve>);

    impl DkgKeypair {
        /// Get the public key of the keypair
        pub fn public(&self) -> DkgPublicKey {
            self.0.public().into()
        }
    }

    impl From<ferveo_common::Keypair<EllipticCurve>> for DkgKeypair {
        fn from(kp: ferveo_common::Keypair<EllipticCurve>) -> Self {
            Self(kp)
        }
    }

    impl From<&DkgKeypair> for ferveo_common::Keypair<EllipticCurve> {
        fn from(kp: &DkgKeypair) -> Self {
            kp.0
        }
    }

    impl BorshSerialize for DkgKeypair {
        fn serialize<W: std::io::Write>(
            &self,
            writer: &mut W,
        ) -> std::io::Result<()> {
            let mut kp_buf = Vec::<u8>::new();
            CanonicalSerialize::serialize(&self.0, &mut kp_buf)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            BorshSerialize::serialize(&kp_buf, writer)
        }
    }

    impl BorshDeserialize for DkgKeypair {
        fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
            let kp_bytes: Vec<u8> = BorshDeserialize::deserialize(buf)?;
            let kp: ferveo_common::Keypair<EllipticCurve> =
                CanonicalDeserialize::deserialize(kp_bytes.as_slice())
                    .map_err(|err| Error::new(ErrorKind::InvalidInput, err))?;
            Ok(kp.into())
        }
    }

    /// A public keyp used in the DKG protocol
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct DkgPublicKey(ferveo_common::PublicKey<EllipticCurve>);

    impl From<ferveo_common::PublicKey<EllipticCurve>> for DkgPublicKey {
        fn from(pk: ferveo_common::PublicKey<EllipticCurve>) -> Self {
            Self(pk)
        }
    }

    impl From<&DkgPublicKey> for ferveo_common::PublicKey<EllipticCurve> {
        fn from(pk: &DkgPublicKey) -> Self {
            pk.0
        }
    }

    impl BorshSerialize for DkgPublicKey {
        fn serialize<W: std::io::Write>(
            &self,
            writer: &mut W,
        ) -> std::io::Result<()> {
            let mut pk_buf = Vec::<u8>::new();
            CanonicalSerialize::serialize(&self.0, &mut pk_buf)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            BorshSerialize::serialize(&pk_buf, writer)
        }
    }

    impl BorshDeserialize for DkgPublicKey {
        fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
            let pk_bytes: Vec<u8> = BorshDeserialize::deserialize(buf)?;
            let pk: ferveo_common::PublicKey<EllipticCurve> =
                CanonicalDeserialize::deserialize(pk_bytes.as_slice())
                    .map_err(|err| Error::new(ErrorKind::InvalidInput, err))?;
            Ok(pk.into())
        }
    }

    impl Display for DkgPublicKey {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let vec = self
                .try_to_vec()
                .expect("Encoding public key shouldn't fail");
            write!(f, "{}", hex::encode(&vec))
        }
    }

    impl FromStr for DkgPublicKey {
        type Err = ParsePublicKeyError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let vec =
                hex::decode(s).map_err(ParsePublicKeyError::InvalidHex)?;
            BorshDeserialize::try_from_slice(&vec)
                .map_err(ParsePublicKeyError::InvalidEncoding)
        }
    }
}

#[cfg(feature = "ferveo-tpke")]
pub use dkg_keys::*;

/// Obtain a storage key for user's public dkg session key.
pub fn dkg_pk_key(owner: &Address) -> Key {
    Key::from(owner.to_db_key())
        .push(&DKG_PK_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Check if the given storage key is a public dkg session key. If it is,
/// returns the owner.
pub fn is_dkg_pk_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(owner), DbKeySeg::StringSeg(key)]
            if key == DKG_PK_STORAGE_KEY =>
        {
            Some(owner)
        }
        _ => None,
    }
}

const DKG_PK_STORAGE_KEY: &str = "dkg_pk_key";
const DKG_KEYPAIR_STORAGE_KEY: &str = "dkg_keypair_key";
const DKG_NEXT_KEYPAIR_STORAGE_KEY: &str = "dkg_next_keypair_key";
const DKG_NEXT_PK_STORAGE_KEY: &str = "dkg_next_pk_key";

/// The storage key for a DKG session keypair
pub fn dkg_keypair_key(owner: &Address) -> Key {
    Key::from(owner.to_db_key())
        .push(&DKG_KEYPAIR_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// The storage key for the next DKG public session key for a validator
pub fn dkg_next_pk_key(owner: &Address) -> Key {
    Key::from(owner.to_db_key())
        .push(&DKG_NEXT_PK_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// The storage key for the next DKG session keypair for a validator
pub fn dkg_next_keypair_key(owner: &Address) -> Key {
    Key::from(owner.to_db_key())
        .push(&DKG_NEXT_KEYPAIR_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}
