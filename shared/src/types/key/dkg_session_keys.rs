use std::io::{Error, ErrorKind};

use ark_serialize::{CanonicalSerialize, CanonicalDeserialize};
use borsh::{BorshSerialize, BorshDeserialize};
use serde::{Serialize, Deserialize};

use crate::types::transaction::EllipticCurve;

/// A keypair used in the DKG protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DkgKeypair(ferveo_common::Keypair<EllipticCurve>);

impl DkgKeypair {
    pub fn public(&self) -> DkgPublicKey {
        self.0.public().into()
    }
}

impl From<ferveo_common::Keypair<EllipticCurve>> for DkgKeypair {
    fn from(kp: ferveo_common::Keypair<EllipticCurve>) -> Self {
        Self(kp)
    }
}

impl BorshSerialize for DkgKeypair {
    fn serialize<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        let mut kp_buf = Vec::<u8>::new();
        CanonicalSerialize::serialize(&self.0, &mut kp_buf)
            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
        BorshSerialize::serialize(&kp_buf, writer)
    }
}

impl BorshDeserialize for DkgKeypair {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        let mut kp_bytes: Vec<u8> = BorshDeserialize::deserialize(buf)?;
        let kp: ferveo_common::Keypair<EllipticCurve> = CanonicalDeserialize::deserialize(kp_bytes.as_slice())
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

impl BorshSerialize for DkgPublicKey {
    fn serialize<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        let mut pk_buf = Vec::<u8>::new();
        CanonicalSerialize::serialize(&self.0, &mut pk_buf)
            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
        BorshSerialize::serialize(&pk_buf, writer)
    }
}

impl BorshDeserialize for DkgPublicKey {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        let mut pk_bytes: Vec<u8> = BorshDeserialize::deserialize(buf)?;
        let pk: ferveo_common::PublicKey<EllipticCurve> = CanonicalDeserialize::deserialize(pk_bytes.as_slice())
            .map_err(|err| Error::new(ErrorKind::InvalidInput, err))?;
        Ok(pk.into())
    }
}

/// Session key used by validator in DKG protocol. Allows us
/// to queue keys if we wish to update our key. Once the change
/// is approved (i.e. on chain), we can update the [`Next`] to
/// [`Current`]
#[derive(Clone, Debug, Serialize, Deserialize, BorshSerialize, BorshDeserialize)]
pub enum UpdatableDkgKey {
    None,
    Some{
        current: DkgKeypair,
        next: Option<DkgKeypair>,
    },
}

impl Default for UpdatableDkgKey {
    fn default() -> Self {
        Self::None
    }
}

impl UpdatableDkgKey {
    /// We give an optional keypair.
    ///
    ///  * If `Self` is `None` and keypair is `None`, no-op. Else,
    ///    change `Self` to `Some` with given keypair as current
    ///  * If `Self` is `Some`, update `current` with `next` if it
    ///    exists and replace `next` with keypair
    pub fn update(&mut self, mut keypair: Option<DkgKeypair>) {
        match self {
            Self::None => {
                if let Some(kp) = keypair.take() {
                    *self = Self::Some{current: kp, next: None}
                }
            },
            Self::Some{current, next} => {
                match next {
                    Some(kp) => {
                        std::mem::swap(current, kp);

                    },
                    None => std::mem::swap(next, &mut keypair)
                }
            }
        }
    }
}