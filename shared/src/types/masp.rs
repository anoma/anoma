//! MASP types.

pub use bellman::groth16::PreparedVerifyingKey;
pub use bls12_381::Bls12;
use borsh::{BorshDeserialize, BorshSerialize};
use group::GroupEncoding;
pub use masp_primitives::primitives::Note;
pub use masp_primitives::redjubjub::PublicKey;
pub use masp_proofs::sapling::SaplingVerificationContext;

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
