pub use ark_bls12_381::Bls12_381 as EllipticCurve;
/// Integration of Ferveo cryptographic primitives
/// to enable decrypting txs.
/// *Not wasm compatible*
#[cfg(feature = "ferveo-tpke")]
pub mod decrypted_tx {

    use ark_ec::PairingEngine;
    use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};

    use super::EllipticCurve;
    use crate::proto::Tx;
    use crate::types::transaction::{hash_tx, Hash, TxType, WrapperTx};

    #[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
    #[allow(clippy::large_enum_variant)]
    /// Holds the result of attempting to decrypt
    /// a transaction and the data necessary for
    /// other validators to verify
    pub enum DecryptedTx {
        /// The decrypted payload
        Decrypted(Tx),
        /// The wrapper whose payload could not be decrypted
        Undecryptable(WrapperTx),
    }

    impl DecryptedTx {
        /// Convert the inner tx value to bytes
        pub fn to_bytes(&self) -> Vec<u8> {
            match self {
                DecryptedTx::Decrypted(tx) => tx.to_bytes(),
                DecryptedTx::Undecryptable(wrapper) => {
                    wrapper.try_to_vec().unwrap()
                }
            }
        }

        /// Return the hash used as a commitment to the tx's contents in the
        /// wrapper tx that includes this tx as an encrypted payload.
        pub fn hash_commitment(&self) -> Hash {
            match self {
                DecryptedTx::Decrypted(tx) => hash_tx(&tx.to_bytes()),
                DecryptedTx::Undecryptable(wrapper) => wrapper.tx_hash.clone(),
            }
        }
    }

    /// Verify that if the encrypted payload was marked
    /// "undecryptable", we should not be able to decrypt
    /// it
    pub fn verify_decrypted_correctly(
        decrypted: &DecryptedTx,
        privkey: <EllipticCurve as PairingEngine>::G2Affine,
    ) -> bool {
        match decrypted {
            DecryptedTx::Decrypted(_) => true,
            DecryptedTx::Undecryptable(tx) => tx.decrypt(privkey).is_err(),
        }
    }

    impl From<DecryptedTx> for Tx {
        fn from(decrypted: DecryptedTx) -> Self {
            Tx::new(
                vec![],
                Some(
                    TxType::Decrypted(decrypted)
                        .try_to_vec()
                        .expect("Encrypting transaction should not fail"),
                ),
            )
        }
    }
}

#[cfg(feature = "ferveo-tpke")]
pub use decrypted_tx::*;
