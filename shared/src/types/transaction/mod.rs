//! Types that are used in transactions.

/// txs that contain decrypted payloads or assertions of
/// non-decryptability
pub mod decrypted;
mod encrypted;
/// txs to manage nfts
pub mod nft;
pub mod pos;
/// wrapper txs with encrypted payloads
pub mod wrapper;

use std::fmt::{self, Display};

use borsh::{BorshDeserialize, BorshSerialize};
pub use decrypted::*;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
#[cfg(not(feature = "ABCI"))]
use tendermint::abci::transaction;
#[cfg(feature = "ABCI")]
use tendermint_stable::abci::transaction;
pub use wrapper::*;

use crate::types::address::Address;
use crate::types::key::ed25519::PublicKey;

#[derive(
    Clone,
    Debug,
    Hash,
    PartialEq,
    Eq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
/// A hash, typically a sha-2 hash of a tx
pub struct Hash(pub [u8; 32]);

impl Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in &self.0 {
            write!(f, "{:02X}", byte)?;
        }
        Ok(())
    }
}

impl From<Hash> for transaction::Hash {
    fn from(hash: Hash) -> Self {
        Self::new(hash.0)
    }
}

/// Get the hash of a transaction
pub fn hash_tx(tx_bytes: &[u8]) -> Hash {
    let digest = Sha256::digest(tx_bytes);
    let mut hash_bytes = [0u8; 32];
    hash_bytes.copy_from_slice(&digest);
    Hash(hash_bytes)
}

/// A tx data type to update an account's validity predicate
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct UpdateVp {
    /// An address of the account
    pub addr: Address,
    /// The new VP code
    pub vp_code: Vec<u8>,
}

/// A tx data type to initialize a new established account
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct InitAccount {
    /// Public key to be written into the account's storage. This can be used
    /// for signature verification of transactions for the newly created
    /// account.
    pub public_key: PublicKey,
    /// The VP code
    pub vp_code: Vec<u8>,
}

/// A tx data type to initialize a new validator account and its staking reward
/// account.
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct InitValidator {
    /// Public key to be written into the account's storage. This can be used
    /// for signature verification of transactions for the newly created
    /// account.
    pub account_key: PublicKey,
    /// A key to be used for signing blocks and votes on blocks.
    pub consensus_key: PublicKey,
    /// Public key to be written into the staking reward account's storage.
    /// This can be used for signature verification of transactions for the
    /// newly created account.
    pub rewards_account_key: PublicKey,
    /// The VP code for validator account
    pub validator_vp_code: Vec<u8>,
    /// The VP code for validator's staking reward account
    pub rewards_vp_code: Vec<u8>,
}

/// Module that includes helper functions for classifying
/// different types of transactions that the ledger
/// must support as well as conversion functions
/// between them.
#[cfg(feature = "ferveo-tpke")]
pub mod tx_types {
    use std::convert::TryFrom;

    use super::*;
    use crate::proto::Tx;
    use crate::types::key::ed25519::{verify_tx_sig, SignedTxData};

    /// Struct that classifies that kind of Tx
    /// based on the contents of its data.
    #[derive(Clone, Debug, BorshSerialize, BorshDeserialize)]
    pub enum TxType {
        /// An ordinary tx
        Raw(Tx),
        /// A Tx that contains an encrypted raw tx
        Wrapper(WrapperTx),
        /// An attempted decryption of a wrapper tx
        Decrypted(DecryptedTx),
    }

    impl From<TxType> for Tx {
        fn from(ty: TxType) -> Self {
            Tx::new(vec![], Some(ty.try_to_vec().unwrap()))
        }
    }

    /// We deserialize the Tx data; it should be a TxType which
    /// tells us how to handle it. Otherwise, we return an error.
    /// The exception is when the Tx data field is empty. We
    /// allow this and type it as a Raw TxType.
    impl TryFrom<Tx> for TxType {
        type Error = std::io::Error;

        fn try_from(tx: Tx) -> std::io::Result<TxType> {
            if let Some(ref data) = tx.data {
                BorshDeserialize::deserialize(&mut data.as_ref())
            } else {
                // We allow Txs with empty data fields, which we
                // will assume to be of Raw TxType
                Ok(TxType::Raw(tx))
            }
        }
    }

    /// Determines the type of the input Tx
    ///
    /// If it is a raw Tx, signed or not, the Tx is
    /// returned unchanged inside an enum variant stating its type.
    ///
    /// If it is a decrypted tx, signing it adds no security so we
    /// extract the signed data without checking the signature (if it
    /// is signed) or return as is. Either way, it is returned in
    /// an enum variant stating its type.
    ///
    /// If it is a WrapperTx, we extract the signed data of
    /// the Tx and verify it is of the appropriate form. This means
    /// 1. The signed Tx data deserializes to a WrapperTx type
    /// 2. The wrapper tx is indeed signed
    /// 3. The signature is valid
    ///
    /// We modify the data of the WrapperTx to contain only the signed
    /// data if valid and return it wrapped in a enum variant
    /// indicating it is a wrapper. Otherwise, an error is
    /// returned indicating the signature was not valid
    pub fn process_tx(tx: Tx) -> Result<TxType, WrapperTxErr> {
        if let Some(Ok(SignedTxData {
            data: Some(data),
            ref sig,
        })) = tx
            .data
            .as_ref()
            .map(|data| SignedTxData::try_from_slice(&data[..]))
        {
            match TxType::try_from(Tx {
                code: vec![],
                data: Some(data),
                timestamp: tx.timestamp,
            })
            .map_err(|err| WrapperTxErr::Deserialization(err.to_string()))?
            {
                // verify signature and extract signed data
                TxType::Wrapper(wrapper) => {
                    verify_tx_sig(&wrapper.pk, &tx, sig)
                        .map_err(WrapperTxErr::SigError)?;
                    Ok(TxType::Wrapper(wrapper))
                }
                // we extract the signed data, but don't check the signature
                decrypted @ TxType::Decrypted(_) => Ok(decrypted),
                // return as is
                raw @ TxType::Raw(_) => Ok(raw),
            }
        } else {
            match TxType::try_from(tx)
                .map_err(|err| WrapperTxErr::Deserialization(err.to_string()))?
            {
                // we only accept signed wrappers
                TxType::Wrapper(_) => Err(WrapperTxErr::Unsigned),
                // return as is
                val => Ok(val),
            }
        }
    }

    #[cfg(test)]
    mod test_process_tx {
        use super::*;
        use crate::types::address::xan;
        use crate::types::key::ed25519::Keypair;
        use crate::types::storage::Epoch;

        fn gen_keypair() -> Keypair {
            use rand::prelude::ThreadRng;
            use rand::thread_rng;

            let mut rng: ThreadRng = thread_rng();
            Keypair::generate(&mut rng)
        }

        /// Test that process_tx correctly identifies a raw tx with no
        /// data and returns an identical copy
        #[test]
        fn test_process_tx_raw_tx_no_data() {
            let tx = Tx::new("wasm code".as_bytes().to_owned(), None);

            match process_tx(tx.clone()).expect("Test failed") {
                TxType::Raw(raw) => assert_eq!(tx, raw),
                _ => panic!("Test failed: Expected Raw Tx"),
            }
        }

        /// Test that process_tx correctly identifies tx containing
        /// a raw tx with some data and returns an identical copy
        /// of the inner data
        #[test]
        fn test_process_tx_raw_tx_some_data() {
            let inner = Tx::new(
                "code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            );
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some(
                    TxType::Raw(inner.clone())
                        .try_to_vec()
                        .expect("Test failed"),
                ),
            );

            match process_tx(tx).expect("Test failed") {
                TxType::Raw(raw) => assert_eq!(inner, raw),
                _ => panic!("Test failed: Expected Raw Tx"),
            }
        }

        /// Test that process_tx correctly identifies a raw tx with some
        /// signed data and returns an identical copy of the inner data
        #[test]
        fn test_process_tx_raw_tx_some_signed_data() {
            let inner = Tx::new(
                "code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            );
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some(
                    TxType::Raw(inner.clone())
                        .try_to_vec()
                        .expect("Test failed"),
                ),
            )
            .sign(&gen_keypair());

            match process_tx(tx).expect("Test failed") {
                TxType::Raw(raw) => assert_eq!(inner, raw),
                _ => panic!("Test failed: Expected Raw Tx"),
            }
        }

        /// Test that process_tx correctly identifies a wrapper tx with some
        /// data and extracts the signed data.
        #[test]
        fn test_process_tx_wrapper_tx() {
            let keypair = gen_keypair();
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            );
            // the signed tx
            let wrapper = WrapperTx::new(
                Fee {
                    amount: 10.into(),
                    token: xan(),
                },
                &keypair,
                Epoch(0),
                0.into(),
                tx.clone(),
            )
            .sign(&keypair)
            .expect("Test failed");

            match process_tx(wrapper).expect("Test failed") {
                TxType::Wrapper(wrapper) => {
                    let decrypted =
                        wrapper.decrypt(<EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator())
                            .expect("Test failed");
                    assert_eq!(tx, decrypted);
                }
                _ => panic!("Test failed: Expected Wrapper Tx"),
            }
        }

        /// Test that process_tx correctly returns an error on a wrapper tx
        /// with some unsigned data
        #[test]
        fn test_process_tx_wrapper_tx_unsigned() {
            let keypair = gen_keypair();
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            );
            // the signed tx
            let wrapper = WrapperTx::new(
                Fee {
                    amount: 10.into(),
                    token: xan(),
                },
                &keypair,
                Epoch(0),
                0.into(),
                tx,
            );

            let tx = Tx::new(
                vec![],
                Some(
                    TxType::Wrapper(wrapper).try_to_vec().expect("Test failed"),
                ),
            );
            let result = process_tx(tx).expect_err("Test failed");
            assert_matches!(result, WrapperTxErr::Unsigned);
        }
    }

    /// Test that process_tx correctly identifies a DecryptedTx
    /// with some unsigned data and returns an identical copy
    #[test]
    fn test_process_tx_decrypted_unsigned() {
        let payload = Tx::new(
            "transaction data".as_bytes().to_owned(),
            Some("transaction data".as_bytes().to_owned()),
        );
        let decrypted = DecryptedTx::Decrypted(payload.clone());
        let tx = Tx::from(TxType::Decrypted(decrypted));
        match process_tx(tx).expect("Test failed") {
            TxType::Decrypted(DecryptedTx::Decrypted(processed)) => {
                assert_eq!(payload, processed);
            }
            _ => panic!("Test failed"),
        }
    }

    /// Test that process_tx correctly identifies a DecryptedTx
    /// with some signed data and extracts it without checking
    /// signature
    #[test]
    fn test_process_tx_decrypted_signed() {
        let payload = Tx::new(
            "transaction data".as_bytes().to_owned(),
            Some("transaction data".as_bytes().to_owned()),
        );
        let decrypted = DecryptedTx::Decrypted(payload.clone());
        // Invalid signed data
        let signed = SignedTxData {
            data: Some(
                TxType::Decrypted(decrypted)
                    .try_to_vec()
                    .expect("Test failed"),
            ),
            sig: ed25519_dalek::Signature::from([0u8; 64]).into(),
        };
        // create the tx with signed decrypted data
        let tx =
            Tx::new(vec![], Some(signed.try_to_vec().expect("Test failed")));
        match process_tx(tx).expect("Test failed") {
            TxType::Decrypted(DecryptedTx::Decrypted(processed)) => {
                assert_eq!(payload, processed);
            }
            _ => panic!("Test failed"),
        }
    }
}

#[cfg(feature = "ferveo-tpke")]
pub use tx_types::*;
