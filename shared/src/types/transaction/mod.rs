//! Types that are used in transactions.

/// txs that contain decrypted payloads or assertions of
/// non-decryptability
pub mod decrypted;
/// tools for encrypted data
pub mod encrypted;
/// txs for managing the Ethereum bridge
pub mod eth_bridge;
/// txs to manage governance
pub mod governance;
/// txs to manage nfts
pub mod nft;
pub mod pos;
/// transaction protocols made by validators
pub mod protocol;
/// wrapper txs with encrypted payloads
pub mod wrapper;

use std::collections::BTreeSet;
use std::fmt;

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
pub use decrypted::*;
#[cfg(feature = "ferveo-tpke")]
pub use encrypted::EncryptionKey;
pub use protocol::UpdateDkgSessionKey;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
pub use wrapper::*;

use super::ibc::IbcEvent;
use super::storage;
use crate::ledger::gas::VpsGas;
use crate::types::address::Address;
use crate::types::hash::Hash;
use crate::types::key::*;

/// Get the hash of a transaction
pub fn hash_tx(tx_bytes: &[u8]) -> Hash {
    let digest = Sha256::digest(tx_bytes);
    Hash(*digest.as_ref())
}

/// Transaction application result
// TODO derive BorshSchema after <https://github.com/near/borsh-rs/issues/82>
#[derive(Clone, Debug, Default, BorshSerialize, BorshDeserialize)]
pub struct TxResult {
    /// Total gas used by the transaction (includes the gas used by VPs)
    pub gas_used: u64,
    /// Storage keys touched by the transaction
    pub changed_keys: BTreeSet<storage::Key>,
    /// The results of all the triggered validity predicates by the transaction
    pub vps_result: VpsResult,
    /// New established addresses created by the transaction
    pub initialized_accounts: Vec<Address>,
    /// Optional IBC event emitted by the transaction
    pub ibc_event: Option<IbcEvent>,
}

impl TxResult {
    /// Check if the tx has been accepted by all the VPs
    pub fn is_accepted(&self) -> bool {
        self.vps_result.rejected_vps.is_empty()
    }
}

/// Result of checking a transaction with validity predicates
// TODO derive BorshSchema after <https://github.com/near/borsh-rs/issues/82>
#[derive(Clone, Debug, Default, BorshSerialize, BorshDeserialize)]
pub struct VpsResult {
    /// The addresses whose VPs accepted the transaction
    pub accepted_vps: BTreeSet<Address>,
    /// The addresses whose VPs rejected the transaction
    pub rejected_vps: BTreeSet<Address>,
    /// The total gas used by all the VPs
    pub gas_used: VpsGas,
    /// Errors occurred in any of the VPs, if any
    pub errors: Vec<(Address, String)>,
}

impl fmt::Display for TxResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Transaction is {}. Gas used: {};{} VPs result: {}",
            if self.is_accepted() {
                "valid"
            } else {
                "invalid"
            },
            self.gas_used,
            iterable_to_string("Changed keys", self.changed_keys.iter()),
            self.vps_result,
        )
    }
}

impl fmt::Display for VpsResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            iterable_to_string("Accepted", self.accepted_vps.iter()),
            iterable_to_string("Rejected", self.rejected_vps.iter()),
            iterable_to_string(
                "Errors",
                self.errors
                    .iter()
                    .map(|(addr, err)| format!("{} in {}", err, addr))
            ),
        )
    }
}

/// Format all the values of the given iterator into a string
fn iterable_to_string<T: fmt::Display>(
    label: &str,
    iter: impl Iterator<Item = T>,
) -> String {
    let mut iter = iter.peekable();
    if iter.peek().is_none() {
        "".into()
    } else {
        format!(
            " {}: {};",
            label,
            iter.map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

/// A tx data type to update an account's validity predicate
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    BorshSchema,
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
    BorshSchema,
    Serialize,
    Deserialize,
)]
pub struct InitAccount {
    /// Public key to be written into the account's storage. This can be used
    /// for signature verification of transactions for the newly created
    /// account.
    pub public_key: common::PublicKey,
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
    BorshSchema,
    Serialize,
    Deserialize,
)]
pub struct InitValidator {
    /// Public key to be written into the account's storage. This can be used
    /// for signature verification of transactions for the newly created
    /// account.
    pub account_key: common::PublicKey,
    /// A key to be used for signing blocks and votes on blocks.
    pub consensus_key: common::PublicKey,
    /// Public key to be written into the staking reward account's storage.
    /// This can be used for signature verification of transactions for the
    /// newly created account.
    pub rewards_account_key: common::PublicKey,
    /// Public key used to sign protocol transactions
    pub protocol_key: common::PublicKey,
    /// Serialization of the public session key used in the DKG
    pub dkg_key: DkgPublicKey,
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

    use thiserror;

    use super::*;
    use crate::proto::{SignedTxData, Tx};
    use crate::types::transaction::protocol::ProtocolTx;

    /// Errors relating to decrypting a wrapper tx and its
    /// encrypted payload from a Tx type
    #[allow(missing_docs)]
    #[derive(thiserror::Error, Debug, PartialEq)]
    pub enum TxError {
        #[error("{0}")]
        Unsigned(String),
        #[error("{0}")]
        SigError(String),
        #[error("Failed to deserialize Tx: {0}")]
        Deserialization(String),
    }

    /// Struct that classifies that kind of Tx
    /// based on the contents of its data.
    #[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
    pub enum TxType {
        /// An ordinary tx
        Raw(Tx),
        /// A Tx that contains an encrypted raw tx
        Wrapper(WrapperTx),
        /// An attempted decryption of a wrapper tx
        Decrypted(DecryptedTx),
        /// Txs issued by validators as part of internal protocols
        Protocol(ProtocolTx),
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
    pub fn process_tx(tx: Tx) -> Result<TxType, TxError> {
        if let Some(Ok(SignedTxData {
            data: Some(data),
            ref sig,
        })) = tx
            .data
            .as_ref()
            .map(|data| SignedTxData::try_from_slice(&data[..]))
        {
            let signed_hash = Tx {
                code: tx.code,
                data: Some(data.clone()),
                timestamp: tx.timestamp,
            }
            .hash();
            match TxType::try_from(Tx {
                code: vec![],
                data: Some(data),
                timestamp: tx.timestamp,
            })
            .map_err(|err| TxError::Deserialization(err.to_string()))?
            {
                // verify signature and extract signed data
                TxType::Wrapper(wrapper) => {
                    wrapper.validate_sig(signed_hash, sig)?;
                    Ok(TxType::Wrapper(wrapper))
                }
                // verify signature and extract signed data
                TxType::Protocol(protocol) => {
                    protocol.validate_sig(signed_hash, sig)?;
                    Ok(TxType::Protocol(protocol))
                }
                // we extract the signed data, but don't check the signature
                decrypted @ TxType::Decrypted(_) => Ok(decrypted),
                // return as is
                raw @ TxType::Raw(_) => Ok(raw),
            }
        } else {
            match TxType::try_from(tx)
                .map_err(|err| TxError::Deserialization(err.to_string()))?
            {
                // we only accept signed wrappers
                TxType::Wrapper(_) => Err(TxError::Unsigned(
                    "Wrapper transactions must be signed".into(),
                )),
                TxType::Protocol(_) => Err(TxError::Unsigned(
                    "Protocol transactions must be signed".into(),
                )),
                // return as is
                val => Ok(val),
            }
        }
    }

    #[cfg(test)]
    mod test_process_tx {
        use super::*;
        use crate::types::address::xan;
        use crate::types::storage::Epoch;

        fn gen_keypair() -> common::SecretKey {
            use rand::prelude::ThreadRng;
            use rand::thread_rng;

            let mut rng: ThreadRng = thread_rng();
            ed25519::SigScheme::generate(&mut rng).try_to_sk().unwrap()
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
                Default::default(),
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
                Default::default(),
            );

            let tx = Tx::new(
                vec![],
                Some(
                    TxType::Wrapper(wrapper).try_to_vec().expect("Test failed"),
                ),
            );
            let result = process_tx(tx).expect_err("Test failed");
            assert_matches!(result, TxError::Unsigned(_));
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
        let ed_sig =
            ed25519::Signature::try_from_slice([0u8; 64].as_ref()).unwrap();
        let signed = SignedTxData {
            data: Some(
                TxType::Decrypted(decrypted)
                    .try_to_vec()
                    .expect("Test failed"),
            ),
            sig: common::Signature::try_from_sig(&ed_sig).unwrap(),
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

use crate::types::key::dkg_session_keys::DkgPublicKey;
