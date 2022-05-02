/// Integration of Ferveo cryptographic primitives
/// to enable encrypted txs inside of normal txs.
/// *Not wasm compatible*
#[cfg(feature = "ferveo-tpke")]
pub mod wrapper_tx {
    use std::convert::TryFrom;

    pub use ark_bls12_381::Bls12_381 as EllipticCurve;
    pub use ark_ec::{AffineCurve, PairingEngine};
    use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
    use serde::{Deserialize, Serialize};
    use thiserror::Error;

    use crate::proto::Tx;
    use crate::types::address::Address;
    use crate::types::key::*;
    use crate::types::storage::Epoch;
    use crate::types::token::Amount;
    use crate::types::transaction::encrypted::EncryptedTx;
    use crate::types::transaction::{
        hash_tx, EncryptionKey, Hash, TxError, TxType,
    };

    /// TODO: Determine a sane number for this
    const GAS_LIMIT_RESOLUTION: u64 = 1_000_000;

    /// Errors relating to decrypting a wrapper tx and its
    /// encrypted payload from a Tx type
    #[allow(missing_docs)]
    #[derive(Error, Debug)]
    pub enum WrapperTxErr {
        #[error(
            "The hash of the decrypted tx does not match the hash commitment"
        )]
        DecryptedHash,
        #[error("The decryption did not produce a valid Tx")]
        InvalidTx,
        #[error("The given Tx data did not contain a valid WrapperTx")]
        InvalidWrapperTx,
        #[error(
            "Attempted to sign WrapperTx with keypair whose public key \
             differs from that in the WrapperTx"
        )]
        InvalidKeyPair,
    }

    /// A fee is an amount of a specified token
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
    pub struct Fee {
        /// amount of the fee
        pub amount: Amount,
        /// address of the token
        pub token: Address,
    }

    /// Gas limits must be multiples of GAS_LIMIT_RESOLUTION
    /// This is done to minimize the amount of information leak from
    /// a wrapper tx. The larger the GAS_LIMIT_RESOLUTION, the
    /// less info leaked.
    ///
    /// This struct only stores the multiple of GAS_LIMIT_RESOLUTION,
    /// not the raw amount
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Serialize,
        Deserialize,
        BorshSerialize,
        BorshDeserialize,
        BorshSchema,
    )]
    #[serde(from = "u64")]
    #[serde(into = "u64")]
    pub struct GasLimit {
        multiplier: u64,
    }

    impl GasLimit {
        /// We refund unused gas up to GAS_LIMIT_RESOLUTION
        pub fn refund_amount(&self, used_gas: u64) -> Amount {
            if used_gas < (u64::from(self) - GAS_LIMIT_RESOLUTION) {
                // we refund only up to GAS_LIMIT_RESOLUTION
                GAS_LIMIT_RESOLUTION
            } else if used_gas >= u64::from(self) {
                // Gas limit was under estimated, no refund
                0
            } else {
                // compute refund
                u64::from(self) - used_gas
            }
            .into()
        }
    }

    /// Round the input number up to the next highest multiple
    /// of GAS_LIMIT_RESOLUTION
    impl From<u64> for GasLimit {
        fn from(amount: u64) -> GasLimit {
            // we could use the ceiling function but this way avoids casts to
            // floats
            if GAS_LIMIT_RESOLUTION * (amount / GAS_LIMIT_RESOLUTION) < amount {
                GasLimit {
                    multiplier: (amount / GAS_LIMIT_RESOLUTION) + 1,
                }
            } else {
                GasLimit {
                    multiplier: (amount / GAS_LIMIT_RESOLUTION),
                }
            }
        }
    }

    /// Round the input number up to the next highest multiple
    /// of GAS_LIMIT_RESOLUTION
    impl From<Amount> for GasLimit {
        fn from(amount: Amount) -> GasLimit {
            GasLimit::from(u64::from(amount))
        }
    }

    /// Get back the gas limit as a raw number
    impl From<&GasLimit> for u64 {
        fn from(limit: &GasLimit) -> u64 {
            limit.multiplier * GAS_LIMIT_RESOLUTION
        }
    }

    /// Get back the gas limit as a raw number
    impl From<GasLimit> for u64 {
        fn from(limit: GasLimit) -> u64 {
            limit.multiplier * GAS_LIMIT_RESOLUTION
        }
    }

    /// Get back the gas limit as a raw number, viewed as an Amount
    impl From<GasLimit> for Amount {
        fn from(limit: GasLimit) -> Amount {
            Amount::from(limit.multiplier * GAS_LIMIT_RESOLUTION)
        }
    }

    /// A transaction with an encrypted payload as well
    /// as some non-encrypted metadata for inclusion
    /// and / or verification purposes
    #[derive(
        Debug,
        Clone,
        BorshSerialize,
        BorshDeserialize,
        BorshSchema,
        Serialize,
        Deserialize,
    )]
    pub struct WrapperTx {
        /// The fee to be payed for including the tx
        pub fee: Fee,
        /// Used to determine an implicit account of the fee payer
        pub pk: common::PublicKey,
        /// The epoch in which the tx is to be submitted. This determines
        /// which decryption key will be used
        pub epoch: Epoch,
        /// Max amount of gas that can be used when executing the inner tx
        pub gas_limit: GasLimit,
        /// the encrypted payload
        pub inner_tx: EncryptedTx,
        /// sha-2 hash of the inner transaction acting as a commitment
        /// the contents of the encrypted payload
        pub tx_hash: Hash,
    }

    impl WrapperTx {
        /// Create a new wrapper tx from unencrypted tx, the personal keypair,
        /// and the metadata surrounding the inclusion of the tx. This method
        /// constructs the signature of relevant data and encrypts the
        /// transaction
        pub fn new(
            fee: Fee,
            keypair: &common::SecretKey,
            epoch: Epoch,
            gas_limit: GasLimit,
            tx: Tx,
            encryption_key: EncryptionKey,
        ) -> WrapperTx {
            let inner_tx = EncryptedTx::encrypt(&tx.to_bytes(), encryption_key);
            Self {
                fee,
                pk: keypair.ref_to(),
                epoch,
                gas_limit,
                inner_tx,
                tx_hash: hash_tx(&tx.to_bytes()),
            }
        }

        /// Get the address of the implicit account associated
        /// with the public key
        pub fn fee_payer(&self) -> Address {
            Address::from(&self.pk)
        }

        /// A validity check on the ciphertext.
        pub fn validate_ciphertext(&self) -> bool {
            self.inner_tx.0.check(&<EllipticCurve as PairingEngine>::G1Prepared::from(
                -<EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator(),
            ))
        }

        /// Decrypt the wrapped transaction.
        ///
        /// Will fail if the inner transaction does match the
        /// hash commitment or we are unable to recover a
        /// valid Tx from the decoded byte stream.
        pub fn decrypt(
            &self,
            privkey: <EllipticCurve as PairingEngine>::G2Affine,
        ) -> Result<Tx, WrapperTxErr> {
            // decrypt the inner tx
            let decrypted = self.inner_tx.decrypt(privkey);
            // check that the hash equals commitment
            if hash_tx(&decrypted) != self.tx_hash {
                Err(WrapperTxErr::DecryptedHash)
            } else {
                // convert back to Tx type
                Tx::try_from(decrypted.as_ref())
                    .map_err(|_| WrapperTxErr::InvalidTx)
            }
        }

        /// Sign the wrapper transaction and convert to a normal Tx type
        pub fn sign(
            &self,
            keypair: &common::SecretKey,
        ) -> Result<Tx, WrapperTxErr> {
            if self.pk != keypair.ref_to() {
                return Err(WrapperTxErr::InvalidKeyPair);
            }
            Ok(Tx::new(
                vec![],
                Some(
                    TxType::Wrapper(self.clone())
                        .try_to_vec()
                        .expect("Could not serialize WrapperTx"),
                ),
            )
            .sign(keypair))
        }

        /// Validate the signature of a wrapper tx
        pub fn validate_sig(
            &self,
            signed_data: [u8; 32],
            sig: &common::Signature,
        ) -> Result<(), TxError> {
            common::SigScheme::verify_signature(&self.pk, &signed_data, sig)
                .map_err(|err| {
                    TxError::SigError(format!(
                        "WrapperTx signature verification failed: {}",
                        err
                    ))
                })
        }
    }

    #[cfg(test)]
    mod test_gas_limits {
        use super::*;

        /// Test serializing and deserializing again gives back original object
        /// Test that serializing converts GasLimit to u64 correctly
        #[test]
        fn test_gas_limit_roundtrip() {
            let limit = GasLimit { multiplier: 1 };
            // Test serde roundtrip
            let js = serde_json::to_string(&limit).expect("Test failed");
            assert_eq!(js, format!("{}", GAS_LIMIT_RESOLUTION));
            let new_limit: GasLimit =
                serde_json::from_str(&js).expect("Test failed");
            assert_eq!(new_limit, limit);

            // Test borsh roundtrip
            let borsh = limit.try_to_vec().expect("Test failed");
            assert_eq!(
                limit,
                BorshDeserialize::deserialize(&mut borsh.as_ref())
                    .expect("Test failed")
            );
        }

        /// Test that when we deserialize a u64 that is not a multiple of
        /// GAS_LIMIT_RESOLUTION to a GasLimit, it rounds up to the next
        /// multiple
        #[test]
        fn test_deserialize_not_multiple_of_resolution() {
            let js = serde_json::to_string(&(GAS_LIMIT_RESOLUTION + 1))
                .expect("Test failed");
            let limit: GasLimit =
                serde_json::from_str(&js).expect("Test failed");
            assert_eq!(limit, GasLimit { multiplier: 2 });
        }

        /// Test that refund is calculated correctly
        #[test]
        fn test_gas_limit_refund() {
            let limit = GasLimit { multiplier: 1 };
            let refund = limit.refund_amount(GAS_LIMIT_RESOLUTION - 1);
            assert_eq!(refund, Amount::from(1u64));
        }

        /// Test that we don't refund more than GAS_LIMIT_RESOLUTION
        #[test]
        fn test_gas_limit_too_high_no_refund() {
            let limit = GasLimit { multiplier: 2 };
            let refund = limit.refund_amount(GAS_LIMIT_RESOLUTION - 1);
            assert_eq!(refund, Amount::from(GAS_LIMIT_RESOLUTION));
        }

        /// Test that if gas usage was underestimated, we issue no refund
        #[test]
        fn test_gas_limit_too_low_no_refund() {
            let limit = GasLimit { multiplier: 1 };
            let refund = limit.refund_amount(GAS_LIMIT_RESOLUTION + 1);
            assert_eq!(refund, Amount::from(0u64));
        }
    }

    #[cfg(test)]
    mod test_wrapper_tx {
        use super::*;
        use crate::proto::SignedTxData;
        use crate::types::address::xan;

        fn gen_keypair() -> common::SecretKey {
            use rand::prelude::ThreadRng;
            use rand::thread_rng;

            let mut rng: ThreadRng = thread_rng();
            ed25519::SigScheme::generate(&mut rng).try_to_sk().unwrap()
        }

        /// We test that when we feed in a Tx and then decrypt it again
        /// that we get what we started with.
        #[test]
        fn test_encryption_round_trip() {
            let keypair = gen_keypair();
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            );

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
            );
            assert!(wrapper.validate_ciphertext());
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            let decrypted = wrapper.decrypt(privkey).expect("Test failed");
            assert_eq!(tx, decrypted);
        }

        /// We test that when we try to decrypt a tx and it
        /// does not match the commitment, an error is returned
        #[test]
        fn test_decryption_invalid_hash() {
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            );

            let mut wrapper = WrapperTx::new(
                Fee {
                    amount: 10.into(),
                    token: xan(),
                },
                &gen_keypair(),
                Epoch(0),
                0.into(),
                tx,
                Default::default(),
            );
            // give a incorrect commitment to the decrypted contents of the tx
            wrapper.tx_hash = Hash([0u8; 32]);
            assert!(wrapper.validate_ciphertext());
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            let err = wrapper.decrypt(privkey).expect_err("Test failed");
            assert_matches!(err, WrapperTxErr::DecryptedHash);
        }

        /// We check that even if the encrypted payload and has of its
        /// contents are correctly changed, we detect fraudulent activity
        /// via the signature.
        #[test]
        fn test_malleability_attack_detection() {
            let pubkey = <EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator();
            let keypair = gen_keypair();
            // The intended tx
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            );
            // the signed tx
            let mut tx = WrapperTx::new(
                Fee {
                    amount: 10.into(),
                    token: xan(),
                },
                &keypair,
                Epoch(0),
                0.into(),
                tx,
                Default::default(),
            )
            .sign(&keypair)
            .expect("Test failed");

            // we now try to alter the inner tx maliciously
            let mut wrapper = if let TxType::Wrapper(wrapper) =
                crate::types::transaction::process_tx(tx.clone())
                    .expect("Test failed")
            {
                wrapper
            } else {
                panic!("Test failed")
            };

            let mut signed_tx_data =
                SignedTxData::try_from_slice(tx.data.try_to_vec()
                                             .expect("Encoding data for verifying signature shouldn't fail").as_ref())
                    .expect("Test failed");

            // malicious transaction
            let malicious =
                Tx::new("Give me all the money".as_bytes().to_owned(), None);

            // We replace the inner tx with a malicious one
            wrapper.inner_tx = EncryptedTx::encrypt(
                &malicious.to_bytes(),
                EncryptionKey(pubkey),
            );

            // We change the commitment appropriately
            wrapper.tx_hash = hash_tx(&malicious.to_bytes());

            // we check ciphertext validity still passes
            assert!(wrapper.validate_ciphertext());
            // we check that decryption still succeeds
            let decrypted = wrapper.decrypt(
                <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator()
            )
                .expect("Test failed");
            assert_eq!(decrypted, malicious);

            // we substitute in the modified wrapper
            signed_tx_data.data = Some(
                TxType::Wrapper(wrapper).try_to_vec().expect("Test failed"),
            );
            tx.data = signed_tx_data.clone();

            // check that the signature is not valid
            tx.verify_sig(&keypair.ref_to(), &signed_tx_data.sig.unwrap())
                .expect_err("Test failed");
            // check that the try from method also fails
            let err = crate::types::transaction::process_tx(tx)
                .expect_err("Test failed");
            assert_matches!(err, TxError::SigError(_));
        }
    }
}

#[cfg(feature = "ferveo-tpke")]
pub use wrapper_tx::*;
