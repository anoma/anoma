//! Types that are used in transactions.

pub mod pos;

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};

use crate::types::address::Address;
use crate::types::key::ed25519::PublicKey;

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

/// Integration of Ferveo cryptographic primitives
/// to enable encrypted txs.
/// *Not wasm compatible*
#[cfg(feature = "ferveo-tpke")]
pub mod wrapper_tx {
    use std::convert::TryFrom;
    use std::io::{Error, ErrorKind, Write};

    use ark_bls12_381::Bls12_381 as EllipticCurve;
    use ark_ec::{AffineCurve, PairingEngine};
    use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
    use borsh::{BorshDeserialize, BorshSerialize};
    use serde::{Deserialize, Serialize};
    use sha2::{Digest, Sha256};
    use thiserror::Error;
    use tpke::{encrypt, Ciphertext};

    use crate::proto::Tx;
    use crate::types::address::Address;
    use crate::types::key::ed25519::{
        verify_tx_sig, Keypair, PublicKey, SignedTxData,
    };
    use crate::types::storage::Epoch;
    use crate::types::token::Amount;

    /// TODO: Determine a sane number for this
    const GAS_LIMIT_RESOLUTION: u64 = 1_000_000;

    /// Errors relating to decrypting a wrapper tx and its
    /// encrypted payload from a Tx type
    #[allow(missing_docs)]
    #[derive(Error, Debug, PartialEq)]
    pub enum WrapperTxErr {
        #[error(
            "The hash of the decrypted tx does not match the hash commitment"
        )]
        DecryptedHash,
        #[error("The decryption did not produce a valid Tx")]
        InvalidTx,
        #[error("The given Tx data did not contain a valid WrapperTx")]
        InvalidWrapperTx,
        #[error("Expected signed WrapperTx data")]
        Unsigned,
        #[error("{0}")]
        SigError(String),
        #[error(
            "Attempted to sign WrapperTx with keypair whose public key \
             differs from that in the WrapperTx"
        )]
        InvalidKeyPair,
    }

    /// We use a specific choice of two groups and bilinear pairing
    /// We use a wrapper type to add traits
    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(from = "SerializedCiphertext")]
    #[serde(into = "SerializedCiphertext")]
    pub struct EncryptedTx(Ciphertext<EllipticCurve>);

    impl EncryptedTx {
        /// Encrypt a message to give a new ciphertext
        fn encrypt(
            msg: &[u8],
            pubkey: <EllipticCurve as PairingEngine>::G1Affine,
        ) -> Self {
            let mut rng = rand_new::thread_rng();
            Self(encrypt(msg, pubkey, &mut rng))
        }

        /// Decrypt a message and return it as raw bytes
        fn decrypt(
            &self,
            privkey: <EllipticCurve as PairingEngine>::G2Affine,
        ) -> Vec<u8> {
            tpke::decrypt(&self.0, privkey)
        }
    }

    impl borsh::ser::BorshSerialize for EncryptedTx {
        fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
            let Ciphertext {
                nonce,
                ciphertext,
                auth_tag,
            } = &self.0;
            // Serialize the nonce into bytes
            let mut nonce_buffer = Vec::<u8>::new();
            nonce
                .serialize(&mut nonce_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            // serialize the auth_tag to bytes
            let mut tag_buffer = Vec::<u8>::new();
            auth_tag
                .serialize(&mut tag_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            // serialize the three byte arrays
            BorshSerialize::serialize(
                &(nonce_buffer, ciphertext, tag_buffer),
                writer,
            )?;
            Ok(())
        }
    }

    impl borsh::BorshDeserialize for EncryptedTx {
        fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
            type VecTuple = (Vec<u8>, Vec<u8>, Vec<u8>);
            let (nonce, ciphertext, auth_tag): VecTuple =
                BorshDeserialize::deserialize(buf)?;
            Ok(EncryptedTx(Ciphertext {
                nonce: CanonicalDeserialize::deserialize(&*nonce)
                    .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
                ciphertext,
                auth_tag: CanonicalDeserialize::deserialize(&*auth_tag)
                    .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
            }))
        }
    }

    /// A helper struct for serializing EncryptedTx structs
    /// as an opaque blob
    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(transparent)]
    struct SerializedCiphertext {
        payload: Vec<u8>,
    }

    impl From<EncryptedTx> for SerializedCiphertext {
        fn from(tx: EncryptedTx) -> Self {
            SerializedCiphertext {
                payload: tx
                    .try_to_vec()
                    .expect("Unable to serialize encrypted transaction"),
            }
        }
    }

    impl From<SerializedCiphertext> for EncryptedTx {
        fn from(ser: SerializedCiphertext) -> Self {
            BorshDeserialize::deserialize(&mut ser.payload.as_ref())
                .expect("Unable to deserialize encrypted transactions")
        }
    }

    /// A fee is an amount of a specified token
    #[derive(
        Debug,
        Clone,
        PartialEq,
        BorshSerialize,
        BorshDeserialize,
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
    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

    impl borsh::ser::BorshSerialize for GasLimit {
        fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
            BorshSerialize::serialize(&u64::from(self), writer)
        }
    }

    impl borsh::BorshDeserialize for GasLimit {
        fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
            let raw: u64 = BorshDeserialize::deserialize(buf)?;
            Ok(GasLimit::from(raw))
        }
    }

    /// A transaction with an encrypted payload as well
    /// as some non-encrypted metadata for inclusion
    /// and / or verification purposes
    #[derive(
        Debug, Clone, BorshSerialize, BorshDeserialize, Serialize, Deserialize,
    )]
    pub struct WrapperTx {
        /// The fee to be payed for including the tx
        pub fee: Fee,
        /// Used to determine an implicit account of the fee payer
        pub pk: PublicKey,
        /// The epoch in which the tx is to be submitted. This determines
        /// which decryption key will be used
        pub epoch: Epoch,
        /// Max amount of gas that can be used when executing the inner tx
        gas_limit: GasLimit,
        /// the encrypted payload
        inner_tx: EncryptedTx,
        /// sha-2 hash of the inner transaction acting as a commitment
        /// the contents of the encrypted payload
        tx_hash: [u8; 32],
    }

    impl WrapperTx {
        /// Create a new wrapper tx from unencrypted tx, the personal keypair,
        /// and the metadata surrounding the inclusion of the tx. This method
        /// constructs the signature of relevant data and encrypts the
        /// transaction
        pub fn new(
            fee: Fee,
            keypair: &Keypair,
            epoch: Epoch,
            gas_limit: GasLimit,
            tx: Tx,
        ) -> WrapperTx {
            // TODO: Look up current public key from storage
            let pubkey = <EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator();
            let inner_tx = EncryptedTx::encrypt(&tx.to_bytes(), pubkey);
            // hash the transaction
            let digest = Sha256::digest(&tx.to_bytes());
            let mut tx_hash = [0u8; 32];
            tx_hash.copy_from_slice(&digest);

            Self {
                fee,
                pk: keypair.public.clone(),
                epoch,
                gas_limit,
                inner_tx,
                tx_hash,
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
            // check that the has equals commitment
            let digest = Sha256::digest(&decrypted);
            let mut tx_hash = [0u8; 32];
            tx_hash.copy_from_slice(&digest);
            if tx_hash != self.tx_hash {
                Err(WrapperTxErr::DecryptedHash)
            } else {
                // convert back to Tx type
                Tx::try_from(decrypted.as_ref())
                    .map_err(|_| WrapperTxErr::InvalidTx)
            }
        }

        /// Sign the wrapper transaction and convert to a normal Tx type
        pub fn sign(&self, keypair: &Keypair) -> Result<Tx, WrapperTxErr> {
            if self.pk != keypair.public {
                return Err(WrapperTxErr::InvalidKeyPair);
            }
            Ok(Tx::new(
                vec![],
                Some(self.try_to_vec().expect("Could not serialize WrapperTx")),
            )
            .sign(keypair))
        }
    }

    impl TryFrom<&Tx> for WrapperTx {
        type Error = WrapperTxErr;

        fn try_from(tx: &Tx) -> Result<Self, Self::Error> {
            if let Some(data) = tx.data.as_ref() {
                BorshDeserialize::deserialize(&mut data.as_ref())
                    .map_err(|_| WrapperTxErr::InvalidWrapperTx)
            } else {
                Err(WrapperTxErr::Unsigned)
            }
        }
    }

    /// Struct that classifies that kind of Tx
    /// based on the contents of its data.
    #[derive(Clone, Debug, PartialEq)]
    pub enum TxType {
        /// An ordinary Tx
        Raw(Tx),
        /// A Tx that contains an encrypted raw Tx
        Wrapper(Tx),
    }

    impl From<TxType> for Tx {
        fn from(ty: TxType) -> Self {
            match ty {
                TxType::Raw(tx) => tx,
                TxType::Wrapper(tx) => tx,
            }
        }
    }

    /// Determines if the input Tx is a raw Tx or a wrapper.
    ///
    /// If it is a raw Tx, the Tx is returned unchanged inside an
    /// enum variant stating that it is a raw Tx.
    ///
    /// If it is a WrapperTx, we extract the signed data of
    /// the Tx and verify it is of the appropriate form. This means
    /// 1. The signed Tx data deserializes to a WrapperTx type
    /// 2. The wrapper tx is indeed signed
    /// 3. The signature is valid
    ///
    /// We modify the data of input Tx to contain only the signed
    /// data if valid and return it wrapped in a enum variant
    /// indicating it is a wrapper. Otherwise, an error is
    /// returned indicating the signature was not valid
    pub fn process_tx(mut tx: Tx) -> Result<TxType, WrapperTxErr> {
        if let Some(Ok(SignedTxData {
            data: Some(data),
            ref sig,
        })) = tx
            .data
            .as_ref()
            .map(|data| SignedTxData::try_from_slice(&data[..]))
        {
            if let Ok(wrapper) =
                <WrapperTx as BorshDeserialize>::deserialize(&mut data.as_ref())
            {
                verify_tx_sig(&wrapper.pk, &tx, sig)
                    .map_err(|err| WrapperTxErr::SigError(err.to_string()))?;
                tx.data = Some(
                    wrapper
                        .try_to_vec()
                        .expect("Serializing WrapperTx should not fail"),
                );
                Ok(TxType::Wrapper(tx))
            } else {
                Ok(TxType::Raw(tx))
            }
        } else {
            match WrapperTx::try_from(&tx) {
                Ok(_) => Err(WrapperTxErr::Unsigned),
                _ => Ok(TxType::Raw(tx)),
            }
        }
    }

    /// Used to determine if a Tx is a
    /// wrapper Tx or not
    impl From<Tx> for TxType {
        fn from(tx: Tx) -> Self {
            if let Some(ref data) = tx.data {
                match <WrapperTx as BorshDeserialize>::deserialize(
                    &mut data.as_ref(),
                ) {
                    Ok(_) => TxType::Wrapper(tx),
                    _ => TxType::Raw(tx),
                }
            } else {
                TxType::Raw(tx)
            }
        }
    }

    #[cfg(test)]
    mod test_encrypted_tx {
        use super::*;

        /// Test that encryption and decryption are inverses.
        #[test]
        fn test_encrypt_decrypt() {
            // The trivial public - private keypair
            let pubkey = <EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator();
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            // generate encrypted payload
            let encrypted =
                EncryptedTx::encrypt("Super secret stuff".as_bytes(), pubkey);
            // check that encryption doesn't do trivial things
            assert_ne!(encrypted.0.ciphertext, "Super secret stuff".as_bytes());
            // decrypt the payload and check we got original data back
            let decrypted = encrypted.decrypt(privkey);
            assert_eq!(decrypted, "Super secret stuff".as_bytes());
        }

        /// Test that serializing and deserializing again via Borsh produces
        /// original payload
        #[test]
        fn test_encrypted_tx_round_trip_borsh() {
            // The trivial public - private keypair
            let pubkey = <EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator();
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            // generate encrypted payload
            let encrypted =
                EncryptedTx::encrypt("Super secret stuff".as_bytes(), pubkey);
            // serialize via Borsh
            let borsh = encrypted.try_to_vec().expect("Test failed");
            // deserialize again
            let new_encrypted: EncryptedTx =
                BorshDeserialize::deserialize(&mut borsh.as_ref())
                    .expect("Test failed");
            // check that decryption works as expected
            let decrypted = new_encrypted.decrypt(privkey);
            assert_eq!(decrypted, "Super secret stuff".as_bytes());
        }

        /// Test that serializing and deserializing again via Serde produces
        /// original payload
        #[test]
        fn test_encrypted_tx_round_trip_serde() {
            // The trivial public - private keypair
            let pubkey = <EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator();
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            // generate encrypted payload
            let encrypted =
                EncryptedTx::encrypt("Super secret stuff".as_bytes(), pubkey);
            // serialize via Serde
            let js = serde_json::to_string(&encrypted).expect("Test failed");
            // deserialize it again
            let new_encrypted: EncryptedTx =
                serde_json::from_str(&js).expect("Test failed");
            let decrypted = new_encrypted.decrypt(privkey);
            // check that decryption works as expected
            assert_eq!(decrypted, "Super secret stuff".as_bytes());
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
        use crate::types::address::xan;
        use crate::types::key::ed25519::{verify_tx_sig, SignedTxData};

        fn gen_keypair() -> Keypair {
            use rand::prelude::ThreadRng;
            use rand::thread_rng;

            let mut rng: ThreadRng = thread_rng();
            Keypair::generate(&mut rng)
        }

        /// We test that when we feed in a Tx and then decrypt it again
        /// that we get what we started with.
        #[test]
        fn test_encryption_round_trip() {
            let keypair = gen_keypair();
            println!("{}", keypair.public.to_string());
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
            );
            assert!(wrapper.validate_ciphertext());
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            let decrypted = wrapper.decrypt(privkey).expect("Test failed");
            assert_eq!(tx, decrypted);
        }

        /// We test that when we try to decrpyt a tx and it
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
            );
            // give a incorrect commitment to the decrypted contents of the tx
            wrapper.tx_hash = [0u8; 32];
            assert!(wrapper.validate_ciphertext());
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            let err = wrapper.decrypt(privkey).expect_err("Test failed");
            assert_eq!(err, WrapperTxErr::DecryptedHash);
        }

        /// We check that even if the encrypted payload and has of its
        /// contents are correctly changed, we detect fraudulent activity
        /// via the signature.
        #[test]
        fn test_malleability_attack_detection() {
            let pubkey = <EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator();
            let keypair = gen_keypair();
            // The intendend tx
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
            )
            .sign(&keypair)
            .expect("Test failed");

            // we now try to alter the inner tx maliciously
            let mut wrapper = WrapperTx::try_from(&Tx::from(
                process_tx(tx.clone()).expect("Test failed"),
            ))
            .expect("Test failed");
            let mut signed_tx_data =
                SignedTxData::try_from_slice(&tx.data.unwrap()[..])
                    .expect("Test failed");

            // malicious transaction
            let malicious =
                Tx::new("Give me all the money".as_bytes().to_owned(), None);

            // We replace the inner tx with a malicious one
            wrapper.inner_tx =
                EncryptedTx::encrypt(&malicious.to_bytes(), pubkey);

            // We change the commitment appropriately
            let digest = Sha256::digest(&malicious.to_bytes());
            let mut hash_bytes = [0u8; 32];
            hash_bytes.copy_from_slice(&digest);
            wrapper.tx_hash = hash_bytes;

            // we check ciphertext validity still passes
            assert!(wrapper.validate_ciphertext());
            // we check that decryption still succeeds
            let decrypted = wrapper.decrypt(
                <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator()
            )
                .expect("Test failed");
            assert_eq!(decrypted, malicious);

            // we substitute in the modified wrapper
            signed_tx_data.data =
                Some(wrapper.try_to_vec().expect("Test failed"));
            tx.data = Some(signed_tx_data.try_to_vec().expect("Test failed"));

            // check that the signature is not valid
            verify_tx_sig(&keypair.public, &tx, &signed_tx_data.sig)
                .expect_err("Test failed");
            // check that the try from method also fails
            let err = process_tx(tx).expect_err("Test failed");
            assert_eq!(
                err,
                WrapperTxErr::SigError(
                    "Signature verification failed: signature error".into()
                )
            );
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

        /// Test that process_tx correctly identifies a raw tx with some
        /// data and returns an identical copy
        #[test]
        fn test_process_tx_raw_tx_some_data() {
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            );

            match process_tx(tx.clone()).expect("Test failed") {
                TxType::Raw(raw) => assert_eq!(tx, raw),
                _ => panic!("Test failed: Expected Raw Tx"),
            }
        }

        /// Test that process_tx correctly identifies a raw tx with some
        /// signed data and returns an identical copy
        #[test]
        fn test_process_tx_raw_tx_some_signed_data() {
            let tx = Tx::new(
                "wasm code".as_bytes().to_owned(),
                Some("transaction data".as_bytes().to_owned()),
            )
            .sign(&gen_keypair());

            match process_tx(tx.clone()).expect("Test failed") {
                TxType::Raw(raw) => assert_eq!(tx, raw),
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
                    let wrapper: WrapperTx = BorshDeserialize::deserialize(
                        &mut wrapper.data.expect("Test failed").as_ref(),
                    )
                    .expect("Test failed");
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
                Some(wrapper.try_to_vec().expect("Test failed")),
            );
            let result = process_tx(tx).expect_err("Test failed");
            assert_eq!(result, WrapperTxErr::Unsigned);
        }
    }
}

#[cfg(feature = "ferveo-tpke")]
pub use wrapper_tx::*;
