/// Integration of Ferveo cryptographic primitives
/// to enable encrypted txs.
/// *Not wasm compatible*
#[cfg(feature = "ferveo-tpke")]
pub mod encrypted_tx {
    use std::io::{Error, ErrorKind, Write};

    use ark_ec::PairingEngine;
    use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
    use borsh::{BorshDeserialize, BorshSerialize};
    use serde::{Deserialize, Serialize};
    use tpke::{encrypt, Ciphertext};

    use crate::types::transaction::{AffineCurve, EllipticCurve};
    /// The first group in our elliptic curve bilinear pairing
    pub type G1 = <EllipticCurve as PairingEngine>::G1Affine;
    /// An encryption key for txs
    #[derive(Debug, Clone, PartialEq)]
    pub struct EncryptionKey(pub G1);

    impl Default for EncryptionKey {
        fn default() -> Self {
            Self(G1::prime_subgroup_generator())
        }
    }

    impl borsh::ser::BorshSerialize for EncryptionKey {
        fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
            let mut buf = Vec::<u8>::new();
            CanonicalSerialize::serialize(&self.0, &mut buf)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            BorshSerialize::serialize(&buf, writer)
        }
    }

    impl borsh::de::BorshDeserialize for EncryptionKey {
        fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
            let key: Vec<u8> = BorshDeserialize::deserialize(buf)?;
            Ok(EncryptionKey(
                CanonicalDeserialize::deserialize(&*key)
                    .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
            ))
        }
    }

    /// We use a specific choice of two groups and bilinear pairing
    /// We use a wrapper type to add traits
    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(from = "SerializedCiphertext")]
    #[serde(into = "SerializedCiphertext")]
    pub struct EncryptedTx(pub Ciphertext<EllipticCurve>);

    impl EncryptedTx {
        /// Encrypt a message to give a new ciphertext
        pub fn encrypt(msg: &[u8], pubkey: EncryptionKey) -> Self {
            let mut rng = rand::thread_rng();
            Self(encrypt(msg, pubkey.0, &mut rng))
        }

        /// Decrypt a message and return it as raw bytes
        pub fn decrypt(
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
            )
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

    impl borsh::BorshSchema for EncryptedTx {
        fn add_definitions_recursively(
            definitions: &mut std::collections::HashMap<
                borsh::schema::Declaration,
                borsh::schema::Definition,
            >,
        ) {
            // Encoded as `(Vec<u8>, Vec<u8>, Vec<u8>)`
            let elements = "u8".into();
            let definition = borsh::schema::Definition::Sequence { elements };
            definitions.insert("Vec<u8>".into(), definition);
            let elements =
                vec!["Vec<u8>".into(), "Vec<u8>".into(), "Vec<u8>".into()];
            let definition = borsh::schema::Definition::Tuple { elements };
            definitions.insert(Self::declaration(), definition);
        }

        fn declaration() -> borsh::schema::Declaration {
            "EncryptedTx".into()
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

    #[cfg(test)]
    mod test_encrypted_tx {
        use ark_ec::AffineCurve;

        use super::*;

        /// Test that encryption and decryption are inverses.
        #[test]
        fn test_encrypt_decrypt() {
            // The trivial public - private keypair
            let pubkey = EncryptionKey(<EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator());
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
            let pubkey = EncryptionKey(<EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator());
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
            let pubkey = EncryptionKey(<EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator());
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
}

#[cfg(feature = "ferveo-tpke")]
pub use encrypted_tx::*;
