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
    pub struct EncryptedTx{
        /// encryption of a hash of the code field of the Tx
        pub encrypted_code_hash :Ciphertext<EllipticCurve>,
        /// encryption of the code field of the Tx
        pub encrypted_code :Ciphertext<EllipticCurve>,
        /// encryption of the data field of the Tx
        pub encrypted_data :Ciphertext<EllipticCurve>,
        /// encryption of the timestamp field of the Tx
        pub encrypted_ts :Ciphertext<EllipticCurve>
        // todo: is the ledger encrypting the data? if not we need to add
        //  an extra hash field here to prevent malleability
    }

    impl EncryptedTx {
        /// Encrypt a message to give a new ciphertext
        pub fn encrypt(code_hash: &[u8],code: &[u8],data: &[u8], ts: &[u8], pubkey: EncryptionKey) -> Self {
            let mut rng = rand::thread_rng();
            let encrypted_code_hash = encrypt(code_hash, pubkey.0.clone(), &mut rng);
            let encrypted_code = encrypt(code, pubkey.0.clone(), &mut rng);
            let encrypted_data = encrypt(data, pubkey.0.clone(), &mut rng);
            let encrypted_ts = encrypt(ts, pubkey.0.clone(), &mut rng);
            Self{
                encrypted_code_hash,
                encrypted_code,
                encrypted_data,
                encrypted_ts
            }
        }

        /// Decrypt a message and return it as raw bytes
        pub fn decrypt(
            &self,
            privkey: <EllipticCurve as PairingEngine>::G2Affine,
        ) -> Vec<u8>{
            let code_hash = tpke::decrypt(&self.encrypted_code_hash, privkey.clone());
            let code = tpke::decrypt(&self.encrypted_code, privkey.clone());
            let data = tpke::decrypt(&self.encrypted_data, privkey.clone());
            let ts = tpke::decrypt(&self.encrypted_ts,privkey.clone());
            [code_hash,code,data, ts].concat()
        }
    }

    impl borsh::ser::BorshSerialize for EncryptedTx {
        fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
            let Ciphertext {
                nonce: nonce_code_hash,
                ciphertext: ciphertext_code_hash,
                auth_tag: auth_tag_code_hash,
            } = &self.encrypted_code_hash;

            let Ciphertext {
                nonce: nonce_code,
                ciphertext: ciphertext_code,
                auth_tag: auth_tag_code,
            } = &self.encrypted_code;

            let Ciphertext {
                nonce: nonce_data,
                ciphertext: ciphertext_data,
                auth_tag: auth_tag_data,
            } = &self.encrypted_data;

            let Ciphertext {
                nonce: nonce_ts,
                ciphertext: ciphertext_ts,
                auth_tag: auth_tag_ts,
            } = &self.encrypted_ts;

            // Serialize the nonce into bytes
            // for nonce_code_hash
            let mut nonce_code_hash_buffer = Vec::<u8>::new();
            nonce_code_hash
                .serialize(&mut nonce_code_hash_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            // for nonce_code
            let mut nonce_code_buffer = Vec::<u8>::new();
            nonce_code
                .serialize(&mut nonce_code_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            // for nonce_data
            let mut nonce_data_buffer = Vec::<u8>::new();
            nonce_data
                .serialize(&mut nonce_data_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            // for nonce_ts
            let mut nonce_ts_buffer = Vec::<u8>::new();
            nonce_ts
                .serialize(&mut nonce_ts_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;

            // serialize the auth_tag to bytes
            // for auth_tag_code_hash
            let mut tag_code_hash_buffer = Vec::<u8>::new();
            auth_tag_code_hash
                .serialize(&mut tag_code_hash_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            // for auth_tag_code
            let mut tag_code_buffer = Vec::<u8>::new();
            auth_tag_code
                .serialize(&mut tag_code_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            // for auth_tag_data
            let mut tag_data_buffer = Vec::<u8>::new();
            auth_tag_data
                .serialize(&mut tag_data_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
            // for auth_tag_ts
            let mut tag_ts_buffer = Vec::<u8>::new();
            auth_tag_ts
                .serialize(&mut tag_ts_buffer)
                .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;


            // serialize the three byte arrays
            BorshSerialize::serialize(
                &(
                    nonce_code_hash_buffer,ciphertext_code_hash,tag_code_hash_buffer,
                    nonce_code_buffer, ciphertext_code, tag_code_buffer,
                    nonce_data_buffer,ciphertext_data,tag_data_buffer,
                    nonce_ts_buffer,ciphertext_ts,tag_ts_buffer
                ),
                writer,
            )
        }
    }

    impl borsh::BorshDeserialize for EncryptedTx {
        fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
            type VecTuple = (Vec<u8>, Vec<u8>, Vec<u8>,
                             Vec<u8>, Vec<u8>, Vec<u8>,
                             Vec<u8>, Vec<u8>, Vec<u8>,
                             Vec<u8>, Vec<u8>, Vec<u8>);
            let (
                nonce_code_hash, ciphertext_code_hash, auth_tag_code_hash,
                nonce_code, ciphertext_code, auth_tag_code,
                nonce_data, ciphertext_data, auth_tag_data,
                nonce_ts, ciphertext_ts, auth_tag_ts
            ): VecTuple =
                BorshDeserialize::deserialize(buf)?;
            Ok(
                EncryptedTx{
                    encrypted_code_hash : Ciphertext {
                        nonce: CanonicalDeserialize::deserialize(&*nonce_code_hash)
                            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
                        ciphertext:ciphertext_code_hash,
                        auth_tag: CanonicalDeserialize::deserialize(&*auth_tag_code_hash)
                            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
                    },
                    encrypted_code : Ciphertext {
                        nonce: CanonicalDeserialize::deserialize(&*nonce_code)
                            .map_err(|err|
                                Error::new(ErrorKind::InvalidData, err))?,
                        ciphertext:ciphertext_code,
                        auth_tag: CanonicalDeserialize::deserialize(&*auth_tag_code)
                            .map_err(|err|
                                Error::new(ErrorKind::InvalidData, err))?,
                    },
                    encrypted_data : Ciphertext {
                        nonce: CanonicalDeserialize::deserialize(&*nonce_data)
                            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
                        ciphertext:ciphertext_data,
                        auth_tag: CanonicalDeserialize::deserialize(&*auth_tag_data)
                            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
                    },
                    encrypted_ts : Ciphertext {
                        nonce: CanonicalDeserialize::deserialize(&*nonce_ts)
                            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
                        ciphertext:ciphertext_ts,
                        auth_tag: CanonicalDeserialize::deserialize(&*auth_tag_ts)
                            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?,
                    }
                })
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
        use crate::types::transaction::hash_tx;

        use super::*;

        /// Test that encryption and decryption are inverses.
        #[test]
        fn test_encrypt_decrypt() {
            // The trivial public - private keypair
            let pubkey = EncryptionKey(<EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator());
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            // generate data to encrypt
            let code = "Super secret stuff".as_bytes();
            let code_hash = &hash_tx(code).0;
            let data = "secret data".as_bytes();
            let ts = "01/10/1995".as_bytes();
            // generate encrypted payload
            let encrypted =
                EncryptedTx::encrypt(code_hash,code,data,ts,pubkey);
            // check that encryption doesn't do trivial things
            assert_ne!(encrypted.encrypted_code.ciphertext, "Super secret stuff".as_bytes());
            // decrypt the payload and check we got original data back
            let decrypted = encrypted.decrypt(privkey);
            assert_eq!(&decrypted[0..code_hash.len()], code_hash);
            assert_eq!(&decrypted[code_hash.len()..code_hash.len()+code.len()], code);
            assert_eq!(&decrypted[code_hash.len()+code.len()..
                code_hash.len()+code.len()+data.len()],
                       data);
        }

        /// Test that serializing and deserializing again via Borsh produces
        /// original payload
        #[test]
        fn test_encrypted_tx_round_trip_borsh() {
            // The trivial public - private keypair
            let pubkey = EncryptionKey(<EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator());
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            // generate encrypted payload
            let code = "Super secret stuff".as_bytes();
            let code_hash = &hash_tx(code).0;
            let data = "secret data on my birthday".as_bytes();
            let ts = "01/10/1995".as_bytes();
            let encrypted =
                EncryptedTx::encrypt(code_hash,code,data,ts,pubkey);
            // serialize via Borsh
            let borsh = encrypted.try_to_vec().expect("Test failed");
            // deserialize again
            let new_encrypted: EncryptedTx =
                BorshDeserialize::deserialize(&mut borsh.as_ref())
                    .expect("Test failed");
            // check that decryption works as expected
            let decrypted = new_encrypted.decrypt(privkey);
            assert_eq!(&decrypted[0..code_hash.len()], code_hash);
            assert_eq!(&decrypted[code_hash.len()..code_hash.len()+code.len()], code);
            assert_eq!(&decrypted[code_hash.len()+code.len()..
                code_hash.len()+code.len()+data.len()],
                       data);
        }

        /// Test that serializing and deserializing again via Serde produces
        /// original payload
        #[test]
        fn test_encrypted_tx_round_trip_serde() {
            // The trivial public - private keypair
            let pubkey = EncryptionKey(<EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator());
            let privkey = <EllipticCurve as PairingEngine>::G2Affine::prime_subgroup_generator();
            // generate data to encrypt
            let code = "Super secret stuff".as_bytes();
            let code_hash = &hash_tx(code).0;
            let data = "secret data on my birthday".as_bytes();
            let ts = "01/10/1995".as_bytes();
            // generate encrypted payload
            let encrypted =
                EncryptedTx::encrypt(code_hash,code,data,ts,pubkey);

            // serialize via Serde
            let js = serde_json::to_string(&encrypted).expect("Test failed");
            // deserialize it again
            let new_encrypted: EncryptedTx =
                serde_json::from_str(&js).expect("Test failed");
            let decrypted = new_encrypted.decrypt(privkey);
            assert_eq!(&decrypted[0..code_hash.len()], code_hash);
            assert_eq!(&decrypted[code_hash.len()..code_hash.len()+code.len()], code);
            assert_eq!(&decrypted[code_hash.len()+code.len()..
                code_hash.len()+code.len()+data.len()],
                       data);
        }
    }
}

#[cfg(feature = "ferveo-tpke")]
pub use encrypted_tx::*;
