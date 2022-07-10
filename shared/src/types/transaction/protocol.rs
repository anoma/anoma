/// Types for sending and verifying txs
/// used in Anoma protocols
use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};

use crate::types::address::Address;

/// A data type containing information used to update the DKG session key
#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
)]
pub struct UpdateDkgSessionKey {
    /// The storage key of the validators public DKG session key
    pub address: Address,
    /// The serialization of the new public key associated with the validator
    pub dkg_public_key: Vec<u8>,
}

#[cfg(feature = "ferveo-tpke")]
mod protocol_txs {
    use std::io::{ErrorKind, Write};
    use std::path::Path;

    use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
    use ferveo::dkg::pv::Message;
    use serde_json;

    use super::*;
    use crate::proto::Tx;
    use crate::types::key::*;
    use crate::types::transaction::{EllipticCurve, TxError, TxType};

    const TX_NEW_DKG_KP_WASM: &str = "tx_update_dkg_session_keypair.wasm";

    #[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
    /// Txs sent by validators as part of internal protocols
    pub struct ProtocolTx {
        /// we require ProtocolTxs be signed
        pub pk: common::PublicKey,
        /// The type of protocol message being sent
        pub tx: ProtocolTxType,
    }

    impl ProtocolTx {
        /// Validate the signature of a protocol tx
        pub fn validate_sig(
            &self,
            signed_hash: [u8; 32],
            sig: &common::Signature,
        ) -> Result<(), TxError> {
            common::SigScheme::verify_signature(&self.pk, &signed_hash, sig)
                .map_err(|err| {
                    TxError::SigError(format!(
                        "ProtocolTx signature verification failed: {}",
                        err
                    ))
                })
        }
    }

    /// DKG message wrapper type that adds Borsh encoding.
    #[derive(Clone, Debug)]
    pub struct DkgMessage(pub Message<EllipticCurve>);

    #[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
    #[allow(clippy::large_enum_variant)]
    /// Types of protocol messages to be sent
    pub enum ProtocolTxType {
        /// Messages to be given to the DKG state machine
        DKG(DkgMessage),
        /// Tx requesting a new DKG session keypair
        NewDkgKeypair(Tx),
        /// Aggregation of Ethereum state changes
        /// voted on by validators in last block
        EthereumStateUpdate(Tx),
    }

    impl ProtocolTxType {
        /// Sign a ProtocolTxType and wrap it up in a normal Tx
        pub fn sign(
            self,
            pk: &common::PublicKey,
            signing_key: &common::SecretKey,
        ) -> Tx {
            Tx::new(
                vec![],
                Some(
                    TxType::Protocol(ProtocolTx {
                        pk: pk.clone(),
                        tx: self,
                    })
                    .try_to_vec()
                    .expect("Could not serialize ProtocolTx"),
                ),
            )
            .sign(signing_key)
        }

        /// Create a new tx requesting a new DKG session keypair
        pub fn request_new_dkg_keypair<'a, F>(
            data: UpdateDkgSessionKey,
            signing_key: &common::SecretKey,
            wasm_dir: &'a Path,
            wasm_loader: F,
        ) -> Self
        where
            F: FnOnce(&'a str, &'static str) -> Vec<u8>,
        {
            let code = wasm_loader(
                wasm_dir
                    .to_str()
                    .expect("Converting path to string should not fail"),
                TX_NEW_DKG_KP_WASM,
            );
            Self::NewDkgKeypair(
                Tx::new(
                    code,
                    Some(
                        data.try_to_vec()
                            .expect("Serializing request should not fail"),
                    ),
                )
                .sign(signing_key),
            )
        }
    }

    impl BorshSerialize for DkgMessage {
        fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
            let blob = serde_json::to_string(&self.0)
                .map_err(|err| {
                    std::io::Error::new(ErrorKind::InvalidData, err)
                })?
                .as_bytes()
                .to_owned();
            BorshSerialize::serialize(&blob, writer)
        }
    }

    impl BorshDeserialize for DkgMessage {
        fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
            let blob: Vec<u8> = BorshDeserialize::deserialize(buf)?;
            let json = String::from_utf8(blob).map_err(|err| {
                std::io::Error::new(ErrorKind::InvalidData, err)
            })?;
            let msg = serde_json::from_str(&json).map_err(|err| {
                std::io::Error::new(ErrorKind::InvalidData, err)
            })?;
            Ok(Self(msg))
        }
    }

    impl BorshSchema for DkgMessage {
        fn add_definitions_recursively(
            definitions: &mut std::collections::HashMap<
                borsh::schema::Declaration,
                borsh::schema::Definition,
            >,
        ) {
            // Encoded as `Vec<u8>`;
            let elements = "u8".into();
            let definition = borsh::schema::Definition::Sequence { elements };
            definitions.insert(Self::declaration(), definition);
        }

        fn declaration() -> borsh::schema::Declaration {
            "DkgMessage".into()
        }
    }

    impl From<Message<EllipticCurve>> for ProtocolTxType {
        fn from(msg: Message<EllipticCurve>) -> ProtocolTxType {
            ProtocolTxType::DKG(DkgMessage(msg))
        }
    }
}

#[cfg(feature = "ferveo-tpke")]
pub use protocol_txs::*;
