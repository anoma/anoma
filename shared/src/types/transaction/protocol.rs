/// Types for sending and verifying txs
/// used in Anoma protocols
#[cfg(feature = "ferveo-tpke")]
mod protocol_txs {
    use std::io::{ErrorKind, Write};

    use borsh::{BorshDeserialize, BorshSerialize};
    use ferveo::dkg::pv::Message;
    use serde::{Deserialize, Serialize};
    use serde_json;

    use crate::proto::Tx;
    use crate::types::key::ed25519::{
        verify_tx_sig, Keypair, PublicKey, Signature,
    };
    use crate::types::transaction::{EllipticCurve, TxError, TxType};

    #[derive(Clone, Debug, Serialize, Deserialize)]
    /// Txs sent by validators as part of internal protocols
    pub struct ProtocolTx {
        /// we require ProtocolTxs be signed
        pub pk: PublicKey,
        /// The type of protocol message being sent
        pub tx: ProtocolTxType,
    }

    impl ProtocolTx {
        /// Validate the signature of a protocol tx
        pub fn validate_sig(
            &self,
            tx: &Tx,
            sig: &Signature,
        ) -> Result<(), TxError> {
            verify_tx_sig(&self.pk, &tx, sig).map_err(|err| {
                TxError::SigError(format!(
                    "ProtocolTx signature verification failed: {}",
                    err
                ))
            })
        }
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    /// Types of protocol messages to be sent
    pub enum ProtocolTxType {
        /// Messages for the DKG protocol
        DKG(Message<EllipticCurve>),
    }

    impl ProtocolTxType {
        /// Sign a ProtocolTxType and wrap it up in a normal Tx
        pub fn sign(self, keypair: &Keypair) -> Tx {
            Tx::new(
                vec![],
                Some(
                    TxType::Protocol(ProtocolTx {
                        pk: keypair.public.clone(),
                        tx: self,
                    })
                    .try_to_vec()
                    .expect("Could not serialize ProtocolTx"),
                ),
            )
            .sign(keypair)
        }
    }

    impl borsh::ser::BorshSerialize for ProtocolTx {
        fn serialize<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
            let json = serde_json::to_string(&self).map_err(|err| {
                std::io::Error::new(ErrorKind::InvalidData, err)
            })?;
            BorshSerialize::serialize(&json, writer)
        }
    }

    impl borsh::de::BorshDeserialize for ProtocolTx {
        fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
            let json = <String as BorshDeserialize>::deserialize(buf)?;
            serde_json::from_str(&json)
                .map_err(|err| std::io::Error::new(ErrorKind::InvalidData, err))
        }
    }

    impl From<Message<EllipticCurve>> for ProtocolTxType {
        fn from(msg: Message<EllipticCurve>) -> ProtocolTxType {
            ProtocolTxType::DKG(msg)
        }
    }
}

#[cfg(feature = "ferveo-tpke")]
pub use protocol_txs::*;
