//! The native validity predicate for verifying the Ethereum
//! headers to be included

#[cfg(all(not(feature = "ABCI"), feature = "ethereum-headers"))]
#[allow(missing_docs)]
pub mod ethereum_headers_vp {
    use std::collections::BTreeSet;
    use std::convert::TryFrom;

    use thiserror::Error;

    use crate::ledger::ethash::EthVerifier;
    use crate::ledger::native_vp::{self, Ctx, NativeVp};
    use crate::ledger::storage::{self as ledger_storage, StorageHasher};
    use crate::proto::Tx;
    use crate::types::address::{Address, InternalAddress};
    use crate::types::storage::{DbKeySeg, Key};
    use crate::types::transaction::protocol::{ProtocolTx, ProtocolTxType};
    use crate::types::transaction::{process_tx, TxType};
    use crate::vm::WasmCacheAccess;

    #[allow(missing_docs)]
    #[derive(Error, Debug)]
    pub enum Error {
        #[error("Native VP error: {0}")]
        NativeVpError(native_vp::Error),
    }

    /// Verifying Ethereum state result
    pub type Result<T> = std::result::Result<T, Error>;

    /// Check if a key belongs to the storage of the Ethereum state VP and gets
    /// the corresponding subkey. Else returns None.
    pub fn get_ethereum_subkey(key: &Key) -> Option<&str> {
        match &key.segments[..] {
            [
                DbKeySeg::AddressSeg(addr),
                DbKeySeg::StringSeg(key),
                DbKeySeg::StringSeg(subkey),
            ] if addr == &Address::Internal(InternalAddress::EthereumState)
                && key == "eth_headers" =>
            {
                Some(subkey.as_str())
            }
            _ => None,
        }
    }

    /// Ethereum state validity predicate
    pub struct EthereumStateVp<'a, DB, H, CA>
    where
        DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
        H: StorageHasher,
        CA: WasmCacheAccess,
    {
        /// Context to interact with the host structures.
        pub ctx: Ctx<'a, DB, H, CA>,
        /// Precomputes the pseudo-random hashes needed for ethash.
        /// This is so that the VP can run quickly
        pub verifier: EthVerifier,
    }

    impl<'a, DB, H, CA> NativeVp for EthereumStateVp<'a, DB, H, CA>
    where
        DB: 'static
            + ledger_storage::DB
            + for<'iter> ledger_storage::DBIter<'iter>,
        H: 'static + StorageHasher,
        CA: 'static + WasmCacheAccess,
    {
        type Error = Error;

        const ADDR: InternalAddress = InternalAddress::EthereumState;

        fn validate_tx(
            &self,
            tx_data: &[u8],
            keys_changed: &BTreeSet<Key>,
            verifiers: &BTreeSet<Address>,
        ) -> Result<bool> {
            let header =
                if let Ok(Ok(TxType::Protocol(ProtocolTx {
                    tx: ProtocolTxType::EthereumHeaders(header),
                    ..
                }))) = Tx::try_from(tx_data).map(process_tx)
                {
                    header
                } else {
                    return Ok(false);
                };
            Ok(true)
        }
    }
}

#[cfg(all(not(feature = "ABCI"), feature = "ethereum-headers"))]
pub use ethereum_headers_vp::*;
