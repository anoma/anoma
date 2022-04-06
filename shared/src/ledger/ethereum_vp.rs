//! The native validity predicate for verifying the Ethereum
//! headers to be included

use crate::types::address::{Address, InternalAddress};
use crate::types::hash::Hash;
use crate::types::storage::{DbKeySeg, Key};

const ETH_ADDRESS: Address = Address::Internal(InternalAddress::EthereumState);

/// The VP storage key for an Ethereum header
pub fn get_header_key(hash: &Hash) -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ETH_ADDRESS),
            DbKeySeg::StringSeg(hash.to_string()),
            DbKeySeg::StringSeg("header".into()),
        ],
    }
}

/// The VP storage key for Ethereum SC messages awaiting
/// enough confirmations
pub fn get_messages_key() -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ETH_ADDRESS),
            DbKeySeg::StringSeg("messages".into()),
        ],
    }
}

/// Key to see which validators have seen a particular
/// header
pub fn get_seen_by_key(hash: &Hash) -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ETH_ADDRESS),
            DbKeySeg::StringSeg(hash.to_string()),
            DbKeySeg::StringSeg("seen_by".into()),
        ],
    }
}

/// Key to see the sum of the voting power of the
/// validators that have seen a given header.
pub fn get_voting_power_key(hash: &Hash) -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ETH_ADDRESS),
            DbKeySeg::StringSeg(hash.to_string()),
            DbKeySeg::StringSeg("voting_power".into()),
        ],
    }
}

/// Key to see if at 2 / 3 of staking validators have
/// seen a particular header
pub fn get_seen_key(hash: &Hash) -> Key {
    Key {
        segments: vec![
            DbKeySeg::AddressSeg(ETH_ADDRESS),
            DbKeySeg::StringSeg(hash.to_string()),
            DbKeySeg::StringSeg("seen".into()),
        ],
    }
}

#[cfg(all(not(feature = "ABCI"), feature = "ethereum-headers"))]
#[allow(missing_docs)]
pub mod ethereum_headers_vp {
    use std::collections::BTreeSet;
    use std::convert::TryFrom;

    use borsh::{BorshDeserialize, BorshSerialize};
    use thiserror::Error;

    use super::*;
    use crate::ledger::ethash::EthVerifier;
    use crate::ledger::native_vp::{self, Ctx, NativeVp};
    use crate::ledger::storage::{self as ledger_storage, StorageHasher};
    use crate::proto::Tx;
    use crate::types::ethereum_headers::{
        EthereumHeader, MultiSignedEthHeader,
    };
    use crate::types::token::Amount;
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

    /// Ethereum state validity predicate
    pub struct EthereumStateVp<'a, DB, H, CA>
    where
        DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
        H: StorageHasher,
        CA: WasmCacheAccess,
    {
        /// Context to interact with the host structures.
        pub ctx: Ctx<'a, DB, H, CA>,
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
            _keys_changed: &BTreeSet<Key>,
            _verifiers: &BTreeSet<Address>,
        ) -> Result<bool> {
            let _header = if let Ok(Ok(TxType::Protocol(ProtocolTx {
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
