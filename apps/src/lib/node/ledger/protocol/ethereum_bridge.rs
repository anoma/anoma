use std::collections::HashSet;

use anoma::ledger::pos::anoma_proof_of_stake::PosBase;
use anoma::types::address::{Address, InternalAddress};
use anoma::types::ethereum_headers::{
    EpochPower, MultiSignedEthHeader, SignedHeader,
};
use anoma::types::hash::Hash;
use anoma::types::storage::{DbKeySeg, Key};
use anoma::vm::types::EthereumHeaderUpdate;
use borsh::BorshDeserialize;
use fraction::GenericFraction;

type Fraction = GenericFraction<u64>;

use super::*;

pub fn make_ethereum_header_data<D, H>(
    header: &MultiSignedEthHeader,
    storage: &Storage<D, H>,
) -> (Result<EthereumHeaderUpdate>, u64)
where
    D: 'static + DB + for<'iter> DBIter<'iter> + Sync,
    H: 'static + StorageHasher + Sync,
{
    let hash = header.hash();
    // storage keys that we look up in order to update
    let voting_power_key = get_voting_power_key(&hash);
    let seen_by_key = get_seen_by_key(&hash);

    // get the voting power that has seen this header
    let (current_voting_power, mut gas) =
        storage.read(&voting_power_key).unwrap_or_default();
    let current_voting_power = current_voting_power
        .iter()
        .filter_map(|bytes| {
            <(u64, u64) as BorshDeserialize>::deserialize(&mut bytes.as_slice())
                .ok()
        })
        .next()
        .unwrap_or((0, 1));
    let mut current_voting_power =
        Fraction::new(current_voting_power.0, current_voting_power.1);

    // get the validators that have seen this header;
    let (seen_by, next_gas) = storage.read(&seen_by_key).unwrap_or_default();
    let mut seen_by = seen_by
        .iter()
        .filter_map(|bytes| {
            <HashSet<EpochPower> as BorshDeserialize>::deserialize(
                &mut bytes.as_slice(),
            )
            .ok()
        })
        .next()
        .unwrap_or_default();
    gas += next_gas;

    // update the seen_by and voting powers
    for power in header.get_voting_powers().into_iter() {
        let voting_power = power.voting_power;
        let block_height = power.block_height;
        if seen_by.insert(power) {
            let epoch = match storage
                .block
                .pred_epochs
                .get_epoch(block_height.into())
            {
                Some(epoch) => epoch,
                _ => {
                    return (
                        Err(Error::EthereumHeaderTxError(
                            "Could not find epoch from block height".into(),
                        )),
                        gas,
                    );
                }
            };
            let validators = storage.read_validator_set();
            let validators = match validators.get(epoch) {
                Some(validators) => validators,
                _ => {
                    return (
                        Err(Error::EthereumHeaderTxError(
                            "Could not find validators for given epoch".into(),
                        )),
                        gas,
                    );
                }
            };
            let total_voting_power: u64 = validators
                .active
                .iter()
                .map(|validator| u64::from(validator.voting_power))
                .sum();
            current_voting_power +=
                Fraction::new(voting_power, total_voting_power);
        }
    }
    let seen = 3 * current_voting_power.numer().unwrap()
        > 2 * current_voting_power.denom().unwrap();
    (
        Ok(EthereumHeaderUpdate {
            header: header.get_header().clone(),
            seen_by,
            voting_power: (
                *current_voting_power.numer().unwrap(),
                *current_voting_power.denom().unwrap(),
            ),
            seen,
        }),
        gas,
    )
}

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
#[allow(dead_code)]
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

pub mod ethereum_headers_vp {
    use std::collections::BTreeSet;
    use std::convert::TryFrom;

    use anoma::ledger::native_vp::{self, Ctx, NativeVp};
    use anoma::ledger::storage::{self as ledger_storage, StorageHasher};
    use anoma::proto::Tx;
    use anoma::types::ethereum_headers::EthereumHeader;
    use anoma::types::transaction::protocol::{ProtocolTx, ProtocolTxType};
    use anoma::types::transaction::{process_tx, TxType};
    use anoma::vm::WasmCacheAccess;
    use borsh::BorshDeserialize;
    use thiserror::Error;

    use super::*;

    #[allow(dead_code)]
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

    impl<'a, DB, H, CA> EthereumStateVp<'a, DB, H, CA>
    where
        DB: 'static
            + ledger_storage::DB
            + for<'iter> ledger_storage::DBIter<'iter>,
        H: 'static + StorageHasher,
        CA: 'static + WasmCacheAccess,
    {
        /// Helper function for reading values from storage
        fn read_value<T>(&self, key: &Key) -> Option<T>
        where
            T: BorshDeserialize,
        {
            if let Ok(Some(bytes)) = self.ctx.read_post(key) {
                <T as BorshDeserialize>::try_from_slice(bytes.as_slice()).ok()
            } else {
                None
            }
        }

        /// Recreate the header update from the tx. Check that
        /// voting powers were given correctly
        fn make_ethereum_header_data(
            &self,
            header: &MultiSignedEthHeader,
        ) -> Option<EthereumHeaderUpdate> {
            let hash = header.hash();
            // storage keys that we look up in order to update
            let voting_power_key = get_voting_power_key(&hash);
            let seen_by_key = get_seen_by_key(&hash);

            // get the voting power that has seen this header
            let current_voting_power =
                self.ctx.read_pre(&voting_power_key).unwrap_or_default();
            let current_voting_power = current_voting_power
                .iter()
                .filter_map(|bytes| {
                    <(u64, u64) as BorshDeserialize>::deserialize(
                        &mut bytes.as_slice(),
                    )
                    .ok()
                })
                .next()
                .unwrap_or((0, 1));
            let mut current_voting_power =
                Fraction::new(current_voting_power.0, current_voting_power.1);

            // get the validators that have seen this header;
            let seen_by = self.ctx.read_pre(&seen_by_key).unwrap_or_default();
            let mut seen_by = seen_by
                .iter()
                .filter_map(|bytes| {
                    <HashSet<EpochPower> as BorshDeserialize>::deserialize(
                        &mut bytes.as_slice(),
                    )
                    .ok()
                })
                .next()
                .unwrap_or_default();

            // update the seen_by and voting powers
            for power in header.get_voting_powers().into_iter() {
                let address = power.validator.clone();
                let voting_power = power.voting_power;
                let block_height = power.block_height;
                if seen_by.insert(power) {
                    let epoch = match self
                        .ctx
                        .storage
                        .block
                        .pred_epochs
                        .get_epoch(block_height.into())
                    {
                        Some(epoch) => epoch,
                        _ => return None,
                    };
                    let validators = self.ctx.storage.read_validator_set();
                    let validators = match validators.get(epoch) {
                        Some(validators) => validators,
                        _ => return None,
                    };
                    // check that the voting power was correctly given
                    match validators
                        .active
                        .iter()
                        .find(|validator| address == validator.address)
                        .map(|validator| u64::from(validator.voting_power))
                    {
                        Some(power) => {
                            if power != voting_power {
                                return None;
                            }
                        }
                        _ => return None,
                    }

                    let total_voting_power: u64 = validators
                        .active
                        .iter()
                        .map(|validator| u64::from(validator.voting_power))
                        .sum();
                    current_voting_power +=
                        Fraction::new(voting_power, total_voting_power);
                }
            }
            let seen = 3 * current_voting_power.numer().unwrap()
                > 2 * current_voting_power.denom().unwrap();
            Some(EthereumHeaderUpdate {
                header: header.get_header().clone(),
                seen_by,
                voting_power: (
                    *current_voting_power.numer().unwrap(),
                    *current_voting_power.denom().unwrap(),
                ),
                seen,
            })
        }
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
            _verifiers: &BTreeSet<Address>,
        ) -> Result<bool> {
            // check that the changes came from a protocol tx
            let header = if let Ok(Ok(TxType::Protocol(ProtocolTx {
                tx: ProtocolTxType::EthereumHeaders(header),
                ..
            }))) = Tx::try_from(tx_data).map(process_tx)
            {
                header
            } else {
                return Ok(false);
            };
            // check that all four relevant storage keys were changed
            let hash = header.hash();
            let header_key = get_header_key(&hash);
            let seen_by_key = get_seen_by_key(&hash);
            let voting_power_key = get_voting_power_key(&hash);
            let seen_key = get_seen_key(&hash);
            let expected_keys_changed = BTreeSet::from([
                header_key.clone(),
                seen_by_key.clone(),
                voting_power_key.clone(),
                seen_key.clone(),
            ]);
            if !keys_changed.is_superset(&expected_keys_changed) {
                return Ok(false);
            }

            // check that the storage keys were changed correctly
            let EthereumHeaderUpdate {
                header,
                voting_power,
                seen_by,
                seen,
            } = match self.make_ethereum_header_data(&header) {
                Some(update) => update,
                _ => return Ok(false),
            };
            let expected_header =
                self.read_value::<EthereumHeader>(&header_key);
            let expected_power =
                self.read_value::<(u64, u64)>(&voting_power_key);
            let expected_seen_by =
                self.read_value::<HashSet<EpochPower>>(&seen_by_key);
            let expected_seen = self.read_value::<bool>(&seen_key);
            match (
                expected_header,
                expected_power,
                expected_seen_by,
                expected_seen,
            ) {
                (
                    Some(exp_header),
                    Some(exp_power),
                    Some(exp_seen_by),
                    Some(exp_seen),
                ) => {
                    if exp_header != header {
                        return Ok(false);
                    }
                    if exp_power != voting_power {
                        return Ok(false);
                    }
                    if exp_seen_by != seen_by {
                        return Ok(false);
                    }
                    if exp_seen != seen {
                        return Ok(false);
                    }
                }
                _ => return Ok(false),
            }

            Ok(true)
        }
    }
}

#[cfg(all(test, feature = "dev"))]
mod test_ethereum_bridge {
    use anoma::ledger::storage::mockdb::MockDB;
    use anoma::ledger::storage::Sha256Hasher;
    use anoma::types::chain::ChainId;
    use anoma::types::ethereum_headers::EthereumHeader;
    use anoma::types::hash::Hash;
    use anoma::types::time::DateTimeUtc;

    use super::*;
    use crate::config::genesis;
    use crate::wallet::defaults::*;

    /// This is not a meaningful default. It is
    /// just for testing
    fn default_eth_header() -> EthereumHeader {
        EthereumHeader {
            hash: Hash([0; 32]),
            parent_hash: Hash([0; 32]),
            number: 0u64,
            difficulty: 0.into(),
            mix_hash: Hash([0; 32]),
            nonce: Default::default(),
            state_root: Hash([0; 32]),
            transactions_root: Hash([0; 32]),
        }
    }

    /// Start a new test shell and initialize it. Returns the shell paired with
    /// a broadcast receiver, which will receives any protocol txs sent by the
    /// shell.
    fn setup() -> Storage<MockDB, Sha256Hasher> {
        let mut storage = Storage::<MockDB, Sha256Hasher>::open(
            std::path::Path::new(""),
            ChainId::default(),
            None,
        );
        let genesis = genesis::genesis();
        parameters::init_genesis_storage(&mut storage, &genesis.parameters);
        // Depends on parameters being initialized
        storage
            .init_genesis_epoch(
                0.into(),
                DateTimeUtc::now(),
                &genesis.parameters,
            )
            .expect("Test failed");
        let (current_epoch, _gas) = storage.get_current_epoch();
        pos::init_genesis_storage(
            &mut storage,
            &genesis.pos_params,
            genesis
                .validators
                .iter()
                .map(|validator| &validator.pos_data),
            current_epoch,
        );
        storage
    }

    /// Test the case when as header is seen for the first time.
    /// Thus, none of the keys exist in storage yet.
    #[test]
    fn test_new_header() {
        let storage = setup();
        let header: MultiSignedEthHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        let EthereumHeaderUpdate {
            header,
            seen_by,
            voting_power,
            seen,
        } = make_ethereum_header_data(&header, &storage)
            .0
            .expect("Test failed");
        assert_eq!(header, default_eth_header());
        assert_eq!(
            seen_by,
            HashSet::from([EpochPower {
                validator: validator_address(),
                voting_power: 200,
                block_height: 0
            }])
        );
        assert_eq!(
            Fraction::new(voting_power.0, voting_power.1),
            Fraction::new(1u64, 1u64)
        );
        assert!(seen);
    }

    /// Test that the produced data ignores a
    /// validator if they are marked as having seen
    /// the header already.
    #[test]
    fn test_seen_by_validator_already() {
        let mut storage = setup();

        let header: MultiSignedEthHeader = default_eth_header()
            .sign(100, validator_address(), 0, &validator_keys().0)
            .into();
        let hash = header.hash();
        // add validator to the seen_by list
        let power = EpochPower {
            validator: validator_address(),
            voting_power: 50,
            block_height: 2,
        };
        storage
            .write(
                &get_seen_by_key(&hash),
                HashSet::from([power.clone()])
                    .try_to_vec()
                    .expect("Test failed"),
            )
            .expect("Test failed");

        // update the current voting power key
        storage
            .write(
                &get_voting_power_key(&hash),
                (1u64, 4u64).try_to_vec().expect("Test failed"),
            )
            .expect("Test failed");

        let EthereumHeaderUpdate {
            header,
            seen_by,
            voting_power,
            seen,
        } = make_ethereum_header_data(&header, &storage)
            .0
            .expect("Test failed");
        assert_eq!(header, default_eth_header());
        assert_eq!(seen_by, HashSet::from([power]));
        assert_eq!(
            Fraction::new(voting_power.0, voting_power.1),
            Fraction::new(1u64, 4u64)
        );
        assert!(!seen);
    }
}
