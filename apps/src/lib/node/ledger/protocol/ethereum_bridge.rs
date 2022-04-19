use std::collections::HashSet;

use anoma::ledger::pos::anoma_proof_of_stake::types::ValidatorSet;
use anoma::ledger::pos::anoma_proof_of_stake::PosBase;
use anoma::ledger::pos::ValidatorSets;
use anoma::types::address::{Address, InternalAddress};
use anoma::types::ethereum_headers::{
    EpochPower, EthereumHeader, MultiSignedEthHeader, SignedHeader,
};
use anoma::types::hash::Hash;
use anoma::types::key::{common, protocol_pk_key};
use anoma::types::storage::{DbKeySeg, Epoch, Key};
use anoma::vm::types::EthereumHeaderUpdate;
use borsh::BorshDeserialize;
use fraction::GenericFraction;

type Fraction = GenericFraction<u64>;

use super::*;

pub trait CraftEthereumStorageData {
    fn read_storage(&self, key: &Key) -> Result<(Option<Vec<u8>>, u64)>;
    fn get_epoch(&self, block_height: u64) -> Option<Epoch>;
    fn get_validator_set(&self) -> ValidatorSets;
    fn verify_eth_header(&self, header: &EthereumHeader) -> bool;
    /// Get the key for a validator's address
    fn get_validator_key(
        &self,
        address: &Address,
    ) -> (Result<common::PublicKey>, u64) {
        let (bytes, gas) = match self.read_storage(&protocol_pk_key(address)) {
            Ok(result) => result,
            Err(e) => return (Err(e), 0),
        };
        if let Some(bytes) = bytes {
            (
                BorshDeserialize::deserialize(&mut bytes.as_ref()).map_err(
                    |_| {
                        Error::EthereumHeaderTxError(
                            "Protocol public key in storage should be \
                             deserializable"
                                .into(),
                        )
                    },
                ),
                gas,
            )
        } else {
            (
                Err(Error::EthereumHeaderTxError(
                    "Could not find the protocol key for validator".into(),
                )),
                gas,
            )
        }
    }

    /// If a validator has not voted on this header before,
    /// get the epoch it voted on this header and return the
    /// corresponding validator set.
    ///
    /// This also updates the list of validators that has seen
    /// this header to include the validator being checked
    fn get_validator_set_by_epoch<'a>(
        &self,
        validator_set: &'a ValidatorSets,
        seen_by: &mut HashSet<EpochPower>,
        power: EpochPower,
    ) -> Result<Option<&'a ValidatorSet<Address>>> {
        let block_height = power.block_height;
        if seen_by.insert(power) {
            let epoch = match self.get_epoch(block_height) {
                Some(epoch) => epoch,
                _ => {
                    return Err(Error::EthereumHeaderTxError(
                        "Could not find epoch from block height".into(),
                    ));
                }
            };

            Some(validator_set.get(epoch).ok_or_else(|| {
                Error::EthereumHeaderTxError(
                    "Could not find validators for given epoch".into(),
                )
            }))
        } else {
            None
        }
        .transpose()
    }

    /// The checks that a validator correctly gave their information about
    /// a header. It checks that they correctly gave their voting power for
    /// the epoch at which they voted on this header.
    ///
    /// It also fetches their public protocol key (for verifying signatures)
    /// and keeps a running total of the stake backing a header
    fn verify_validator_data(
        &self,
        power: EpochPower,
        gas: &mut u64,
        current_voting_power: &mut Fraction,
        seen_by: &mut HashSet<EpochPower>,
        public_keys: &mut Vec<common::PublicKey>,
    ) -> Result<()> {
        let address = power.validator.clone();
        let voting_power = power.voting_power;
        let validator_set = self.get_validator_set();
        let validators = match self.get_validator_set_by_epoch(
            &validator_set,
            seen_by,
            power,
        )? {
            Some(validators) => validators,
            None => {
                let (key, next_gas) = self.get_validator_key(&address);
                *gas += next_gas;
                public_keys.push(key?);
                return Ok(());
            }
        };

        let pk = match validators
            .active
            .iter()
            .find(|validator| address == validator.address)
            .map(|validator| u64::from(validator.voting_power))
        {
            Some(power) if power != voting_power => {
                Err(Error::EthereumHeaderTxError(
                    "A validator's voting power was incorrectly given".into(),
                ))
            }
            Some(_) => {
                let (key, next_gas) = self.get_validator_key(&address);
                *gas += next_gas;
                key
            }
            _ => Err(Error::EthereumHeaderTxError(
                "A validator's voting power could not be found".into(),
            )),
        }?;
        public_keys.push(pk);
        let total_voting_power: u64 = validators
            .active
            .iter()
            .map(|validator| u64::from(validator.voting_power))
            .sum();
        *current_voting_power +=
            Fraction::new(voting_power, total_voting_power);
        Ok(())
    }

    /// Craft the storage changes to be written given
    /// a multi-signed Ethereum header
    fn make_ethereum_header_data(
        &self,
        header: &MultiSignedEthHeader,
    ) -> (Result<EthereumHeaderUpdate>, u64) {
        let hash = header.header_hash();
        // storage keys that we look up in order to update
        let voting_power_key = get_voting_power_key(&hash);
        let seen_by_key = get_seen_by_key(&hash);

        // get the voting power that has seen this header
        let (current_voting_power, mut gas) =
            self.read_storage(&voting_power_key).unwrap_or_default();
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

        // a running total of the voting power backing a header
        let mut current_voting_power =
            Fraction::new(current_voting_power.0, current_voting_power.1);

        // get the validators that have seen this header;
        let (seen_by, next_gas) =
            self.read_storage(&seen_by_key).unwrap_or_default();

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

        // the public keys used to verify the signatures
        let mut public_keys = vec![];

        // update the seen_by, voting powers, current voting power,
        // and fetch the public keys for signature verification.
        for power in header.get_voting_powers().into_iter() {
            // check that the voting power was correctly given
            if let Err(e) = self.verify_validator_data(
                power,
                &mut gas,
                &mut current_voting_power,
                &mut seen_by,
                &mut public_keys,
            ) {
                return (Err(e), gas);
            }
        }

        // verify the signatures
        if header.verify_signatures(&public_keys).is_err() {
            return (
                Err(Error::EthereumHeaderTxError(
                    "The Ethereum header was not correctly signed".into(),
                )),
                gas,
            );
        }

        // verify the Ethereum header via the ethash algorithm
        if !self.verify_eth_header(header.get_header()) {
            return (
                Err(Error::EthereumHeaderTxError(
                    "The Ethereum header is not valid".into(),
                )),
                gas,
            );
        }

        // check if 2 / 3 of the signing validators have seen this header
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
}

impl<D, H> CraftEthereumStorageData for Storage<D, H>
where
    D: 'static + DB + for<'iter> DBIter<'iter> + Sync,
    H: 'static + StorageHasher + Sync,
{
    fn read_storage(&self, key: &Key) -> Result<(Option<Vec<u8>>, u64)> {
        self.read(key).map_err(Error::StorageError)
    }

    fn get_epoch(&self, block_height: u64) -> Option<Epoch> {
        self.block.pred_epochs.get_epoch(block_height.into())
    }

    fn get_validator_set(&self) -> ValidatorSets {
        self.read_validator_set()
    }

    fn verify_eth_header(&self, header: &EthereumHeader) -> bool {
        match self.eth_verifier.as_ref() {
            Some(verifier) => verifier.verify_header(header),
            _ => false,
        }
    }
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

    use anoma::ledger::native_vp::{Ctx, NativeVp};
    use anoma::ledger::storage::{self as ledger_storage, StorageHasher};
    use anoma::proto::Tx;
    use anoma::types::ethereum_headers::{
        EthereumHeader, MultiSignedEthHeader, SignedHeader,
    };
    use anoma::vm::WasmCacheAccess;
    use borsh::BorshDeserialize;

    use super::*;

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
    }

    impl<'a, DB, H, CA> CraftEthereumStorageData for EthereumStateVp<'a, DB, H, CA>
    where
        DB: 'static
            + ledger_storage::DB
            + for<'iter> ledger_storage::DBIter<'iter>,
        H: 'static + StorageHasher,
        CA: 'static + WasmCacheAccess,
    {
        fn read_storage(&self, key: &Key) -> Result<(Option<Vec<u8>>, u64)> {
            self.ctx
                .read_pre(key)
                .map(|result| (result, 0))
                .map_err(super::Error::EthereumStateVpError)
        }

        fn get_epoch(&self, block_height: u64) -> Option<Epoch> {
            self.ctx
                .storage
                .block
                .pred_epochs
                .get_epoch(block_height.into())
        }

        fn get_validator_set(&self) -> ValidatorSets {
            self.ctx.storage.read_validator_set()
        }

        fn verify_eth_header(&self, header: &EthereumHeader) -> bool {
            match self.ctx.storage.eth_verifier.as_ref() {
                Some(verifier) => verifier.verify_header(header),
                _ => false,
            }
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
            let header = if let Ok(Tx {
                data: Some(header), ..
            }) = Tx::try_from(tx_data)
            {
                if let Ok(header) =
                    MultiSignedEthHeader::try_from_slice(&header[..])
                {
                    header
                } else {
                    return Ok(false);
                }
            } else {
                return Ok(false);
            };
            // check that all four relevant storage keys were changed
            let hash = header.header_hash();
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
                (Ok(update), _) => update,
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

    /// A VP to reject changes to Ethereum state storage from
    /// txs that are not protocol txs.
    ///
    /// For protocol txs of type `ProtocolTxType::EthereumHeaders`,
    /// this VP is skipped (i.e. removed from the verifiers list).
    pub struct EthereumSentinelVp;

    impl NativeVp for EthereumSentinelVp {
        type Error = Error;

        const ADDR: InternalAddress = InternalAddress::EthereumSentinel;

        fn validate_tx(
            &self,
            _tx_data: &[u8],
            keys_changed: &BTreeSet<Key>,
            _verifiers: &BTreeSet<Address>,
        ) -> Result<bool> {
            Ok(!keys_changed.iter().any(|key| {
                key.segments.contains(&DbKeySeg::AddressSeg(ETH_ADDRESS))
            }))
        }
    }
}

#[cfg(all(test, feature = "dev", not(feature = "eth-fullnode")))]
mod test_ethereum_bridge {
    use std::env::temp_dir;

    use anoma::ledger::ethash::MockVerifier;
    use anoma::ledger::native_vp::Ctx;
    use anoma::ledger::storage::mockdb::MockDB;
    use anoma::ledger::storage::Sha256Hasher;
    use anoma::types::chain::ChainId;
    use anoma::types::ethereum_headers::{
        EthereumHeader, SignedEthereumHeader,
    };
    use anoma::types::hash::Hash;
    use anoma::types::time::DateTimeUtc;
    use anoma::vm::WasmCacheRwAccess;
    use paste::paste;

    use super::*;
    use crate::config::genesis;
    use crate::node::ledger::shell::test_utils::gen_keypair;
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
        for validator in &genesis.validators {
            storage
                .write(
                    &protocol_pk_key(&validator.pos_data.address),
                    validator
                        .protocol_key
                        .try_to_vec()
                        .expect("encode protocol public key"),
                )
                .expect("Unable to set genesis user protocol public key");
        }
        storage.eth_verifier = Some(MockVerifier::new(0, 0, 0));
        storage
    }

    /// Setup a ctx for running native vps
    fn setup_ctx<'a>(
        tx: &'a Tx,
        storage: &'a Storage<MockDB, Sha256Hasher>,
        write_log: &'a WriteLog,
    ) -> Ctx<'a, MockDB, Sha256Hasher, WasmCacheRwAccess> {
        Ctx::new(
            storage,
            &write_log,
            tx,
            VpGasMeter::new(0u64),
            VpCache::new(temp_dir(), 100usize),
        )
    }

    /// Write the storage updates from an Ethereum header tx
    /// in the exact same way the wasm blob run by the protocol
    /// would.
    fn mock_wasm_code(
        storage: &Storage<MockDB, Sha256Hasher>,
        write_log: &mut WriteLog,
        header: &MultiSignedEthHeader,
    ) {
        let (update, _) = storage.make_ethereum_header_data(header);
        let EthereumHeaderUpdate {
            header,
            seen_by,
            voting_power,
            seen,
        } = update.expect("Test failed");
        let hash = header.hash();
        let header_key = get_header_key(&hash);
        let seen_by_key = get_seen_by_key(&hash);
        let voting_power_key = get_voting_power_key(&hash);
        let seen_key = get_seen_key(&hash);
        write_log
            .write(&header_key, header.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&seen_by_key, seen_by.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&voting_power_key, voting_power.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&seen_key, seen.try_to_vec().unwrap())
            .expect("Test failed");
    }

    /// Parameterize tests for both structs implementing the
    /// [`CraftEthereumStorageData`] trait
    macro_rules! make_tests {
        ($func:ident) => {
            paste! {
                #[test]
                fn [<$func _storage>]() {
                    let storage = setup();
                    $func(storage);
                }
            }
            paste! {
                #[test]
                fn [<$func _vp>]() {
                    let storage = setup();
                    let write_log = WriteLog::default();
                    let tx = Tx::new(vec![], None);
                    let ctx = setup_ctx(&tx, &storage, &write_log);
                    let eth_vp = EthereumStateVp { ctx };
                    $func(eth_vp);
                }
            }
        };
    }

    /// Test the case when as header is seen for the first time.
    /// Thus, none of the keys exist in storage yet.
    fn test_new_header(update_crafter: impl CraftEthereumStorageData) {
        let header: MultiSignedEthHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        let EthereumHeaderUpdate {
            header,
            seen_by,
            voting_power,
            seen,
        } = update_crafter
            .make_ethereum_header_data(&header)
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
        let hash = header.header_hash();
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

        let write_log = WriteLog::default();
        let tx = Tx::new(vec![], None);
        let ctx = setup_ctx(&tx, &storage, &write_log);
        let eth_vp = EthereumStateVp { ctx };

        // check the update date from the native vp
        let EthereumHeaderUpdate {
            header: eth_header,
            seen_by,
            voting_power,
            seen,
        } = eth_vp
            .make_ethereum_header_data(&header)
            .0
            .expect("Test failed");
        // check the update date from the storage
        let EthereumHeaderUpdate {
            header: eth_header_other,
            seen_by: seen_by_other,
            voting_power: voting_power_other,
            seen: seen_other,
        } = storage
            .make_ethereum_header_data(&header)
            .0
            .expect("Test failed");
        // check that the two updates are equal
        assert_eq!(eth_header, eth_header_other);
        assert_eq!(seen_by, seen_by_other);
        assert_eq!(voting_power, voting_power_other);
        assert_eq!(seen, seen_other);

        assert_eq!(eth_header, default_eth_header());
        assert_eq!(seen_by, HashSet::from([power]));
        assert_eq!(
            Fraction::new(voting_power.0, voting_power.1),
            Fraction::new(1u64, 4u64)
        );
        assert!(!seen);
    }

    /// Test that the VP will reject an invalid Ethereum header
    fn test_invalid_header(update_crafter: impl CraftEthereumStorageData) {
        let mut header = default_eth_header();
        header.hash = Hash([1; 32]);
        let header: MultiSignedEthHeader = header
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        assert!(update_crafter.make_ethereum_header_data(&header).0.is_err())
    }

    /// Test that a incorrect signature is rejected
    fn test_invalid_sig_rejected(
        update_crafter: impl CraftEthereumStorageData,
    ) {
        let keypair = gen_keypair();
        let header = default_eth_header()
            .sign(200, validator_address(), 0, &keypair)
            .into();
        assert!(update_crafter.make_ethereum_header_data(&header).0.is_err())
    }

    /// Test that a validator that gives the incorrect voting power
    /// for themselves is rejected by the VP.
    fn test_incorrect_voting_power_rejected(
        update_crafter: impl CraftEthereumStorageData,
    ) {
        let keypair = gen_keypair();
        let header = default_eth_header()
            .sign(100, validator_address(), 0, &keypair)
            .into();
        assert!(update_crafter.make_ethereum_header_data(&header).0.is_err())
    }

    /// Test that a non-validator address is rejected by the VP.
    fn test_non_validator_address_rejected(
        update_crafter: impl CraftEthereumStorageData,
    ) {
        let header = default_eth_header()
            .sign(200, bertha_address(), 0, &validator_keypair())
            .into();
        assert!(update_crafter.make_ethereum_header_data(&header).0.is_err())
    }

    make_tests!(test_new_header);
    make_tests!(test_invalid_header);
    make_tests!(test_invalid_sig_rejected);
    make_tests!(test_incorrect_voting_power_rejected);
    make_tests!(test_non_validator_address_rejected);

    /// Test the happy flow that a state change crafted by protocol
    /// passes the native vp's validation
    #[test]
    fn test_native_vp() {
        let header: MultiSignedEthHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        let hash = header.header_hash();

        let storage = setup();
        let mut write_log = WriteLog::default();
        // mock running the tx and applying the changes to the WAL
        mock_wasm_code(&storage, &mut write_log, &header);

        let tx = Tx::new(vec![], None);
        let ctx = setup_ctx(&tx, &storage, &write_log);

        let eth_vp = EthereumStateVp { ctx };
        let tx_to_verify = Tx::new(vec![], Some(header.try_to_vec().unwrap()));

        let keys_changed = BTreeSet::from([
            get_voting_power_key(&hash),
            get_seen_key(&hash),
            get_seen_by_key(&hash),
            get_header_key(&hash),
        ]);
        let verifiers = BTreeSet::from([]);
        assert!(
            eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        );
    }

    /// Test that the changed key set must contain keys
    /// for `voting_power`, `seen_by`, `seen`, and `header`.
    /// Otherwise the change is rejected
    #[test]
    fn test_wrong_changed_keys_rejected() {
        let header: MultiSignedEthHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        let hash = header.header_hash();

        let storage = setup();
        let mut write_log = WriteLog::default();
        // mock running the tx and applying the changes to the WAL
        mock_wasm_code(&storage, &mut write_log, &header);
        let tx = Tx::new(vec![], None);
        let ctx = setup_ctx(&tx, &storage, &write_log);

        let eth_vp = EthereumStateVp { ctx };
        let tx_to_verify = Tx::new(vec![], Some(header.try_to_vec().unwrap()));

        let keys_changed = BTreeSet::from([
            get_voting_power_key(&hash),
            get_seen_by_key(&hash),
            get_header_key(&hash),
        ]);
        let verifiers = BTreeSet::from([]);
        assert!(
            !eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        );
    }

    /// Check that if the input tx data does not
    /// serialize to the correct struct, it is rejected.
    #[test]
    fn test_incorrect_tx_data_rejected() {
        let header: SignedEthereumHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        let hash = header.header_hash();

        let storage = setup();
        let mut write_log = WriteLog::default();
        // mock running the tx and applying the changes to the WAL
        mock_wasm_code(
            &storage,
            &mut write_log,
            &MultiSignedEthHeader::from(header.clone()),
        );
        let tx = Tx::new(vec![], None);
        let ctx = setup_ctx(&tx, &storage, &write_log);

        let eth_vp = EthereumStateVp { ctx };
        let tx_to_verify = Tx::new(vec![], Some(header.try_to_vec().unwrap()));
        let keys_changed = BTreeSet::from([
            get_voting_power_key(&hash),
            get_seen_key(&hash),
            get_seen_by_key(&hash),
            get_header_key(&hash),
        ]);
        let verifiers = BTreeSet::from([]);
        assert!(
            !eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        );
    }

    /// Test that if the changed keys don't contain
    /// the correct hash of the included header, the
    /// changes are rejected.
    #[test]
    fn test_wrong_header_hash_rejected() {
        let header: MultiSignedEthHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();

        let storage = setup();
        let mut write_log = WriteLog::default();
        let (update, _) = storage.make_ethereum_header_data(&header);
        let EthereumHeaderUpdate {
            header: eth_header,
            seen_by,
            voting_power,
            seen,
        } = update.expect("Test failed");
        let hash = Hash([0; 32]);
        let header_key = get_header_key(&hash);
        let seen_by_key = get_seen_by_key(&hash);
        let voting_power_key = get_voting_power_key(&hash);
        let seen_key = get_seen_key(&hash);
        write_log
            .write(&header_key, eth_header.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&seen_by_key, seen_by.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&voting_power_key, voting_power.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&seen_key, seen.try_to_vec().unwrap())
            .expect("Test failed");
        let tx = Tx::new(vec![], None);
        let ctx = setup_ctx(&tx, &storage, &write_log);

        let eth_vp = EthereumStateVp { ctx };
        let tx_to_verify = Tx::new(vec![], Some(header.try_to_vec().unwrap()));

        let keys_changed = BTreeSet::from([
            get_voting_power_key(&hash),
            get_seen_key(&hash),
            get_seen_by_key(&hash),
            get_header_key(&hash),
        ]);
        let verifiers = BTreeSet::from([]);
        assert!(
            !eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        );
    }

    /// Test that if an attempt to recreate the
    /// `EthereumUpdate` instance returns an error,
    /// the changes are rejected.
    #[test]
    fn test_invalid_update_rejected() {
        let mut header: MultiSignedEthHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        let hash = header.header_hash();

        let storage = setup();
        let mut write_log = WriteLog::default();
        let (update, _) = storage.make_ethereum_header_data(&header);
        let EthereumHeaderUpdate {
            header: eth_header,
            seen_by,
            voting_power: _,
            seen,
        } = update.expect("Test failed");
        let header_key = get_header_key(&hash);
        let seen_by_key = get_seen_by_key(&hash);
        let voting_power_key = get_voting_power_key(&hash);
        let seen_key = get_seen_key(&hash);
        write_log
            .write(&header_key, eth_header.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&seen_by_key, seen_by.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&voting_power_key, (100u64, 200u64).try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&seen_key, seen.try_to_vec().unwrap())
            .expect("Test failed");
        let tx = Tx::new(vec![], None);
        let ctx = setup_ctx(&tx, &storage, &write_log);

        let eth_vp = EthereumStateVp { ctx };
        // this should now cause an error when trying to make the EthereumUpdate
        // struct
        header.signers[0].1 = 100;
        let tx_to_verify = Tx::new(vec![], Some(header.try_to_vec().unwrap()));

        let keys_changed = BTreeSet::from([
            get_voting_power_key(&hash),
            get_seen_key(&hash),
            get_seen_by_key(&hash),
            get_header_key(&hash),
        ]);
        let verifiers = BTreeSet::from([]);
        assert!(
            !eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        );
    }

    /// Test that if the storage changes aren't correct,
    /// they are rejected.
    #[test]
    fn test_incorrect_update_rejected() {
        let header: MultiSignedEthHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        let hash = header.header_hash();

        let storage = setup();
        let mut write_log = WriteLog::default();
        let (update, _) = storage.make_ethereum_header_data(&header);
        let EthereumHeaderUpdate {
            header: eth_header,
            seen_by,
            voting_power: _,
            seen,
        } = update.expect("Test failed");
        let header_key = get_header_key(&hash);
        let seen_by_key = get_seen_by_key(&hash);
        let voting_power_key = get_voting_power_key(&hash);
        let seen_key = get_seen_key(&hash);
        write_log
            .write(&header_key, eth_header.try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&seen_by_key, seen_by.try_to_vec().unwrap())
            .expect("Test failed");
        // this is not correct
        write_log
            .write(&voting_power_key, (100u64, 200u64).try_to_vec().unwrap())
            .expect("Test failed");
        write_log
            .write(&seen_key, seen.try_to_vec().unwrap())
            .expect("Test failed");
        let tx = Tx::new(vec![], None);
        let ctx = setup_ctx(&tx, &storage, &write_log);

        let eth_vp = EthereumStateVp { ctx };
        let tx_to_verify = Tx::new(vec![], Some(header.try_to_vec().unwrap()));

        let keys_changed = BTreeSet::from([
            get_voting_power_key(&hash),
            get_seen_key(&hash),
            get_seen_by_key(&hash),
            get_header_key(&hash),
        ]);
        let verifiers = BTreeSet::from([]);
        assert!(
            !eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        );
    }

    /// Check that if some keys are changed but not others,
    /// all are rejected.
    #[test]
    fn test_not_all_keys_changed_rejected() {
        let header: MultiSignedEthHeader = default_eth_header()
            .sign(200, validator_address(), 0, &validator_keys().0)
            .into();
        let hash = header.header_hash();
        let storage = setup();
        let write_log = WriteLog::default();
        let tx = Tx::new(vec![], None);
        let ctx = setup_ctx(&tx, &storage, &write_log);

        let eth_vp = EthereumStateVp { ctx };
        let tx_to_verify = Tx::new(vec![], Some(header.try_to_vec().unwrap()));

        let keys_changed = BTreeSet::from([
            get_voting_power_key(&hash),
            get_seen_key(&hash),
            get_seen_by_key(&hash),
            get_header_key(&hash),
        ]);
        let verifiers = BTreeSet::from([]);
        assert!(
            !eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        );
    }

    /// Test that the sentinel rejects any changes
    /// that tries to change storage keys belonging
    /// to Ethereum bridge
    #[test]
    fn test_ethereum_sentinel() {
        let eth_vp = EthereumSentinelVp {};
        let tx_to_verify = Tx::new(vec![], None);

        // check we reject changes that touch the Ethereum bridge storage
        let keys_changed = BTreeSet::from([get_seen_key(&Hash([0; 32]))]);
        let verifiers = BTreeSet::from([]);
        assert!(
            !eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        );

        // check we allow other storage changes
        let keys_changed =
            BTreeSet::from([protocol_pk_key(&validator_address())]);
        assert!(
            eth_vp
                .validate_tx(
                    tx_to_verify.to_bytes().as_slice(),
                    &keys_changed,
                    &verifiers,
                )
                .expect("Test failed")
        )
    }
}
