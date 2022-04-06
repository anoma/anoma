use std::collections::HashSet;

use anoma::ledger::ethereum_vp::{get_seen_by_key, get_voting_power_key};
use anoma::types::address::Address;
use anoma::types::ethereum_headers::{
    EpochPower, MultiSignedEthHeader, SignedHeader,
};
use anoma::vm::types::EthereumHeaderUpdate;
use borsh::BorshDeserialize;
use fraction::GenericFraction;

type Fraction = GenericFraction<u64>;

use anoma::ledger::pos::anoma_proof_of_stake::PosBase;

use super::*;

pub fn make_ethereum_header_data<D, H>(
    header: MultiSignedEthHeader,
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
        storage.read(&voting_power_key).unwrap();
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
    let (seen_by, next_gas) =
        storage.read(&seen_by_key).unwrap_or_default();
    let mut seen_by = seen_by
        .iter()
        .filter_map(|bytes| {
            <HashSet<Address> as BorshDeserialize>::deserialize(
                &mut bytes.as_slice(),
            )
            .ok()
        })
        .next()
        .unwrap_or_default();
    gas += next_gas;

    // update the seen_by and voting powers
    for EpochPower {
        validator,
        voting_power,
        block_height,
    } in header.get_voting_powers().into_iter()
    {
        if !seen_by.contains(validator) {
            seen_by.insert(validator.clone());
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
            header: header.into_header(),
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

#[cfg(all(test, feature = "dev"))]
mod test_ethereum_bridge {
    use super::*;

    use anoma::ledger::storage::Sha256Hasher;
    use anoma::ledger::storage::mockdb::MockDB;
    use anoma::types::chain::ChainId;
    use anoma::types::ethereum_headers::EthereumHeader;
    use anoma::types::hash::Hash;
    use anoma::types::time::DateTimeUtc;

    use crate::config::genesis;
    use crate::wallet::defaults::*;
    use anoma::ledger::pos::validator_set_key;

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
            None
        );
        let genesis = genesis::genesis();
        parameters::init_genesis_storage(
            &mut storage,
            &genesis.parameters,
        );
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
            .sign(
                200,
                validator_address(),
                0,
                &validator_keys().0,
            ).into();
        let EthereumHeaderUpdate {
            header,
            seen_by,
            voting_power,
            seen,
        } = make_ethereum_header_data(header, &storage)
            .0
            .expect("Test failed");
        assert_eq!(header, default_eth_header());
        assert_eq!(seen_by, HashSet::from([validator_address()]));
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
            .sign(
                100,
                validator_address(),
                0,
                &validator_keys().0,
            ).into();
        let hash = header.hash();
        // add validator to the seen_by list
        storage.write(
            &get_seen_by_key(&hash),
            HashSet::from([validator_address()])
                .try_to_vec()
                .expect("Test failed")
        )
        .expect("Test failed");

        // update the current voting power key
        storage.write(
            &get_voting_power_key(&hash),
            (1u64, 3u64).try_to_vec().expect("Test failed")
        )
        .expect("Test failed");

        let EthereumHeaderUpdate {
            header,
            seen_by,
            voting_power,
            seen,
        } = make_ethereum_header_data(header, &storage)
            .0
            .expect("Test failed");
        assert_eq!(header, default_eth_header());
        assert_eq!(seen_by, HashSet::from([validator_address()]));
        assert_eq!(
            Fraction::new(voting_power.0, voting_power.1),
            Fraction::new(1u64, 3u64)
        );
        assert!(!seen);
    }
}