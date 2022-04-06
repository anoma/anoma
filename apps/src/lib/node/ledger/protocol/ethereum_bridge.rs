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
