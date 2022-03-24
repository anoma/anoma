use std::collections::HashMap;
use std::str::FromStr;

use anoma_proof_of_stake::types::Slashes;
use borsh::BorshDeserialize;
use itertools::Itertools;
use thiserror::Error;

use crate::ledger::governance::storage as gov_storage;
use crate::ledger::pos;
use crate::ledger::pos::{BondId, Bonds, ValidatorSets, ValidatorTotalDeltas};
use crate::ledger::storage::{DBIter, Storage, StorageHasher, DB};
use crate::types::address::Address;
use crate::types::governance::{ProposalVote, TallyResult};
use crate::types::storage::{Epoch, Key};
use crate::types::token;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Invalid validator set")]
    InvalidValidatorSet,
    #[error("Invalid proposal {0}")]
    InvalidProposal(u64),
}

pub struct ProposalEvent {
    pub event_type: String,
    pub attributes: HashMap<String, String>
}

impl ProposalEvent {
    pub fn new(event_type: String, tally: TallyResult, id: u64, has_proposal_code: bool, proposal_code_exit_status: bool) -> Self {
        let attributes = HashMap::from([
            ("tally_result".to_string(), tally.to_string()), 
            ("proposal_id".to_string(), id.to_string()),
            ("has_proposal_code".to_string(), (!has_proposal_code as u64).to_string()),
            ("proposal_code_exit_status".to_string(), (!proposal_code_exit_status as u64).to_string())
        ]);
        Self {
            event_type,
            attributes
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

/// Return a proposal result and his associated proposal code (if any)
pub fn compute_tally<D, H>(
    storage: &Storage<D, H>,
    proposal_id: u64,
) -> Result<(TallyResult, Option<Vec<u8>>)>
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    let start_epoch_key = gov_storage::get_voting_start_epoch_key(proposal_id);
    let (start_epoch_bytes, _) = storage
        .read(&start_epoch_key)
        .expect("Key should be defined");
    let start_epoch = if let Some(start_epoch_bytes) = start_epoch_bytes {
        Epoch::try_from_slice(&start_epoch_bytes[..])
            .map_err(|_| Error::InvalidProposal(proposal_id))
    } else {
        Err(Error::InvalidProposal(proposal_id))
    };

    match start_epoch {
        Ok(start_epoch) => {
            let mut bond_data: HashMap<Address, (Address, token::Amount)> =
                HashMap::new();
            let (validator_voters, delegator_voters) =
                get_votes(storage, proposal_id, start_epoch)?;
            for validator_addr in validator_voters.keys() {
                let bond_amount = get_bond_amount_at(
                    storage,
                    validator_addr,
                    validator_addr,
                    start_epoch,
                )
                .expect("Bond should be definied.");
                bond_data.insert(
                    validator_addr.clone(),
                    (validator_addr.clone(), bond_amount),
                );
                for delegator_addr in delegator_voters.keys() {
                    let bond_amount = get_bond_amount_at(
                        storage,
                        delegator_addr,
                        validator_addr,
                        start_epoch,
                    )
                    .expect("Bond should be definied.");
                    bond_data.insert(
                        delegator_addr.clone(),
                        (validator_addr.clone(), bond_amount),
                    );
                }
            }

            let mut total_tokens = token::Amount::from(0);
            let mut yay_total_tokens = token::Amount::whole(0);
            for (addr, vote) in validator_voters.clone() {
                if vote.is_yay() {
                    yay_total_tokens += bond_data.get(&addr).unwrap().1;
                }
                let validator_total_deltas_key =
                    pos::validator_total_deltas_key(&addr);
                let (validator_total_deltas_bytes, _) = storage
                    .read(&validator_total_deltas_key)
                    .expect("Key should exist");
                let epoched_validator_total_deltas =
                    ValidatorTotalDeltas::try_from_slice(
                        &validator_total_deltas_bytes.unwrap()[..],
                    )
                    .expect("ValidatorTotalDeltas should be definied");
                let amount = epoched_validator_total_deltas
                    .get(start_epoch)
                    .unwrap_or_default();

                total_tokens += token::Amount::from_change(amount);
            }

            for (addr, vote) in delegator_voters {
                if !bond_data.contains_key(&addr) {
                    if vote.is_yay() {
                        yay_total_tokens += bond_data.get(&addr).unwrap().1;
                    }
                } else {
                    let delegator_data = bond_data.get(&addr).unwrap();
                    let validator_vote =
                        validator_voters.get(&delegator_data.0).unwrap();
                    if validator_vote.is_yay() && validator_vote.ne(&vote) {
                        yay_total_tokens -= delegator_data.1;
                    } else {
                        yay_total_tokens += delegator_data.1;
                    }
                }
            }

            if yay_total_tokens / total_tokens >= 0.66 {
                let proposal_code = gov_storage::get_proposal_code_key(proposal_id);
                let (proposal_code_bytes, _) = storage
                    .read(&proposal_code)
                    .expect("Key should be defined");

                return Ok((TallyResult::Passed, proposal_code_bytes))
            } else {
                return Ok((TallyResult::Rejected, None))
            }
        }
        Err(e) => return Err(e),
    }
}

// Get bond token amount
fn get_bond_amount_at<D, H>(
    storage: &Storage<D, H>,
    delegator: &Address,
    validator: &Address,
    epoch: Epoch,
) -> Option<token::Amount>
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    let slashes_key = pos::validator_slashes_key(&validator);
    let bond_key = pos::bond_key(&BondId {
        source: delegator.clone(),
        validator: validator.clone(),
    });

    let (slashes_bytes, _) = storage
        .read(&slashes_key)
        .expect("Should be able to read key.");
    let (epoched_bonds_bytes, _) = storage
        .read(&bond_key)
        .expect("Should be able to read key.");
    match epoched_bonds_bytes.clone() {
        Some(epoched_bonds_bytes) => {
            let epoched_bonds =
                Bonds::try_from_slice(&epoched_bonds_bytes[..]).ok();
            let slashes = if let Some(slashes_bytes) = slashes_bytes {
                Slashes::try_from_slice(&slashes_bytes[..]).ok()
            } else {
                Some(Slashes::default())
            };
            match (epoched_bonds, slashes) {
                (Some(epoched_bonds), Some(_slashes)) => {
                    let mut delegated_amount: token::Amount = 0.into();
                    for bond in epoched_bonds.iter() {
                        for (start_epoch, amount) in
                            bond.deltas.iter().sorted_unstable()
                        {
                            let epoch_start: Epoch = (*start_epoch).into();
                            if epoch >= epoch_start {
                                delegated_amount += amount.clone();
                            }
                        }
                    }
                    Some(delegated_amount)
                }
                _ => {
                    println!("bond: inner fail");
                    return None
                }
            }
        }
        _ => {
            println!("bond: outer fail {:?}, {:?}", epoched_bonds_bytes, slashes_bytes);
            None
        }
    }
}

fn get_votes<D, H>(
    storage: &Storage<D, H>,
    proposal_id: u64,
    epoch: Epoch,
) -> Result<(
    HashMap<Address, ProposalVote>,
    HashMap<Address, ProposalVote>,
)>
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    let validator_set_key = pos::validator_set_key();
    let vote_prefix_key = gov_storage::get_proposal_prefix_key(proposal_id);
    let (validator_set_bytes, _) = storage
        .read(&validator_set_key)
        .expect("Validator set should be defined.");
    let (votes, _) = storage.iter_prefix(&vote_prefix_key);

    let (mut validator_voters, mut delegator_voters) =
        (HashMap::new(), HashMap::new());

    match validator_set_bytes {
        Some(bytes) => {
            let epoched_validator_set =
                ValidatorSets::try_from_slice(&bytes[..]).ok();
            if let Some(epoched_validator_set) = epoched_validator_set {
                let validator_set = epoched_validator_set.get(epoch);
                if let Some(validator_set) = validator_set {
                    let active_validators = validator_set
                        .active
                        .iter()
                        .map(|validator| validator.address.clone())
                        .collect::<Vec<Address>>();
                    for (key, value_bytes, _) in votes {
                        let vote =
                            ProposalVote::try_from_slice(&value_bytes[..]).ok();
                        if let Some(vote) = vote {
                            let key = Key::from_str(key.as_str())
                                .expect("Key shoould be parsable");
                            let voter_addr = gov_storage::get_address(&key)
                                .expect(
                                    "Should be able to get address from key",
                                );
                            if active_validators.contains(&voter_addr) {
                                validator_voters.insert(voter_addr, vote);
                            } else {
                                delegator_voters.insert(voter_addr, vote);
                            }
                        } else {
                            continue;
                        }
                    }
                    return Ok((validator_voters, delegator_voters));
                } else {
                    return Err(Error::InvalidValidatorSet);
                }
            } else {
                return Err(Error::InvalidValidatorSet);
            }
        }
        None => return Err(Error::InvalidValidatorSet),
    }
}
