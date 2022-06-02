use std::collections::HashMap;
use std::str::FromStr;

use anoma_proof_of_stake::types::{Slash, Slashes};
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

/// Proposal structure holding votes information necessary to compute the
/// outcome
pub struct Votes {
    /// Map from validators who votes yay to their total stake amount
    pub yay_validators: HashMap<Address, token::Amount>,
    /// Map from delegation who votes yay to their bond amount
    pub yay_delegators: HashMap<Address, token::Amount>,
    /// Map from delegation who votes nay to their bond amount
    pub nay_delegators: HashMap<Address, token::Amount>,
}

/// Proposal errors
#[derive(Error, Debug)]
pub enum Error {
    /// Invalid validator set deserialization
    #[error("Invalid validator set")]
    InvalidValidatorSet,
    /// Invalid proposal field deserialization
    #[error("Invalid proposal {0}")]
    InvalidProposal(u64),
}

/// Proposal event definition
pub struct ProposalEvent {
    /// Proposal event type
    pub event_type: String,
    /// Proposal event attributes
    pub attributes: HashMap<String, String>,
}

impl ProposalEvent {
    /// Create a proposal event
    pub fn new(
        event_type: String,
        tally: TallyResult,
        id: u64,
        has_proposal_code: bool,
        proposal_code_exit_status: bool,
    ) -> Self {
        let attributes = HashMap::from([
            ("tally_result".to_string(), tally.to_string()),
            ("proposal_id".to_string(), id.to_string()),
            (
                "has_proposal_code".to_string(),
                (!has_proposal_code as u64).to_string(),
            ),
            (
                "proposal_code_exit_status".to_string(),
                (!proposal_code_exit_status as u64).to_string(),
            ),
        ]);
        Self {
            event_type,
            attributes,
        }
    }
}

/// Return a proposal result and his associated proposal code (if any)
pub fn compute_tally<D, H>(
    storage: &Storage<D, H>,
    epoch: Epoch,
    votes: Votes,
) -> TallyResult
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    let validators = get_all_validators(storage, epoch);
    let total_stacked_tokens =
        get_total_stacked_tokens(storage, epoch, &validators);

    let Votes {
        yay_validators,
        yay_delegators,
        nay_delegators,
    } = votes;

    let mut total_yay_stacked_tokens = token::Amount::from(0);
    for (_, amount) in yay_validators.clone().into_iter() {
        total_yay_stacked_tokens += amount;
    }

    // YAY: Add delegator amount whose validator didn't vote / voted nay
    for (validator_address, amount) in yay_delegators.into_iter() {
        if !yay_validators.contains_key(&validator_address) {
            total_yay_stacked_tokens += amount;
        }
    }

    // NAY: Remove delegator amount whose validator validator vote yay
    for (validator_address, amount) in nay_delegators.into_iter() {
        if yay_validators.contains_key(&validator_address) {
            total_yay_stacked_tokens -= amount;
        }
    }

    if 3 * total_yay_stacked_tokens >= 2 * total_stacked_tokens {
        TallyResult::Passed
    } else {
        TallyResult::Rejected
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
    let slashes_key = pos::validator_slashes_key(validator);
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
    match epoched_bonds_bytes {
        Some(epoched_bonds_bytes) => {
            let epoched_bonds =
                Bonds::try_from_slice(&epoched_bonds_bytes[..]).ok();
            let slashes = if let Some(slashes_bytes) = slashes_bytes {
                Slashes::try_from_slice(&slashes_bytes[..]).ok()
            } else {
                Some(Slashes::default())
            };
            match (epoched_bonds, slashes) {
                (Some(epoched_bonds), Some(slashes)) => {
                    let mut delegated_amount: token::Amount = 0.into();
                    for bond in epoched_bonds.iter() {
                        let mut to_deduct = bond.neg_deltas;
                        for (start_epoch, &(mut delta)) in
                            bond.pos_deltas.iter().sorted()
                        {
                            // deduct bond's neg_deltas
                            if to_deduct > delta {
                                to_deduct -= delta;
                                // If the whole bond was deducted, continue to
                                // the next one
                                continue;
                            } else {
                                delta -= to_deduct;
                                to_deduct = token::Amount::default();
                            }

                            let start_epoch = Epoch::from(*start_epoch);
                            delta = apply_slashes(&slashes, delta, start_epoch);
                            if epoch >= start_epoch {
                                delegated_amount += delta;
                            }
                        }
                    }
                    Some(delegated_amount)
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn apply_slashes(
    slashes: &[Slash],
    mut delta: token::Amount,
    epoch_start: Epoch,
) -> token::Amount {
    for slash in slashes {
        if Epoch::from(slash.epoch) >= epoch_start {
            let raw_delta: u64 = delta.into();
            let current_slashed = token::Amount::from(slash.rate * raw_delta);
            delta -= current_slashed;
        }
    }
    delta
}

/// Prepare Votes structure to compute proposal tally
pub fn get_proposal_votes<D, H>(
    storage: &Storage<D, H>,
    epoch: Epoch,
    proposal_id: u64,
) -> Votes
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    let validators = get_all_validators(storage, epoch);

    let vote_prefix_key =
        gov_storage::get_proposal_vote_prefix_key(proposal_id);
    let (vote_iter, _) = storage.iter_prefix(&vote_prefix_key);

    let mut yay_validators: HashMap<Address, token::Amount> = HashMap::new();
    let mut yay_delegators: HashMap<Address, token::Amount> = HashMap::new();
    let mut nay_delegators: HashMap<Address, token::Amount> = HashMap::new();

    for (key, vote_bytes, _) in vote_iter {
        let vote_key = Key::from_str(key.as_str()).ok();
        let vote = ProposalVote::try_from_slice(&vote_bytes[..]).ok();
        match (vote_key, vote) {
            (Some(key), Some(vote)) => {
                let voter_address = gov_storage::get_voter_address(&key);
                match voter_address {
                    Some(address) => {
                        if vote.is_yay() && validators.contains(address) {
                            let amount =
                                get_validator_stake(storage, epoch, address);
                            yay_validators.insert(address.clone(), amount);
                        } else if !validators.contains(address) {
                            let validator_address =
                                gov_storage::get_vote_delegation_address(&key);
                            match validator_address {
                                Some(validator_address) => {
                                    let amount = get_bond_amount_at(
                                        storage,
                                        address,
                                        validator_address,
                                        epoch,
                                    );
                                    if let Some(amount) = amount {
                                        if vote.is_yay() {
                                            yay_delegators.insert(
                                                address.clone(),
                                                amount,
                                            );
                                        } else {
                                            nay_delegators.insert(
                                                address.clone(),
                                                amount,
                                            );
                                        }
                                    }
                                }
                                None => continue,
                            }
                        }
                    }
                    None => continue,
                }
            }
            _ => continue,
        }
    }

    Votes {
        yay_validators,
        yay_delegators,
        nay_delegators,
    }
}

fn get_all_validators<D, H>(
    storage: &Storage<D, H>,
    epoch: Epoch,
) -> Vec<Address>
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    let validator_set_key = pos::validator_set_key();
    let (validator_set_bytes, _) = storage
        .read(&validator_set_key)
        .expect("Validator set should be defined.");
    if let Some(validator_set_bytes) = validator_set_bytes {
        let epoched_validator_set =
            ValidatorSets::try_from_slice(&validator_set_bytes[..]).ok();
        if let Some(epoched_validator_set) = epoched_validator_set {
            let validator_set = epoched_validator_set.get(epoch);
            if let Some(validator_set) = validator_set {
                let all_validators =
                    validator_set.active.union(&validator_set.inactive);
                return all_validators
                    .into_iter()
                    .map(|validator| validator.address.clone())
                    .collect::<Vec<Address>>();
            }
        }
        Vec::new()
    } else {
        Vec::new()
    }
}

fn get_total_stacked_tokens<D, H>(
    storage: &Storage<D, H>,
    epoch: Epoch,
    validators: &[Address],
) -> token::Amount
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    return validators
        .iter()
        .fold(token::Amount::from(0), |acc, validator| {
            acc + get_validator_stake(storage, epoch, validator)
        });
}

fn get_validator_stake<D, H>(
    storage: &Storage<D, H>,
    epoch: Epoch,
    validator: &Address,
) -> token::Amount
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    let total_delta_key = pos::validator_total_deltas_key(validator);
    let (total_delta_bytes, _) = storage
        .read(&total_delta_key)
        .expect("Validator delta should be defined.");
    if let Some(total_delta_bytes) = total_delta_bytes {
        let total_delta =
            ValidatorTotalDeltas::try_from_slice(&total_delta_bytes[..]).ok();
        if let Some(total_delta) = total_delta {
            let epoched_total_delta = total_delta.get(epoch);
            if let Some(epoched_total_delta) = epoched_total_delta {
                return token::Amount::from_change(epoched_total_delta);
            }
        }
    }
    token::Amount::from(0)
}
