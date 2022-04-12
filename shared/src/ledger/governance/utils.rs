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

    let start_epoch_bytes = storage
        .read(&start_epoch_key)
        .map(|(bytes, _gas)| bytes)
        .map_err(|_e| Error::InvalidProposal(proposal_id))?;

    let start_epoch = if let Some(bytes) = start_epoch_bytes {
        Epoch::try_from_slice(&bytes[..])
            .map_err(|_| Error::InvalidProposal(proposal_id))
    } else {
        Err(Error::InvalidProposal(proposal_id))
    };

    match start_epoch {
        Ok(start_epoch) => {
            let mut bond_data: HashMap<Address, (Address, token::Amount)> =
                HashMap::new();
            let (validator_voters, delegator_voters, validator_set) =
                get_votes(storage, proposal_id, start_epoch)?;
            for validator_addr in validator_voters.keys() {
                match get_bond_amount_at(
                    storage,
                    validator_addr,
                    validator_addr,
                    start_epoch,
                ) {
                    Some(amount) => {
                        bond_data.insert(
                            validator_addr.clone(),
                            (validator_addr.clone(), amount),
                        );
                    }
                    None => continue,
                };
                for delegator_addr in delegator_voters.keys() {
                    match get_bond_amount_at(
                        storage,
                        delegator_addr,
                        validator_addr,
                        start_epoch,
                    ) {
                        Some(amount) => {
                            bond_data.insert(
                                delegator_addr.clone(),
                                (validator_addr.clone(), amount),
                            );
                        }
                        None => continue,
                    };
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
                // here we check delegator votes in the case the corrisponding
                // validator didn't vote
                if !bond_data.contains_key(&addr) {
                    if vote.is_yay() {
                        for validator_addr in &validator_set {
                            if bond_data.contains_key(validator_addr) {
                                continue;
                            }
                            match get_bond_amount_at(
                                storage,
                                &addr,
                                validator_addr,
                                start_epoch,
                            ) {
                                Some(amount) => yay_total_tokens += amount,
                                None => continue,
                            };
                        }
                    }
                } else {
                    let (validator_addr, amount) =
                        bond_data.get(&addr).unwrap();
                    let validator_vote =
                        validator_voters.get(validator_addr).unwrap();
                    if validator_vote.is_yay() && validator_vote.ne(&vote) {
                        yay_total_tokens -= *amount;
                    } else if !validator_vote.is_yay()
                        && validator_vote.ne(&vote)
                    {
                        yay_total_tokens += *amount;
                    }
                }
            }

            if 3 * yay_total_tokens >= 2 * total_tokens {
                let proposal_code =
                    gov_storage::get_proposal_code_key(proposal_id);
                let (proposal_code_bytes, _) = storage
                    .read(&proposal_code)
                    .expect("Should be able to read from storage.");

                Ok((TallyResult::Passed, proposal_code_bytes))
            } else {
                Ok((TallyResult::Rejected, None))
            }
        }
        Err(e) => Err(e),
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
                        for (start_epoch, &(mut delta)) in
                            bond.deltas.iter().sorted()
                        {
                            let start_epoch = Epoch::from(*start_epoch);
                            delta = apply_slashes(&slashes, delta, start_epoch);
                            if epoch <= start_epoch {
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

#[allow(clippy::type_complexity)]
fn get_votes<D, H>(
    storage: &Storage<D, H>,
    proposal_id: u64,
    epoch: Epoch,
) -> Result<(
    HashMap<Address, ProposalVote>,
    HashMap<Address, ProposalVote>,
    Vec<Address>,
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
                    let mut active_validators = validator_set
                        .active
                        .iter()
                        .map(|validator| validator.address.clone());
                    for (key, value_bytes, _) in votes {
                        let vote =
                            ProposalVote::try_from_slice(&value_bytes[..]).ok();
                        let key = Key::from_str(key.as_str());
                        match (key, vote) {
                            (Ok(key), Some(vote)) => {
                                let voter_addr = gov_storage::get_address(&key);
                                if let Some(address) = voter_addr {
                                    if active_validators.contains(&address) {
                                        validator_voters.insert(address, vote);
                                    } else {
                                        delegator_voters.insert(address, vote);
                                    }
                                }
                            }
                            _ => continue,
                        }
                    }
                    Ok((
                        validator_voters,
                        delegator_voters,
                        active_validators.collect(),
                    ))
                } else {
                    Err(Error::InvalidValidatorSet)
                }
            } else {
                Err(Error::InvalidValidatorSet)
            }
        }
        None => Err(Error::InvalidValidatorSet),
    }
}
