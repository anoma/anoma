pub mod epoched;

use std::collections::{BTreeSet, HashMap};

use epoched::{Epoched, EpochedDelta, OffsetPipelineLen, OffsetUnboundingLen};

type Address = String;
type Validator = Address;
type TokenAmount = u64;
type TokenChange = i128;
struct BondId {
    source: Address,
    validator: Validator,
}
type Epoch = u64;
// TODO parameterize
type PublicKey = String;
type VotingPower = u64;

// struct InitialPosStorage {
//     validators: Vec<ValidatorData>,
//     bonds: Bonds,
//     validator_sets: ValidatorSets,
//     total_voting_power: TotalVotingPower,
// }

// struct ValidatorData {
//     pub consensus_key: Epoched<PublicKey>,
//     pub state: Epoched<ValidatorState>,
//     pub total_deltas: Epoched<TokenChange>,
//     pub voting_power: Epoched<VotingPower>,
// }

/// Validator's address with its voting power.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct WeightedValidator {
    /// The `voting_power` field must be on top, because lexicographic ordering
    /// is based on the top-to-bottom declaration order and in the
    /// `ValidatorSet` the `WeighedValidator`s these need to be sorted by
    /// the `voting_power`.
    voting_power: VotingPower,
    address: Address,
}

struct ValidatorSet {
    /// Active validator set with maximum size equal to `max_active_validators`
    active: BTreeSet<WeightedValidator>,
    /// All the other validators that are not active
    inactive: BTreeSet<WeightedValidator>,
}

type ValidatorSets = Epoched<ValidatorSet, OffsetUnboundingLen>;

/// The sum of all active and inactive validators' voting power
type TotalVotingPower = Epoched<VotingPower, OffsetUnboundingLen>;

trait Pos {
    // TODO consider paraterizing types like so:
    // type Address;

    fn read_validator_consensus_key(
        &self,
        validator: Address,
    ) -> Option<Epoched<PublicKey, OffsetPipelineLen>>;

    fn write_validator_consensus_key(
        &mut self,
        validator: Address,
        key: Epoched<PublicKey, OffsetPipelineLen>,
    );
    fn write_validator_state(
        &mut self,
        validator: Address,
        key: Epoched<ValidatorState, OffsetPipelineLen>,
    );
    fn write_validator_total_deltas(
        &mut self,
        validator: Address,
        key: Epoched<i128, OffsetUnboundingLen>,
    );
    fn write_validator_voting_power(
        &mut self,
        validator: Address,
        key: Epoched<u64, OffsetUnboundingLen>,
    );
}

fn init_genesis(
    pos: &mut impl Pos,
    params: &PosParams,
    validators: Vec<(Validator, TokenAmount, PublicKey)>,
    epoch: Epoch,
) {
    todo!()
    // for (adDr, tokens, pk) in validators {
    //     let consensus_key = Epoched::init_at_genesis(pk, epoch);
    //     pos.write_validator_consensus_key(addr, consensus_key);
    //     let state = Epoched::init_at_genesis(ValidatorState::Candidate,
    // epoch);     pos.write_validator_state(addr, state);
    //     let total_deltas = Epoched::init_at_genesis(tokens as i128, epoch);
    //     pos.write_validator_total_deltas(addr, total_deltas);
    //     let voting_power = Epoched::init_at_genesis(
    //         tokens * 10_000 / params.votes_per_token,
    //         epoch,
    //     );
    //     pos.write_validator_voting_power(addr, voting_power);
    // }

    // let mut bonds = HashMap::default;
    // for (addr, tokens, pk) in validators.iter() {
    //     let bond_id = BondId {
    //         source: addr.clone(),
    //         validator: addr.clone(),
    //     };
    //     let mut delta = HashMap::default();
    //     delta.insert(epoch, tokens);
    //     let bond = Epoched::init_at_genesis(Bond { delta }, epoch);
    //     bonds.insert(bond_id, bond);
    // }

    // let mut active_validators: BTreeSet<WeightedValidator> = validators
    //     .iter()
    //     .map(|(address, tokens, pk)| WeightedValidator {
    //         voting_power: tokens * 10_000 / params.votes_per_token,
    //         address,
    //     })
    //     .collect();
    // let mut inactive_validators: BTreeSet<WeightedValidator> =
    //     BTreeSet::default();
    // while active_validators.len() > params.max_validator_slots as usize {
    //     match active_validators.pop_first_shim() {
    //         Some(first) => {
    //             inactive_validators.insert(first);
    //         }
    //         None => break,
    //     }
    // }
    // let mut validator_set = ValidatorSet {
    //     active: active_validators,
    //     inactive: inactive_validators,
    // };
    // let validator_sets = Epoched::init_at_genesis(validator_set, epoch);

    // let total_voting_power = todo!();
    // let mut storage = InitialPosStorage {
    //     validators: (),
    //     bonds: (),
    //     validator_sets: (),
    //     total_voting_power: (),
    // };
    // let mut state = EpochState {
    //     bonds: HashMap::default(),
    //     validators: validators.map(|(validator, tokens)| {
    //         (validator, ValidatorState::Active, tokens)
    //     }),
    // };
    // for (validator, tokens) in validators {
    //     let bond = Bond {
    //         delegator: validator.clone(),
    //         validators: (),
    //         amount: (),
    //     };
    // }
    // for epoch_ix in 0..params.pipeline_len {
    //     let epoch = Epoch(epoch_ix);
    // }
}

/// This trait add shims for BTreeSet methods that not yet stable. They have the
/// same behavior as their nightly counterparts, but additionally require
/// `Clone` bound on the element type (for `pop_first` and `pop_last`).
trait BTreeSetShims<T> {
    fn first_shim(&self) -> Option<&T>;
    fn last_shim(&self) -> Option<&T>;
    fn pop_first_shim(&mut self) -> Option<T>;
    fn pop_last_shim(&mut self) -> Option<T>;
}

impl<T: Ord + Clone> BTreeSetShims<T> for BTreeSet<T> {
    /// Returns a reference to the first value in the set, if any. This value is
    /// always the minimum of all values in the set.
    fn first_shim(&self) -> Option<&T> {
        let mut iter = self.iter();
        iter.next()
    }

    /// Returns a reference to the last value in the set, if any. This value is
    /// always the maximum of all values in the set.
    fn last_shim(&self) -> Option<&T> {
        let iter = self.iter();
        iter.last()
    }

    /// Removes the first value from the set and returns it, if any. The first
    /// value is always the minimum value in the set.
    fn pop_first_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let first = iter.last().cloned();
        if let Some(first) = first {
            return self.take(&first);
        }
        None
    }

    /// Removes the last value from the set and returns it, if any. The last
    /// value is always the maximum value in the set.
    fn pop_last_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let last = iter.last().cloned();
        if let Some(last) = last {
            return self.take(&last);
        }
        None
    }
}

#[derive(Debug, Clone, Copy)]
enum ValidatorState {
    Inactive,
    Pending,
    Candidate,
    // TODO consider adding `Jailed`
}

type Bonds = HashMap<BondId, Epoched<Bond, OffsetPipelineLen>>;
type Unbonds = HashMap<BondId, Epoched<Unbond, OffsetUnboundingLen>>;

#[derive(Debug)]
pub struct Bond {
    /// A key is a the epoch set for the bond. This is used in unbonding, where
    // it's needed for slash epoch range check.
    delta: HashMap<Epoch, TokenAmount>,
}

pub struct Unbond {
    /// A key is a pair of the epoch of the bond from which a unbond was
    /// created the epoch of unboding. This is needed for slash epoch range
    /// check.
    deltas: HashMap<(Epoch, Epoch), TokenAmount>,
}

#[derive(Debug, Clone)]
pub struct PosParams {
    /// A maximum number of [`ValidatorState::Active`] validators
    max_validator_slots: u64,
    /// Any change applied during an epoch `n` will become active at the
    /// beginning of epoch `n + pipeline_len`.
    pipeline_len: u64,
    /// How many epochs after a committed fault a validator can be slashed.
    /// If a fault is detected in epoch `n`, it can slashed up until the end of
    /// `n + slashable_period_len` epoch.
    unbonding_len: u64,
    /// Used in validators' voting power calculation
    votes_per_token: u64,
    /// Amount of tokens rewarded to a validator for proposing a block
    block_proposer_reward: u64,
    /// Amount of tokens rewarded to each validator that voted on a block
    /// proposal
    block_vote_reward: u64,
}

impl Default for PosParams {
    fn default() -> Self {
        Self {
            max_validator_slots: 128,
            pipeline_len: 2,
            unbonding_len: 6,
            votes_per_token: 1000,
            block_proposer_reward: 100,
            block_vote_reward: 1,
        }
    }
}

// TODO
// impl Bond {
//     fn current_validator(&self) -> Address {
//         return self
//             .validators
//             .last()
//             .expect("Error retrieving current validator.")
//             .1
//             .expect("Bond is in the process of unbonding.");
//     }

//     fn add_to_bond(self, tokens: Tokens) -> Bond {
//         let updated_amount = self.amount + tokens;
//         let updated_bond = Bond {
//             amount: updated_amount,
//             ..self
//         };
//         return updated_bond;
//         // add ledger interaction
//     }

//     fn delegate(
//         current_epoch: Epoch,
//         delegator_address: Address,
//         validator_address: Address,
//         tokens: Tokens,
//     ) -> () {
//         // create transaction to lock tokens [still needed]
//         // submit it for processing [ledger interaction]

//         let delegation = Bond {
//             delegator: delegator_address,
//             validators: vec![(current_epoch, Some(validator_address))],
//             amount: tokens,
//         };

//         // increment validator voting power
//         let voting_change: Result<u64, i64> = tokens.try_into();

//         // TO FIX: ValidatorState enum should be read from the EpochState,
// for         // now set to Active
//         let delta: EpochDelta =
//             vec![(validator_address, ValidatorState::Active, voting_change)];
//     }

//     fn redelegate(current_epoch: Epoch, bond_id: BondId) -> () {}

//     // fn undelegate
//     // fn complete_undelegate
// }
