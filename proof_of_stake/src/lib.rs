use std::collections::{BTreeSet, HashMap};
use std::convert::TryInto;

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

struct EpochedPipelined<Data> {
    /// The epoch in which this data was last updated
    last_update: Epoch,
    /// Fixed-size array in which the head is the data for epoch in which the
    /// `last_update` was performed and every consecutive array element is the
    /// successor epoch of the predecessor array element.
    /// For system parameters, validator's consensus key and state,
    /// `LENGTH = pipeline_length`. For all others, `LENGTH =
    /// unbonding_length`.
    // TODO parameterize the length
    data: [Option<Data>; 3],
}

struct EpochedUnbonding<Data> {
    /// The epoch in which this data was last updated
    last_update: Epoch,
    /// Fixed-size array in which the head is the data for epoch in which the
    /// `last_update` was performed and every consecutive array element is the
    /// successor epoch of the predecessor array element.
    /// For system parameters, validator's consensus key and state,
    /// `LENGTH = pipeline_length`. For all others, `LENGTH =
    /// unbonding_length`.
    // TODO parameterize the length
    data: [Option<Data>; 7],
}

impl<Data> EpochedPipelined<Data> {
    pub fn init(value: Data, epoch: Epoch) -> Self {
        let mut data = [None; 3];
        data[0] = Some(value);
        Self {
            last_update: epoch,
            data,
        }
    }
}

impl<Data> EpochedUnbonding<Data> {
    pub fn init(value: Data, epoch: Epoch) -> Self {
        let mut data = [None; 7];
        data[0] = Some(value);
        Self {
            last_update: epoch,
            data,
        }
    }
}

struct InitialPosStorage {
    validators: Vec<ValidatorData>,
    bonds: Bonds,
    validator_sets: ValidatorSets,
    total_voting_power: TotalVotingPower,
}

struct ValidatorData {
    pub consensus_key: EpochedPipelined<PublicKey>,
    pub state: EpochedPipelined<ValidatorState>,
    pub total_deltas: EpochedUnbonding<TokenChange>,
    pub voting_power: EpochedUnbonding<VotingPower>,
}

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

type ValidatorSets = EpochedUnbonding<ValidatorSet>;

/// The sum of all active and inactive validators' voting power
type TotalVotingPower = EpochedUnbonding<VotingPower>;

fn init_genesis(
    params: &PosParams,
    validators: Vec<(Validator, TokenAmount, PublicKey)>,
    epoch: Epoch,
) -> InitialPosStorage {
    let validators = validators
        .iter()
        .map(|(addr, tokens, pk)| ValidatorData {
            consensus_key: EpochedPipelined::init(*pk, epoch),
            state: EpochedPipelined::init(ValidatorState::Candidate, epoch),
            total_deltas: EpochedUnbonding::init(*tokens as i128, epoch),
            voting_power: EpochedUnbonding::init(
                tokens * 10_000 / params.votes_per_token,
                epoch,
            ),
        })
        .collect();

    let mut bonds = HashMap::default;
    for (addr, tokens, pk) in validators.iter() {
        let bond_id = BondId {
            source: addr.clone(),
            validator: addr.clone(),
        };
        let mut delta = HashMap::default();
        delta.insert(epoch, tokens);
        let bond = EpochedPipelined::init(Bond { delta }, epoch);
        bonds.insert(bond_id, bond);
    }

    let mut active_validators: BTreeSet<WeightedValidator> = validators
        .iter()
        .map(|(address, tokens, pk)| WeightedValidator {
            voting_power: tokens * 10_000 / params.votes_per_token,
            address,
        })
        .collect();
    let mut inactive_validators: BTreeSet<WeightedValidator> =
        BTreeSet::default();
    while active_validators.len() > params.max_validator_slots as usize {
        match active_validators.pop_first_shim() {
            Some(first) => {
                inactive_validators.insert(first);
            }
            None => break,
        }
    }
    let mut validator_set = ValidatorSet {
        active: active_validators,
        inactive: inactive_validators,
    };
    let validator_sets = EpochedUnbonding::init(validator_set, epoch);

    let total_voting_power = todo!();
    let mut storage = InitialPosStorage {
        validators: (),
        bonds: (),
        validator_sets: (),
        total_voting_power: (),
    };
    let mut state = EpochState {
        bonds: HashMap::default(),
        validators: validators.map(|(validator, tokens)| {
            (validator, ValidatorState::Active, tokens)
        }),
    };
    for (validator, tokens) in validators {
        let bond = Bond {
            delegator: validator.clone(),
            validators: (),
            amount: (),
        };
    }
    for epoch_ix in 0..params.pipeline_len {
        let epoch = Epoch(epoch_ix);
    }
}

trait BTreeSetShims<T> {
    fn first_shim(&self) -> Option<&T>;
    fn last_shim(&self) -> Option<&T>;
    fn pop_first_shim(&mut self) -> Option<T>;
    fn pop_last_shim(&mut self) -> Option<T>;
}

impl<T: Ord> BTreeSetShims<T> for BTreeSet<T> {
    fn pop_last_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let last = iter.last().map(|last| *last);
        if let Some(last) = last {
            self.remove(&last);
        }
        last
    }

    fn first_shim(&self) -> Option<&T> {
        let iter = self.iter();
        iter.next()
    }

    fn last_shim(&self) -> Option<&T> {
        let iter = self.iter();
        iter.last()
    }

    fn pop_first_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let first = iter.last().map(|first| *first);
        if let Some(first) = first {
            self.remove(&first);
        }
        first
    }
}

struct EpochState {
    bonds: HashMap<BondId, Bond>,
    validators: Vec<(Validator, ValidatorState, Tokens)>,
}

struct EpochDelta {
    bond_changes: Vec<(BondId, Tokens)>,
    validators_changes: Vec<(Validator, ValidatorState, ShareChange)>,
}

enum ValidatorState {
    Inactive,
    Pending,
    Candidate,
    // TODO consider adding `Jailed`
}

type Bonds = HashMap<BondId, EpochedPipelined<Bond>>;
type Unbonds = HashMap<BondId, EpochedUnbonding<Unbond>>;

#[derive(Debug)]
struct Bond {
    /// A key is a the epoch set for the bond. This is used in unbonding, where
    // it's needed for slash epoch range check.
    delta: HashMap<Epoch, TokenAmount>,
}

struct Unbond {
    /// A key is a pair of the epoch of the bond from which a unbond was
    /// created the epoch of unboding. This is needed for slash epoch range
    /// check.
    deltas: HashMap<(Epoch, Epoch), TokenAmount>,
}

struct PosParams {
    /// A maximum number of [`ValidatorState::Active`] validators
    max_validator_slots: u64,
    /// Any change applied during an epoch `n` will become active at the
    /// beginning of epoch `n + pipeline_len`.
    pipeline_len: u64,
    /// How many epochs after a committed fault a validator can be slashed.
    /// If a fault is detected in epoch `n`, it can slashed up until the end of
    /// `n + slashable_period_len` epoch.
    unboding_len: u64,
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
            epoch_duration: 1024,
            max_validator_slots: 128,
            pipeline_len: 2,
            // TODO figure out the value, after we spec out the specifics of how
            // slashing works
            slashable_period_len: 4,
        }
    }
}

impl Bond {
    fn current_validator(&self) -> Address {
        return self
            .validators
            .last()
            .expect("Error retrieving current validator.")
            .1
            .expect("Bond is in the process of unbonding.");
    }

    fn add_to_bond(self, tokens: Tokens) -> Bond {
        let updated_amount = self.amount + tokens;
        let updated_bond = Bond {
            amount: updated_amount,
            ..self
        };
        return updated_bond;
        // add ledger interaction
    }

    fn delegate(
        current_epoch: Epoch,
        delegator_address: Address,
        validator_address: Address,
        tokens: Tokens,
    ) -> () {
        // create transaction to lock tokens [still needed]
        // submit it for processing [ledger interaction]

        let delegation = Bond {
            delegator: delegator_address,
            validators: vec![(current_epoch, Some(validator_address))],
            amount: tokens,
        };

        // increment validator voting power
        let voting_change: Result<u64, i64> = tokens.try_into();

        // TO FIX: ValidatorState enum should be read from the EpochState, for
        // now set to Active
        let delta: EpochDelta =
            vec![(validator_address, ValidatorState::Active, voting_change)];
    }

    fn redelegate(current_epoch: Epoch, bond_id: BondId) -> () {}

    // fn undelegate
    // fn complete_undelegate
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
