use std::collections::HashMap;

type Address = String;
type Validator = Address;
type Tokens = u64;
type BondId = u64;

/// Epoch is an index starting from 0.
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Epoch(u64);

struct PosStorage {
    epochs: HashMap<Epoch, EpochState>,
    delta: EpochDelta,
}

fn init_genesis(params:&PosParams, validators: Vec<Validator,Tokens>) -> PosStorage {
    let mut storage = PosStorage {
        epochs: HashMap::default(),
        delta: EpochDelta::default(),
    };
    let mut state = EpochState {
        bonds: HashMap::default(), 
        validators: validators.map(|(validator, tokens)| (validator, ValidatorState::Active, tokens)),
    };
    for (validator, tokens) in validators {
let bond = Bond {
    delegator: validator.clone(),
    validators: ,
    amount: (),
};
    }
    for epoch_ix in 0..params.pipeline_len {
        let epoch = Epoch(epoch_ix);

    }

}

struct EpochState {
    bonds: HashMap<BondId, Bond>,
    validators: Vec<(Validator, ValidatorState, Tokens)>,
}

struct EpochDelta {
    bond_changes: Vec<(BondId, Tokens)>,
    validators_changes: Vec<(Validator, ValidatorState)>,
}

enum ValidatorState {
    Inactive,
    Pending,
    Active,
    // TODO consider adding `Jailed`
}

#[derive(Debug)]
struct ValidatorBond {
    validator: Address,
    amount: Tokens,
}

#[derive(Debug)]
struct DelegatorBond {
    validator: Address,
    amount: Shares,
}

struct PosParams {
    /// A number of blocks per [`Epoch`].
    /// A first epoch is expected to start at the beginning (in Tendermint ABCI
    /// `BeginBlock`) of block height 0 and end at the end of block
    /// `epoch_duration - 1` (in Tendermint ABCI `EndBlock`).
    /// Then the following epoch starts at the beginning of block height
    /// `epoch_duration`, etc.
    /// Invariant: Within a block, an epoch is always the same at the beginning
    /// and the end.
    epoch_duration: u64,
    /// A maximum number of [`ValidatorState::Active`] validators
    max_validator_slots: u64,
    /// Any change applied during an epoch `n` will become active at the
    /// beginning of epoch `n + pipeline_len`.
    pipeline_len: u64,
    /// How many epochs after a committed fault a validator can be slashed.
    /// If a fault is detected in epoch `n`, it can slashed up until the end of
    /// `n + slashable_period_len` epoch.
    slashable_period_len: u64,
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

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
