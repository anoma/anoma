mod btree_set;
pub mod epoched;
pub mod parameters;
pub mod types;

use core::fmt::Debug;
use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;
use std::ops::{self, Add, Sub};

use epoched::{Epoched, EpochedDelta, OffsetPipelineLen, OffsetUnboundingLen};
use parameters::PosParams;
use types::{
    Epoch, GenesisValidator, ValidatorSet, ValidatorState, VotingPower,
};

use crate::btree_set::BTreeSetShims;
use crate::types::{Bond, BondId, WeightedValidator};

pub trait Pos {
    type Address: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash;
    type TokenAmount: Debug
        + Clone
        + Copy
        + Add
        + Sub
        + Into<u64>
        + Into<Self::TokenChange>;
    type TokenChange: Debug
        + Clone
        + Copy
        + Add<Output = Self::TokenChange>
        + Sub
        + From<Self::TokenAmount>
        + Into<i128>;
    type PublicKey: Debug + Clone;

    // TODO it may be nicer to instead provide generic functions for storage
    // write/read and a way for implementors to assign storage keys and convert
    // data into/from storage values (e.g. our ledger storage key type and
    // borsh encoding for values)
    fn write_params(&mut self, params: &PosParams);
    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: Self::Address,
    );
    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: Epoched<Self::PublicKey, OffsetPipelineLen>,
    );
    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: Epoched<ValidatorState, OffsetPipelineLen>,
    );
    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: EpochedDelta<Self::TokenChange, OffsetUnboundingLen>,
    );
    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: Epoched<VotingPower, OffsetUnboundingLen>,
    );
    fn write_bond(
        &mut self,
        key: &BondId<Self::Address>,
        value: Epoched<Bond<Self::TokenAmount>, OffsetPipelineLen>,
    );
    fn write_validator_set(
        &mut self,
        value: Epoched<ValidatorSet<Self::Address>, OffsetUnboundingLen>,
    );
    fn write_total_voting_power(
        &mut self,
        value: Epoched<VotingPower, OffsetUnboundingLen>,
    );

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<Epoched<Self::PublicKey, OffsetPipelineLen>>;

    /// Initialize the PoS system storage data in the genesis block for the
    /// given PoS parameters and initial validator set. The validators'
    /// tokens will be put into self-bonds. The given PoS parameters are written
    /// with the [`Pos::write_params`] method.
    fn init_genesis(
        &mut self,
        params: &PosParams,
        validators: impl AsRef<
            [GenesisValidator<
                Self::Address,
                Self::TokenAmount,
                Self::PublicKey,
            >],
        >,
        epoch: Epoch,
    ) {
        self.write_params(params);

        let GenesisData {
            validators,
            validator_set,
            total_voting_power,
        } = init_genesis_data(params, validators.as_ref().iter(), epoch);

        validators.for_each(
            |GenesisValidatorData {
                 ref address,
                 staking_reward_address,
                 consensus_key,
                 state,
                 total_deltas,
                 voting_power,
                 bond: (bond_id, bond),
             }| {
                self.write_validator_staking_reward_address(
                    address,
                    staking_reward_address,
                );
                self.write_validator_consensus_key(address, consensus_key);
                self.write_validator_state(address, state);
                self.write_validator_total_deltas(address, total_deltas);
                self.write_validator_voting_power(address, voting_power);
                self.write_bond(&bond_id, bond);
            },
        );
        self.write_validator_set(validator_set);
        self.write_total_voting_power(total_voting_power);
    }
}

struct GenesisData<Validators, Address, TokenAmount, TokenChange, PK>
where
    Validators: Iterator<
        Item = GenesisValidatorData<Address, TokenAmount, TokenChange, PK>,
    >,
    Address: Debug + Clone + Ord + Hash,
    TokenAmount: Debug + Clone,
    TokenChange: Debug + Copy + ops::Add<Output = TokenChange>,
    PK: Debug + Clone,
{
    validators: Validators,
    /// Active and inactive validator sets
    validator_set: Epoched<ValidatorSet<Address>, OffsetUnboundingLen>,
    /// The sum of all active and inactive validators' voting power
    total_voting_power: Epoched<VotingPower, OffsetUnboundingLen>,
}
struct GenesisValidatorData<Address, TokenAmount, TokenChange, PK>
where
    Address: Debug + Clone + Ord + Hash,
    TokenAmount: Debug + Clone,
    TokenChange: Debug + Copy + ops::Add<Output = TokenChange>,
    PK: Debug + Clone,
{
    address: Address,
    staking_reward_address: Address,
    consensus_key: Epoched<PK, OffsetPipelineLen>,
    state: Epoched<ValidatorState, OffsetPipelineLen>,
    total_deltas: EpochedDelta<TokenChange, OffsetUnboundingLen>,
    voting_power: Epoched<VotingPower, OffsetUnboundingLen>,
    bond: (
        BondId<Address>,
        Epoched<Bond<TokenAmount>, OffsetPipelineLen>,
    ),
}

/// A function that returns genesis data created from the initial validator set.
fn init_genesis_data<'a, Address, TokenAmount, TokenChange, PK>(
    params: &'a PosParams,
    validators: impl Iterator<Item = &'a GenesisValidator<Address, TokenAmount, PK>>
    + Clone
    + 'a,
    epoch: Epoch,
) -> GenesisData<
    impl Iterator<
        Item = GenesisValidatorData<Address, TokenAmount, TokenChange, PK>,
    > + 'a,
    Address,
    TokenAmount,
    TokenChange,
    PK,
>
where
    Address: 'a + Debug + Clone + Ord + Hash,
    TokenAmount: 'a + Debug + Clone + Into<u64>,
    TokenChange:
        'a + Debug + Copy + ops::Add<Output = TokenChange> + From<TokenAmount>,
    PK: 'a + Debug + Clone,
{
    // Accumulate the validator set and total voting power
    let mut active: BTreeSet<WeightedValidator<Address>> = BTreeSet::default();
    let mut total_voting_power = VotingPower::default();
    for GenesisValidator {
        address, tokens, ..
    } in validators.clone()
    {
        let voting_power = VotingPower::from_tokens(tokens.clone(), params);
        total_voting_power += voting_power;
        active.insert(WeightedValidator {
            voting_power,
            address: address.clone(),
        });
    }
    // Pop the smallest validators from the active set until its size is under
    // the limit and insert them into the inactive set
    let mut inactive: BTreeSet<WeightedValidator<Address>> =
        BTreeSet::default();
    while active.len() > params.max_validator_slots as usize {
        match active.pop_first_shim() {
            Some(first) => {
                inactive.insert(first);
            }
            None => break,
        }
    }
    let validator_set = ValidatorSet { active, inactive };
    let validator_set = Epoched::init_at_genesis(validator_set, epoch);
    let total_voting_power =
        Epoched::init_at_genesis(total_voting_power, epoch);

    // Adapt the genesis validators data to PoS data
    let validators = validators.map(
        move |GenesisValidator {
                  address,

                  staking_reward_address,
                  tokens,
                  consensus_key,
              }| {
            let consensus_key =
                Epoched::init_at_genesis(consensus_key.clone(), epoch);
            let state =
                Epoched::init_at_genesis(ValidatorState::Candidate, epoch);
            let token_delta = TokenChange::from(tokens.clone());
            let total_deltas =
                EpochedDelta::init_at_genesis(token_delta, epoch);
            let voting_power = VotingPower::from_tokens(tokens.clone(), params);
            let voting_power = Epoched::init_at_genesis(voting_power, epoch);
            let bond_id = BondId {
                source: address.clone(),
                validator: address.clone(),
            };
            let mut delta = HashMap::default();
            delta.insert(epoch, tokens.clone());
            let bond = Epoched::init_at_genesis(Bond { delta }, epoch);
            GenesisValidatorData {
                address: address.clone(),
                staking_reward_address: staking_reward_address.clone(),
                consensus_key,
                state,
                total_deltas,
                voting_power,
                bond: (bond_id, bond),
            }
        },
    );

    GenesisData {
        validators,
        validator_set,
        total_voting_power,
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
