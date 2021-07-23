mod btree_set;
pub mod epoched;
pub mod parameters;
pub mod types;

use core::fmt::Debug;
use std::hash::Hash;
use std::ops::{Add, Sub};

use epoched::{Epoched, OffsetPipelineLen, OffsetUnboundingLen};
use parameters::PosParams;
use types::{Epoch, GenesisValidator, ValidatorState};

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

pub trait Pos {
    type Address: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash;
    type TokenAmount: Debug + Clone + Copy + Add + Sub;
    type TokenChange: Debug + Clone + Copy + Add + Sub;
    type PublicKey: Debug + Clone;

    fn read_validator_consensus_key(
        &self,
        validator: Self::Address,
    ) -> Option<Epoched<Self::PublicKey, OffsetPipelineLen>>;

    fn write_validator_consensus_key(
        &mut self,
        validator: Self::Address,
        key: Epoched<Self::PublicKey, OffsetPipelineLen>,
    );
    fn write_validator_state(
        &mut self,
        validator: Self::Address,
        key: Epoched<ValidatorState, OffsetPipelineLen>,
    );
    fn write_validator_total_deltas(
        &mut self,
        validator: Self::Address,
        key: Epoched<i128, OffsetUnboundingLen>,
    );
    fn write_validator_voting_power(
        &mut self,
        validator: Self::Address,
        key: Epoched<u64, OffsetUnboundingLen>,
    );

    fn init_genesis(
        pos: &mut impl Pos,
        params: &PosParams,
        validators: Vec<
            GenesisValidator<Self::Address, Self::TokenAmount, Self::PublicKey>,
        >,
        epoch: Epoch,
    ) {
        todo!()
        // for (adDr, tokens, pk) in validators {
        //     let consensus_key = Epoched::init_at_genesis(pk, epoch);
        //     pos.write_validator_consensus_key(addr, consensus_key);
        //     let state = Epoched::init_at_genesis(ValidatorState::Candidate,
        // epoch);     pos.write_validator_state(addr, state);
        //     let total_deltas = Epoched::init_at_genesis(tokens as i128,
        // epoch);     pos.write_validator_total_deltas(addr,
        // total_deltas);     let voting_power =
        // Epoched::init_at_genesis(         tokens * 10_000 /
        // params.votes_per_token,         epoch,
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
