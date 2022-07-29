//! A tx for a PoS bond that stakes tokens via a self-bond or delegation.

use anoma_tx_prelude::proof_of_stake::bond_tokens;
use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let bond =
        transaction::pos::Bond::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();

    if let Err(err) =
        bond_tokens(bond.source.as_ref(), &bond.validator, bond.amount)
    {
        debug_log!("Bond failed with: {}", err);
        panic!()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use anoma::ledger::pos::PosParams;
    use anoma::proto::Tx;
    use anoma::types::storage::Epoch;
    use anoma_tests::log::test;
    use anoma_tests::native_vp::pos::init_pos;
    use anoma_tests::native_vp::TestNativeVpEnv;
    use anoma_tests::tx::*;
    use anoma_tx_prelude::address::testing::{
        arb_established_address, arb_non_internal_address,
    };
    use anoma_tx_prelude::address::InternalAddress;
    use anoma_tx_prelude::key::testing::arb_common_keypair;
    use anoma_tx_prelude::key::RefTo;
    use anoma_tx_prelude::proof_of_stake::parameters::testing::arb_pos_params;
    use anoma_tx_prelude::token;
    use anoma_vp_prelude::proof_of_stake::types::{
        Bond, VotingPower, VotingPowerDelta,
    };
    use anoma_vp_prelude::proof_of_stake::{
        staking_token_address, BondId, GenesisValidator, PosVP,
    };
    use proptest::prelude::*;

    use super::*;

    proptest! {
        /// In this test we setup the ledger and PoS system with an arbitrary
        /// initial state with 1 genesis validator and arbitrary PoS parameters. We then
        /// generate an arbitrary bond that we'd like to apply.
        ///
        /// After we apply the bond, we check that all the storage values
        /// in PoS system have been updated as expected and then we also check
        /// that this transaction is accepted by the PoS validity predicate.
        #[test]
        fn test_tx_bond(
            (initial_stake, bond) in arb_initial_stake_and_bond(),
            // A key to sign the transaction
            key in arb_common_keypair(),
            pos_params in arb_pos_params()) {
            test_tx_bond_aux(initial_stake, bond, key, pos_params)
        }
    }

    fn test_tx_bond_aux(
        initial_stake: token::Amount,
        bond: transaction::pos::Bond,
        key: key::common::SecretKey,
        pos_params: PosParams,
    ) {
        let staking_reward_address = address::testing::established_address_1();
        let consensus_key = key::testing::keypair_1().ref_to();
        let staking_reward_key = key::testing::keypair_2().ref_to();

        let genesis_validators = [GenesisValidator {
            address: bond.validator.clone(),
            staking_reward_address,
            tokens: initial_stake,
            consensus_key,
            staking_reward_key,
        }];

        init_pos(&genesis_validators[..], &pos_params, Epoch(0));

        tx_host_env::with(|tx_env| {
            if let Some(source) = &bond.source {
                tx_env.spawn_accounts([source]);
            }

            // Ensure that the bond's source has enough tokens for the bond
            let target = bond.source.as_ref().unwrap_or(&bond.validator);
            tx_env.credit_tokens(target, &staking_token_address(), bond.amount);
        });

        let tx_code = vec![];
        let tx_data = bond.try_to_vec().unwrap();
        let tx = Tx::new(tx_code, Some(tx_data));
        let signed_tx = tx.sign(&key);
        let tx_data = signed_tx.data.unwrap();

        // Read the data before the tx is executed
        let pos_balance_key = token::balance_key(
            &staking_token_address(),
            &Address::Internal(InternalAddress::PoS),
        );
        let pos_balance_pre: token::Amount =
            read(&pos_balance_key.to_string()).expect("PoS must have balance");
        assert_eq!(pos_balance_pre, initial_stake);
        let total_voting_powers_pre = PoS.read_total_voting_power();
        let validator_sets_pre = PoS.read_validator_set();
        let validator_voting_powers_pre =
            PoS.read_validator_voting_power(&bond.validator).unwrap();

        apply_tx(tx_data);

        // Read the data after the tx is executed

        // The following storage keys should be updated:

        //     - `#{PoS}/validator/#{validator}/total_deltas`
        let total_delta_post = PoS.read_validator_total_deltas(&bond.validator);
        for epoch in 0..pos_params.pipeline_len {
            assert_eq!(
                total_delta_post.as_ref().unwrap().get(epoch),
                Some(initial_stake.into()),
                "The total deltas before the pipeline offset must not change \
                 - checking in epoch: {epoch}"
            );
        }
        for epoch in pos_params.pipeline_len..=pos_params.unbonding_len {
            let expected_stake =
                i128::from(initial_stake) + i128::from(bond.amount);
            assert_eq!(
                total_delta_post.as_ref().unwrap().get(epoch),
                Some(expected_stake),
                "The total deltas at and after the pipeline offset epoch must \
                 be incremented by the bonded amount - checking in epoch: \
                 {epoch}"
            );
        }

        //     - `#{staking_token}/balance/#{PoS}`
        let pos_balance_post: token::Amount =
            read(&pos_balance_key.to_string()).unwrap();
        assert_eq!(pos_balance_pre + bond.amount, pos_balance_post);

        //     - `#{PoS}/bond/#{owner}/#{validator}`
        let bond_src = bond
            .source
            .clone()
            .unwrap_or_else(|| bond.validator.clone());
        let bond_id = BondId {
            validator: bond.validator.clone(),
            source: bond_src,
        };
        let bonds_post = PoS.read_bond(&bond_id).unwrap();
        match &bond.source {
            Some(_) => {
                // This bond was a delegation
                for epoch in 0..pos_params.pipeline_len {
                    let bond: Option<Bond<token::Amount>> =
                        bonds_post.get(epoch);
                    assert!(
                        bond.is_none(),
                        "Delegation before pipeline offset should be empty - \
                         checking epoch {epoch}"
                    );
                }
                for epoch in pos_params.pipeline_len..=pos_params.unbonding_len
                {
                    let start_epoch =
                        anoma_tx_prelude::proof_of_stake::types::Epoch::from(
                            pos_params.pipeline_len,
                        );
                    let expected_bond =
                        HashMap::from_iter([(start_epoch, bond.amount)]);
                    let bond: Bond<token::Amount> =
                        bonds_post.get(epoch).unwrap();
                    assert_eq!(
                        bond.pos_deltas, expected_bond,
                        "Delegation at and after pipeline offset should be \
                         equal to the bonded amount - checking epoch {epoch}"
                    );
                }
            }
            None => {
                let genesis_epoch =
                    anoma_tx_prelude::proof_of_stake::types::Epoch::from(0);
                // It was a self-bond
                for epoch in 0..pos_params.pipeline_len {
                    let expected_bond =
                        HashMap::from_iter([(genesis_epoch, initial_stake)]);
                    let bond: Bond<token::Amount> =
                        bonds_post.get(epoch).expect(
                            "Genesis validator should already have self-bond",
                        );
                    assert_eq!(
                        bond.pos_deltas, expected_bond,
                        "Delegation before pipeline offset should be equal to \
                         the genesis initial stake - checking epoch {epoch}"
                    );
                }
                for epoch in pos_params.pipeline_len..=pos_params.unbonding_len
                {
                    let start_epoch =
                        anoma_tx_prelude::proof_of_stake::types::Epoch::from(
                            pos_params.pipeline_len,
                        );
                    let expected_bond = HashMap::from_iter([
                        (genesis_epoch, initial_stake),
                        (start_epoch, bond.amount),
                    ]);
                    let bond: Bond<token::Amount> =
                        bonds_post.get(epoch).unwrap();
                    assert_eq!(
                        bond.pos_deltas, expected_bond,
                        "Delegation at and after pipeline offset should \
                         contain genesis stake and the bonded amount - \
                         checking epoch {epoch}"
                    );
                }
            }
        }

        // If the voting power from validator's initial stake is different
        // from the voting power after the bond is applied, we expect the
        // following 3 fields to be updated:
        //     - `#{PoS}/total_voting_power` (optional)
        //     - `#{PoS}/validator_set` (optional)
        //     - `#{PoS}/validator/#{validator}/voting_power` (optional)
        let total_voting_powers_post = PoS.read_total_voting_power();
        let validator_sets_post = PoS.read_validator_set();
        let validator_voting_powers_post =
            PoS.read_validator_voting_power(&bond.validator).unwrap();

        let voting_power_pre =
            VotingPower::from_tokens(initial_stake, &pos_params);
        let voting_power_post =
            VotingPower::from_tokens(initial_stake + bond.amount, &pos_params);
        if voting_power_pre == voting_power_post {
            // None of the optional storage fields should have been updated
            assert_eq!(total_voting_powers_pre, total_voting_powers_post);
            assert_eq!(validator_sets_pre, validator_sets_post);
            assert_eq!(
                validator_voting_powers_pre,
                validator_voting_powers_post
            );
        } else {
            for epoch in 0..pos_params.pipeline_len {
                let total_voting_power_pre = total_voting_powers_pre.get(epoch);
                let total_voting_power_post =
                    total_voting_powers_post.get(epoch);
                assert_eq!(
                    total_voting_power_pre, total_voting_power_post,
                    "Total voting power before pipeline offset must not \
                     change - checking epoch {epoch}"
                );

                let validator_set_pre = validator_sets_pre.get(epoch);
                let validator_set_post = validator_sets_post.get(epoch);
                assert_eq!(
                    validator_set_pre, validator_set_post,
                    "Validator set before pipeline offset must not change - \
                     checking epoch {epoch}"
                );

                let validator_voting_power_pre =
                    validator_voting_powers_pre.get(epoch);
                let validator_voting_power_post =
                    validator_voting_powers_post.get(epoch);
                assert_eq!(
                    validator_voting_power_pre, validator_voting_power_post,
                    "Validator's voting power before pipeline offset must not \
                     change - checking epoch {epoch}"
                );
            }
            for epoch in pos_params.pipeline_len..=pos_params.unbonding_len {
                let total_voting_power_pre =
                    total_voting_powers_pre.get(epoch).unwrap();
                let total_voting_power_post =
                    total_voting_powers_post.get(epoch).unwrap();
                assert_ne!(
                    total_voting_power_pre, total_voting_power_post,
                    "Total voting power at and after pipeline offset must \
                     have changed - checking epoch {epoch}"
                );

                let validator_set_pre = validator_sets_pre.get(epoch).unwrap();
                let validator_set_post =
                    validator_sets_post.get(epoch).unwrap();
                assert_ne!(
                    validator_set_pre, validator_set_post,
                    "Validator set at and after pipeline offset must have \
                     changed - checking epoch {epoch}"
                );

                let validator_voting_power_pre =
                    validator_voting_powers_pre.get(epoch).unwrap();
                let validator_voting_power_post =
                    validator_voting_powers_post.get(epoch).unwrap();
                assert_ne!(
                    validator_voting_power_pre, validator_voting_power_post,
                    "Validator's voting power at and after pipeline offset \
                     must have changed - checking epoch {epoch}"
                );

                // Expected voting power from the model ...
                let expected_validator_voting_power: VotingPowerDelta =
                    voting_power_post.try_into().unwrap();
                // ... must match the voting power read from storage
                assert_eq!(
                    validator_voting_power_post,
                    expected_validator_voting_power
                );
            }
        }

        // Use the tx_env to run PoS VP
        let tx_env = tx_host_env::take();
        let vp_env = TestNativeVpEnv::new(tx_env);
        let result = vp_env.validate_tx(PosVP::new, |_tx_data| {});
        let result =
            result.expect("Validation of valid changes must not fail!");
        assert!(
            result,
            "PoS Validity predicate must accept this transaction"
        );
    }

    prop_compose! {
        /// Generates an initial validator stake and a bond, while making sure
        /// that the `initial_stake + bond.amount <= u64::MAX` to avoid
        /// overflow.
        fn arb_initial_stake_and_bond()
            // Generate initial stake
            (initial_stake in token::testing::arb_amount())
            // Use the initial stake to limit the bond amount
            (bond in arb_bond(u64::MAX - u64::from(initial_stake)),
            // Use the generated initial stake too
            initial_stake in Just(initial_stake),
        ) -> (token::Amount, transaction::pos::Bond) {
            (initial_stake, bond)
        }
    }

    fn arb_bond(
        max_amount: u64,
    ) -> impl Strategy<Value = transaction::pos::Bond> {
        (
            arb_established_address(),
            prop::option::of(arb_non_internal_address()),
            token::testing::arb_amount_ceiled(max_amount),
        )
            .prop_map(|(validator, source, amount)| {
                transaction::pos::Bond {
                    validator: Address::Established(validator),
                    amount,
                    source,
                }
            })
    }
}
