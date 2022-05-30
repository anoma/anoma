//! A tx for a PoS unbond that removes staked tokens from a self-bond or a
//! delegation to be withdrawn in or after unbonding epoch.

use anoma_tx_prelude::proof_of_stake::unbond_tokens;
use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let unbond =
        transaction::pos::Unbond::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();

    if let Err(err) =
        unbond_tokens(unbond.source.as_ref(), &unbond.validator, unbond.amount)
    {
        debug_log!("Unbonding failed with: {}", err);
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
    use anoma_tx_prelude::address::InternalAddress;
    use anoma_tx_prelude::key::testing::arb_common_keypair;
    use anoma_tx_prelude::key::RefTo;
    use anoma_tx_prelude::proof_of_stake::parameters::testing::arb_pos_params;
    use anoma_tx_prelude::token;
    use anoma_vp_prelude::proof_of_stake::types::{
        Bond, Unbond, VotingPower, VotingPowerDelta,
    };
    use anoma_vp_prelude::proof_of_stake::{
        staking_token_address, BondId, GenesisValidator, PosVP,
    };
    use proptest::prelude::*;

    use super::*;

    proptest! {
        /// In this test we setup the ledger and PoS system with an arbitrary
        /// initial state with 1 genesis validator, a delegation bond if the
        /// unbond is for a delegation, arbitrary PoS parameters and
        /// a we generate an arbitrary unbond that we'd like to apply.
        ///
        /// After we apply the unbond, we're checking that all the storage values
        /// in PoS system have been updated as expected and then we also check
        /// that this transaction is accepted by the PoS validity predicate.
        #[test]
        fn test_tx_unbond(
        (initial_stake, unbond) in arb_initial_stake_and_unbond(),
        // A key to sign the transaction
        key in arb_common_keypair(),
        pos_params in arb_pos_params()) {
            test_tx_unbond_aux(initial_stake, unbond, key, pos_params)
        }
    }

    fn test_tx_unbond_aux(
        initial_stake: token::Amount,
        unbond: transaction::pos::Unbond,
        key: key::common::SecretKey,
        pos_params: PosParams,
    ) {
        let is_delegation = matches!(
            &unbond.source, Some(source) if *source != unbond.validator);
        let staking_reward_address = address::testing::established_address_1();
        let consensus_key = key::testing::keypair_1().ref_to();
        let staking_reward_key = key::testing::keypair_2().ref_to();

        let genesis_validators = [GenesisValidator {
            address: unbond.validator.clone(),
            staking_reward_address,
            tokens: if is_delegation {
                // If we're unbonding a delegation, we'll give the initial stake
                // to the delegation instead of the validator
                token::Amount::default()
            } else {
                initial_stake
            },
            consensus_key,
            staking_reward_key,
        }];

        init_pos(&genesis_validators[..], &pos_params, Epoch(0));

        tx_host_env::with(|tx_env| {
            if is_delegation {
                let source = unbond.source.as_ref().unwrap();
                tx_env.spawn_accounts([source]);

                // To allow to unbond delegation, there must be a delegation
                // bond first.
                // First, credit the bond's source with the initial stake,
                // before we initialize the bond below
                tx_env.credit_tokens(
                    source,
                    &staking_token_address(),
                    initial_stake,
                );
            }
        });

        if is_delegation {
            // Initialize the delegation - unlike genesis validator's self-bond,
            // this happens at pipeline offset
            anoma_tx_prelude::proof_of_stake::bond_tokens(
                unbond.source.as_ref(),
                &unbond.validator,
                initial_stake,
            )
            .unwrap();
        }
        tx_host_env::commit_tx_and_block();

        let tx_code = vec![];
        let tx_data = unbond.try_to_vec().unwrap();
        let tx = Tx::new(tx_code, Some(tx_data));
        let signed_tx = tx.sign(&key);
        let tx_data = signed_tx.data.unwrap();

        let unbond_src = unbond
            .source
            .clone()
            .unwrap_or_else(|| unbond.validator.clone());
        let unbond_id = BondId {
            validator: unbond.validator.clone(),
            source: unbond_src,
        };

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
            PoS.read_validator_voting_power(&unbond.validator).unwrap();
        let bonds_pre = PoS.read_bond(&unbond_id).unwrap();
        dbg!(&bonds_pre);

        apply_tx(tx_data);

        // Read the data after the tx is executed

        // The following storage keys should be updated:

        //     - `#{PoS}/validator/#{validator}/total_deltas`
        let total_delta_post =
            PoS.read_validator_total_deltas(&unbond.validator);

        let expected_deltas_at_pipeline = if is_delegation {
            // When this is a delegation, there will be no bond until pipeline
            0.into()
        } else {
            // Before pipeline offset, there can only be self-bond
            initial_stake
        };

        // Before pipeline offset, there can only be self-bond for genesis
        // validator. In case of a delegation the state is setup so that there
        // is no bond until pipeline offset.
        for epoch in 0..pos_params.pipeline_len {
            assert_eq!(
                total_delta_post.as_ref().unwrap().get(epoch),
                Some(expected_deltas_at_pipeline.into()),
                "The total deltas before the pipeline offset must not change \
                 - checking in epoch: {epoch}"
            );
        }

        // At and after pipeline offset, there can be either delegation or
        // self-bond, both of which are initialized to the same `initial_stake`
        for epoch in pos_params.pipeline_len..pos_params.unbonding_len {
            assert_eq!(
                total_delta_post.as_ref().unwrap().get(epoch),
                Some(initial_stake.into()),
                "The total deltas before the unbonding offset must not change \
                 - checking in epoch: {epoch}"
            );
        }

        {
            let epoch = pos_params.unbonding_len + 1;
            let expected_stake =
                i128::from(initial_stake) - i128::from(unbond.amount);
            assert_eq!(
                total_delta_post.as_ref().unwrap().get(epoch),
                Some(expected_stake),
                "The total deltas after the unbonding offset epoch must be \
                 decremented by the unbonded amount - checking in epoch: \
                 {epoch}"
            );
        }

        //     - `#{staking_token}/balance/#{PoS}`
        let pos_balance_post: token::Amount =
            read(&pos_balance_key.to_string()).unwrap();
        assert_eq!(
            pos_balance_pre, pos_balance_post,
            "Unbonding doesn't affect PoS system balance"
        );

        //     - `#{PoS}/unbond/#{owner}/#{validator}`
        let unbonds_post = PoS.read_unbond(&unbond_id).unwrap();
        let bonds_post = PoS.read_bond(&unbond_id).unwrap();
        for epoch in 0..pos_params.unbonding_len {
            let unbond: Option<Unbond<token::Amount>> = unbonds_post.get(epoch);

            assert!(
                unbond.is_none(),
                "There should be no unbond until unbonding offset - checking \
                 epoch {epoch}"
            );
        }
        let start_epoch = match &unbond.source {
            Some(_) => {
                // This bond was a delegation
                anoma_tx_prelude::proof_of_stake::types::Epoch::from(
                    pos_params.pipeline_len,
                )
            }
            None => {
                // This bond was a genesis validator self-bond
                anoma_tx_prelude::proof_of_stake::types::Epoch::default()
            }
        };
        let end_epoch = anoma_tx_prelude::proof_of_stake::types::Epoch::from(
            pos_params.unbonding_len - 1,
        );

        let expected_unbond =
            HashMap::from_iter([((start_epoch, end_epoch), unbond.amount)]);
        let actual_unbond: Unbond<token::Amount> =
            unbonds_post.get(pos_params.unbonding_len).unwrap();
        assert_eq!(
            actual_unbond.deltas, expected_unbond,
            "Delegation at unbonding offset should be equal to the unbonded \
             amount"
        );

        for epoch in pos_params.pipeline_len..pos_params.unbonding_len {
            let bond: Bond<token::Amount> = bonds_post.get(epoch).unwrap();
            let expected_bond =
                HashMap::from_iter([(start_epoch, initial_stake)]);
            assert_eq!(
                bond.pos_deltas, expected_bond,
                "Before unbonding offset, the bond should be untouched, \
                 checking epoch {epoch}"
            );
        }
        {
            let epoch = pos_params.unbonding_len + 1;
            let bond: Bond<token::Amount> = bonds_post.get(epoch).unwrap();
            let expected_bond =
                HashMap::from_iter([(start_epoch, initial_stake)]);
            assert_eq!(
                bond.pos_deltas, expected_bond,
                "At unbonding offset, the pos deltas should not change, \
                 checking epoch {epoch}"
            );
            assert_eq!(
                bond.neg_deltas, unbond.amount,
                "At unbonding offset, the unbonded amount should have been \
                 deducted, checking epoch {epoch}"
            )
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
            PoS.read_validator_voting_power(&unbond.validator).unwrap();

        let voting_power_pre =
            VotingPower::from_tokens(initial_stake, &pos_params);
        let voting_power_post = VotingPower::from_tokens(
            initial_stake - unbond.amount,
            &pos_params,
        );
        if voting_power_pre == voting_power_post {
            // None of the optional storage fields should have been updated
            assert_eq!(total_voting_powers_pre, total_voting_powers_post);
            assert_eq!(validator_sets_pre, validator_sets_post);
            assert_eq!(
                validator_voting_powers_pre,
                validator_voting_powers_post
            );
        } else {
            for epoch in 0..pos_params.unbonding_len {
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
            {
                let epoch = pos_params.unbonding_len;
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

    fn arb_initial_stake_and_unbond()
    -> impl Strategy<Value = (token::Amount, transaction::pos::Unbond)> {
        // Generate initial stake
        token::testing::arb_amount().prop_flat_map(|initial_stake| {
            // Use the initial stake to limit the bond amount
            let unbond = arb_unbond(u64::from(initial_stake));
            // Use the generated initial stake too too
            (Just(initial_stake), unbond)
        })
    }

    /// Generates an initial validator stake and a unbond, while making sure
    /// that the `initial_stake >= unbond.amount`.
    fn arb_unbond(
        max_amount: u64,
    ) -> impl Strategy<Value = transaction::pos::Unbond> {
        (
            address::testing::arb_established_address(),
            prop::option::of(address::testing::arb_non_internal_address()),
            token::testing::arb_amount_ceiled(max_amount),
        )
            .prop_map(|(validator, source, amount)| {
                let validator = Address::Established(validator);
                transaction::pos::Unbond {
                    validator,
                    amount,
                    source,
                }
            })
    }
}
