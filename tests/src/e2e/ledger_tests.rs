//! By default, these tests will run in release mode. This can be disabled
//! by setting environment variable `ANOMA_E2E_DEBUG=true`. For debugging,
//! you'll typically also want to set `RUST_BACKTRACE=1`, e.g.:
//!
//! ```ignore,shell
//! ANOMA_E2E_DEBUG=true RUST_BACKTRACE=1 cargo test e2e::ledger_tests -- --test-threads=1 --nocapture
//! ```
//!
//! To keep the temporary files created by a test, use env var
//! `ANOMA_E2E_KEEP_TEMP=true`.

use std::collections::HashMap;
use std::net::SocketAddr;
use std::process::Command;
use std::str::FromStr;
use std::sync::Arc;
use std::time::{Duration, Instant};

use anoma::types::chain::ChainId;
use anoma::types::token;
use anoma_apps::config::genesis::genesis_config::{
    self, GenesisConfig, ParametersConfig, PosParamsConfig,
    ValidatorPreGenesisConfig,
};
use anoma_apps::config::Config;
use borsh::BorshSerialize;
use color_eyre::eyre::Result;
use setup::constants::*;
use tempfile::tempdir;

use crate::e2e::helpers::{
    find_address, find_voting_power, get_actor_rpc, get_epoch,
};
use crate::e2e::setup::{self, sleep, Bin, Who};
use crate::{run, run_as};

/// Test that when we "run-ledger" with all the possible command
/// combinations from fresh state, the node starts-up successfully for both a
/// validator and non-validator user.
#[test]
fn run_ledger() -> Result<()> {
    let test = setup::single_node_net()?;
    let cmd_combinations = vec![vec!["ledger"], vec!["ledger", "run"]];

    // Start the ledger as a validator
    for args in &cmd_combinations {
        let mut ledger =
            run_as!(test, Who::Validator(0), Bin::Node, args, Some(40))?;
        ledger.exp_string("Anoma ledger node started")?;
        ledger.exp_string("This node is a validator")?;
    }

    // Start the ledger as a non-validator
    for args in &cmd_combinations {
        let mut ledger =
            run_as!(test, Who::NonValidator, Bin::Node, args, Some(40))?;
        ledger.exp_string("Anoma ledger node started")?;
        if !cfg!(feature = "ABCI") {
            ledger.exp_string("This node is a fullnode")?;
        } else {
            ledger.exp_string("This node is not a validator")?;
        }
    }

    Ok(())
}

/// In this test we:
/// 1. Run 2 genesis validator ledger nodes and 1 non-validator node
/// 2. Submit a valid token transfer tx
/// 3. Check that all the nodes processed the tx with the same result
#[test]
fn test_node_connectivity() -> Result<()> {
    // Setup 2 genesis validator nodes
    let test =
        setup::network(|genesis| setup::add_validators(1, genesis), None)?;

    // 1. Run 2 genesis validator ledger nodes and 1 non-validator node
    let args = ["ledger"];
    let mut validator_0 =
        run_as!(test, Who::Validator(0), Bin::Node, args, Some(40))?;
    validator_0.exp_string("Anoma ledger node started")?;
    validator_0.exp_string("This node is a validator")?;
    let mut validator_1 =
        run_as!(test, Who::Validator(1), Bin::Node, args, Some(40))?;
    validator_1.exp_string("Anoma ledger node started")?;
    validator_1.exp_string("This node is a validator")?;
    let mut non_validator =
        run_as!(test, Who::NonValidator, Bin::Node, args, Some(40))?;
    non_validator.exp_string("Anoma ledger node started")?;
    non_validator.exp_string("This node is not a validator")?;

    // 2. Submit a valid token transfer tx
    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));
    let tx_args = [
        "transfer",
        "--source",
        BERTHA,
        "--target",
        ALBERT,
        "--token",
        XAN,
        "--amount",
        "10.1",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 3. Check that all the nodes processed the tx with the same result
    let expected_result = "all VPs accepted apply_tx storage modification";
    validator_0.exp_string(expected_result)?;
    validator_1.exp_string(expected_result)?;
    non_validator.exp_string(expected_result)?;

    Ok(())
}

/// In this test we:
/// 1. Start up the ledger
/// 2. Kill the tendermint process
/// 3. Check that the node detects this
/// 4. Check that the node shuts down
#[test]
fn test_anoma_shuts_down_if_tendermint_dies() -> Result<()> {
    let test = setup::single_node_net()?;

    // 1. Run the ledger node
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    ledger.exp_string("Anoma ledger node started")?;

    // 2. Kill the tendermint node
    sleep(1);
    Command::new("pkill")
        .args(&["tendermint"])
        .spawn()
        .expect("Test failed")
        .wait()
        .expect("Test failed");

    // 3. Check that anoma detects that the tendermint node is dead
    ledger.exp_string("Tendermint node is no longer running.")?;

    // 4. Check that the ledger node shuts down
    ledger.exp_string("Anoma ledger node has shut down.")?;
    ledger.exp_eof()?;

    Ok(())
}

/// In this test we:
/// 1. Run the ledger node
/// 2. Shut it down
/// 3. Run the ledger again, it should load its previous state
/// 4. Shut it down
/// 5. Reset the ledger's state
/// 6. Run the ledger again, it should start from fresh state
#[test]
fn run_ledger_load_state_and_reset() -> Result<()> {
    let test = setup::single_node_net()?;

    // 1. Run the ledger node
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    ledger.exp_string("Anoma ledger node started")?;
    // There should be no previous state
    ledger.exp_string("No state could be found")?;
    // Wait to commit a block
    ledger.exp_regex(r"Committed block hash.*, height: [0-9]+")?;

    // 2. Shut it down
    ledger.send_control('c')?;
    // Wait for the node to stop running to finish writing the state and tx
    // queue
    ledger.exp_string("Anoma ledger node has shut down.")?;
    ledger.exp_eof()?;
    drop(ledger);

    // 3. Run the ledger again, it should load its previous state
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    ledger.exp_string("Anoma ledger node started")?;

    // There should be previous state now
    ledger.exp_string("Last state root hash:")?;

    // 4. Shut it down
    ledger.send_control('c')?;
    // Wait for it to stop
    ledger.exp_eof()?;
    drop(ledger);

    // 5. Reset the ledger's state
    let mut session = run_as!(
        test,
        Who::Validator(0),
        Bin::Node,
        &["ledger", "reset"],
        Some(10),
    )?;
    session.exp_eof()?;

    // 6. Run the ledger again, it should start from fresh state
    let mut session =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    session.exp_string("Anoma ledger node started")?;

    // There should be no previous state
    session.exp_string("No state could be found")?;

    Ok(())
}

/// In this test we:
/// 1. Run the ledger node
/// 2. Submit a token transfer tx
/// 3. Submit a transaction to update an account's validity predicate
/// 4. Submit a custom tx
/// 5. Submit a tx to initialize a new account
/// 6. Query token balance
#[test]
fn ledger_txs_and_queries() -> Result<()> {
    let test = setup::network(|genesis| genesis, None)?;

    // 1. Run the ledger node
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    ledger.exp_string("Anoma ledger node started")?;
    if !cfg!(feature = "ABCI") {
        ledger.exp_string("started node")?;
    } else {
        ledger.exp_string("Started node")?;
    }

    let vp_user = wasm_abs_path(VP_USER_WASM);
    let vp_user = vp_user.to_string_lossy();
    let tx_no_op = wasm_abs_path(TX_NO_OP_WASM);
    let tx_no_op = tx_no_op.to_string_lossy();

    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));

    let txs_args = vec![
        // 2. Submit a token transfer tx
        vec![
            "transfer",
            "--source",
            BERTHA,
            "--target",
            ALBERT,
            "--token",
            XAN,
            "--amount",
            "10.1",
            "--fee-amount",
            "0",
            "--gas-limit",
            "0",
            "--fee-token",
            XAN,
            "--ledger-address",
            &validator_one_rpc,
        ],
        // 3. Submit a transaction to update an account's validity
        // predicate
        vec![
            "update",
             "--address",
             BERTHA,
             "--code-path",
             &vp_user,
             "--fee-amount",
             "0",
             "--gas-limit",
             "0",
             "--fee-token",
             XAN,
            "--ledger-address",
            &validator_one_rpc,
        ],
        // 4. Submit a custom tx
        vec![
            "tx",
            "--signer",
            BERTHA,
            "--code-path",
            &tx_no_op,
            "--data-path",
            "README.md",
            "--fee-amount",
            "0",
            "--gas-limit",
            "0",
            "--fee-token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        // 5. Submit a tx to initialize a new account
        vec![
            "init-account",
            "--source",
            BERTHA,
            "--public-key",
            // Value obtained from `anoma::types::key::ed25519::tests::gen_keypair`
            "001be519a321e29020fa3cbfbfd01bd5e92db134305609270b71dace25b5a21168",
            "--code-path",
            &vp_user,
            "--alias",
            "Test-Account",
            "--fee-amount",
            "0",
            "--gas-limit",
            "0",
            "--fee-token",
            XAN,
            "--ledger-address",
            &validator_one_rpc,
        ],
    ];

    for tx_args in &txs_args {
        for &dry_run in &[true, false] {
            let tx_args = if dry_run {
                vec![tx_args.clone(), vec!["--dry-run"]].concat()
            } else {
                tx_args.clone()
            };
            let mut client = run!(test, Bin::Client, tx_args, Some(40))?;

            if !dry_run {
                if !cfg!(feature = "ABCI") {
                    client.exp_string("Transaction accepted")?;
                }
                client.exp_string("Transaction applied")?;
            }
            client.exp_string("Transaction is valid.")?;
            client.assert_success();
        }
    }

    let query_args_and_expected_response = vec![
        // 6. Query token balance
        (
            vec![
                "balance",
                "--owner",
                BERTHA,
                "--token",
                XAN,
                "--ledger-address",
                &validator_one_rpc,
            ],
            // expect a decimal
            r"XAN: \d+(\.\d+)?",
        ),
    ];
    for (query_args, expected) in &query_args_and_expected_response {
        let mut client = run!(test, Bin::Client, query_args, Some(40))?;
        client.exp_regex(expected)?;

        client.assert_success();
    }

    Ok(())
}

/// In this test we:
/// 1. Run the ledger node
/// 2. Submit an invalid transaction (disallowed by state machine)
/// 3. Shut down the ledger
/// 4. Restart the ledger
/// 5. Submit and invalid transactions (malformed)
#[test]
fn invalid_transactions() -> Result<()> {
    let test = setup::single_node_net()?;

    // 1. Run the ledger node
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;
    ledger.exp_string("Anoma ledger node started")?;
    if !cfg!(feature = "ABCI") {
        ledger.exp_string("started node")?;
    } else {
        ledger.exp_string("Started node")?;
    }
    // Wait to commit a block
    ledger.exp_regex(r"Committed block hash.*, height: [0-9]+")?;

    // 2. Submit a an invalid transaction (trying to mint tokens should fail
    // in the token's VP)
    let tx_data_path = test.base_dir.path().join("tx.data");
    let transfer = token::Transfer {
        source: find_address(&test, DAEWON)?,
        target: find_address(&test, ALBERT)?,
        token: find_address(&test, XAN)?,
        amount: token::Amount::whole(1),
    };
    let data = transfer
        .try_to_vec()
        .expect("Encoding unsigned transfer shouldn't fail");
    let tx_wasm_path = wasm_abs_path(TX_MINT_TOKENS_WASM);
    std::fs::write(&tx_data_path, data).unwrap();
    let tx_wasm_path = tx_wasm_path.to_string_lossy();
    let tx_data_path = tx_data_path.to_string_lossy();

    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));

    let tx_args = vec![
        "tx",
        "--code-path",
        &tx_wasm_path,
        "--data-path",
        &tx_data_path,
        "--signing-key",
        DAEWON,
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    if !cfg!(feature = "ABCI") {
        client.exp_string("Transaction accepted")?;
    }
    client.exp_string("Transaction applied")?;
    client.exp_string("Transaction is invalid")?;
    client.exp_string(r#""code": "1"#)?;

    client.assert_success();
    ledger.exp_string("some VPs rejected apply_tx storage modification")?;

    // Wait to commit a block
    ledger.exp_regex(r"Committed block hash.*, height: [0-9]+")?;

    // 3. Shut it down
    ledger.send_control('c')?;
    // Wait for the node to stop running to finish writing the state and tx
    // queue
    ledger.exp_string("Anoma ledger node has shut down.")?;
    ledger.exp_eof()?;
    drop(ledger);

    // 4. Restart the ledger
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    ledger.exp_string("Anoma ledger node started")?;

    // There should be previous state now
    ledger.exp_string("Last state root hash:")?;

    // 5. Submit an invalid transactions (invalid token address)
    let tx_args = vec![
        "transfer",
        "--source",
        DAEWON,
        "--signing-key",
        DAEWON,
        "--target",
        ALBERT,
        "--token",
        BERTHA,
        "--amount",
        "1_000_000.1",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        // Force to ignore client check that fails on the balance check of the
        // source address
        "--force",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    if !cfg!(feature = "ABCI") {
        client.exp_string("Transaction accepted")?;
    }
    client.exp_string("Transaction applied")?;

    client.exp_string("Error trying to apply a transaction")?;

    client.exp_string(r#""code": "3"#)?;

    client.assert_success();
    Ok(())
}

/// PoS bonding, unbonding and withdrawal tests. In this test we:
///
/// 1. Run the ledger node with shorter epochs for faster progression
/// 2. Submit a self-bond for the genesis validator
/// 3. Submit a delegation to the genesis validator
/// 4. Submit an unbond of the self-bond
/// 5. Submit an unbond of the delegation
/// 6. Wait for the unbonding epoch
/// 7. Submit a withdrawal of the self-bond
/// 8. Submit a withdrawal of the delegation
#[test]
fn pos_bonds() -> Result<()> {
    let unbonding_len = 2;
    let test = setup::network(
        |genesis| {
            let parameters = ParametersConfig {
                min_num_of_blocks: 2,
                min_duration: 1,
                max_expected_time_per_block: 1,
                ..genesis.parameters
            };
            let pos_params = PosParamsConfig {
                pipeline_len: 1,
                unbonding_len,
                ..genesis.pos_params
            };
            GenesisConfig {
                parameters,
                pos_params,
                ..genesis
            }
        },
        None,
    )?;

    // 1. Run the ledger node
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    ledger.exp_string("Anoma ledger node started")?;
    if !cfg!(feature = "ABCI") {
        ledger.exp_string("started node")?;
    } else {
        ledger.exp_string("Started node")?;
    }

    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));

    // 2. Submit a self-bond for the genesis validator
    let tx_args = vec![
        "bond",
        "--validator",
        "validator-0",
        "--amount",
        "10.1",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client =
        run_as!(test, Who::Validator(0), Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 3. Submit a delegation to the genesis validator
    let tx_args = vec![
        "bond",
        "--validator",
        "validator-0",
        "--source",
        BERTHA,
        "--amount",
        "10.1",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 4. Submit an unbond of the self-bond
    let tx_args = vec![
        "unbond",
        "--validator",
        "validator-0",
        "--amount",
        "5.1",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client =
        run_as!(test, Who::Validator(0), Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 5. Submit an unbond of the delegation
    let tx_args = vec![
        "unbond",
        "--validator",
        "validator-0",
        "--source",
        BERTHA,
        "--amount",
        "3.2",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 6. Wait for the unbonding epoch
    let epoch = get_epoch(&test, &validator_one_rpc)?;
    let earliest_withdrawal_epoch = epoch + unbonding_len;
    println!(
        "Current epoch: {}, earliest epoch for withdrawal: {}",
        epoch, earliest_withdrawal_epoch
    );
    let start = Instant::now();
    let loop_timeout = Duration::new(20, 0);
    loop {
        if Instant::now().duration_since(start) > loop_timeout {
            panic!(
                "Timed out waiting for epoch: {}",
                earliest_withdrawal_epoch
            );
        }
        let epoch = get_epoch(&test, &validator_one_rpc)?;
        if epoch >= earliest_withdrawal_epoch {
            break;
        }
    }

    // 7. Submit a withdrawal of the self-bond
    let tx_args = vec![
        "withdraw",
        "--validator",
        "validator-0",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client =
        run_as!(test, Who::Validator(0), Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 8. Submit a withdrawal of the delegation
    let tx_args = vec![
        "withdraw",
        "--validator",
        "validator-0",
        "--source",
        BERTHA,
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    Ok(())
}

/// PoS validator creation test. In this test we:
///
/// 1. Run the ledger node with shorter epochs for faster progression
/// 2. Initialize a new validator account
/// 3. Submit a delegation to the new validator
/// 4. Transfer some XAN to the new validator
/// 5. Submit a self-bond for the new validator
/// 6. Wait for the pipeline epoch
/// 7. Check the new validator's voting power
#[test]
fn pos_init_validator() -> Result<()> {
    let pipeline_len = 1;
    let test = setup::network(
        |genesis| {
            let parameters = ParametersConfig {
                min_num_of_blocks: 2,
                min_duration: 1,
                max_expected_time_per_block: 1,
                ..genesis.parameters
            };
            let pos_params = PosParamsConfig {
                pipeline_len,
                unbonding_len: 2,
                ..genesis.pos_params
            };
            GenesisConfig {
                parameters,
                pos_params,
                ..genesis
            }
        },
        None,
    )?;

    // 1. Run the ledger node
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    ledger.exp_string("Anoma ledger node started")?;
    if !cfg!(feature = "ABCI") {
        ledger.exp_string("started node")?;
    } else {
        ledger.exp_string("Started node")?;
    }

    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));

    // 2. Initialize a new validator account
    let new_validator = "new-validator";
    let new_validator_key = format!("{}-key", new_validator);
    let tx_args = vec![
        "init-validator",
        "--alias",
        new_validator,
        "--source",
        BERTHA,
        "--unsafe-dont-encrypt",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 3. Submit a delegation to the new validator
    //    First, transfer some tokens to the validator's key for fees:
    let tx_args = vec![
        "transfer",
        "--source",
        BERTHA,
        "--target",
        &new_validator_key,
        "--token",
        XAN,
        "--amount",
        "0.5",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();
    //     Then self-bond the tokens:
    let tx_args = vec![
        "bond",
        "--validator",
        new_validator,
        "--source",
        BERTHA,
        "--amount",
        "1000.5",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 4. Transfer some XAN to the new validator
    let tx_args = vec![
        "transfer",
        "--source",
        BERTHA,
        "--target",
        new_validator,
        "--token",
        XAN,
        "--amount",
        "10999.5",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 5. Submit a self-bond for the new validator
    let tx_args = vec![
        "bond",
        "--validator",
        new_validator,
        "--amount",
        "10000",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 6. Wait for the pipeline epoch when the validator's voting power should
    // be non-zero
    let epoch = get_epoch(&test, &validator_one_rpc)?;
    let earliest_update_epoch = epoch + pipeline_len;
    println!(
        "Current epoch: {}, earliest epoch with updated voting power: {}",
        epoch, earliest_update_epoch
    );
    let start = Instant::now();
    let loop_timeout = Duration::new(20, 0);
    loop {
        if Instant::now().duration_since(start) > loop_timeout {
            panic!("Timed out waiting for epoch: {}", earliest_update_epoch);
        }
        let epoch = get_epoch(&test, &validator_one_rpc)?;
        if epoch >= earliest_update_epoch {
            break;
        }
    }

    // 7. Check the new validator's voting power
    let voting_power =
        find_voting_power(&test, new_validator, &validator_one_rpc)?;
    assert_eq!(voting_power, 11);

    Ok(())
}
/// Test that multiple txs submitted in the same block all get the tx result.
///
/// In this test we:
/// 1. Run the ledger node with 10s consensus timeout
/// 2. Spawn threads each submitting token transfer tx
#[test]
fn ledger_many_txs_in_a_block() -> Result<()> {
    let test = Arc::new(setup::network(
        |genesis| genesis,
        // Set 10s consensus timeout to have more time to submit txs
        Some("10s"),
    )?);

    // 1. Run the ledger node
    let mut ledger =
        run_as!(*test, Who::Validator(0), Bin::Node, &["ledger"], Some(40))?;

    ledger.exp_string("Anoma ledger node started")?;
    if !cfg!(feature = "ABCI") {
        ledger.exp_string("started node")?;
    } else {
        ledger.exp_string("Started node")?;
    }

    // Wait to commit a block
    ledger.exp_regex(r"Committed block hash.*, height: [0-9]+")?;

    let validator_one_rpc = Arc::new(get_actor_rpc(&test, &Who::Validator(0)));

    // A token transfer tx args
    let tx_args = Arc::new(vec![
        "transfer",
        "--source",
        BERTHA,
        "--target",
        ALBERT,
        "--token",
        XAN,
        "--amount",
        "10.1",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
    ]);

    // 2. Spawn threads each submitting token transfer tx
    // We collect to run the threads in parallel.
    #[allow(clippy::needless_collect)]
    let tasks: Vec<std::thread::JoinHandle<_>> = (0..3)
        .into_iter()
        .map(|_| {
            let test = Arc::clone(&test);
            let validator_one_rpc = Arc::clone(&validator_one_rpc);
            let tx_args = Arc::clone(&tx_args);
            std::thread::spawn(move || {
                let mut args = (*tx_args).clone();
                args.push(&*validator_one_rpc);
                let mut client = run!(*test, Bin::Client, args, Some(40))?;
                if !cfg!(feature = "ABCI") {
                    client.exp_string("Transaction accepted")?;
                }
                client.exp_string("Transaction applied")?;
                client.exp_string("Transaction is valid.")?;
                client.assert_success();
                let res: Result<()> = Ok(());
                res
            })
        })
        .collect();
    for task in tasks.into_iter() {
        task.join().unwrap()?;
    }

    Ok(())
}

/// In this test we:
/// 1. Setup 2 genesis validators
/// 2. Initialize a new network with the 2 validators
/// 3. Setup and start the 2 genesis validator nodes and a non-validator node
/// 4. Submit a valid token transfer tx from one validator to the other
/// 5. Check that all the nodes processed the tx with the same result
#[test]
fn test_genesis_validators() -> Result<()> {
    // This test is not using the `setup::network`, because we're setting up
    // custom genesis validators
    setup::INIT.call_once(|| {
        if let Err(err) = color_eyre::install() {
            eprintln!("Failed setting up colorful error reports {}", err);
        }
    });

    let working_dir = setup::working_dir();
    let base_dir = tempdir().unwrap();
    let checksums_path = working_dir
        .join("wasm/checksums.json")
        .to_string_lossy()
        .into_owned();

    // Same as in `genesis/e2e-tests-single-node.toml` for `validator-0`
    let net_address_0 = SocketAddr::from_str("127.0.0.1:27656").unwrap();
    let net_address_port_0 = net_address_0.port();
    // Find the first port (ledger P2P) that should be used for a validator at
    // the given index
    let get_first_port = |ix: u8| {
        net_address_port_0
            + 6 * (ix as u16 + 1)
            + if cfg!(feature = "ABCI") {
                0
            } else {
                // The ABCI++ ports at `26670 + ABCI_PLUS_PLUS_PORT_OFFSET`,
                // see `network`
                setup::ABCI_PLUS_PLUS_PORT_OFFSET
            }
    };

    // 1. Setup 2 genesis validators
    let validator_0_alias = "validator-0";
    let validator_1_alias = "validator-1";

    let init_genesis_validator_0 = setup::run_cmd(
        Bin::Client,
        [
            "utils",
            "init-genesis-validator",
            "--unsafe-dont-encrypt",
            "--alias",
            validator_0_alias,
            "--net-address",
            &format!("127.0.0.1:{}", get_first_port(0)),
        ],
        Some(5),
        &working_dir,
        &base_dir,
        "validator",
        format!("{}:{}", std::file!(), std::line!()),
    )?;
    init_genesis_validator_0.assert_success();
    let validator_0_pre_genesis_dir =
        anoma_apps::client::utils::validator_pre_genesis_dir(
            base_dir.path(),
            validator_0_alias,
        );
    let config = std::fs::read_to_string(
        anoma_apps::client::utils::validator_pre_genesis_file(
            &validator_0_pre_genesis_dir,
        ),
    )
    .unwrap();
    let mut validator_0_config: ValidatorPreGenesisConfig =
        toml::from_str(&config).unwrap();
    let validator_0_config = validator_0_config
        .validator
        .remove(validator_0_alias)
        .unwrap();

    let init_genesis_validator_1 = setup::run_cmd(
        Bin::Client,
        [
            "utils",
            "init-genesis-validator",
            "--unsafe-dont-encrypt",
            "--alias",
            validator_1_alias,
            "--net-address",
            &format!("127.0.0.1:{}", get_first_port(1)),
        ],
        Some(5),
        &working_dir,
        &base_dir,
        "validator",
        format!("{}:{}", std::file!(), std::line!()),
    )?;
    init_genesis_validator_1.assert_success();
    let validator_1_pre_genesis_dir =
        anoma_apps::client::utils::validator_pre_genesis_dir(
            base_dir.path(),
            validator_1_alias,
        );
    let config = std::fs::read_to_string(
        &anoma_apps::client::utils::validator_pre_genesis_file(
            &validator_1_pre_genesis_dir,
        ),
    )
    .unwrap();
    let mut validator_1_config: ValidatorPreGenesisConfig =
        toml::from_str(&config).unwrap();
    let validator_1_config = validator_1_config
        .validator
        .remove(validator_1_alias)
        .unwrap();

    // 2. Initialize a new network with the 2 validators
    let mut genesis = genesis_config::open_genesis_config(
        working_dir.join(setup::SINGLE_NODE_NET_GENESIS),
    );
    let update_validator_config =
        |ix: u8, mut config: genesis_config::ValidatorConfig| {
            // Setup tokens balances and validity predicates
            config.tokens = Some(200000);
            config.non_staked_balance = Some(1000000000000);
            config.validator_vp = Some("vp_user".into());
            config.staking_reward_vp = Some("vp_user".into());
            // Setup the validator ports same as what
            // `setup::add_validators` would do
            let mut net_address = net_address_0;
            // 6 ports for each validator
            let first_port = get_first_port(ix);
            net_address.set_port(first_port);
            config.net_address = Some(net_address.to_string());
            config
        };
    genesis.validator = HashMap::from_iter([
        (
            validator_0_alias.to_owned(),
            update_validator_config(0, validator_0_config),
        ),
        (
            validator_1_alias.to_owned(),
            update_validator_config(1, validator_1_config),
        ),
    ]);
    let genesis_file = base_dir.path().join("e2e-test-genesis-src.toml");
    genesis_config::write_genesis_config(&genesis, &genesis_file);
    let genesis_path = genesis_file.to_string_lossy();

    let archive_dir = base_dir.path().to_string_lossy().to_string();
    let args = vec![
        "utils",
        "init-network",
        "--unsafe-dont-encrypt",
        "--genesis-path",
        &genesis_path,
        "--chain-prefix",
        "e2e-test",
        "--localhost",
        "--allow-duplicate-ip",
        "--wasm-checksums-path",
        &checksums_path,
        "--archive-dir",
        &archive_dir,
    ];
    let mut init_network = setup::run_cmd(
        Bin::Client,
        args,
        Some(5),
        &working_dir,
        &base_dir,
        "validator",
        format!("{}:{}", std::file!(), std::line!()),
    )?;

    // Get the generated chain_id` from result of the last command
    let (unread, matched) =
        init_network.exp_regex(r"Derived chain ID: .*\n")?;
    let chain_id_raw =
        matched.trim().split_once("Derived chain ID: ").unwrap().1;
    let chain_id = ChainId::from_str(chain_id_raw.trim())?;
    println!("'init-network' output: {}", unread);
    let net = setup::Network {
        chain_id: chain_id.clone(),
    };
    let test = setup::Test {
        working_dir: working_dir.clone(),
        base_dir,
        net,
        genesis,
    };

    // Host the network archive to make it available for `join-network` commands
    let network_archive_server = file_serve::Server::new(&working_dir);
    let network_archive_addr = network_archive_server.addr().to_owned();
    std::thread::spawn(move || {
        network_archive_server.serve().unwrap();
    });

    // 3. Setup and start the 2 genesis validator nodes and a non-validator node

    // Clean-up the chain dir from the existing validator dir that were created
    // by `init-network`, because we want to set them up with `join-network`
    // instead
    let validator_0_base_dir = test.get_base_dir(&Who::Validator(0));
    let validator_1_base_dir = test.get_base_dir(&Who::Validator(1));
    std::fs::remove_dir_all(&validator_0_base_dir).unwrap();
    std::fs::remove_dir_all(&validator_1_base_dir).unwrap();

    std::env::set_var(
        anoma_apps::client::utils::ENV_VAR_NETWORK_CONFIGS_SERVER,
        format!("http://{network_archive_addr}/{}", archive_dir),
    );
    let pre_genesis_path = validator_0_pre_genesis_dir.to_string_lossy();
    let mut join_network_val_0 = run_as!(
        test,
        Who::Validator(0),
        Bin::Client,
        [
            "utils",
            "join-network",
            "--chain-id",
            chain_id.as_str(),
            "--pre-genesis-path",
            pre_genesis_path.as_ref(),
        ],
        Some(5)
    )?;
    join_network_val_0.exp_string("Successfully configured for chain")?;

    let pre_genesis_path = validator_1_pre_genesis_dir.to_string_lossy();
    let mut join_network_val_1 = run_as!(
        test,
        Who::Validator(1),
        Bin::Client,
        [
            "utils",
            "join-network",
            "--chain-id",
            chain_id.as_str(),
            "--pre-genesis-path",
            pre_genesis_path.as_ref(),
        ],
        Some(5)
    )?;
    join_network_val_1.exp_string("Successfully configured for chain")?;

    // We have to update the ports in the configs again, because the ones from
    // `join-network` use the defaults
    let update_config = |ix: u8, mut config: Config| {
        let first_port = net_address_port_0
            + 6 * (ix as u16 + 1)
            + if cfg!(feature = "ABCI") {
                0
            } else {
                setup::ABCI_PLUS_PLUS_PORT_OFFSET
            };
        config.ledger.tendermint.p2p_address.set_port(first_port);
        config
            .ledger
            .tendermint
            .rpc_address
            .set_port(first_port + 1);
        config.ledger.shell.ledger_address.set_port(first_port + 2);
        config
    };

    let validator_0_config = update_config(
        0,
        Config::load(&validator_0_base_dir, &test.net.chain_id, None),
    );
    validator_0_config
        .write(&validator_0_base_dir, &chain_id, true)
        .unwrap();

    let validator_1_config = update_config(
        1,
        Config::load(&validator_1_base_dir, &test.net.chain_id, None),
    );
    validator_1_config
        .write(&validator_1_base_dir, &chain_id, true)
        .unwrap();

    // Copy WASMs to each node's chain dir
    let chain_dir = test.base_dir.path().join(chain_id.as_str());
    setup::copy_wasm_to_chain_dir(
        &working_dir,
        &chain_dir,
        &chain_id,
        test.genesis.validator.keys(),
    );

    let args = ["ledger"];
    let mut validator_0 =
        run_as!(test, Who::Validator(0), Bin::Node, args, Some(40))?;
    validator_0.exp_string("Anoma ledger node started")?;
    validator_0.exp_string("This node is a validator")?;

    let mut validator_1 =
        run_as!(test, Who::Validator(1), Bin::Node, args, Some(40))?;
    validator_1.exp_string("Anoma ledger node started")?;
    validator_1.exp_string("This node is a validator")?;

    let mut non_validator =
        run_as!(test, Who::NonValidator, Bin::Node, args, Some(40))?;
    non_validator.exp_string("Anoma ledger node started")?;
    non_validator.exp_string("This node is not a validator")?;

    // 4. Submit a valid token transfer tx
    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));
    let tx_args = [
        "transfer",
        "--source",
        validator_0_alias,
        "--target",
        validator_1_alias,
        "--token",
        XAN,
        "--amount",
        "10.1",
        "--fee-amount",
        "0",
        "--gas-limit",
        "0",
        "--fee-token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client =
        run_as!(test, Who::Validator(0), Bin::Client, tx_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 3. Check that all the nodes processed the tx with the same result
    let expected_result = "all VPs accepted apply_tx storage modification";
    validator_0.exp_string(expected_result)?;
    validator_1.exp_string(expected_result)?;
    non_validator.exp_string(expected_result)?;

    Ok(())
}
