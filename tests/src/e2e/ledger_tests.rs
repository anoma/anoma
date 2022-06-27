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

use std::fs::{self, OpenOptions};
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::time::{Duration, Instant};

use anoma::types::token;
use anoma_apps::config::genesis::genesis_config::{
    GenesisConfig, ParametersConfig, PosParamsConfig,
};
use borsh::BorshSerialize;
use color_eyre::eyre::Result;
use serde_json::json;
use setup::constants::*;

use super::setup::working_dir;
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
#[cfg(feature = "skip-tendermint-8840")]
#[test]
fn test_node_connectivity() -> Result<()> {
    // Setup 2 genesis validator nodes
    let test =
        setup::network(|genesis| setup::add_validators(1, genesis), None)?;

    // 1. Run 2 genesis validator ledger nodes and 1 non-validator node
    let args = ["ledger"];
    let mut validator_0 =
        run_as!(test, Who::Validator(0), Bin::Node, args, Some(40))?;
    let mut validator_1 =
        run_as!(test, Who::Validator(1), Bin::Node, args, Some(40))?;
    let mut non_validator =
        run_as!(test, Who::NonValidator, Bin::Node, args, Some(40))?;

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
/// 7. Query the raw bytes of a storage key
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
    let christel = find_address(&test, CHRISTEL)?;
    // as setup in `genesis/e2e-tests-single-node.toml`
    let christel_balance = token::Amount::whole(1000000);
    let xan = find_address(&test, XAN)?;
    let storage_key = token::balance_key(&xan, &christel).to_string();
    let query_args_and_expected_response = vec![
        // 7. Query storage key and get hex-encoded raw bytes
        (
            vec![
                "query-bytes",
                "--storage-key",
                &storage_key,
                "--ledger-address",
                &validator_one_rpc,
            ],
            // expect hex encoded of borsh encoded bytes
            hex::encode(christel_balance.try_to_vec().unwrap()),
        ),
    ];
    for (query_args, expected) in &query_args_and_expected_response {
        let mut client = run!(test, Bin::Client, query_args, Some(40))?;
        client.exp_string(expected)?;

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
    ledger.exp_string("some VPs rejected ")?;
    if !cfg!(feature = "ABCI") {
        client.exp_string("Transaction accepted")?;
    }
    client.exp_string("Transaction applied")?;
    client.exp_string("Transaction is invalid")?;
    client.exp_string(r#""code": "1"#)?;

    client.assert_success();

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

    // 2. Submit a self-bond for the gepnesis validator
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
/// 1. Run the ledger node
/// 2. Submit a valid proposal
/// 3. Query the proposal
/// 4. Query token balance (submitted funds)
/// 5. Query governance address balance
/// 6. Submit an invalid proposal
/// 7. Check invalid proposal was not accepted
/// 8. Query token balance (funds shall not be submitted)
/// 9. Send a yay vote from a validator
/// 10. Send a yay vote from a normal user
/// 11. Query the proposal and check the result
/// 12. Wait proposal grace and check proposal author funds
/// 13. Check governance address funds are 0
#[test]
fn proposal_submission() -> Result<()> {
    let test = setup::network(|genesis| genesis, None)?;

    let anomac_help = vec!["--help"];

    let mut client = run!(test, Bin::Client, anomac_help, Some(40))?;
    client.exp_string("Anoma client command line interface.")?;
    client.assert_success();

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

    // 1.1 Delegate some token
    let tx_args = vec![
        "bond",
        "--validator",
        "validator-0",
        "--source",
        BERTHA,
        "--amount",
        "900",
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

    // 2. Submit valid proposal
    let valid_proposal_json_path =
        test.base_dir.path().join("valid_proposal.json");
    let proposal_code = wasm_abs_path(TX_PROPOSAL_CODE);

    let albert = find_address(&test, ALBERT)?;
    let valid_proposal_json = json!(
        {
            "content": {
                "title": "TheTitle",
                "authors": "test@test.com",
                "discussions-to": "www.github.com/anoma/aip/1",
                "created": "2022-03-10T08:54:37Z",
                "license": "MIT",
                "abstract": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices. Quisque viverra varius cursus. Praesent sed mauris gravida, pharetra turpis non, gravida eros. Nullam sed ex justo. Ut at placerat ipsum, sit amet rhoncus libero. Sed blandit non purus non suscipit. Phasellus sed quam nec augue bibendum bibendum ut vitae urna. Sed odio diam, ornare nec sapien eget, congue viverra enim.",
                "motivation": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices.",
                "details": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices. Quisque viverra varius cursus. Praesent sed mauris gravida, pharetra turpis non, gravida eros.",
                "requires": "2"
            },
            "author": albert,
            "voting_start_epoch": 6,
            "voting_end_epoch": 18,
            "grace_epoch": 24,
            "proposal_code_path": proposal_code.to_str().unwrap()
        }
    );

    generate_proposal_json(
        valid_proposal_json_path.clone(),
        valid_proposal_json,
    );

    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));

    let submit_proposal_args = vec![
        "init-proposal",
        "--data-path",
        valid_proposal_json_path.to_str().unwrap(),
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, submit_proposal_args, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 3. Query the proposal

    let proposal_query_args = vec![
        "query-proposal",
        "--proposal-id",
        "0",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, proposal_query_args, Some(40))?;
    client.exp_string("Proposal: 0")?;
    client.assert_success();

    // 4. Query token balance proposal author (submitted funds)
    let query_balance_args = vec![
        "balance",
        "--owner",
        ALBERT,
        "--token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, query_balance_args, Some(40))?;
    client.exp_string("XAN: 999500")?;
    client.assert_success();

    // 5. Query token balance governance
    let query_balance_args = vec![
        "balance",
        "--owner",
        GOVERNANCE_ADDRESS,
        "--token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, query_balance_args, Some(40))?;
    client.exp_string("XAN: 500")?;
    client.assert_success();

    // 6. Submit an invalid proposal
    // proposal is invalid due to voting_end_epoch - voting_start_epoch < 3
    let invalid_proposal_json_path =
        test.base_dir.path().join("invalid_proposal.json");
    let albert = find_address(&test, ALBERT)?;
    let invalid_proposal_json = json!(
        {
            "content": {
                "title": "TheTitle",
                "authors": "test@test.com",
                "discussions-to": "www.github.com/anoma/aip/1",
                "created": "2022-03-10T08:54:37Z",
                "license": "MIT",
                "abstract": "Ut convallis eleifend orci vel venenatis. Duis
    vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit
    ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum
    fermentum. Morbi aliquet purus at sollicitudin ultrices. Quisque viverra
    varius cursus. Praesent sed mauris gravida, pharetra turpis non, gravida
    eros. Nullam sed ex justo. Ut at placerat ipsum, sit amet rhoncus libero.
    Sed blandit non purus non suscipit. Phasellus sed quam nec augue bibendum
    bibendum ut vitae urna. Sed odio diam, ornare nec sapien eget, congue
    viverra enim.",
                "motivation": "Ut convallis eleifend orci vel venenatis. Duis
    vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit
    ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum
    fermentum. Morbi aliquet purus at sollicitudin ultrices.",
                "details": "Ut convallis eleifend orci vel venenatis. Duis
    vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit
    ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum
    fermentum. Morbi aliquet purus at sollicitudin ultrices. Quisque viverra
    varius cursus. Praesent sed mauris gravida, pharetra turpis non, gravida
    eros.",             "requires": "2"
            },
            "author": albert,
            "voting_start_epoch": 9999,
            "voting_end_epoch": 10000,
            "grace_epoch": 10009,
        }
    );
    generate_proposal_json(
        invalid_proposal_json_path.clone(),
        invalid_proposal_json,
    );

    let submit_proposal_args = vec![
        "init-proposal",
        "--data-path",
        invalid_proposal_json_path.to_str().unwrap(),
        "--ledger-address",
        &validator_one_rpc,
    ];
    let mut client = run!(test, Bin::Client, submit_proposal_args, Some(40))?;
    client.exp_string("Transaction is invalid.")?;
    client.assert_success();

    // 7. Check invalid proposal was not accepted
    let proposal_query_args = vec![
        "query-proposal",
        "--proposal-id",
        "1",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, proposal_query_args, Some(40))?;
    client.exp_string("No valid proposal was found with id 1")?;
    client.assert_success();

    // 8. Query token balance (funds shall not be submitted)
    let query_balance_args = vec![
        "balance",
        "--owner",
        ALBERT,
        "--token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, query_balance_args, Some(40))?;
    client.exp_string("XAN: 999500")?;
    client.assert_success();

    // 9. Send a yay vote from a validator
    let mut epoch = get_epoch(&test, &validator_one_rpc).unwrap();
    while epoch.0 <= 7 {
        sleep(1);
        epoch = get_epoch(&test, &validator_one_rpc).unwrap();
    }

    let submit_proposal_vote = vec![
        "vote-proposal",
        "--proposal-id",
        "0",
        "--vote",
        "yay",
        "--signer",
        "validator-0",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run_as!(
        test,
        Who::Validator(0),
        Bin::Client,
        submit_proposal_vote,
        Some(15)
    )?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    let submit_proposal_vote_delagator = vec![
        "vote-proposal",
        "--proposal-id",
        "0",
        "--vote",
        "nay",
        "--signer",
        BERTHA,
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client =
        run!(test, Bin::Client, submit_proposal_vote_delagator, Some(40))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 10. Send a yay vote from a non-validator/non-delegator user
    let submit_proposal_vote = vec![
        "vote-proposal",
        "--proposal-id",
        "0",
        "--vote",
        "yay",
        "--signer",
        ALBERT,
        "--ledger-address",
        &validator_one_rpc,
    ];

    // this is valid because the client filter ALBERT delegation and there are
    // none
    let mut client = run!(test, Bin::Client, submit_proposal_vote, Some(15))?;
    client.exp_string("Transaction is valid.")?;
    client.assert_success();

    // 11. Query the proposal and check the result
    let mut epoch = get_epoch(&test, &validator_one_rpc).unwrap();
    while epoch.0 <= 19 {
        sleep(1);
        epoch = get_epoch(&test, &validator_one_rpc).unwrap();
    }

    let query_proposal = vec![
        "query-proposal-result",
        "--proposal-id",
        "0",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, query_proposal, Some(15))?;
    client.exp_string("Result: passed")?;
    client.assert_success();

    // 12. Wait proposal grace and check proposal author funds
    let mut epoch = get_epoch(&test, &validator_one_rpc).unwrap();
    while epoch.0 < 26 {
        sleep(1);
        epoch = get_epoch(&test, &validator_one_rpc).unwrap();
    }

    let query_balance_args = vec![
        "balance",
        "--owner",
        ALBERT,
        "--token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, query_balance_args, Some(30))?;
    client.exp_string("XAN: 1000000")?;
    client.assert_success();

    // 13. Check if governance funds are 0
    let query_balance_args = vec![
        "balance",
        "--owner",
        GOVERNANCE_ADDRESS,
        "--token",
        XAN,
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, query_balance_args, Some(30))?;
    client.exp_string("XAN: 0")?;
    client.assert_success();

    // // 14. Query parameters
    let query_protocol_parameters = vec![
        "query-protocol-parameters",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client =
        run!(test, Bin::Client, query_protocol_parameters, Some(30))?;
    client.exp_regex(".*Min. proposal grace epoch: 9.*")?;
    client.assert_success();

    Ok(())
}

/// In this test we:
/// 1. Run the ledger node
/// 2. Create an offline proposal
/// 3. Create an offline vote
/// 4. Tally offline
#[test]
fn proposal_offline() -> Result<()> {
    let test = setup::network(|genesis| genesis, None)?;

    // 1. Run the ledger node
    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(20))?;

    ledger.exp_string("Anoma ledger node started")?;
    if !cfg!(feature = "ABCI") {
        ledger.exp_string("started node")?;
    } else {
        ledger.exp_string("Started node")?;
    }

    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));

    // 1.1 Delegate some token
    let tx_args = vec![
        "bond",
        "--validator",
        "validator-0",
        "--source",
        ALBERT,
        "--amount",
        "900",
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

    // 2. Create an offline proposal
    let valid_proposal_json_path =
        test.base_dir.path().join("valid_proposal.json");
    let albert = find_address(&test, ALBERT)?;
    let valid_proposal_json = json!(
        {
            "content": {
                "title": "TheTitle",
                "authors": "test@test.com",
                "discussions-to": "www.github.com/anoma/aip/1",
                "created": "2022-03-10T08:54:37Z",
                "license": "MIT",
                "abstract": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices. Quisque viverra varius cursus. Praesent sed mauris gravida, pharetra turpis non, gravida eros. Nullam sed ex justo. Ut at placerat ipsum, sit amet rhoncus libero. Sed blandit non purus non suscipit. Phasellus sed quam nec augue bibendum bibendum ut vitae urna. Sed odio diam, ornare nec sapien eget, congue viverra enim.",
                "motivation": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices.",
                "details": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices. Quisque viverra varius cursus. Praesent sed mauris gravida, pharetra turpis non, gravida eros.",
                "requires": "2"
            },
            "author": albert,
            "voting_start_epoch": 3,
            "voting_end_epoch": 6,
            "grace_epoch": 6
        }
    );
    generate_proposal_json(
        valid_proposal_json_path.clone(),
        valid_proposal_json,
    );

    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));

    let offline_proposal_args = vec![
        "init-proposal",
        "--data-path",
        valid_proposal_json_path.to_str().unwrap(),
        "--offline",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, offline_proposal_args, Some(15))?;
    client.exp_string("Proposal created: ")?;
    client.assert_success();

    // 3. Generate an offline yay vote
    let mut epoch = get_epoch(&test, &validator_one_rpc).unwrap();
    while epoch.0 <= 5 {
        sleep(1);
        epoch = get_epoch(&test, &validator_one_rpc).unwrap();
    }

    let proposal_path = working_dir().join("proposal");
    let proposal_ref = proposal_path.to_string_lossy();
    let submit_proposal_vote = vec![
        "vote-proposal",
        "--data-path",
        &proposal_ref,
        "--vote",
        "yay",
        "--signer",
        ALBERT,
        "--offline",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, submit_proposal_vote, Some(15))?;
    client.exp_string("Proposal vote created: ")?;
    client.assert_success();

    let expected_file_name = format!("proposal-vote-{}", albert);
    let expected_path_vote = working_dir().join(&expected_file_name);
    assert!(expected_path_vote.exists());

    let expected_path_proposal = working_dir().join("proposal");
    assert!(expected_path_proposal.exists());

    // 4. Compute offline tally
    let proposal_data_folder = working_dir().join("proposal-test-data");
    fs::create_dir_all(&proposal_data_folder)
        .expect("Should create a new folder.");
    fs::copy(
        expected_path_proposal,
        &proposal_data_folder.join("proposal"),
    )
    .expect("Should copy proposal file.");
    fs::copy(
        expected_path_vote,
        &proposal_data_folder.join(&expected_file_name),
    )
    .expect("Should copy proposal vote file.");

    let tally_offline = vec![
        "query-proposal-result",
        "--data-path",
        proposal_data_folder.to_str().unwrap(),
        "--offline",
        "--ledger-address",
        &validator_one_rpc,
    ];

    let mut client = run!(test, Bin::Client, tally_offline, Some(15))?;
    client.exp_string("Result: rejected")?;
    client.assert_success();

    Ok(())
}

fn generate_proposal_json(
    proposal_path: PathBuf,
    proposal_content: serde_json::Value,
) {
    let intent_writer = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(proposal_path)
        .unwrap();
    serde_json::to_writer(intent_writer, &proposal_content).unwrap();
}
