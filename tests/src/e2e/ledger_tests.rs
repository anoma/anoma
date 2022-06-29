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

use std::process::Command;
use std::str::FromStr;
use std::sync::Arc;
use std::time::{Duration, Instant};

use anoma::types::address::{btc, eth, masp_rewards};
use anoma::types::token;
use anoma_apps::config::genesis::genesis_config::{
    GenesisConfig, ParametersConfig, PosParamsConfig,
};
use borsh::BorshSerialize;
use color_eyre::eyre::Result;
use setup::constants::*;

use crate::e2e::helpers::{
    epoch_sleep, find_address, find_voting_power, get_actor_rpc, get_epoch,
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
/// 2. Attempt to spend 10 BTC at SK(A) to PA(B)
/// 3. Attempt to spend 15 BTC at SK(A) to Bertha
/// 4. Send 20 BTC from Albert to PA(A)
/// 5. Attempt to spend 10 ETH at SK(A) to PA(B)
/// 6. Spend 7 BTC at SK(A) to PA(B)
/// 7. Spend 7 BTC at SK(A) to PA(B)
/// 8. Attempt to spend 7 BTC at SK(A) to PA(B)
/// 9. Spend 6 BTC at SK(A) to PA(B)
/// 10. Assert BTC balance at VK(A) is 0
/// 11. Assert ETH balance at VK(A) is 0
/// 12. Assert balance at VK(B) is 10 BTC
/// 13. Send 10 BTC from SK(B) to Bertha

#[test]
fn masp_txs_and_queries() -> Result<()> {
    // Lengthen epoch to ensure that a transaction can be constructed and
    // submitted within the same block. Necessary to ensure that conversion is
    // not invalidated.
    let test = setup::network(
        |genesis| {
            let parameters = ParametersConfig {
                min_duration: 3600,
                ..genesis.parameters
            };
            GenesisConfig {
                parameters,
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

    let txs_args = vec![
        // 2. Attempt to spend 10 BTC at SK(A) to PA(B)
        (
            vec![
                "transfer",
                "--source",
                A_SPENDING_KEY,
                "--target",
                AB_PAYMENT_ADDRESS,
                "--token",
                BTC,
                "--amount",
                "10",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "No balance found",
        ),
        // 3. Attempt to spend 15 BTC at SK(A) to Bertha
        (
            vec![
                "transfer",
                "--source",
                A_SPENDING_KEY,
                "--target",
                BERTHA,
                "--token",
                BTC,
                "--amount",
                "15",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "No balance found",
        ),
        // 4. Send 20 BTC from Albert to PA(A)
        (
            vec![
                "transfer",
                "--source",
                ALBERT,
                "--target",
                AA_PAYMENT_ADDRESS,
                "--token",
                BTC,
                "--amount",
                "20",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "Transaction is valid",
        ),
        // 5. Attempt to spend 10 ETH at SK(A) to PA(B)
        (
            vec![
                "transfer",
                "--source",
                A_SPENDING_KEY,
                "--target",
                AB_PAYMENT_ADDRESS,
                "--token",
                ETH,
                "--amount",
                "10",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "No balance found",
        ),
        // 6. Spend 7 BTC at SK(A) to PA(B)
        (
            vec![
                "transfer",
                "--source",
                A_SPENDING_KEY,
                "--target",
                AB_PAYMENT_ADDRESS,
                "--token",
                BTC,
                "--amount",
                "7",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "Transaction is valid",
        ),
        // 7. Spend 7 BTC at SK(A) to PA(B)
        (
            vec![
                "transfer",
                "--source",
                A_SPENDING_KEY,
                "--target",
                BB_PAYMENT_ADDRESS,
                "--token",
                BTC,
                "--amount",
                "7",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "Transaction is valid",
        ),
        // 8. Attempt to spend 7 BTC at SK(A) to PA(B)
        (
            vec![
                "transfer",
                "--source",
                A_SPENDING_KEY,
                "--target",
                BB_PAYMENT_ADDRESS,
                "--token",
                BTC,
                "--amount",
                "7",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "ChangeIsNegative",
        ),
        // 9. Spend 6 BTC at SK(A) to PA(B)
        (
            vec![
                "transfer",
                "--source",
                A_SPENDING_KEY,
                "--target",
                BB_PAYMENT_ADDRESS,
                "--token",
                BTC,
                "--amount",
                "6",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "Transaction is valid",
        ),
        // 10. Assert BTC balance at VK(A) is 0
        (
            vec![
                "balance",
                "--owner",
                AA_VIEWING_KEY,
                "--token",
                BTC,
                "--ledger-address",
                &validator_one_rpc,
            ],
            "No shielded BTC balance found",
        ),
        // 11. Assert ETH balance at VK(A) is 0
        (
            vec![
                "balance",
                "--owner",
                AA_VIEWING_KEY,
                "--token",
                ETH,
                "--ledger-address",
                &validator_one_rpc,
            ],
            "No shielded ETH balance found",
        ),
        // 12. Assert balance at VK(B) is 10 BTC
        (
            vec![
                "balance",
                "--owner",
                AB_VIEWING_KEY,
                "--ledger-address",
                &validator_one_rpc,
            ],
            "BTC: 20",
        ),
        // 13. Send 10 BTC from SK(B) to Bertha
        (
            vec![
                "transfer",
                "--source",
                B_SPENDING_KEY,
                "--target",
                BERTHA,
                "--token",
                BTC,
                "--amount",
                "20",
                "--ledger-address",
                &validator_one_rpc,
            ],
            "Transaction is valid",
        ),
    ];

    // Wait till epoch boundary
    let _ep0 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    for (tx_args, tx_result) in &txs_args {
        for &dry_run in &[true, false] {
            let tx_args = if dry_run && tx_args[0] == "transfer" {
                vec![tx_args.clone(), vec!["--dry-run"]].concat()
            } else {
                tx_args.clone()
            };
            let mut client = run!(test, Bin::Client, tx_args, Some(300))?;

            if *tx_result == "Transaction is valid" && !dry_run {
                if !cfg!(feature = "ABCI") {
                    client.exp_string("Transaction accepted")?;
                }
                client.exp_string("Transaction applied")?;
            }
            client.exp_string(tx_result)?;
        }
    }

    Ok(())
}

/// In this test we:
/// 1. Run the ledger node
/// 2. Assert PPA(C) cannot be recognized by incorrect viewing key
/// 3. Assert PPA(C) has not transaction pinned to it
/// 4. Send 20 BTC from Albert to PPA(C)
/// 5. Assert PPA(C) has the 20 BTC transaction pinned to it

#[test]
fn masp_pinned_txs() -> Result<()> {
    // Lengthen epoch to ensure that a transaction can be constructed and
    // submitted within the same block. Necessary to ensure that conversion is
    // not invalidated.
    let test = setup::network(
        |genesis| {
            let parameters = ParametersConfig {
                min_duration: 180,
                ..genesis.parameters
            };
            GenesisConfig {
                parameters,
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

    // Wait till epoch boundary
    let _ep0 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Assert PPA(C) cannot be recognized by incorrect viewing key
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AC_PAYMENT_ADDRESS,
            "--token",
            BTC,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.send_line(AB_VIEWING_KEY)?;
    client.exp_string("Supplied viewing key cannot decode transactions to")?;
    drop(client);

    // Assert PPA(C) has no transaction pinned to it
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AC_PAYMENT_ADDRESS,
            "--token",
            BTC,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.send_line(AC_VIEWING_KEY)?;
    client.exp_string("has not yet been consumed")?;
    drop(client);

    // Send 20 BTC from Albert to PPA(C)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "transfer",
            "--source",
            ALBERT,
            "--target",
            AC_PAYMENT_ADDRESS,
            "--token",
            BTC,
            "--amount",
            "20",
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("Transaction is valid")?;
    drop(client);

    // Assert PPA(C) has the 20 BTC transaction pinned to it
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AC_PAYMENT_ADDRESS,
            "--token",
            BTC,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.send_line(AC_VIEWING_KEY)?;
    client.exp_string("Received 20 BTC")?;
    drop(client);

    // Assert PPA(C) has no XAN pinned to it
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AC_PAYMENT_ADDRESS,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.send_line(AC_VIEWING_KEY)?;
    client.exp_string("Received no shielded XAN")?;
    drop(client);

    // Wait till epoch boundary
    let _ep1 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Assert PPA(C) does not XAN pinned to it on epoch boundary
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AC_PAYMENT_ADDRESS,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.send_line(AC_VIEWING_KEY)?;
    client.exp_string("Received no shielded XAN")?;
    drop(client);

    Ok(())
}

/// In this test we verify that users of the MASP receive the correct rewards
/// for leaving their assets in the pool for varying periods of time.

#[test]
fn masp_incentives() -> Result<()> {
    // Lengthen epoch to ensure that a transaction can be constructed and
    // submitted within the same block. Necessary to ensure that conversion is
    // not invalidated.
    let test = setup::network(
        |genesis| {
            let parameters = ParametersConfig {
                min_duration: 180,
                ..genesis.parameters
            };
            GenesisConfig {
                parameters,
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

    // Wait till epoch boundary
    let ep0 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Send 20 BTC from Albert to PA(A)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "transfer",
            "--source",
            ALBERT,
            "--target",
            AA_PAYMENT_ADDRESS,
            "--token",
            BTC,
            "--amount",
            "20",
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("Transaction is valid")?;
    drop(client);

    // Assert BTC balance at VK(A) is 20
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            BTC,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("BTC: 20")?;
    drop(client);

    // Assert XAN balance at VK(A) is 0
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("No shielded XAN balance found")?;
    drop(client);

    let masp_rewards = masp_rewards();

    // Wait till epoch boundary
    let ep1 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Assert BTC balance at VK(A) is 20
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            BTC,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("BTC: 20")?;
    drop(client);

    let amt20 = token::Amount::from_str("20").unwrap();
    let amt30 = token::Amount::from_str("30").unwrap();

    // Assert XAN balance at VK(A) is 20*BTC_reward*(epoch_1-epoch_0)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt20 * masp_rewards[&btc()]).0 * (ep1.0 - ep0.0)
    ))?;
    drop(client);

    // Assert XAN balance at MASP pool is 20*BTC_reward*(epoch_1-epoch_0)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            MASP,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt20 * masp_rewards[&btc()]).0 * (ep1.0 - ep0.0)
    ))?;
    drop(client);

    // Wait till epoch boundary
    let ep2 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Assert BTC balance at VK(A) is 20
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            BTC,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("BTC: 20")?;
    drop(client);

    // Assert XAN balance at VK(A) is 20*BTC_reward*(epoch_2-epoch_0)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt20 * masp_rewards[&btc()]).0 * (ep2.0 - ep0.0)
    ))?;
    drop(client);

    // Assert XAN balance at MASP pool is 20*BTC_reward*(epoch_2-epoch_0)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            MASP,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt20 * masp_rewards[&btc()]).0 * (ep2.0 - ep0.0)
    ))?;
    drop(client);

    // Wait till epoch boundary
    let ep3 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Send 30 ETH from Albert to PA(B)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "transfer",
            "--source",
            ALBERT,
            "--target",
            AB_PAYMENT_ADDRESS,
            "--token",
            ETH,
            "--amount",
            "30",
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("Transaction is valid")?;
    drop(client);

    // Assert ETH balance at VK(B) is 30
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AB_VIEWING_KEY,
            "--token",
            ETH,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("ETH: 30")?;
    drop(client);

    // Assert XAN balance at VK(B) is 0
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AB_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("No shielded XAN balance found")?;
    drop(client);

    // Wait till epoch boundary
    let ep4 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Assert ETH balance at VK(B) is 30
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AB_VIEWING_KEY,
            "--token",
            ETH,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("ETH: 30")?;
    drop(client);

    // Assert XAN balance at VK(B) is 30*ETH_reward*(epoch_4-epoch_3)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AB_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt30 * masp_rewards[&eth()]).0 * (ep4.0 - ep3.0)
    ))?;
    drop(client);

    // Assert XAN balance at MASP pool is
    // 20*BTC_reward*(epoch_4-epoch_0)+30*ETH_reward*(epoch_4-epoch_3)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            MASP,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        ((amt20 * masp_rewards[&btc()]).0 * (ep4.0 - ep0.0))
            + ((amt30 * masp_rewards[&eth()]).0 * (ep4.0 - ep3.0))
    ))?;
    drop(client);

    // Wait till epoch boundary
    let ep5 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Send 30 ETH from SK(B) to Christel
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "transfer",
            "--source",
            B_SPENDING_KEY,
            "--target",
            CHRISTEL,
            "--token",
            ETH,
            "--amount",
            "30",
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("Transaction is valid")?;
    drop(client);

    // Assert ETH balance at VK(B) is 0
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AB_VIEWING_KEY,
            "--token",
            ETH,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("No shielded ETH balance found")?;
    drop(client);

    // Assert XAN balance at VK(B) is 30*ETH_reward*(epoch_5-epoch_3)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AB_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt30 * masp_rewards[&eth()]).0 * (ep5.0 - ep3.0)
    ))?;
    drop(client);

    // Assert XAN balance at MASP pool is
    // 20*BTC_reward*(epoch_5-epoch_0)+30*ETH_reward*(epoch_5-epoch_3)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            MASP,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        ((amt20 * masp_rewards[&btc()]).0 * (ep5.0 - ep0.0))
            + ((amt30 * masp_rewards[&eth()]).0 * (ep5.0 - ep3.0))
    ))?;
    drop(client);

    // Wait till epoch boundary
    let ep6 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Send 20 BTC from SK(A) to Christel
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "transfer",
            "--source",
            A_SPENDING_KEY,
            "--target",
            CHRISTEL,
            "--token",
            BTC,
            "--amount",
            "20",
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("Transaction is valid")?;
    drop(client);

    // Assert BTC balance at VK(A) is 0
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            BTC,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("No shielded BTC balance found")?;
    drop(client);

    // Assert XAN balance at VK(A) is 20*BTC_reward*(epoch_6-epoch_0)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt20 * masp_rewards[&btc()]).0 * (ep6.0 - ep0.0)
    ))?;
    drop(client);

    // Assert XAN balance at MASP pool is
    // 20*BTC_reward*(epoch_6-epoch_0)+20*ETH_reward*(epoch_5-epoch_3)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            MASP,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        ((amt20 * masp_rewards[&btc()]).0 * (ep6.0 - ep0.0))
            + ((amt30 * masp_rewards[&eth()]).0 * (ep5.0 - ep3.0))
    ))?;
    drop(client);

    // Wait till epoch boundary
    let _ep7 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Assert XAN balance at VK(A) is 20*BTC_reward*(epoch_6-epoch_0)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt20 * masp_rewards[&btc()]).0 * (ep6.0 - ep0.0)
    ))?;
    drop(client);

    // Assert XAN balance at VK(B) is 30*ETH_reward*(epoch_5-epoch_3)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AB_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        (amt30 * masp_rewards[&eth()]).0 * (ep5.0 - ep3.0)
    ))?;
    drop(client);

    // Assert XAN balance at MASP pool is
    // 20*BTC_reward*(epoch_6-epoch_0)+30*ETH_reward*(epoch_5-epoch_3)
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            MASP,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string(&format!(
        "XAN: {}",
        ((amt20 * masp_rewards[&btc()]).0 * (ep6.0 - ep0.0))
            + ((amt30 * masp_rewards[&eth()]).0 * (ep5.0 - ep3.0))
    ))?;
    drop(client);

    // Wait till epoch boundary
    let _ep8 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Send 30*ETH_reward*(epoch_5-epoch_3) XAN from SK(B) to Christel
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "transfer",
            "--source",
            B_SPENDING_KEY,
            "--target",
            CHRISTEL,
            "--token",
            XAN,
            "--amount",
            &((amt30 * masp_rewards[&eth()]).0 * (ep5.0 - ep3.0)).to_string(),
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("Transaction is valid")?;
    drop(client);

    // Wait till epoch boundary
    let _ep9 = epoch_sleep(&test, &validator_one_rpc, 720)?;

    // Send 20*BTC_reward*(epoch_6-epoch_0) XAN from SK(A) to Christel
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "transfer",
            "--source",
            A_SPENDING_KEY,
            "--target",
            BERTHA,
            "--token",
            XAN,
            "--amount",
            &((amt20 * masp_rewards[&btc()]).0 * (ep6.0 - ep0.0)).to_string(),
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("Transaction is valid")?;
    drop(client);

    // Assert XAN balance at VK(A) is 0
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AA_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("No shielded XAN balance found")?;
    drop(client);

    // Assert XAN balance at VK(B) is 0
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            AB_VIEWING_KEY,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("No shielded XAN balance found")?;
    drop(client);

    // Assert XAN balance at MASP pool is 0
    let mut client = run!(
        test,
        Bin::Client,
        vec![
            "balance",
            "--owner",
            MASP,
            "--token",
            XAN,
            "--ledger-address",
            &validator_one_rpc
        ],
        Some(300)
    )?;
    client.exp_string("XAN: 0")?;
    drop(client);

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
        key: None,
        shielded: None,
    };
    let data = transfer
        .try_to_vec()
        .expect("Encoding unsigned transfer shouldn't fail");
    let tx_wasm_path = wasm_abs_path(TX_MINT_TOKENS_WASM);
    std::fs::write(&tx_data_path, data).unwrap();
    let tx_wasm_path = tx_wasm_path.to_string_lossy();
    let tx_data_path = tx_data_path.to_string_lossy();

    let validator_one_rpc = get_actor_rpc(&test, &Who::Validator(0));

    let daewon_lower = DAEWON.to_lowercase();
    let tx_args = vec![
        "tx",
        "--code-path",
        &tx_wasm_path,
        "--data-path",
        &tx_data_path,
        "--signing-key",
        &daewon_lower,
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
    let daewon_lower = DAEWON.to_lowercase();
    let tx_args = vec![
        "transfer",
        "--source",
        DAEWON,
        "--signing-key",
        &daewon_lower,
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
