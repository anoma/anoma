use std::fs;
use std::process::Command;

use anoma::proto::Tx;
use anoma::types::address::Address;
use anoma::types::token;
use anoma_apps::wallet;
use assert_cmd::assert::OutputAssertExt;
use assert_cmd::cargo::CommandCargoExt;
use borsh::BorshSerialize;
use color_eyre::eyre::Result;
use eyre::eyre;
use rexpect::process::wait::WaitStatus;
use rexpect::session::spawn_command;
use setup::constants::*;
use tempfile::tempdir;

use crate::e2e::setup::{self, sleep};

/// Test that when we "run-ledger" with all the possible command
/// combinations from fresh state, the node starts-up successfully.
#[test]
fn run_ledger() -> Result<()> {
    let dir = setup::working_dir();

    let base_dir = tempdir().unwrap();

    let cmd_combinations = vec![
        ("anoma", vec!["ledger"]),
        ("anoma", vec!["ledger", "run"]),
        ("anoma", vec!["node", "ledger"]),
        ("anoma", vec!["node", "ledger", "run"]),
        ("anoman", vec!["ledger"]),
        ("anoman", vec!["ledger", "run"]),
    ];

    // Start the ledger
    for (cmd_name, args) in cmd_combinations {
        let mut cmd = Command::cargo_bin(cmd_name)?;

        cmd.current_dir(&dir)
            .env("ANOMA_LOG", "debug")
            .args(&["--base-dir", &base_dir.path().to_string_lossy()])
            .args(args);

        let cmd_str = format!("{:?}", cmd);

        let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

        session
            .exp_string("Anoma ledger node started")
            .map_err(|e| {
                eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
            })?;
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
    let dir = setup::working_dir();

    let base_dir = tempdir().unwrap();
    let base_dir_arg = &base_dir.path().to_string_lossy();

    // 1. Run the ledger node
    let mut cmd = Command::cargo_bin("anoma")?;
    cmd.current_dir(&dir).env("ANOMA_LOG", "debug").args(&[
        "--base-dir",
        base_dir_arg,
        "ledger",
    ]);
    println!("Running {:?}", cmd);
    let mut session = spawn_command(cmd, Some(20_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    session
        .exp_string("Anoma ledger node started")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // 2. Kill the tendermint node
    sleep(1);
    Command::new("pkill")
        .args(&["tendermint"])
        .spawn()
        .expect("Test failed")
        .wait()
        .expect("Test failed");

    // 3. Check that anoma detects that the tendermint node is dead
    session
        .exp_string("Tendermint node is no longer running.")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // 4. Check that the ledger node shuts down
    session
        .exp_string("Shutting down Anoma node")
        .map_err(|e| eyre!(format!("{}", e)))?;

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
    let dir = setup::working_dir();

    let base_dir = tempdir().unwrap();
    let base_dir_arg = &base_dir.path().to_string_lossy();

    // 1. Run the ledger node
    let mut cmd = Command::cargo_bin("anoma")?;
    cmd.current_dir(&dir).env("ANOMA_LOG", "debug").args(&[
        "--base-dir",
        base_dir_arg,
        "ledger",
    ]);
    println!("Running {:?}", cmd);
    let mut session = spawn_command(cmd, Some(20_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    session
        .exp_string("Anoma ledger node started")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // There should be no previous state
    session
        .exp_string("No state could be found")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // Wait to commit a block
    session
        .exp_regex(r"Committed block hash.*, height: 2")
        .map_err(|e| eyre!(format!("{}", e)))?;
    // 2. Shut it down
    session
        .send_control('c')
        .map_err(|e| eyre!(format!("{}", e)))?;
    drop(session);

    // 3. Run the ledger again, it should load its previous state
    let mut cmd = Command::cargo_bin("anoma")?;
    cmd.current_dir(&dir).env("ANOMA_LOG", "debug").args(&[
        "--base-dir",
        base_dir_arg,
        "ledger",
    ]);
    println!("Running {:?}", cmd);
    let mut session = spawn_command(cmd, Some(20_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    session
        .exp_string("Anoma ledger node started")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // There should be previous state now
    session
        .exp_string("Last state root hash:")
        .map_err(|e| eyre!(format!("{}", e)))?;
    // 4. Shut it down
    session
        .send_control('c')
        .map_err(|e| eyre!(format!("{}", e)))?;
    drop(session);

    // 5. Reset the ledger's state
    let mut cmd = Command::cargo_bin("anoma")?;
    cmd.current_dir(&dir).env("ANOMA_LOG", "debug").args(&[
        "--base-dir",
        base_dir_arg,
        "ledger",
        "reset",
    ]);
    cmd.assert().success();

    // 6. Run the ledger again, it should start from fresh state
    let mut cmd = Command::cargo_bin("anoma")?;
    cmd.current_dir(&dir).env("ANOMA_LOG", "debug").args(&[
        "--base-dir",
        &base_dir.path().to_string_lossy(),
        "ledger",
    ]);
    let mut session = spawn_command(cmd, Some(20_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    session
        .exp_string("Anoma ledger node started")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // There should be no previous state
    session
        .exp_string("No state could be found")
        .map_err(|e| eyre!(format!("{}", e)))?;

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
    let dir = setup::working_dir();

    let base_dir = tempdir().unwrap();
    let base_dir_arg = &base_dir.path().to_string_lossy();

    // 1. Run the ledger node
    let mut cmd = Command::cargo_bin("anoman")?;
    cmd.current_dir(&dir).env("ANOMA_LOG", "debug").args(&[
        "--base-dir",
        base_dir_arg,
        "ledger",
    ]);
    println!("Running {:?}", cmd);
    let mut session = spawn_command(cmd, Some(20_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    session
        .exp_string("Anoma ledger node started")
        .map_err(|e| eyre!(format!("{}", e)))?;
    session
        .exp_string("Started node")
        .map_err(|e| eyre!(format!("{}", e)))?;

    let txs_args = vec![
            // 2. Submit a token transfer tx
            vec![
                "transfer", "--source", BERTHA, "--target", ALBERT, "--token",
                XAN, "--amount", "10.1",
            ],
            // 3. Submit a transaction to update an account's validity
            // predicate
            vec!["update", "--address", BERTHA, "--code-path", VP_USER_WASM],
            // 4. Submit a custom tx
            vec![
                "tx",
                "--code-path",
                TX_NO_OP_WASM,
                "--data-path",
                "README.md",
            ],
            // 5. Submit a tx to initialize a new account
            vec![
                "init-account", 
                "--source", 
                BERTHA,
                "--public-key", 
                // Value obtained from `anoma::types::key::ed25519::tests::gen_keypair`
                "200000001be519a321e29020fa3cbfbfd01bd5e92db134305609270b71dace25b5a21168",
                "--code-path",
                VP_USER_WASM,
                "--alias",
                "test-account"
            ],
        ];
    for tx_args in &txs_args {
        for &dry_run in &[true, false] {
            let mut cmd = Command::cargo_bin("anomac")?;
            cmd.current_dir(&dir)
                .env("ANOMA_LOG", "debug")
                .args(&["--base-dir", base_dir_arg])
                .args(tx_args);
            if dry_run {
                cmd.arg("--dry-run");
            }
            let cmd_str = format!("{:?}", cmd);

            let mut request =
                spawn_command(cmd, Some(20_000)).map_err(|e| {
                    eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
                })?;
            if !dry_run {
                request.exp_string("Mempool validation passed").map_err(
                    |e| {
                        eyre!(format!(
                            "in command: {}\n\nReason: {}",
                            cmd_str, e
                        ))
                    },
                )?;
            }
            request.exp_string("Transaction is valid.").map_err(|e| {
                eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
            })?;

            let status = request.process.wait().unwrap();
            assert_eq!(
                WaitStatus::Exited(request.process.child_pid, 0),
                status
            );
        }
    }

    let query_args_and_expected_response = vec![
        // 6. Query token balance
        (
            vec!["balance", "--owner", BERTHA, "--token", XAN],
            // expect a decimal
            r"XAN: (\d*\.)\d+",
        ),
    ];
    for (query_args, expected) in &query_args_and_expected_response {
        let mut cmd = Command::cargo_bin("anomac")?;
        cmd.current_dir(&dir)
            .env("ANOMA_LOG", "debug")
            .args(&["--base-dir", base_dir_arg])
            .args(query_args);
        let cmd_str = format!("{:?}", cmd);

        let mut session = spawn_command(cmd, Some(10_000)).map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;
        session.exp_regex(expected).map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

        let status = session.process.wait().unwrap();
        assert_eq!(WaitStatus::Exited(session.process.child_pid, 0), status);
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
    let working_dir = setup::working_dir();

    let base_dir = tempdir().unwrap();
    let base_dir_arg = &base_dir.path().to_string_lossy();

    // 1. Run the ledger node
    let mut cmd = Command::cargo_bin("anoman")?;
    cmd.current_dir(&working_dir)
        .env("ANOMA_LOG", "debug")
        .args(&["--base-dir", base_dir_arg, "ledger"]);
    println!("Running {:?}", cmd);
    let mut session = spawn_command(cmd, Some(20_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    session
        .exp_string("Anoma ledger node started")
        .map_err(|e| eyre!(format!("{}", e)))?;
    session
        .exp_string("Started node")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // 2. Submit a an invalid transaction (trying to mint tokens should fail
    // in the token's VP)
    let tx_data_path = base_dir.path().join("tx.data");
    let transfer = token::Transfer {
        source: Address::decode(BERTHA).unwrap(),
        target: Address::decode(ALBERT).unwrap(),
        token: Address::decode(XAN).unwrap(),
        amount: token::Amount::whole(1),
    };
    let data = transfer
        .try_to_vec()
        .expect("Encoding unsigned transfer shouldn't fail");
    let source_key = wallet::defaults::key_of(BERTHA);
    let tx_wasm_path = TX_MINT_TOKENS_WASM;
    let tx_wasm_path_abs = working_dir.join(&tx_wasm_path);
    println!("Reading tx wasm for test from {:?}", tx_wasm_path_abs);
    let tx_code = fs::read(tx_wasm_path_abs).unwrap();
    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    let tx_data = tx.data.unwrap();
    std::fs::write(&tx_data_path, tx_data).unwrap();
    let tx_data_path = tx_data_path.to_string_lossy();
    let tx_args = vec![
        "tx",
        "--code-path",
        tx_wasm_path,
        "--data-path",
        &tx_data_path,
    ];

    let mut cmd = Command::cargo_bin("anomac")?;
    cmd.current_dir(&working_dir)
        .env("ANOMA_LOG", "debug")
        .args(&["--base-dir", base_dir_arg])
        .args(tx_args);

    let cmd_str = format!("{:?}", cmd);

    let mut request = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    request
        .exp_string("Mempool validation passed")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    request.exp_string("Transaction is invalid").map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    request.exp_string(r#""code": "1"#).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    let status = request.process.wait().unwrap();
    assert_eq!(WaitStatus::Exited(request.process.child_pid, 0), status);

    session
        .exp_string("some VPs rejected apply_tx storage modification")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    // Wait to commit a block
    session
        .exp_regex(r"Committed block hash.*, height: 2")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // 3. Shut it down
    session
        .send_control('c')
        .map_err(|e| eyre!(format!("{}", e)))?;
    drop(session);

    // 4. Restart the ledger
    let mut cmd = Command::cargo_bin("anoma")?;
    cmd.current_dir(&working_dir)
        .env("ANOMA_LOG", "debug")
        .args(&["--base-dir", base_dir_arg, "ledger"]);
    println!("Running {:?}", cmd);
    let mut session = spawn_command(cmd, Some(20_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    session
        .exp_string("Anoma ledger node started")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // There should be previous state now
    session
        .exp_string("Last state root hash:")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // 5. Submit and invalid transactions (invalid token address)
    let tx_args = vec![
        "transfer",
        "--source",
        BERTHA,
        "--target",
        ALBERT,
        "--token",
        BERTHA,
        "--amount",
        "1_000_000.1",
    ];
    let mut cmd = Command::cargo_bin("anomac")?;
    cmd.current_dir(&working_dir)
        .env("ANOMA_LOG", "debug")
        .args(&["--base-dir", base_dir_arg])
        .args(tx_args);

    let cmd_str = format!("{:?}", cmd);

    let mut request = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    request
        .exp_string("Mempool validation passed")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    request
        .exp_string("Error trying to apply a transaction")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    request.exp_string(r#""code": "2"#).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;
    let status = request.process.wait().unwrap();
    assert_eq!(WaitStatus::Exited(request.process.child_pid, 0), status);
    Ok(())
}
