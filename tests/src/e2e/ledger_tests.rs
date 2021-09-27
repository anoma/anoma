use std::fs;
use std::path::PathBuf;
use std::process::Command;

use anoma::proto::Tx;
use anoma::types::address::{xan, Address};
use anoma::types::storage::Epoch;
use anoma::types::token;
use anoma::types::transaction::{Fee, WrapperTx};
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
            .env("ANOMA_LOG", "anoma=debug")
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
    cmd.current_dir(&dir)
        .env("ANOMA_LOG", "anoma=debug")
        .args(&["--base-dir", base_dir_arg, "ledger"]);
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
    cmd.current_dir(&dir)
        .env("ANOMA_LOG", "anoma=debug")
        .args(&["--base-dir", base_dir_arg, "ledger"]);
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
    cmd.current_dir(&dir)
        .env("ANOMA_LOG", "anoma=debug")
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
    // 4. Shut it down
    session
        .send_control('c')
        .map_err(|e| eyre!(format!("{}", e)))?;
    drop(session);

    // 5. Reset the ledger's state
    let mut cmd = Command::cargo_bin("anoma")?;
    cmd.current_dir(&dir)
        .env("ANOMA_LOG", "anoma=debug")
        .args(&["--base-dir", base_dir_arg, "ledger", "reset"]);
    cmd.assert().success();

    // 6. Run the ledger again, it should start from fresh state
    let mut cmd = Command::cargo_bin("anoma")?;
    cmd.current_dir(&dir)
        .env("ANOMA_LOG", "anoma=debug")
        .args(&["--base-dir", &base_dir.path().to_string_lossy(), "ledger"]);
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
    cmd.current_dir(&dir)
        .env("ANOMA_LOG", "anoma=debug")
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

    let vp_user = wasm_abs_path(VP_USER_WASM);
    let vp_user = vp_user.to_string_lossy();
    let tx_no_op = wasm_abs_path(TX_NO_OP_WASM);
    let tx_no_op = tx_no_op.to_string_lossy();

    let txs_args = vec![
            // 2. Submit a token transfer tx
            vec![
                "transfer", "--source", BERTHA, "--target", ALBERT, "--token",
                XAN, "--amount", "10.1",
            ],
            // 3. Submit a transaction to update an account's validity
            // predicate
            vec!["update", "--address", BERTHA, "--code-path", &vp_user],
            // 4. Submit a custom tx
            vec![
                "tx",
                "--code-path",
                &tx_no_op,
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
                &vp_user,
                "--alias",
                "test-account"
            ],
        ];
    for tx_args in &txs_args {
        for &dry_run in &[true, false] {
            let mut cmd = Command::cargo_bin("anomac")?;
            cmd.current_dir(&dir)
                .env("ANOMA_LOG", "anoma=debug")
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
                request
                    .exp_string("Process proposal accepted this transaction")
                    .map_err(|e| {
                        eyre!(format!(
                            "in command: {}\n\nReason: {}",
                            cmd_str, e
                        ))
                    })?;
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
            .env("ANOMA_LOG", "anoma=debug")
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
        .env("ANOMA_LOG", "anoma=debug")
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
    let source_key = wallet::defaults::bertha_keypair();
    let tx_wasm_path = wasm_abs_path(TX_MINT_TOKENS_WASM);
    let tx_code = fs::read(&tx_wasm_path).unwrap();
    let tx = Tx::new(tx_code, Some(data)).sign(&source_key);

    let tx_data = tx.data.unwrap();
    std::fs::write(&tx_data_path, tx_data).unwrap();
    let tx_wasm_path = tx_wasm_path.to_string_lossy();
    let tx_data_path = tx_data_path.to_string_lossy();
    let tx_args = vec![
        "tx",
        "--code-path",
        &tx_wasm_path,
        "--data-path",
        &tx_data_path,
    ];

    let mut cmd = Command::cargo_bin("anomac")?;
    cmd.current_dir(&working_dir)
        .env("ANOMA_LOG", "anoma=debug")
        .args(&["--base-dir", base_dir_arg])
        .args(tx_args);

    let cmd_str = format!("{:?}", cmd);

    let mut request = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    request
        .exp_string("Process proposal accepted this transaction")
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
        .env("ANOMA_LOG", "anoma=debug")
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
        .env("ANOMA_LOG", "anoma=debug")
        .args(&["--base-dir", base_dir_arg])
        .args(tx_args);

    let cmd_str = format!("{:?}", cmd);

    let mut request = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    request
        .exp_string("Process proposal accepted this transaction")
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

/// 1. Start the ledger
/// 2. Submit a valid wrapper tx and check it is accepted.
/// Rejected cases:
/// 3. Submit a wrapper tx without signing
/// 4. Submit a wrapper tx signed with wrong key
/// 5. Submit a wrapper tx where the fee > user's balance
#[test]
fn test_wrapper_txs() -> Result<()> {
    use wallet::defaults;
    let working_dir = setup::working_dir();

    let base_dir = tempdir().unwrap();
    let base_dir_arg = &base_dir.path().to_string_lossy();

    let keypair = defaults::daewon_keypair();

    use anoma::types::token::Amount;
    let tx = WrapperTx::new(
        Fee {
            amount: Amount::whole(1_000_000),
            token: xan(),
        },
        &keypair,
        Epoch(1),
        1.into(),
        Tx::new(vec![], Some("transaction data".as_bytes().to_owned())),
    );

    // write out the tx code and data to files
    let mut wasm = PathBuf::from(base_dir.path());
    wasm.push("tx_wasm");
    std::fs::write(&wasm, vec![]).expect("Test failed");
    let wasm_path = wasm.to_str().unwrap();
    let mut data = PathBuf::from(base_dir.path());
    data.push("tx_data");
    std::fs::write(&data, tx.try_to_vec().expect("Test failed"))
        .expect("Test failed");
    let data_path = data.to_str().unwrap();

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

    // 2. Submit a valid wrapper tx and check it is accepted.
    let tx_args = vec![
        "tx",
        "--code-path",
        wasm_path,
        "--data-path",
        data_path,
        "--signing-key",
        "Daewon",
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
    // check that it is accepted by the process proposal method
    request
        .exp_string("Process proposal accepted this transaction")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;
    // check that it is placed on - chain
    request.exp_string("Transaction is valid.").map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;
    drop(request);

    // 3. Submit a wrapper tx without signing
    let tx_args =
        vec!["tx", "--code-path", wasm_path, "--data-path", data_path];
    let mut cmd = Command::cargo_bin("anomac")?;
    cmd.current_dir(&working_dir)
        .env("ANOMA_LOG", "debug")
        .args(&["--base-dir", base_dir_arg])
        .args(tx_args);

    let cmd_str = format!("{:?}", cmd);

    let mut request = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;
    // check that it is rejected by the process proposal method
    request
        .exp_string("Expected signed WrapperTx data")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;
    drop(request);

    // 4. Submit a wrapper tx signed with wrong key
    let tx_args = vec![
        "tx",
        "--code-path",
        wasm_path,
        "--data-path",
        data_path,
        "--signing-key",
        "Albert",
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
    // check that it is rejected by the process proposal method
    request
        .exp_string("Signature verification failed")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;
    drop(request);

    // 5. Submit a wrapper tx where the fee > user's balance
    let tx = WrapperTx::new(
        Fee {
            amount: Amount::whole(1_000_001),
            token: xan(),
        },
        &keypair,
        Epoch(1),
        1.into(),
        Tx::new(vec![], Some("transaction data".as_bytes().to_owned())),
    );

    // write out the tx data to file
    let mut data = PathBuf::from(base_dir.path());
    data.push("tx_data");
    std::fs::write(&data, tx.try_to_vec().expect("Test failed"))
        .expect("Test failed");
    let tx_args = vec![
        "tx",
        "--code-path",
        wasm_path,
        "--data-path",
        data_path,
        "--signing-key",
        "Daewon",
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
    // check that it is rejected by the process proposal method
    request
        .exp_string(
            "The address given does not have sufficient balance to pay fee",
        )
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    Ok(())
}
