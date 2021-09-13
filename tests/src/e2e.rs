//! End-to-end tests for Anoma binaries

#[cfg(test)]
mod tests {
    use core::time;
    use std::fs::OpenOptions;
    use std::path::PathBuf;
    use std::process::Command;
    use std::{fs, thread};

    use anoma::proto::Tx;
    use anoma::types::address::Address;
    use anoma::types::token;
    use anoma_apps::config::{Config, IntentGossiper, Ledger};
    use anoma_apps::wallet;
    use assert_cmd::assert::OutputAssertExt;
    use assert_cmd::cargo::CommandCargoExt;
    use borsh::BorshSerialize;
    use color_eyre::eyre::Result;
    use constants::*;
    use eyre::eyre;
    use libp2p::identity::Keypair;
    use libp2p::PeerId;
    use rexpect::process::wait::WaitStatus;
    use rexpect::session::spawn_command;
    use serde_json::json;
    use tempfile::tempdir;

    /// A helper that should be ran on start of every e2e test case.
    fn setup() -> PathBuf {
        let working_dir = fs::canonicalize("..").unwrap();
        // Build the workspace
        Command::new("cargo")
            .arg("build")
            .current_dir(&working_dir)
            .output()
            .unwrap();
        // Check that tendermint is on $PATH
        Command::new("which").arg("tendermint").assert().success();
        working_dir
    }

    /// Test that when we "run-ledger" with all the possible command
    /// combinations from fresh state, the node starts-up successfully.
    #[test]
    fn run_ledger() -> Result<()> {
        let dir = setup();

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

            let mut session =
                spawn_command(cmd, Some(20_000)).map_err(|e| {
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
        let dir = setup();

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
        std::thread::sleep(std::time::Duration::from_secs(1));
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
        let dir = setup();

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
        let dir = setup();

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
                VP_USER_WASM
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
                        eyre!(format!(
                            "in command: {}\n\nReason: {}",
                            cmd_str, e
                        ))
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

            let mut session =
                spawn_command(cmd, Some(10_000)).map_err(|e| {
                    eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
                })?;
            session.exp_regex(expected).map_err(|e| {
                eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
            })?;

            let status = session.process.wait().unwrap();
            assert_eq!(
                WaitStatus::Exited(session.process.child_pid, 0),
                status
            );
        }

        Ok(())
    }

    /// Test that when we "run-gossip" a peer with no seeds should fail
    /// bootstrapping kademlia. A peer with a seed should be able to
    /// bootstrap kademia and connect to the other peer.
    #[test]
    fn run_gossip() -> Result<()> {
        setup();

        let base_dir = tempdir().unwrap();
        let node_dirs = generate_network_of(
            base_dir.path().to_path_buf(),
            2,
            false,
            true,
            false,
        );

        let first_node_dir = node_dirs[0].0.to_str().unwrap();
        let first_node_peer_id = node_dirs[0].1.to_string();

        let second_node_dir = node_dirs[1].0.to_str().unwrap();
        let second_node_peer_id = node_dirs[1].1.to_string();

        let mut base_node = Command::cargo_bin("anoman")?;
        base_node.env("ANOMA_LOG", "debug,libp2p=debug");
        base_node.args(&["--base-dir", first_node_dir, "gossip", "run"]);

        //  Node without peers
        let mut session = spawn_command(base_node, Some(20_000))
            .map_err(|e| eyre!(format!("{}", e)))?;

        session
            .exp_string(&format!("Peer id: PeerId(\"{}\")", first_node_peer_id))
            .map_err(|e| eyre!(format!("{}", e)))?;

        session
            .exp_string("failed to bootstrap kad : NoKnownPeers")
            .map_err(|e| eyre!(format!("{}", e)))?;

        session
            .exp_string("listening on 127.0.0.1:20201")
            .map_err(|e| eyre!(format!("{}", e)))?;

        session
            .exp_string(
                "HEARTBEAT: Mesh low. Topic: asset_v0 Contains: 0 needs: 2",
            )
            .map_err(|e| eyre!(format!("{}", e)))?;

        drop(session);

        let mut base_node = Command::cargo_bin("anoman")?;
        base_node.args(&["--base-dir", first_node_dir, "gossip"]);

        let mut peer_node = Command::cargo_bin("anoman")?;
        peer_node.args(&["--base-dir", second_node_dir, "gossip"]);

        let mut session = spawn_command(base_node, Some(20_000))
            .map_err(|e| eyre!(format!("{}", e)))?;

        session
            .exp_string(&format!("Peer id: PeerId(\"{}\")", first_node_peer_id))
            .map_err(|e| eyre!(format!("{}", e)))?;

        session
            .exp_regex(&format!(
                ".*(PeerConnected(PeerId(\"{}\")))*",
                second_node_peer_id
            ))
            .map_err(|e| eyre!(format!("{}", e)))?;

        sleep(2);

        let mut session_two = spawn_command(peer_node, Some(20_000))
            .map_err(|e| eyre!(format!("{}", e)))?;

        session_two
            .exp_string(&format!("Peer id: PeerId(\"{}\"", second_node_peer_id))
            .map_err(|e| eyre!(format!("{}", e)))?;

        session_two
            .exp_regex(&format!(
                ".*(PeerConnected(PeerId(\"{}\")))*",
                first_node_peer_id
            ))
            .map_err(|e| eyre!(format!("{}", e)))?;

        drop(session);

        drop(session_two);

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
        let working_dir = setup();

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
        let source_key = wallet::key_of(BERTHA);
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

    /// This test run the ledger and gossip binaries. It then craft 3 intents
    /// and sends them to the matchmaker. The matchmaker should be able to craft
    /// a transfer transaction with the 3 intents.
    #[test]
    fn match_intent() -> Result<()> {
        let working_dir = setup();

        let base_dir = tempdir().unwrap();
        let node_dirs = generate_network_of(
            base_dir.path().to_path_buf(),
            1,
            false,
            true,
            true,
        );

        println!("{}", base_dir.path().to_path_buf().to_string_lossy());

        let _intent_a_path = base_dir.path().to_path_buf().join("intent.A");
        let _intent_b_path = base_dir.path().to_path_buf().join("intent.B");
        let _intent_c_path = base_dir.path().to_path_buf().join("intent.C");

        let intent_a_path_input =
            base_dir.path().to_path_buf().join("intent.A.data");
        let intent_b_path_input =
            base_dir.path().to_path_buf().join("intent.B.data");
        let intent_c_path_input =
            base_dir.path().to_path_buf().join("intent.C.data");

        let intent_a_json = json!([
            {
                "key": BERTHA,
                "addr": BERTHA,
                "min_buy": "100.0",
                "max_sell": "70",
                "token_buy": XAN,
                "token_sell": BTC,
                "rate_min": "2",
                "vp_path": working_dir.join(VP_ALWAYS_TRUE_WASM).to_string_lossy().into_owned(),
            }
        ]);

        let intent_b_json = json!([
            {
                "key": ALBERT,
                "addr": ALBERT,
                "min_buy": "50",
                "max_sell": "300",
                "token_buy": BTC,
                "token_sell": ETH,
                "rate_min": "0.7"
            }
        ]);
        let intent_c_json = json!([
            {
                "key": CHRISTEL,
                "addr": CHRISTEL,
                "min_buy": "20",
                "max_sell": "200",
                "token_buy": ETH,
                "token_sell": XAN,
                "rate_min": "0.5"
            }
        ]);
        generate_intent_json(intent_a_path_input.clone(), intent_a_json);
        generate_intent_json(intent_b_path_input.clone(), intent_b_json);
        generate_intent_json(intent_c_path_input.clone(), intent_c_json);

        let first_node_dir = node_dirs[0].0.to_str().unwrap();

        let mut base_node_gossip = Command::cargo_bin("anoman")?;
        base_node_gossip.args(&["--base-dir", first_node_dir, "gossip"]);

        let mut base_node_ledger = Command::cargo_bin("anoman")?;
        base_node_ledger.current_dir(&working_dir).args(&[
            "--base-dir",
            first_node_dir,
            "ledger",
        ]);

        //  Start gossip
        let mut session_gossip = spawn_command(base_node_gossip, Some(60_000))
            .map_err(|e| eyre!(format!("{}", e)))?;

        //  Start ledger
        let mut session_ledger = spawn_command(base_node_ledger, Some(60_000))
            .map_err(|e| eyre!(format!("{}", e)))?;

        session_ledger
            .exp_string("No state could be found")
            .map_err(|e| eyre!(format!("{}", e)))?;

        // Wait gossip to start
        sleep(3);

        // cargo run --bin anomac -- intent --node "http://127.0.0.1:39111" --data-path intent.A --topic "asset_v1"
        // cargo run --bin anomac -- intent --node "http://127.0.0.1:39112" --data-path intent.B --topic "asset_v1"
        // cargo run --bin anomac -- intent --node "http://127.0.0.1:39112" --data-path intent.C --topic "asset_v1"
        //  Send intent A
        let mut send_intent_a = Command::cargo_bin("anomac")?;
        send_intent_a.args(&[
            "intent",
            "--node",
            "http://127.0.0.1:39111",
            "--data-path",
            intent_a_path_input.to_str().unwrap(),
            "--topic",
            "asset_v1",
            "--key",
            BERTHA,
        ]);

        let mut session_send_intent_a =
            spawn_command(send_intent_a, Some(20_000))
                .map_err(|e| eyre!(format!("{}", e)))?;

        // means it sent it correctly but not able to gossip it (which is
        // correct since there is only 1 node)
        session_send_intent_a
            .exp_regex(".*Failed to publish_intent InsufficientPeers*")
            .map_err(|e| eyre!(format!("{}", e)))?;
        drop(session_send_intent_a);

        session_gossip
            .exp_regex(".*trying to match new intent*")
            .map_err(|e| eyre!(format!("{}", e)))?;

        // Send intent B
        let mut send_intent_b = Command::cargo_bin("anomac")?;
        send_intent_b.args(&[
            "intent",
            "--node",
            "http://127.0.0.1:39111",
            "--data-path",
            intent_b_path_input.to_str().unwrap(),
            "--topic",
            "asset_v1",
            "--key",
            ALBERT,
        ]);
        let mut session_send_intent_b =
            spawn_command(send_intent_b, Some(20_000))
                .map_err(|e| eyre!(format!("{}", e)))?;

        // means it sent it correctly but not able to gossip it (which is
        // correct since there is only 1 node)
        session_send_intent_b
            .exp_regex(".*Failed to publish_intent InsufficientPeers*")
            .map_err(|e| eyre!(format!("{}", e)))?;
        drop(session_send_intent_b);

        session_gossip
            .exp_regex(".*trying to match new intent*")
            .map_err(|e| eyre!(format!("{}", e)))?;

        // Send intent C
        let mut send_intent_c = Command::cargo_bin("anomac")?;
        send_intent_c.args(&[
            "intent",
            "--node",
            "http://127.0.0.1:39111",
            "--data-path",
            intent_c_path_input.to_str().unwrap(),
            "--topic",
            "asset_v1",
            "--key",
            CHRISTEL,
        ]);
        let mut session_send_intent_c =
            spawn_command(send_intent_c, Some(20_000))
                .map_err(|e| eyre!(format!("{}", e)))?;

        // means it sent it correctly but not able to gossip it (which is
        // correct since there is only 1 node)
        session_send_intent_c
            .exp_string("Failed to publish_intent InsufficientPeers")
            .map_err(|e| eyre!(format!("{}", e)))?;
        drop(session_send_intent_c);

        // check that the transfers transactions are correct
        session_gossip
            .exp_string("crafting transfer: Established: a1qq5qqqqqxv6yydz9xc6ry33589q5x33eggcnjs2xx9znydj9xuens3phxppnwvzpg4rrqdpswve4n9, Established: a1qq5qqqqqg4znssfsgcurjsfhgfpy2vjyxy6yg3z98pp5zvp5xgersvfjxvcnx3f4xycrzdfkak0xhx, 70")
            .map_err(|e| eyre!(format!("{}", e)))?;

        session_gossip
            .exp_string("crafting transfer: Established: a1qq5qqqqqxsuygd2x8pq5yw2ygdryxs6xgsmrsdzx8pryxv34gfrrssfjgccyg3zpxezrqd2y2s3g5s, Established: a1qq5qqqqqxv6yydz9xc6ry33589q5x33eggcnjs2xx9znydj9xuens3phxppnwvzpg4rrqdpswve4n9, 200")
            .map_err(|e| eyre!(format!("{}", e)))?;

        session_gossip
            .exp_string("crafting transfer: Established: a1qq5qqqqqg4znssfsgcurjsfhgfpy2vjyxy6yg3z98pp5zvp5xgersvfjxvcnx3f4xycrzdfkak0xhx, Established: a1qq5qqqqqxsuygd2x8pq5yw2ygdryxs6xgsmrsdzx8pryxv34gfrrssfjgccyg3zpxezrqd2y2s3g5s, 100")
            .map_err(|e| eyre!(format!("{}", e)))?;

        // check that the intent vp passes evaluation
        session_ledger
            .exp_string("eval result: true")
            .map_err(|e| eyre!(format!("{}", e)))?;

        Ok(())
    }

    fn generate_intent_json(
        intent_path: PathBuf,
        exchange_json: serde_json::Value,
    ) {
        let intent_writer = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(intent_path)
            .unwrap();
        serde_json::to_writer(intent_writer, &exchange_json).unwrap();
    }

    /// Returns directories with generated config files that should be used as
    /// the `--base-dir` for Anoma commands. The first intent gossiper node is
    /// setup to also open RPC for receiving intents and run a matchmaker.
    fn generate_network_of(
        path: PathBuf,
        n_of_peers: u32,
        with_mdns: bool,
        with_kademlia: bool,
        with_matchmaker: bool,
    ) -> Vec<(PathBuf, PeerId)> {
        let mut index = 0;

        let mut node_dirs: Vec<(PathBuf, PeerId)> = Vec::new();

        while index < n_of_peers {
            let node_path = path.join(format!("anoma-{}", index));

            let mut config = Config {
                ledger: Some(Ledger {
                    tendermint: node_path.join("tendermint").to_path_buf(),
                    db: node_path.join("db").to_path_buf(),
                    ..Default::default()
                }),
                ..Config::default()
            };

            let info = build_peers(index, node_dirs.clone());

            let gossiper_config = IntentGossiper::default_with_address(
                "127.0.0.1".to_string(),
                20201 + index,
                info,
                with_mdns,
                with_kademlia,
                index == 0 && with_matchmaker,
                index == 0 && with_matchmaker,
            );
            let peer_key =
                Keypair::Ed25519(gossiper_config.gossiper.key.clone());
            let peer_id = PeerId::from(peer_key.public());

            node_dirs.push((node_path.clone(), peer_id));

            config.intent_gossiper = Some(gossiper_config);

            config.write(&node_path, false).unwrap();
            index += 1;
        }
        node_dirs
    }

    fn sleep(seconds: u64) {
        thread::sleep(time::Duration::from_secs(seconds));
    }

    fn build_peers(
        index: u32,
        network: Vec<(PathBuf, PeerId)>,
    ) -> Vec<(String, u32, PeerId)> {
        if index > 0 {
            return vec![(
                "127.0.0.1".to_string(),
                20201 + index - 1,
                network[index as usize - 1].1,
            )];
        }
        return vec![];
    }

    #[cfg(test)]
    #[allow(dead_code)]
    mod constants {

        // User addresses
        pub const ALBERT: &str = "a1qq5qqqqqg4znssfsgcurjsfhgfpy2vjyxy6yg3z98pp5zvp5xgersvfjxvcnx3f4xycrzdfkak0xhx";
        pub const BERTHA: &str = "a1qq5qqqqqxv6yydz9xc6ry33589q5x33eggcnjs2xx9znydj9xuens3phxppnwvzpg4rrqdpswve4n9";
        pub const CHRISTEL: &str = "a1qq5qqqqqxsuygd2x8pq5yw2ygdryxs6xgsmrsdzx8pryxv34gfrrssfjgccyg3zpxezrqd2y2s3g5s";

        // Fungible token addresses
        pub const XAN: &str = "a1qq5qqqqqxuc5gvz9gycryv3sgye5v3j9gvurjv34g9prsd6x8qu5xs2ygdzrzsf38q6rss33xf42f3";
        pub const BTC: &str = "a1qq5qqqqq8q6yy3p4xyurys3n8qerz3zxxeryyv6rg4pnxdf3x3pyv32rx3zrgwzpxu6ny32r3laduc";
        pub const ETH: &str = "a1qq5qqqqqx3z5xd3ngdqnzwzrgfpnxd3hgsuyx3phgfry2s3kxsc5xves8qe5x33sgdprzvjptzfry9";
        pub const DOT: &str = "a1qq5qqqqqxq652v3sxap523fs8pznjse5g3pyydf3xqurws6ygvc5gdfcxyuy2deeggenjsjrjrl2ph";

        // Bite-sized tokens
        pub const SCHNITZEL: &str = "a1qq5qqqqq8prrzv6xxcury3p4xucygdp5gfprzdfex9prz3jyg56rxv69gvenvsj9g5enswpcl8npyz";
        pub const APFEL: &str = "a1qq5qqqqqgfp52de4x56nqd3ex56y2wph8pznssjzx5ersw2pxfznsd3jxeqnjd3cxapnqsjz2fyt3j";
        pub const KARTOFFEL: &str = "a1qq5qqqqqxs6yvsekxuuyy3pjxsmrgd2rxuungdzpgsmyydjrxsenjdp5xaqn233sgccnjs3eak5wwh";

        // Paths to the WASMs used for tests
        pub const TX_TRANSFER_WASM: &str = "wasm/tx_transfer.wasm";
        pub const VP_USER_WASM: &str = "wasm/vp_user.wasm";
        pub const TX_NO_OP_WASM: &str = "wasm_for_tests/tx_no_op.wasm";
        pub const VP_ALWAYS_TRUE_WASM: &str =
            "wasm_for_tests/vp_always_true.wasm";
        pub const VP_ALWAYS_FALSE_WASM: &str =
            "wasm_for_tests/vp_always_false.wasm";
        pub const TX_MINT_TOKENS_WASM: &str =
            "wasm_for_tests/tx_mint_tokens.wasm";
    }
}
