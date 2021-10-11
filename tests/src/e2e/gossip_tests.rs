use std::fs::OpenOptions;
use std::path::PathBuf;
use std::process::Command;

use assert_cmd::cargo::CommandCargoExt;
use color_eyre::eyre::Result;
use eyre::eyre;
use rexpect::session::spawn_command;
use serde_json::json;
use setup::constants::*;
use tempfile::tempdir;

use crate::e2e::setup::{self, generate_network_of, sleep};

/// Test that when we "run-gossip" a peer with no seeds should fail
/// bootstrapping kademlia. A peer with a seed should be able to
/// bootstrap kademia and connect to the other peer.
#[test]
fn run_gossip() -> Result<()> {
    setup::working_dir();

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
    base_node.env("ANOMA_LOG", "anoma=debug,libp2p=debug");
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
        .exp_string("HEARTBEAT: Mesh low. Topic: asset_v0 Contains: 0 needs: 2")
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

/// This test run the ledger and gossip binaries. It then craft 3 intents
/// and sends them to the matchmaker. The matchmaker should be able to craft
/// a transfer transaction with the 3 intents.
#[test]
fn match_intent() -> Result<()> {
    let working_dir = setup::working_dir();

    let base_dir = tempdir().unwrap();
    let node_dirs = generate_network_of(
        base_dir.path().to_path_buf(),
        1,
        false,
        true,
        true,
    );
    let first_node_dir = node_dirs[0].0.to_str().unwrap();

    println!("{}", base_dir.path().to_path_buf().to_string_lossy());

    let mut base_node_ledger = Command::cargo_bin("anoman")?;
    base_node_ledger.current_dir(&working_dir).args(&[
        "--base-dir",
        first_node_dir,
        "ledger",
    ]);

    // Start ledger
    let mut session_ledger = spawn_command(base_node_ledger, Some(60_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    session_ledger
        .exp_string("No state could be found")
        .map_err(|e| eyre!(format!("{}", e)))?;

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

    let mut base_node_gossip = Command::cargo_bin("anoman")?;
    base_node_gossip.args(&[
        "--base-dir",
        first_node_dir,
        "gossip",
        "--source",
        "matchmaker",
        "--signing-key",
        "matchmaker",
    ]);

    //  Start gossip
    let mut session_gossip = spawn_command(base_node_gossip, Some(100_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    // Wait gossip to start
    sleep(3);

    // cargo run --bin anomac -- intent --node "http://127.0.0.1:39111" --data-path intent.A --topic "asset_v1"
    // cargo run --bin anomac -- intent --node "http://127.0.0.1:39112" --data-path intent.B --topic "asset_v1"
    // cargo run --bin anomac -- intent --node "http://127.0.0.1:39112" --data-path intent.C --topic "asset_v1"
    //  Send intent A
    let mut send_intent_a = Command::cargo_bin("anomac")?;
    send_intent_a.args(&[
        "--base-dir",
        first_node_dir,
        "intent",
        "--node",
        "http://127.0.0.1:39111",
        "--data-path",
        intent_a_path_input.to_str().unwrap(),
        "--topic",
        "asset_v1",
        "--signing-key",
        "Bertha",
    ]);

    let mut session_send_intent_a = spawn_command(send_intent_a, Some(40_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    // means it sent it correctly but not able to gossip it (which is
    // correct since there is only 1 node)
    session_send_intent_a
        .exp_string("Failed to publish intent in gossiper: InsufficientPeers")
        .map_err(|e| eyre!(format!("{}", e)))?;
    drop(session_send_intent_a);

    session_gossip
        .exp_string("trying to match new intent")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // Send intent B
    let mut send_intent_b = Command::cargo_bin("anomac")?;
    send_intent_b.args(&[
        "--base-dir",
        first_node_dir,
        "intent",
        "--node",
        "http://127.0.0.1:39111",
        "--data-path",
        intent_b_path_input.to_str().unwrap(),
        "--topic",
        "asset_v1",
        "--signing-key",
        "Albert",
    ]);
    let mut session_send_intent_b = spawn_command(send_intent_b, Some(40_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    // means it sent it correctly but not able to gossip it (which is
    // correct since there is only 1 node)
    session_send_intent_b
        .exp_string("Failed to publish intent in gossiper: InsufficientPeers")
        .map_err(|e| eyre!(format!("{}", e)))?;
    drop(session_send_intent_b);

    session_gossip
        .exp_string("trying to match new intent")
        .map_err(|e| eyre!(format!("{}", e)))?;

    // Send intent C
    let mut send_intent_c = Command::cargo_bin("anomac")?;
    send_intent_c.args(&[
        "--base-dir",
        first_node_dir,
        "intent",
        "--node",
        "http://127.0.0.1:39111",
        "--data-path",
        intent_c_path_input.to_str().unwrap(),
        "--topic",
        "asset_v1",
        "--signing-key",
        "Christel",
    ]);
    let mut session_send_intent_c = spawn_command(send_intent_c, Some(40_000))
        .map_err(|e| eyre!(format!("{}", e)))?;

    // means it sent it correctly but not able to gossip it (which is
    // correct since there is only 1 node)
    session_send_intent_c
        .exp_string("Failed to publish intent in gossiper: InsufficientPeers")
        .map_err(|e| eyre!(format!("{}", e)))?;
    drop(session_send_intent_c);

    // check that the transfers transactions are correct
    session_gossip
            .exp_string("crafting transfer: Established: atest1v4ehgw36xvcyyvejgvenxs34g3zygv3jxqunjd6rxyeyys3sxy6rwvfkx4qnj33hg9qnvse4lsfctw, Established: atest1v4ehgw368ycryv2z8qcnxv3cxgmrgvjpxs6yg333gym5vv2zxepnj334g4rryvj9xucrgve4x3xvr4, 70")
            .map_err(|e| eyre!(format!("{}", e)))?;

    session_gossip
            .exp_string("crafting transfer: Established: atest1v4ehgw36x3qng3jzggu5yvpsxgcngv2xgguy2dpkgvu5x33kx3pr2w2zgep5xwfkxscrxs2pj8075p, Established: atest1v4ehgw36xvcyyvejgvenxs34g3zygv3jxqunjd6rxyeyys3sxy6rwvfkx4qnj33hg9qnvse4lsfctw, 200")
            .map_err(|e| eyre!(format!("{}", e)))?;

    session_gossip
            .exp_string("crafting transfer: Established: atest1v4ehgw368ycryv2z8qcnxv3cxgmrgvjpxs6yg333gym5vv2zxepnj334g4rryvj9xucrgve4x3xvr4, Established: atest1v4ehgw36x3qng3jzggu5yvpsxgcngv2xgguy2dpkgvu5x33kx3pr2w2zgep5xwfkxscrxs2pj8075p, 100")
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
