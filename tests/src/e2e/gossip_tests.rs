//! By default, these tests will run in release mode. This can be disabled
//! by setting environment variable `ANOMA_E2E_DEBUG=true`. For debugging,
//! you'll typically also want to set `RUST_BACKTRACE=1`, e.g.:
//!
//! ```ignore,shell
//! ANOMA_E2E_DEBUG=true RUST_BACKTRACE=1 cargo test e2e::gossip_tests -- --test-threads=1 --nocapture
//! ```
//!
//! To keep the temporary files created by a test, use env var
//! `ANOMA_E2E_KEEP_TEMP=true`.

use std::env;
use std::fs::OpenOptions;
use std::path::PathBuf;

use color_eyre::eyre::Result;
use escargot::CargoBuild;
use serde_json::json;
use setup::constants::*;

use super::setup::ENV_VAR_DEBUG;
use crate::e2e::helpers::find_address;
use crate::e2e::setup::{self, Bin, Who};
use crate::{run, run_as};

/// Test that when we "run-gossip" a peer with no seeds should fail
/// bootstrapping kademlia. A peer with a seed should be able to
/// bootstrap kademia and connect to the other peer.
#[test]
fn run_gossip() -> Result<()> {
    let test =
        setup::network(|genesis| setup::add_validators(1, genesis), None)?;

    let mut cmd =
        run_as!(test, Who::Validator(0), Bin::Node, &["gossip"], Some(20),)?;
    // Node without peers
    cmd.exp_regex(r"Peer id: PeerId\(.*\)")?;

    drop(cmd);

    let mut first_node =
        run_as!(test, Who::Validator(0), Bin::Node, &["gossip"], Some(20),)?;
    let (_unread, matched) = first_node.exp_regex(r"Peer id: PeerId\(.*\)")?;
    let first_node_peer_id = matched
        .trim()
        .rsplit_once("\"")
        .unwrap()
        .0
        .rsplit_once("\"")
        .unwrap()
        .1;

    let mut second_node =
        run_as!(test, Who::Validator(1), Bin::Node, &["gossip"], Some(20),)?;

    second_node.exp_regex(r"Peer id: PeerId\(.*\)")?;
    second_node.exp_string(&format!(
        "Connect to a new peer: PeerId(\"{}\")",
        first_node_peer_id
    ))?;

    Ok(())
}

/// This test runs a ledger node and 2 gossip nodes. It then crafts 3 intents
/// and sends them to the matchmaker. The matchmaker should be able to match
/// them into a transfer transaction and submit it to the ledger.
#[test]
fn match_intents() -> Result<()> {
    let test = setup::single_node_net()?;

    // Make sure that the default matchmaker is built
    println!("Building the matchmaker \"mm_token_exch\" implementation...");
    let run_debug = match env::var(ENV_VAR_DEBUG) {
        Ok(val) => val.to_ascii_lowercase() != "false",
        _ => false,
    };
    let manifest_path = test
        .working_dir
        .join("matchmaker")
        .join("mm_token_exch")
        .join("Cargo.toml");
    let cmd = if !cfg!(feature = "ABCI") {
        CargoBuild::new()
            .manifest_path(manifest_path)
            .no_default_features()
            .features("ABCI-plus-plus")
    } else {
        CargoBuild::new()
            .manifest_path(manifest_path)
            .features("ABCI")
    };
    let cmd = if run_debug { cmd } else { cmd.release() };
    let msgs = cmd.exec().unwrap();
    for msg in msgs {
        msg.unwrap();
    }
    println!("Done building the matchmaker.");

    let mut ledger =
        run_as!(test, Who::Validator(0), Bin::Node, &["ledger"], Some(20),)?;
    ledger.exp_string("Anoma ledger node started")?;
    ledger.exp_string("No state could be found")?;
    // Wait to commit a block
    ledger.exp_regex(r"Committed block hash.*, height: [0-9]+")?;

    let intent_a_path_input = test.base_dir.path().join("intent.A.data");
    let intent_b_path_input = test.base_dir.path().join("intent.B.data");
    let intent_c_path_input = test.base_dir.path().join("intent.C.data");

    let albert = find_address(&test, ALBERT)?;
    let bertha = find_address(&test, BERTHA)?;
    let christel = find_address(&test, CHRISTEL)?;
    let xan = find_address(&test, XAN)?;
    let btc = find_address(&test, BTC)?;
    let eth = find_address(&test, ETH)?;
    let intent_a_json = json!([
        {
            "key": bertha,
            "addr": bertha,
            "min_buy": "100.0",
            "max_sell": "70",
            "token_buy": xan,
            "token_sell": btc,
            "rate_min": "2",
            "vp_path": test.working_dir.join(VP_ALWAYS_TRUE_WASM).to_string_lossy().into_owned(),
        }
    ]);

    let intent_b_json = json!([
        {
            "key": albert,
            "addr": albert,
            "min_buy": "50",
            "max_sell": "300",
            "token_buy": btc,
            "token_sell": eth,
            "rate_min": "0.7"
        }
    ]);
    let intent_c_json = json!([
        {
            "key": christel,
            "addr": christel,
            "min_buy": "20",
            "max_sell": "200",
            "token_buy": eth,
            "token_sell": xan,
            "rate_min": "0.5"
        }
    ]);
    generate_intent_json(intent_a_path_input.clone(), intent_a_json);
    generate_intent_json(intent_b_path_input.clone(), intent_b_json);
    generate_intent_json(intent_c_path_input.clone(), intent_c_json);

    // Start gossip
    let mut session_gossip = run_as!(
        test,
        Who::Validator(0),
        Bin::Node,
        &[
            "gossip",
            "--source",
            "matchmaker",
            "--signing-key",
            "matchmaker-key",
        ],
        Some(20),
    )?;

    // Wait gossip to start
    session_gossip.exp_string("RPC started at 127.0.0.1:26660")?;

    // cargo run --bin anomac -- intent --node "http://127.0.0.1:26660" --data-path intent.A --topic "asset_v1"
    // cargo run --bin anomac -- intent --node "http://127.0.0.1:26660" --data-path intent.B --topic "asset_v1"
    // cargo run --bin anomac -- intent --node "http://127.0.0.1:26660" --data-path intent.C --topic "asset_v1"
    //  Send intent A
    let mut session_send_intent_a = run!(
        test,
        Bin::Client,
        &[
            "intent",
            "--node",
            "http://127.0.0.1:26660",
            "--data-path",
            intent_a_path_input.to_str().unwrap(),
            "--topic",
            "asset_v1",
            "--signing-key",
            BERTHA_KEY,
        ],
        Some(40),
    )?;

    // means it sent it correctly but not able to gossip it (which is
    // correct since there is only 1 node)
    session_send_intent_a.exp_string(
        "Failed to publish intent in gossiper: InsufficientPeers",
    )?;
    drop(session_send_intent_a);

    session_gossip.exp_string("trying to match new intent")?;

    // Send intent B
    let mut session_send_intent_b = run!(
        test,
        Bin::Client,
        &[
            "intent",
            "--node",
            "http://127.0.0.1:26660",
            "--data-path",
            intent_b_path_input.to_str().unwrap(),
            "--topic",
            "asset_v1",
            "--signing-key",
            ALBERT_KEY,
        ],
        Some(40),
    )?;

    // means it sent it correctly but not able to gossip it (which is
    // correct since there is only 1 node)
    session_send_intent_b.exp_string(
        "Failed to publish intent in gossiper: InsufficientPeers",
    )?;
    drop(session_send_intent_b);

    session_gossip.exp_string("trying to match new intent")?;

    // Send intent C
    let mut session_send_intent_c = run!(
        test,
        Bin::Client,
        &[
            "intent",
            "--node",
            "http://127.0.0.1:26660",
            "--data-path",
            intent_c_path_input.to_str().unwrap(),
            "--topic",
            "asset_v1",
            "--signing-key",
            CHRISTEL_KEY,
        ],
        Some(40),
    )?;

    // means it sent it correctly but not able to gossip it (which is
    // correct since there is only 1 node)
    session_send_intent_c.exp_string(
        "Failed to publish intent in gossiper: InsufficientPeers",
    )?;
    drop(session_send_intent_c);

    // check that the transfers transactions are correct
    session_gossip.exp_string(&format!(
        "crafting transfer: {}, {}, 70",
        bertha, albert
    ))?;
    session_gossip.exp_string(&format!(
        "crafting transfer: {}, {}, 200",
        christel, bertha
    ))?;
    session_gossip.exp_string(&format!(
        "crafting transfer: {}, {}, 100",
        albert, christel
    ))?;

    // check that the all VPs accept the transaction
    ledger.exp_string("all VPs accepted apply_tx storage modification")?;

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
