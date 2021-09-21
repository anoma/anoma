use std::path::PathBuf;
use std::process::Command;
use std::{fs, thread, time};

use anoma_apps::config::{Config, IntentGossiper, Ledger};
use assert_cmd::assert::OutputAssertExt;
use libp2p::identity::Keypair;
use libp2p::PeerId;

/// A helper that should be ran on start of every e2e test case.
pub fn working_dir() -> PathBuf {
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

/// Returns directories with generated config files that should be used as
/// the `--base-dir` for Anoma commands. The first intent gossiper node is
/// setup to also open RPC for receiving intents and run a matchmaker.
pub fn generate_network_of(
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
        let peer_key = Keypair::Ed25519(gossiper_config.gossiper.key.clone());
        let peer_id = PeerId::from(peer_key.public());

        node_dirs.push((node_path.clone(), peer_id));

        config.intent_gossiper = Some(gossiper_config);

        config.write(&node_path, false).unwrap();
        index += 1;
    }
    node_dirs
}

pub fn sleep(seconds: u64) {
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

#[allow(dead_code)]
pub mod constants {

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
    pub const VP_ALWAYS_TRUE_WASM: &str = "wasm_for_tests/vp_always_true.wasm";
    pub const VP_ALWAYS_FALSE_WASM: &str =
        "wasm_for_tests/vp_always_false.wasm";
    pub const TX_MINT_TOKENS_WASM: &str = "wasm_for_tests/tx_mint_tokens.wasm";
}
