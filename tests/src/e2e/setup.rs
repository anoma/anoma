use std::path::PathBuf;
use std::process::Command;
use std::{fs, thread, time};

use anoma::types::chain::ChainId;
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

    let chain_id = ChainId::default();

    while index < n_of_peers {
        let node_path = path.join(format!("anoma-{}", index));

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

        let config = Config {
            ledger: Ledger::new(&node_path, chain_id.clone()),
            intent_gossiper: gossiper_config,
        };

        config.write(&node_path, &chain_id, false).unwrap();
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
    use std::fs;
    use std::path::PathBuf;

    // User addresses
    pub const ALBERT: &str = "atest1v4ehgw368ycryv2z8qcnxv3cxgmrgvjpxs6yg333gym5vv2zxepnj334g4rryvj9xucrgve4x3xvr4";
    pub const BERTHA: &str = "atest1v4ehgw36xvcyyvejgvenxs34g3zygv3jxqunjd6rxyeyys3sxy6rwvfkx4qnj33hg9qnvse4lsfctw";
    pub const CHRISTEL: &str = "atest1v4ehgw36x3qng3jzggu5yvpsxgcngv2xgguy2dpkgvu5x33kx3pr2w2zgep5xwfkxscrxs2pj8075p";
    pub const DAEWON: &str = "atest1d9khqw36xprrzdpk89rrws69g4z5vd6pgv65gvjrgeqnv3pcg4zns335xymry335gcerqs3etd0xfa";

    // Fungible token addresses
    pub const XAN: &str = "atest1v4ehgw36x3prswzxggunzv6pxqmnvdj9xvcyzvpsggeyvs3cg9qnywf589qnwvfsg5erg3fkl09rg5";
    pub const BTC: &str = "atest1v4ehgw36xdzryve5gsc52veeg5cnsv2yx5eygvp38qcrvd29xy6rys6p8yc5xvp4xfpy2v694wgwcp";
    pub const ETH: &str = "atest1v4ehgw36xqmr2d3nx3ryvd2xxgmrq33j8qcns33sxezrgv6zxdzrydjrxveygd2yxumrsdpsf9jc2p";
    pub const DOT: &str = "atest1v4ehgw36gg6nvs2zgfpyxsfjgc65yv6pxy6nwwfsxgungdzrggeyzv35gveyxsjyxymyz335hur2jn";

    // Bite-sized tokens
    pub const SCHNITZEL: &str = "atest1v4ehgw36xue5xvf5xvuyzvpjx5un2v3k8qeyvd3cxdqns32p89rrxd6xx9zngvpegccnzs699rdnnt";
    pub const APFEL: &str = "atest1v4ehgw36gfryydj9g3p5zv3kg9znyd358ycnzsfcggc5gvecgc6ygs2rxv6ry3zpg4zrwdfeumqcz9";
    pub const KARTOFFEL: &str = "atest1v4ehgw36gep5ysecxq6nyv3jg3zygv3e89qn2vp48pryxsf4xpznvve5gvmy23fs89pryvf5a6ht90";

    // Paths to the WASMs used for tests
    pub const TX_TRANSFER_WASM: &str = "wasm/tx_transfer.wasm";
    pub const VP_USER_WASM: &str = "wasm/vp_user.wasm";
    pub const TX_NO_OP_WASM: &str = "wasm_for_tests/tx_no_op.wasm";
    pub const VP_ALWAYS_TRUE_WASM: &str = "wasm_for_tests/vp_always_true.wasm";
    pub const VP_ALWAYS_FALSE_WASM: &str =
        "wasm_for_tests/vp_always_false.wasm";
    pub const TX_MINT_TOKENS_WASM: &str = "wasm_for_tests/tx_mint_tokens.wasm";

    /// Find the absolute path to one of the WASM files above
    pub fn wasm_abs_path(file_name: &str) -> PathBuf {
        let working_dir = fs::canonicalize("..").unwrap();
        working_dir.join(file_name)
    }
}
