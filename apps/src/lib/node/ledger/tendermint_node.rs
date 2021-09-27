use std::fs::{self, File, OpenOptions};
use std::io::{BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;
use std::sync::mpsc::Receiver;
use std::time::Duration;

use anoma::types::address::Address;
use anoma::types::key::ed25519::Keypair;
use serde_json::json;
use signal_hook::consts::TERM_SIGNALS;
use signal_hook::iterator::Signals;
use tendermint::config::TendermintConfig;
use tendermint::net;
use thiserror::Error;

use crate::config;
use crate::std::sync::mpsc::Sender;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to initialize Tendermint: {0}")]
    Init(std::io::Error),
    #[error("Failed to load Tendermint config file: {0}")]
    LoadConfig(tendermint::error::Error),
    #[error("Failed to open Tendermint config for writing: {0}")]
    OpenWriteConfig(std::io::Error),
    #[error("Failed to serialize Tendermint config TOML to string: {0}")]
    ConfigSerializeToml(toml::ser::Error),
    #[error("Failed to write Tendermint config: {0}")]
    WriteConfig(std::io::Error),
    #[error("Failed to start up Tendermint node: {0}")]
    StartUp(std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

/// Run the tendermint node.
pub fn run(
    home_dir: PathBuf,
    ledger_address: String,
    rpc_address: String,
    p2p_address: String,
    kill_switch: Sender<bool>,
    receiver: Receiver<bool>,
) -> Result<()> {
    let home_dir_string = home_dir.to_string_lossy().to_string();
    let rpc_address: net::Address =
        net::Address::from_str(&rpc_address).unwrap();
    let p2p_address: net::Address =
        net::Address::from_str(&p2p_address).unwrap();

    #[cfg(feature = "dev")]
    // This has to be checked before we run tendermint init
    let has_validator_key = {
        let path = home_dir.join("config").join("priv_validator_key.json");
        Path::new(&path).exists()
    };

    // init and run a tendermint node child process
    let output = Command::new("tendermint")
        .args(&["init", "--home", &home_dir_string])
        .output()
        .map_err(Error::Init)?;
    if !output.status.success() {
        panic!("Tendermint failed to initialize with {:#?}", output);
    }

    write_chain_id(&home_dir, config::DEFAULT_CHAIN_ID);

    #[cfg(feature = "dev")]
    {
        let genesis = &crate::config::genesis::genesis();
        // write the validator key file if it didn't already exist
        if !has_validator_key {
            write_validator_key(
                &home_dir,
                &genesis
                    .validators
                    .first()
                    .expect(
                        "There should be one genesis validator in \"dev\" mode",
                    )
                    .pos_data
                    .address,
                &crate::wallet::defaults::validator_keypair(),
            );
        }
    }

    update_tendermint_config(&home_dir, rpc_address, p2p_address)?;

    let tendermint_node = Command::new("tendermint")
        .args(&[
            "node",
            "--proxy_app",
            &ledger_address,
            "--home",
            &home_dir_string,
        ])
        .spawn()
        .map_err(Error::StartUp)?;
    let pid = tendermint_node.id();
    tracing::info!("Tendermint node started");
    // make sure to shut down when receiving a termination signal
    kill_on_term_signal(kill_switch.clone());
    // shut down the anoma node if tendermint unexpectedly stops
    monitor_process(tendermint_node, kill_switch);
    if receiver.recv().unwrap() {
        unsafe {
            libc::kill(pid as i32, libc::SIGTERM);
        };
    }
    Ok(())
}

/// Listens for termination signals and forwards a kill command to the
/// tendermint node when it encounters one.
fn kill_on_term_signal(kill_switch: Sender<bool>) {
    let _ = std::thread::spawn(move || {
        let mut signals = Signals::new(TERM_SIGNALS)
            .expect("Failed to creat OS signal handlers");
        for sig in signals.forever() {
            if TERM_SIGNALS.contains(&sig) {
                tracing::info!(
                    "Received termination signal, shutting down Tendermint \
                     node"
                );
                let _ = kill_switch.send(true);
                break;
            }
        }
    });
}

/// Monitors the tendermint node. If it shuts down, this detects it and
/// shuts down the anoma node
fn monitor_process(
    mut process: std::process::Child,
    kill_switch: Sender<bool>,
) {
    std::thread::spawn(move || {
        process.wait().expect("Tendermint was not running");
        tracing::info!("Tendermint node is no longer running.");
        let _ = kill_switch.send(true);
    });
}

pub fn reset(config: config::Ledger) -> Result<()> {
    // reset all the Tendermint state, if any
    Command::new("tendermint")
        .args(&[
            "unsafe-reset-all",
            // NOTE: log config: https://docs.tendermint.com/master/nodes/logging.html#configuring-log-levels
            // "--log-level=\"*debug\"",
            "--home",
            &config.tendermint.to_string_lossy(),
        ])
        .output()
        .expect("Failed to reset tendermint node's data");
    fs::remove_dir_all(format!(
        "{}/config",
        &config.tendermint.to_string_lossy()
    ))
    .expect("Failed to reset tendermint node's config");
    Ok(())
}

/// Initialize validator private key for Tendermint
pub fn write_validator_key(
    home_dir: impl AsRef<Path>,
    address: &Address,
    consensus_key: &Keypair,
) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("priv_validator_key.json");
    // Make sure the dir exists
    let wallet_dir = path.parent().unwrap();
    fs::create_dir_all(wallet_dir)
        .expect("Couldn't create private validator key directory");
    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&path)
        .expect("Couldn't create private validator key file");
    let pk: ed25519_dalek::PublicKey = consensus_key.public.clone().into();
    let pk = base64::encode(pk.as_bytes());
    let sk = base64::encode(consensus_key.to_bytes());
    let address = address.raw_hash().unwrap();
    let key = json!({
       "address": address,
       "pub_key": {
         "type": "tendermint/PubKeyEd25519",
         "value": pk,
       },
       "priv_key": {
         "type": "tendermint/PrivKeyEd25519",
         "value": sk,
      }
    });
    serde_json::to_writer_pretty(file, &key)
        .expect("Couldn't write private validator key file");
}

/// Initialize validator private state for Tendermint
pub fn write_validator_state(home_dir: impl AsRef<Path>) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("data").join("priv_validator_state.json");
    // Make sure the dir exists
    let wallet_dir = path.parent().unwrap();
    fs::create_dir_all(wallet_dir)
        .expect("Couldn't create private validator state directory");
    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&path)
        .expect("Couldn't create private validator state file");
    let state = json!({
       "height": "0",
       "round": 0,
       "step": 0
    });
    serde_json::to_writer_pretty(file, &state)
        .expect("Couldn't write private validator state file");
}

fn update_tendermint_config(
    home_dir: impl AsRef<Path>,
    rpc_address: net::Address,
    p2p_address: net::Address,
) -> Result<()> {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("config.toml");
    let mut config =
        TendermintConfig::load_toml_file(&path).map_err(Error::LoadConfig)?;

    config.rpc.laddr = rpc_address;
    config.p2p.laddr = p2p_address;

    // In "dev", only produce blocks when there are txs or when the AppHash
    // changes
    config.consensus.create_empty_blocks = !cfg!(feature = "dev");

    // We set this to true as we don't want any invalid tx be re-applied. This
    // also implies that it's not possible for an invalid tx to become valid
    // again in the future.
    config.mempool.keep_invalid_txs_in_cache = false;

    // Bumped from the default `1_000_000`, because some WASMs can be
    // quite large
    config.rpc.max_body_bytes = 2_000_000;

    // TODO broadcast_tx_commit shouldn't be used live;
    config.rpc.timeout_broadcast_tx_commit = Duration::from_secs(20).into();

    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(path)
        .map_err(Error::OpenWriteConfig)?;
    let config_str =
        toml::to_string(&config).map_err(Error::ConfigSerializeToml)?;
    file.write_all(config_str.as_bytes())
        .map_err(Error::WriteConfig)
}

fn write_chain_id(home_dir: impl AsRef<Path>, chain_id: impl AsRef<str>) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("genesis.json");
    let file = File::open(&path).unwrap_or_else(|err| {
        panic!(
            "Couldn't open the genesis file at {:?}, error: {}",
            path, err
        )
    });
    let reader = BufReader::new(file);
    let mut genesis: tendermint::Genesis = serde_json::from_reader(reader)
        .expect("Couldn't deserialize the genesis file");
    genesis.chain_id =
        FromStr::from_str(chain_id.as_ref()).expect("Invalid chain ID");

    let file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(&path)
        .unwrap_or_else(|err| {
            panic!(
                "Couldn't open the genesis file at {:?} for writing, error: {}",
                path, err
            )
        });
    serde_json::to_writer_pretty(file, &genesis)
        .expect("Couldn't write the genesis file");
}
