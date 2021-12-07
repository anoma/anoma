use std::fs::{self, File, OpenOptions};
use std::io::{BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;
use std::sync::mpsc::Receiver;

use anoma::types::address::Address;
use anoma::types::chain::ChainId;
use anoma::types::key::ed25519::Keypair;
use anoma::types::time::DateTimeUtc;
use serde_json::json;
use signal_hook::consts::TERM_SIGNALS;
use signal_hook::iterator::Signals;
#[cfg(not(feature = "ABCI"))]
use tendermint::error::Error as TendermintError;
#[cfg(not(feature = "ABCI"))]
use tendermint::{config::TendermintConfig, net, Genesis};
#[cfg(feature = "ABCI")]
use tendermint_stable::error::Error as TendermintError;
#[cfg(feature = "ABCI")]
use tendermint_stable::{config::TendermintConfig, net, Genesis};
use thiserror::Error;

use crate::config;
use crate::std::sync::mpsc::Sender;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to initialize Tendermint: {0}")]
    Init(std::io::Error),
    #[error("Failed to load Tendermint config file: {0}")]
    LoadConfig(TendermintError),
    #[error("Failed to open Tendermint config for writing: {0}")]
    OpenWriteConfig(std::io::Error),
    #[error("Failed to serialize Tendermint config TOML to string: {0}")]
    ConfigSerializeToml(toml::ser::Error),
    #[error("Failed to write Tendermint config: {0}")]
    WriteConfig(std::io::Error),
    #[error("Failed to start up Tendermint node: {0}")]
    StartUp(std::io::Error),
    #[error("Runtime error")]
    Runtime,
    #[error("Failed to convert to String: {0:?}")]
    TendermintPath(std::ffi::OsString),
}

pub type Result<T> = std::result::Result<T, Error>;

/// Check if the TENDERMINT env var has been set and use that as the
/// location of the tendermint binary. Otherwise, assume it is on path
///
/// Returns an error if the env var is defined but not a valid Unicode.
fn from_env_or_default() -> Result<String> {
    match std::env::var("TENDERMINT") {
        Ok(path) => {
            tracing::info!("Using tendermint path from env variable: {}", path);
            Ok(path)
        }
        Err(std::env::VarError::NotPresent) => Ok(String::from("tendermint")),
        Err(std::env::VarError::NotUnicode(msg)) => {
            Err(Error::TendermintPath(msg))
        }
    }
}

/// Run the tendermint node.
pub fn run(
    home_dir: PathBuf,
    chain_id: ChainId,
    genesis_time: DateTimeUtc,
    ledger_address: String,
    config: config::Tendermint,
    abort_sender: Sender<bool>,
    abort_receiver: Receiver<bool>,
) -> Result<()> {
    let home_dir_string = home_dir.to_string_lossy().to_string();
    let tendermint_path = from_env_or_default()?;
    let mode = config.tendermint_mode.to_str().to_owned();

    #[cfg(feature = "dev")]
    // This has to be checked before we run tendermint init
    let has_validator_key = {
        let path = home_dir.join("config").join("priv_validator_key.json");
        Path::new(&path).exists()
    };

    // init and run a tendermint node child process
    let output = if !cfg!(featuer = "ABCI") {
        Command::new(&tendermint_path)
            .args(&["init", &mode, "--home", &home_dir_string])
            .output()
            .map_err(Error::Init)?
    } else {
        Command::new(&tendermint_path)
            .args(&["init", "--home", &home_dir_string])
            .output()
            .map_err(Error::Init)?
    };
    if !output.status.success() {
        panic!("Tendermint failed to initialize with {:#?}", output);
    }

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
                crate::wallet::defaults::validator_keypair().as_ref(),
            );
        }
    }

    write_tm_genesis(&home_dir, chain_id, genesis_time);

    update_tendermint_config(&home_dir, config)?;

    let tendermint_node = if !cfg!(feature = "ABCI") {
        Command::new(&tendermint_path)
            .args(&[
                "start",
                "--mode",
                &mode,
                "--proxy-app",
                &ledger_address,
                "--home",
                &home_dir_string,
            ])
            .spawn()
            .map_err(Error::StartUp)?
    } else {
        Command::new(&tendermint_path)
            .args(&[
                "start",
                "--proxy_app",
                &ledger_address,
                "--home",
                &home_dir_string,
            ])
            .spawn()
            .map_err(Error::StartUp)?
    };
    let pid = tendermint_node.id();
    tracing::info!("Tendermint node started");
    // make sure to shut down when receiving a termination signal
    kill_on_term_signal(abort_sender.clone());
    // shut down the anoma node if tendermint stops
    monitor_process(tendermint_node, abort_sender);

    // Wait for abort signal (blocking)
    let exit_gracefully = abort_receiver.recv().unwrap_or_default();
    // Send signal to shut down Tendermint node
    unsafe {
        libc::kill(pid as i32, libc::SIGTERM);
    };
    if exit_gracefully {
        Ok(())
    } else {
        Err(Error::Runtime)
    }
}

/// Listens for termination signals and forwards a kill command to the
/// tendermint node when it encounters one.
fn kill_on_term_signal(abort_sender: Sender<bool>) {
    let _ = std::thread::spawn(move || {
        let mut signals = Signals::new(TERM_SIGNALS)
            .expect("Failed to creat OS signal handlers");
        for sig in signals.forever() {
            if TERM_SIGNALS.contains(&sig) {
                tracing::info!(
                    "Received termination signal, shutting down Tendermint \
                     node"
                );
                let _ = abort_sender.send(true);
                break;
            }
        }
    });
}

/// Monitors the tendermint node. If it shuts down, this detects it and
/// shuts down the anoma node
fn monitor_process(
    mut process: std::process::Child,
    abort_sender: Sender<bool>,
) {
    std::thread::spawn(move || {
        let status = process.wait().expect("Tendermint was not running");
        tracing::info!("Tendermint node is no longer running.");
        let _ = abort_sender.send(status.success());
    });
}

pub fn reset(tendermint_dir: impl AsRef<Path>) -> Result<()> {
    let tendermint_path = from_env_or_default()?;
    let tendermint_dir = tendermint_dir.as_ref().to_string_lossy();
    // reset all the Tendermint state, if any
    Command::new(tendermint_path)
        .args(&[
            "unsafe-reset-all",
            // NOTE: log config: https://docs.tendermint.com/master/nodes/logging.html#configuring-log-levels
            // "--log-level=\"*debug\"",
            "--home",
            &tendermint_dir,
        ])
        .output()
        .expect("Failed to reset tendermint node's data");
    fs::remove_dir_all(format!("{}/config", tendermint_dir,))
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
    let pk: ed25519_dalek::PublicKey = consensus_key.public.into();
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
    tendermint_config: config::Tendermint,
) -> Result<()> {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("config.toml");
    let mut config =
        TendermintConfig::load_toml_file(&path).map_err(Error::LoadConfig)?;

    config.p2p.laddr =
        net::Address::from_str(&tendermint_config.p2p_address.to_string())
            .unwrap();
    config.p2p.persistent_peers = tendermint_config.p2p_persistent_peers;
    config.p2p.pex = tendermint_config.p2p_pex;
    config.p2p.allow_duplicate_ip = tendermint_config.p2p_allow_duplicate_ip;

    // In "dev", only produce blocks when there are txs or when the AppHash
    // changes
    config.consensus.create_empty_blocks = true; // !cfg!(feature = "dev");
    config.consensus.timeout_commit =
        tendermint_config.consensus_timeout_commit;

    // We set this to true as we don't want any invalid tx be re-applied. This
    // also implies that it's not possible for an invalid tx to become valid
    // again in the future.
    config.mempool.keep_invalid_txs_in_cache = false;

    config.rpc.laddr =
        net::Address::from_str(&tendermint_config.rpc_address.to_string())
            .unwrap();
    // Bumped from the default `1_000_000`, because some WASMs can be
    // quite large
    config.rpc.max_body_bytes = 2_000_000;

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

fn write_tm_genesis(
    home_dir: impl AsRef<Path>,
    chain_id: ChainId,
    genesis_time: DateTimeUtc,
) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("genesis.json");
    let file = File::open(&path).unwrap_or_else(|err| {
        panic!(
            "Couldn't open the genesis file at {:?}, error: {}",
            path, err
        )
    });
    let reader = BufReader::new(file);
    let mut genesis: Genesis = serde_json::from_reader(reader)
        .expect("Couldn't deserialize the genesis file");
    genesis.chain_id =
        FromStr::from_str(chain_id.as_str()).expect("Invalid chain ID");
    genesis.genesis_time = genesis_time.into();

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
