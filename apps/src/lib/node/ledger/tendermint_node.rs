use std::fs::{self, File, OpenOptions};
use std::io::{BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;
use std::sync::mpsc::Receiver;
use std::time::Duration;

use serde_json::json;
use tendermint::config::TendermintConfig;
use thiserror::Error;

use crate::config;
use crate::genesis::{self, Validator};



#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to initialize Tendermint: {0}")]
    TendermintInit(std::io::Error),
    #[error("Failed to load Tendermint config file: {0}")]
    TendermintLoadConfig(tendermint::error::Error),
    #[error("Failed to open Tendermint config for writing: {0}")]
    TendermintOpenWriteConfig(std::io::Error),
    #[error("Failed to serialize Tendermint config TOML to string: {0}")]
    TendermintConfigSerializeToml(toml::ser::Error),
    #[error("Failed to write Tendermint config: {0}")]
    TendermintWriteConfig(std::io::Error),
    #[error("Failed to start up Tendermint node: {0}")]
    TendermintStartUp(std::io::Error),
    //#[error("Failed to create OS signal handlers: {0}")]
    //SignalsHandlers(std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

/// Run the ABCI server in the current thread (blocking).
pub fn run(home_dir: PathBuf, receiver: Receiver<bool>) -> Result<()> {
    let home_dir_string = home_dir.to_string_lossy().to_string();
    // init and run a Tendermint node child process
    Command::new("tendermint")
        .args(&["init", "--home", &home_dir_string])
        .output()
        .map_err(Error::TendermintInit)?;

    if cfg!(feature = "dev") {
        // override the validator key file
        write_validator_key(&home_dir, &genesis::genesis().validator);
        write_chain_id(&home_dir, config::DEFAULT_CHAIN_ID);
    }

    update_tendermint_config(&home_dir)?;

    let tendermint_node = Command::new("tendermint")
        .args(&["node", "--home", &home_dir_string])
        .spawn()
        .map_err(Error::TendermintStartUp)?;
    tracing::info!("Tendermint node started");

    if receiver.recv().unwrap() {
        tracing::info!(
            "Received termination signal, shutting down Tendermint node"
        );
        unsafe {
            libc::kill(tendermint_node.id() as i32, libc::SIGTERM);
        };
    }

    Ok(())
}

pub fn reset(config: config::Ledger) {
    // reset all the Tendermint state, if any
    Command::new("tendermint")
        .args(&[
            "unsafe_reset_all",
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
}

fn update_tendermint_config(home_dir: impl AsRef<Path>) -> Result<()> {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("config.toml");
    let mut config = TendermintConfig::load_toml_file(&path)
        .map_err(Error::TendermintLoadConfig)?;

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
        .map_err(Error::TendermintOpenWriteConfig)?;
    let config_str = toml::to_string(&config)
        .map_err(Error::TendermintConfigSerializeToml)?;
    file.write(config_str.as_bytes())
        .map(|_| ())
        .map_err(Error::TendermintWriteConfig)
}

#[cfg(feature = "dev")]
fn write_validator_key(home_dir: impl AsRef<Path>, account: &Validator) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("priv_validator_key.json");
    let file =
        File::create(path).expect("Couldn't create private validator key file");
    let pk = base64::encode(account.keypair.public.as_bytes());
    let sk = base64::encode(account.keypair.to_bytes());
    let key = json!({
       "address": account.address,
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

#[cfg(feature = "dev")]
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
