use std::path::{Path, PathBuf};
use std::str::FromStr;

use anoma::types::chain::ChainId;
use anoma::types::key::*;
use anoma::types::time::DateTimeUtc;
use borsh::BorshSerialize;
use serde_json::json;
#[cfg(not(feature = "ABCI"))]
use tendermint::Genesis;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::net::Address as TendermintAddress;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::Error as TendermintError;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::TendermintConfig;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::Error as TendermintError;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::TendermintConfig;
#[cfg(feature = "ABCI")]
use tendermint_stable::Genesis;
use thiserror::Error;
use tokio::fs::{self, File, OpenOptions};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::Command;

use crate::config;

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
    #[error("{0}")]
    Runtime(String),
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
pub async fn run(
    home_dir: PathBuf,
    chain_id: ChainId,
    genesis_time: DateTimeUtc,
    ledger_address: String,
    config: config::Tendermint,
    abort_recv: tokio::sync::oneshot::Receiver<
        tokio::sync::oneshot::Sender<()>,
    >,
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
    let output = if !cfg!(feature = "ABCI") {
        Command::new(&tendermint_path)
            .args(&["init", &mode, "--home", &home_dir_string])
            .output()
            .await
            .map_err(Error::Init)?
    } else {
        Command::new(&tendermint_path)
            .args(&["init", "--home", &home_dir_string])
            .output()
            .await
            .map_err(Error::Init)?
    };
    if !output.status.success() {
        panic!("Tendermint failed to initialize with {:#?}", output);
    }

    #[cfg(feature = "dev")]
    {
        let consensus_key = crate::wallet::defaults::validator_keypair();
        // write the validator key file if it didn't already exist
        if !has_validator_key {
            write_validator_key_async(&home_dir, &consensus_key).await;
        }
    }
    #[cfg(not(feature = "ABCI"))]
    {
        write_tm_genesis(&home_dir, chain_id, genesis_time, &config).await;
    }
    #[cfg(feature = "ABCI")]
    {
        write_tm_genesis(&home_dir, chain_id, genesis_time).await;
    }

    update_tendermint_config(&home_dir, config).await?;

    let mut tendermint_node = if !cfg!(feature = "ABCI") {
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
            .kill_on_drop(true)
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
            .kill_on_drop(true)
            .spawn()
            .map_err(Error::StartUp)?
    };
    tracing::info!("Tendermint node started");

    tokio::select! {
        status = tendermint_node.wait() => {
            match status {
                Ok(status) => {
                    if status.success() {
                        Ok(())
                    } else {
                        Err(Error::Runtime(status.to_string()))
                    }
                },
                Err(err) => {
                    Err(Error::Runtime(err.to_string()))
                }
            }
        },
        resp_sender = abort_recv => {
            match resp_sender {
                Ok(resp_sender) => {
                    tracing::info!("Shutting down Tendermint node...");
                    tendermint_node.kill().await.unwrap();
                    resp_sender.send(()).unwrap();
                },
                Err(err) => {
                    tracing::error!("The Tendermint abort sender has unexpectedly dropped: {}", err);
                    tracing::info!("Shutting down Tendermint node...");
                    tendermint_node.kill().await.unwrap();
                }
            }
            Ok(())
        }
    }
}

pub fn reset(tendermint_dir: impl AsRef<Path>) -> Result<()> {
    let tendermint_path = from_env_or_default()?;
    let tendermint_dir = tendermint_dir.as_ref().to_string_lossy();
    // reset all the Tendermint state, if any
    #[cfg(not(feature = "ABCI"))]
    std::process::Command::new(tendermint_path)
        .args(&[
            "reset",
            "unsafe-all",
            // NOTE: log config: https://docs.tendermint.com/master/nodes/logging.html#configuring-log-levels
            // "--log-level=\"*debug\"",
            "--home",
            &tendermint_dir,
        ])
        .output()
        .expect("Failed to reset tendermint node's data");
    #[cfg(feature = "ABCI")]
    std::process::Command::new(tendermint_path)
        .args(&[
            "unsafe-reset-all",
            // NOTE: log config: https://docs.tendermint.com/master/nodes/logging.html#configuring-log-levels
            // "--log-level=\"*debug\"",
            "--home",
            &tendermint_dir,
        ])
        .output()
        .expect("Failed to reset tendermint node's data");
    std::fs::remove_dir_all(format!("{}/config", tendermint_dir,))
        .expect("Failed to reset tendermint node's config");
    Ok(())
}

/// Convert a common signing scheme validator key into JSON for
/// Tendermint
fn validator_key_to_json<SK: SecretKey>(
    sk: &SK,
) -> std::result::Result<serde_json::Value, ParseSecretKeyError> {
    ed25519::SecretKey::try_from_sk(sk).map(|sk| {
        let pk: ed25519::PublicKey = sk.ref_to();
        let pk_common = common::PublicKey::try_from_pk(&pk)
            .expect("must be able to convert ed25519 to common");
        let raw_hash = tm_consensus_key_raw_hash(&pk_common);
        let ck_arr =
            [sk.try_to_vec().unwrap(), pk.try_to_vec().unwrap()].concat();
        json!({
            "address": raw_hash,
            "pub_key": {
                "type": "tendermint/PubKeyEd25519",
                "value": base64::encode(pk.try_to_vec().unwrap()),
            },
            "priv_key": {
                "type": "tendermint/PrivKeyEd25519",
                "value": base64::encode(ck_arr),
            }
        })
    })
}

/// Initialize validator private key for Tendermint
pub async fn write_validator_key_async(
    home_dir: impl AsRef<Path>,
    consensus_key: &common::SecretKey,
) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("priv_validator_key.json");
    // Make sure the dir exists
    let wallet_dir = path.parent().unwrap();
    fs::create_dir_all(wallet_dir)
        .await
        .expect("Couldn't create private validator key directory");
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&path)
        .await
        .expect("Couldn't create private validator key file");
    let key = validator_key_to_json(consensus_key).unwrap();
    let data = serde_json::to_vec_pretty(&key)
        .expect("Couldn't encode private validator key file");
    file.write_all(&data[..])
        .await
        .expect("Couldn't write private validator key file");
}

/// Initialize validator private key for Tendermint
pub fn write_validator_key(
    home_dir: impl AsRef<Path>,
    consensus_key: &common::SecretKey,
) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("priv_validator_key.json");
    // Make sure the dir exists
    let wallet_dir = path.parent().unwrap();
    std::fs::create_dir_all(wallet_dir)
        .expect("Couldn't create private validator key directory");
    let file = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&path)
        .expect("Couldn't create private validator key file");
    let key = validator_key_to_json(consensus_key).unwrap();
    serde_json::to_writer_pretty(file, &key)
        .expect("Couldn't write private validator key file");
}

/// Initialize validator private state for Tendermint
pub fn write_validator_state(home_dir: impl AsRef<Path>) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("data").join("priv_validator_state.json");
    // Make sure the dir exists
    let wallet_dir = path.parent().unwrap();
    std::fs::create_dir_all(wallet_dir)
        .expect("Couldn't create private validator state directory");
    let file = std::fs::OpenOptions::new()
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

async fn update_tendermint_config(
    home_dir: impl AsRef<Path>,
    tendermint_config: config::Tendermint,
) -> Result<()> {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("config.toml");
    let mut config =
        TendermintConfig::load_toml_file(&path).map_err(Error::LoadConfig)?;

    config.p2p.laddr =
        TendermintAddress::from_str(&tendermint_config.p2p_address.to_string())
            .unwrap();
    config.p2p.persistent_peers = tendermint_config.p2p_persistent_peers;
    config.p2p.pex = tendermint_config.p2p_pex;
    config.p2p.allow_duplicate_ip = tendermint_config.p2p_allow_duplicate_ip;
    #[cfg(feature = "ABCI")]
    {
        config.p2p.addr_book_strict = tendermint_config.p2p_addr_book_strict;
    }

    // In "dev", only produce blocks when there are txs or when the AppHash
    // changes
    config.consensus.create_empty_blocks = true; // !cfg!(feature = "dev");
    #[cfg(feature = "ABCI")]
    {
        config.consensus.timeout_commit =
            tendermint_config.consensus_timeout_commit;
    }

    // We set this to true as we don't want any invalid tx be re-applied. This
    // also implies that it's not possible for an invalid tx to become valid
    // again in the future.
    config.mempool.keep_invalid_txs_in_cache = false;

    config.rpc.laddr =
        TendermintAddress::from_str(&tendermint_config.rpc_address.to_string())
            .unwrap();
    // Bumped from the default `1_000_000`, because some WASMs can be
    // quite large
    config.rpc.max_body_bytes = 2_000_000;

    config.instrumentation.prometheus =
        tendermint_config.instrumentation_prometheus;
    config.instrumentation.prometheus_listen_addr = tendermint_config
        .instrumentation_prometheus_listen_addr
        .to_string();
    config.instrumentation.namespace =
        tendermint_config.instrumentation_namespace;

    // setup the events log
    #[cfg(not(feature = "ABCI"))]
    {
        // keep events for one minute
        config.rpc.event_log_window_size =
            std::time::Duration::from_secs(59).into();
        // we do not limit the size of the events log
        config.rpc.event_log_max_items = 0;
    }

    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(path)
        .await
        .map_err(Error::OpenWriteConfig)?;
    let config_str =
        toml::to_string(&config).map_err(Error::ConfigSerializeToml)?;
    file.write_all(config_str.as_bytes())
        .await
        .map_err(Error::WriteConfig)
}

async fn write_tm_genesis(
    home_dir: impl AsRef<Path>,
    chain_id: ChainId,
    genesis_time: DateTimeUtc,
    #[cfg(not(feature = "ABCI"))] config: &config::Tendermint,
) {
    let home_dir = home_dir.as_ref();
    let path = home_dir.join("config").join("genesis.json");
    let mut file = File::open(&path).await.unwrap_or_else(|err| {
        panic!(
            "Couldn't open the genesis file at {:?}, error: {}",
            path, err
        )
    });
    let mut file_contents = vec![];
    file.read_to_end(&mut file_contents)
        .await
        .expect("Couldn't read Tendermint genesis file");
    let mut genesis: Genesis = serde_json::from_slice(&file_contents[..])
        .expect("Couldn't deserialize the genesis file");
    genesis.chain_id =
        FromStr::from_str(chain_id.as_str()).expect("Invalid chain ID");
    genesis.genesis_time = genesis_time
        .try_into()
        .expect("Couldn't convert DateTimeUtc to Tendermint Time");
    #[cfg(not(feature = "ABCI"))]
    {
        genesis.consensus_params.timeout.commit =
            config.consensus_timeout_commit.into()
    }

    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(&path)
        .await
        .unwrap_or_else(|err| {
            panic!(
                "Couldn't open the genesis file at {:?} for writing, error: {}",
                path, err
            )
        });
    let data = serde_json::to_vec_pretty(&genesis)
        .expect("Couldn't encode the Tendermint genesis file");
    file.write_all(&data[..])
        .await
        .expect("Couldn't write the Tendermint genesis file");
}
