//! Node and client configuration

pub mod genesis;
pub mod global;
pub mod utils;

use std::collections::HashSet;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::io::Write;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anoma::types::chain::ChainId;
use anoma::types::time::Rfc3339String;
use libp2p::multiaddr::{Multiaddr, Protocol};
use libp2p::multihash::Multihash;
use libp2p::PeerId;
use regex::Regex;
use serde::{de, Deserialize, Serialize};
#[cfg(not(feature = "ABCI"))]
use tendermint::Timeout;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_stable::Timeout;
use thiserror::Error;

use crate::cli;

/// Base directory contains global config and chain directories.
pub const DEFAULT_BASE_DIR: &str = ".anoma";
/// Default WASM dir. Note that WASM dirs are nested in chain dirs.
pub const DEFAULT_WASM_DIR: &str = "wasm";
/// The WASM checksums file contains the hashes of built WASMs. It is inside the
/// WASM dir.
pub const DEFAULT_WASM_CHECKSUMS_FILE: &str = "checksums.json";
/// Chain-specific Anoma configuration. Nested in chain dirs.
pub const FILENAME: &str = "config.toml";
/// Chain-specific Tendermint configuration. Nested in chain dirs.
pub const TENDERMINT_DIR: &str = "tendermint";
/// Chain-specific Anoma DB. Nested in chain dirs.
pub const DB_DIR: &str = "db";

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Config {
    pub wasm_dir: PathBuf,
    pub ledger: Ledger,
    pub intent_gossiper: IntentGossiper,
    // TODO allow to configure multiple matchmakers
    pub matchmaker: Matchmaker,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TendermintMode {
    Full,
    Validator,
    Seed,
}

impl TendermintMode {
    pub fn to_str(&self) -> &str {
        match *self {
            TendermintMode::Full => "full",
            TendermintMode::Validator => "validator",
            TendermintMode::Seed => "seed",
        }
    }
}

impl From<String> for TendermintMode {
    fn from(mode: String) -> Self {
        match mode.as_str() {
            "full" => TendermintMode::Full,
            "validator" => TendermintMode::Validator,
            "seed" => TendermintMode::Seed,
            _ => panic!("Unrecognized mode"),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Ledger {
    pub genesis_time: Rfc3339String,
    pub chain_id: ChainId,
    pub shell: Shell,
    pub tendermint: Tendermint,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Shell {
    pub base_dir: PathBuf,
    pub ledger_address: SocketAddr,
    /// RocksDB block cache maximum size in bytes.
    /// When not set, defaults to 1/3 of the available memory.
    pub block_cache_bytes: Option<u64>,
    /// VP WASM compilation cache maximum size in bytes.
    /// When not set, defaults to 1/6 of the available memory.
    pub vp_wasm_compilation_cache_bytes: Option<u64>,
    /// Tx WASM compilation in-memory cache maximum size in bytes.
    /// When not set, defaults to 1/6 of the available memory.
    pub tx_wasm_compilation_cache_bytes: Option<u64>,
    /// Use the [`Ledger::db_dir()`] method to read the value.
    db_dir: PathBuf,
    /// Use the [`Ledger::tendermint_dir()`] method to read the value.
    tendermint_dir: PathBuf,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Tendermint {
    pub rpc_address: SocketAddr,
    pub p2p_address: SocketAddr,
    /// The persistent peers addresses must include node ID
    pub p2p_persistent_peers: Vec<TendermintAddress>,
    /// Turns the peer exchange reactor on or off. Validator node will want the
    /// pex turned off.
    pub p2p_pex: bool,
    /// Toggle to disable guard against peers connecting from the same IP
    pub p2p_allow_duplicate_ip: bool,
    /// Set `true` for strict address routability rules
    /// Set `false` for private or local networks
    pub p2p_addr_book_strict: bool,
    /// How long we wait after committing a block, before starting on the new
    /// height
    pub consensus_timeout_commit: Timeout,
    pub tendermint_mode: TendermintMode,
    pub instrumentation_prometheus: bool,
    pub instrumentation_prometheus_listen_addr: SocketAddr,
    pub instrumentation_namespace: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct IntentGossiper {
    // Simple values
    pub address: Multiaddr,
    pub topics: HashSet<String>,
    /// The server address to which matchmakers can connect to receive intents
    pub matchmakers_server_addr: SocketAddr,

    // Nested structures ⚠️ no simple values below any of these ⚠️
    pub subscription_filter: SubscriptionFilter,
    pub seed_peers: HashSet<PeerAddress>,
    pub rpc: Option<RpcServer>,
    pub discover_peer: Option<DiscoverPeer>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct RpcServer {
    pub address: SocketAddr,
}

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct Matchmaker {
    pub matchmaker_path: Option<PathBuf>,
    pub tx_code_path: Option<PathBuf>,
}

impl Ledger {
    pub fn new(
        base_dir: impl AsRef<Path>,
        chain_id: ChainId,
        mode: TendermintMode,
    ) -> Self {
        Self {
            genesis_time: Rfc3339String("1970-01-01T00:00:00Z".to_owned()),
            chain_id,
            shell: Shell {
                base_dir: base_dir.as_ref().to_owned(),
                ledger_address: SocketAddr::new(
                    IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                    26658,
                ),
                block_cache_bytes: None,
                vp_wasm_compilation_cache_bytes: None,
                tx_wasm_compilation_cache_bytes: None,
                db_dir: DB_DIR.into(),
                tendermint_dir: TENDERMINT_DIR.into(),
            },
            tendermint: Tendermint {
                rpc_address: SocketAddr::new(
                    IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                    26657,
                ),
                p2p_address: SocketAddr::new(
                    IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                    26656,
                ),
                p2p_persistent_peers: vec![],
                p2p_pex: true,
                p2p_allow_duplicate_ip: false,
                p2p_addr_book_strict: true,
                consensus_timeout_commit: Timeout::from_str("1s").unwrap(),
                tendermint_mode: mode,
                instrumentation_prometheus: false,
                instrumentation_prometheus_listen_addr: SocketAddr::new(
                    IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                    26661,
                ),
                instrumentation_namespace: "anoman_tm".to_string(),
            },
        }
    }

    /// Get the chain directory path
    pub fn chain_dir(&self) -> PathBuf {
        self.shell.base_dir.join(self.chain_id.as_str())
    }

    /// Get the directory path to the DB
    pub fn db_dir(&self) -> PathBuf {
        self.shell.db_dir(&self.chain_id)
    }

    /// Get the directory path to Tendermint
    pub fn tendermint_dir(&self) -> PathBuf {
        self.shell.tendermint_dir(&self.chain_id)
    }
}

impl Shell {
    /// Get the directory path to the DB
    pub fn db_dir(&self, chain_id: &ChainId) -> PathBuf {
        self.base_dir.join(chain_id.as_str()).join(&self.db_dir)
    }

    /// Get the directory path to Tendermint
    pub fn tendermint_dir(&self, chain_id: &ChainId) -> PathBuf {
        self.base_dir
            .join(chain_id.as_str())
            .join(&self.tendermint_dir)
    }
}

// TODO maybe add also maxCount for a maximum number of subscription for a
// filter.

// TODO toml failed to serialize without "untagged" because does not support
// enum with nested data, unless with the untagged flag. This might be a source
// of confusion in the future... Another approach would be to have multiple
// field for each filter possibility but it's less nice.
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum SubscriptionFilter {
    RegexFilter(#[serde(with = "serde_regex")] Regex),
    WhitelistFilter(Vec<String>),
}

// TODO peer_id can be part of Multiaddr, mayby this splitting is not useful ?
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct PeerAddress {
    pub address: Multiaddr,
    pub peer_id: PeerId,
}

// TODO add reserved_peers: explicit peers for gossipsub network, to not be
// added to kademlia
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DiscoverPeer {
    pub max_discovery_peers: u64,
    /// Toggle Kademlia remote peer discovery, on by default
    pub kademlia: bool,
    /// Toggle local network mDNS peer discovery, off by default
    pub mdns: bool,
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Error while reading config: {0}")]
    ReadError(config::ConfigError),
    #[error("Error while deserializing config: {0}")]
    DeserializationError(config::ConfigError),
    #[error("Error while serializing to toml: {0}")]
    TomlError(toml::ser::Error),
    #[error("Error while writing config: {0}")]
    WriteError(std::io::Error),
    #[error("A config file already exists in {0}")]
    AlreadyExistingConfig(PathBuf),
    #[error(
        "Bootstrap peer {0} is not valid. Format needs to be \
         {{protocol}}/{{ip}}/tcp/{{port}}/p2p/{{peerid}}"
    )]
    BadBootstrapPeerFormat(String),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum SerdeError {
    // This is needed for serde https://serde.rs/error-handling.html
    #[error(
        "Bootstrap peer {0} is not valid. Format needs to be \
         {{protocol}}/{{ip}}/tcp/{{port}}/p2p/{{peerid}}"
    )]
    BadBootstrapPeerFormat(String),
    #[error("{0}")]
    Message(String),
}

impl Config {
    pub fn new(
        base_dir: impl AsRef<Path>,
        chain_id: ChainId,
        mode: TendermintMode,
    ) -> Self {
        Self {
            wasm_dir: DEFAULT_WASM_DIR.into(),
            ledger: Ledger::new(base_dir, chain_id, mode),
            intent_gossiper: IntentGossiper::default(),
            matchmaker: Matchmaker::default(),
        }
    }

    /// Load config from expected path in the `base_dir` or generate a new one
    /// if it doesn't exist. Terminates with an error if the config loading
    /// fails.
    pub fn load(
        base_dir: impl AsRef<Path>,
        chain_id: &ChainId,
        mode: Option<TendermintMode>,
    ) -> Self {
        let base_dir = base_dir.as_ref();
        match Self::read(base_dir, chain_id, mode) {
            Ok(mut config) => {
                config.ledger.shell.base_dir = base_dir.to_path_buf();
                config
            }
            Err(err) => {
                eprintln!(
                    "Tried to read config in {} but failed with: {}",
                    base_dir.display(),
                    err
                );
                cli::safe_exit(1)
            }
        }
    }

    /// Read the config from a file, or generate a default one and write it to
    /// a file if it doesn't already exist. Keys that are expected but not set
    /// in the config file are filled in with default values.
    pub fn read(
        base_dir: &Path,
        chain_id: &ChainId,
        mode: Option<TendermintMode>,
    ) -> Result<Self> {
        let file_path = Self::file_path(base_dir, chain_id);
        let file_name = file_path.to_str().expect("Expected UTF-8 file path");
        let mode = mode.unwrap_or(TendermintMode::Full);
        if !file_path.exists() {
            return Self::generate(base_dir, chain_id, mode, true);
        };
        let defaults = config::Config::try_from(&Self::new(
            base_dir,
            chain_id.clone(),
            mode,
        ))
        .map_err(Error::ReadError)?;
        let mut config = config::Config::new();
        config
            .merge(defaults)
            .and_then(|c| c.merge(config::File::with_name(file_name)))
            .and_then(|c| {
                c.merge(
                    config::Environment::with_prefix("anoma").separator("__"),
                )
            })
            .map_err(Error::ReadError)?;
        config.try_into().map_err(Error::DeserializationError)
    }

    /// Generate configuration and write it to a file.
    pub fn generate(
        base_dir: &Path,
        chain_id: &ChainId,
        mode: TendermintMode,
        replace: bool,
    ) -> Result<Self> {
        let config = Config::new(base_dir, chain_id.clone(), mode);
        config.write(base_dir, chain_id, replace)?;
        Ok(config)
    }

    /// Write configuration to a file.
    pub fn write(
        &self,
        base_dir: &Path,
        chain_id: &ChainId,
        replace: bool,
    ) -> Result<()> {
        let file_path = Self::file_path(base_dir, chain_id);
        let file_dir = file_path.parent().unwrap();
        create_dir_all(file_dir).map_err(Error::WriteError)?;
        if file_path.exists() && !replace {
            Err(Error::AlreadyExistingConfig(file_path))
        } else {
            let mut file =
                File::create(file_path).map_err(Error::WriteError)?;
            let toml = toml::ser::to_string(&self).map_err(|err| {
                if let toml::ser::Error::ValueAfterTable = err {
                    tracing::error!("{}", VALUE_AFTER_TABLE_ERROR_MSG);
                }
                Error::TomlError(err)
            })?;
            file.write_all(toml.as_bytes()).map_err(Error::WriteError)
        }
    }

    /// Get the file path to the config
    pub fn file_path(
        base_dir: impl AsRef<Path>,
        chain_id: &ChainId,
    ) -> PathBuf {
        // Join base dir to the chain ID
        base_dir.as_ref().join(chain_id.to_string()).join(FILENAME)
    }
}

impl Default for IntentGossiper {
    fn default() -> Self {
        Self {
            address: Multiaddr::from_str("/ip4/0.0.0.0/tcp/26659").unwrap(),
            topics: vec!["asset_v0"].into_iter().map(String::from).collect(),
            matchmakers_server_addr: SocketAddr::new(
                IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                26661,
            ),
            subscription_filter: SubscriptionFilter::RegexFilter(
                Regex::new("asset_v\\d{1,2}").unwrap(),
            ),
            seed_peers: HashSet::default(),
            rpc: None,
            discover_peer: Some(DiscoverPeer::default()),
        }
    }
}

impl IntentGossiper {
    pub fn update(&mut self, addr: Option<Multiaddr>, rpc: Option<SocketAddr>) {
        if let Some(addr) = addr {
            self.address = addr;
        }
        if let Some(address) = rpc {
            self.rpc = Some(RpcServer { address });
        }
    }
}

impl Default for RpcServer {
    fn default() -> Self {
        Self {
            address: SocketAddr::new(
                IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                26660,
            ),
        }
    }
}

impl Serialize for PeerAddress {
    fn serialize<S>(
        &self,
        serializer: S,
    ) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut address = self.address.clone();
        address.push(Protocol::P2p(Multihash::from(self.peer_id)));
        address.serialize(serializer)
    }
}

impl de::Error for SerdeError {
    fn custom<T: Display>(msg: T) -> Self {
        SerdeError::Message(msg.to_string())
    }
}

impl<'de> Deserialize<'de> for PeerAddress {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let mut address = Multiaddr::deserialize(deserializer)
            .map_err(|err| SerdeError::BadBootstrapPeerFormat(err.to_string()))
            .map_err(D::Error::custom)?;
        if let Some(Protocol::P2p(mh)) = address.pop() {
            let peer_id = PeerId::from_multihash(mh).unwrap();
            Ok(Self { address, peer_id })
        } else {
            Err(SerdeError::BadBootstrapPeerFormat(address.to_string()))
                .map_err(D::Error::custom)
        }
    }
}

impl Default for DiscoverPeer {
    fn default() -> Self {
        Self {
            max_discovery_peers: 16,
            kademlia: true,
            mdns: false,
        }
    }
}

pub const VALUE_AFTER_TABLE_ERROR_MSG: &str = r#"
Error while serializing to toml. It means that some nested structure is followed
 by simple fields.
This fails:
    struct Nested{
       i:int
    }

    struct Broken{
       nested:Nested,
       simple:int
    }
And this is correct
    struct Nested{
       i:int
    }

    struct Correct{
       simple:int
       nested:Nested,
    }
"#;
