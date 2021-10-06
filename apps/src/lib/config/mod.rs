//! Node and client configuration

pub mod genesis;
pub mod global;
pub mod gossiper;

use std::collections::HashSet;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::io::Write;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anoma::types::chain::ChainId;
use anoma::types::time::Rfc3339String;
use gossiper::Gossiper;
use libp2p::multiaddr::{Multiaddr, Protocol};
use libp2p::multihash::Multihash;
use libp2p::PeerId;
use regex::Regex;
use serde::{de, Deserialize, Serialize};
use tendermint::net;
use thiserror::Error;

use crate::cli;

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

pub const FILENAME: &str = "config.toml";
pub const TENDERMINT_DIR: &str = "tendermint";
pub const DB_DIR: &str = "db";

pub type Result<T> = std::result::Result<T, Error>;
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

#[derive(Debug, Serialize, Deserialize)]
pub struct Ledger {
    pub base_dir: PathBuf,
    pub chain_id: ChainId,
    pub genesis_time: Rfc3339String,
    pub tendermint: PathBuf,
    pub db: PathBuf,
    pub ledger_address: SocketAddr,
    pub rpc_address: SocketAddr,
    pub p2p_address: SocketAddr,
    /// The persistent peers addresses must include node ID
    pub p2p_persistent_peers: Vec<tendermint::net::Address>,
    /// Turns the peer exchange reactor on or off. Validator node will want the
    /// pex turned off.
    pub p2p_pex: bool,
    pub wasm_dir: PathBuf,
}

impl Ledger {
    pub fn new(base_dir: impl AsRef<Path>, chain_id: ChainId) -> Self {
        let base_dir = base_dir.as_ref().to_owned();
        let sub_dir = base_dir.join(chain_id.as_str());

        #[cfg(feature = "dev")]
        let p2p_persistent_peers = vec![];
        #[cfg(not(feature = "dev"))]
        let p2p_persistent_peers = vec![
            tendermint::net::Address::from_str(
                "72eb02444a4736d569734675c6ba893aadb3fb99@52.210.23.30:26656",
            )
            .unwrap(),
            tendermint::net::Address::from_str(
                "39f31bcf0a0b73a08d9e18f4e9cada33d4997779@63.34.55.152:26656",
            )
            .unwrap(),
            tendermint::net::Address::from_str(
                "d7d2e6e942b157df4137ca3b47258ca20db00416@54.195.72.213:26656",
            )
            .unwrap(),
            tendermint::net::Address::from_str(
                "31752d323de34e4dcd36ecf30b4ed179de7dc48a@79.125.112.218:26656",
            )
            .unwrap(),
        ];

        Self {
            base_dir,
            chain_id,
            genesis_time: Rfc3339String("1970-01-01T00:00:00Z".to_owned()),
            tendermint: sub_dir.join(TENDERMINT_DIR),
            db: sub_dir.join(DB_DIR),
            ledger_address: SocketAddr::new(
                IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                26658,
            ),
            rpc_address: SocketAddr::new(
                IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
                26657,
            ),
            p2p_address: SocketAddr::new(
                IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
                26656,
            ),
            p2p_persistent_peers,
            p2p_pex: true,
            wasm_dir: "wasm".into(),
        }
    }
}
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct RpcServer {
    pub address: SocketAddr,
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Matchmaker {
    pub matchmaker: PathBuf,
    pub tx_code: PathBuf,
    pub ledger_address: net::Address,
    pub filter: Option<PathBuf>,
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

// TODO add reserved_peers: explicit peers for gossipsub network, to not be
// added to kademlia
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DiscoverPeer {
    pub max_discovery_peers: u64,
    pub kademlia: bool,
    pub mdns: bool,
    pub bootstrap_peers: HashSet<PeerAddress>,
}

impl Default for DiscoverPeer {
    /// default configuration for discovering peer.
    /// max_discovery_peers: 16,
    /// kademlia: true,
    /// mdns: true,
    fn default() -> Self {
        Self {
            max_discovery_peers: 16,
            kademlia: true,
            mdns: true,
            bootstrap_peers: HashSet::new(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct IntentGossiper {
    pub address: Multiaddr,
    pub topics: HashSet<String>,
    pub subscription_filter: SubscriptionFilter,
    pub rpc: Option<RpcServer>,
    pub gossiper: Gossiper,
    pub discover_peer: Option<DiscoverPeer>,
    pub matchmaker: Option<Matchmaker>,
}

impl Default for IntentGossiper {
    fn default() -> Self {
        Self {
            address: Multiaddr::from_str("/ip4/0.0.0.0/tcp/26659").unwrap(),
            rpc: None,
            subscription_filter: SubscriptionFilter::RegexFilter(
                Regex::new("asset_v\\d{1,2}").unwrap(),
            ),

            topics: vec!["asset_v0"].into_iter().map(String::from).collect(),
            gossiper: Gossiper::new(),
            matchmaker: None,
            discover_peer: Some(DiscoverPeer::default()),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    pub ledger: Ledger,
    pub intent_gossiper: IntentGossiper,
}

impl Config {
    pub fn new(base_dir: impl AsRef<Path>, chain_id: ChainId) -> Self {
        Self {
            ledger: Ledger::new(base_dir, chain_id),
            intent_gossiper: IntentGossiper::default(),
        }
    }

    /// Load config from expected path in the `base_dir` or generate a new one
    /// if it doesn't exist. Terminates with an error if the config loading
    /// fails.
    pub fn load(base_dir: &Path, chain_id: &ChainId) -> Self {
        match Self::read(base_dir, chain_id) {
            Ok(config) => config,
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
}

impl Config {
    /// Read the config from a file, or generate a default one and write it to
    /// a file if it doesn't already exist.
    pub fn read(base_dir: &Path, chain_id: &ChainId) -> Result<Self> {
        let file_path = Self::file_path(base_dir, chain_id);
        let file_name = file_path.to_str().expect("Expected UTF-8 file path");
        if !file_path.exists() {
            return Self::generate(base_dir, chain_id, true);
        };
        let mut config = config::Config::new();
        config
            .merge(config::File::with_name(file_name))
            .map_err(Error::ReadError)?;
        config.try_into().map_err(Error::DeserializationError)
    }

    /// Generate configuration and write it to a file.
    pub fn generate(
        base_dir: &Path,
        chain_id: &ChainId,
        replace: bool,
    ) -> Result<Self> {
        let config = Config::new(base_dir, chain_id.clone());
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

    fn file_path(base_dir: &Path, chain_id: &ChainId) -> PathBuf {
        // Join base dir to the chain ID
        base_dir.join(chain_id.to_string()).join(FILENAME)
    }
}

impl IntentGossiper {
    pub fn update(
        &mut self,
        addr: Option<Multiaddr>,
        rpc: Option<SocketAddr>,
        matchmaker_path: Option<PathBuf>,
        tx_code_path: Option<PathBuf>,
        ledger_addr: Option<tendermint::net::Address>,
        filter_path: Option<PathBuf>,
    ) {
        if let Some(addr) = addr {
            self.address = addr;
        }

        let matchmaker_arg = matchmaker_path;
        let tx_code_arg = tx_code_path;
        let ledger_address_arg = ledger_addr;
        let filter_arg = filter_path;
        if let Some(mut matchmaker_cfg) = self.matchmaker.as_mut() {
            if let Some(matchmaker) = matchmaker_arg {
                matchmaker_cfg.matchmaker = matchmaker
            }
            if let Some(tx_code) = tx_code_arg {
                matchmaker_cfg.tx_code = tx_code
            }
            if let Some(ledger_address) = ledger_address_arg {
                matchmaker_cfg.ledger_address = ledger_address
            }
            if let Some(filter) = filter_arg {
                matchmaker_cfg.filter = Some(filter)
            }
        } else if let (Some(matchmaker), Some(tx_code), Some(ledger_address)) = (
            matchmaker_arg.as_ref(),
            tx_code_arg.as_ref(),
            ledger_address_arg.as_ref(),
        ) {
            self.matchmaker = Some(Matchmaker {
                matchmaker: matchmaker.clone(),
                tx_code: tx_code.clone(),
                ledger_address: ledger_address.clone(),
                filter: filter_arg,
            });
        } else if matchmaker_arg.is_some()
            || tx_code_arg.is_some()
            || ledger_address_arg.is_some()
        // if at least one argument is not none then fail
        {
            panic!(
                "No complete matchmaker configuration found (matchmaker code \
                 path, tx code path, and ledger address). Please update the \
                 configuration with default value or use all cli argument to \
                 use the matchmaker"
            );
        }
        if let Some(address) = rpc {
            self.rpc = Some(RpcServer { address });
        }
    }

    #[cfg(any(test, feature = "testing"))]
    pub fn default_with_address(
        ip: String,
        port: u32,
        peers_info: Vec<(String, u32, PeerId)>,
        mdns: bool,
        kademlia: bool,
        matchmaker: bool,
        rpc: bool,
    ) -> Self {
        let mut gossiper_config = IntentGossiper::default();
        let mut discover_config = DiscoverPeer::default();

        gossiper_config.address =
            Multiaddr::from_str(format!("/ip4/{}/tcp/{}", ip, port).as_str())
                .unwrap();

        if matchmaker {
            gossiper_config.matchmaker = Some(Matchmaker {
                matchmaker: "../wasm/mm_token_exch.wasm".parse().unwrap(),
                tx_code: "../wasm/tx_from_intent.wasm".parse().unwrap(),
                ledger_address: "0.0.0.0:26657".parse().unwrap(),
                filter: None,
            })
        }

        if rpc {
            gossiper_config.rpc = Some(RpcServer::default())
        }

        let bootstrap_peers: HashSet<PeerAddress> = peers_info
            .iter()
            .map(|info| PeerAddress {
                address: Multiaddr::from_str(
                    format!("/ip4/{}/tcp/{}", info.0, info.1).as_str(),
                )
                .unwrap(),
                peer_id: info.2,
            })
            .collect();
        discover_config.bootstrap_peers = bootstrap_peers;
        discover_config.mdns = mdns;
        discover_config.kademlia = kademlia;

        gossiper_config.discover_peer = Some(discover_config);

        gossiper_config
    }
}
