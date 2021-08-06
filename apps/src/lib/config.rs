//! Node and client configuration
use std::collections::HashSet;
use std::fmt::Display;
use std::fs::{create_dir_all, File};
use std::io::Write;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use libp2p::multiaddr::{Multiaddr, Protocol};
use libp2p::multihash::Multihash;
use libp2p::PeerId;
use regex::Regex;
use serde::{de, Deserialize, Serialize};
use tendermint::net;
use thiserror::Error;

use crate::gossiper::Gossiper;

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
    #[error("Error while creating config file: {0}")]
    FileError(std::io::Error),
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

pub const BASEDIR: &str = ".anoma";
pub const FILENAME: &str = "config.toml";
pub const TENDERMINT_DIR: &str = "tendermint";
pub const DB_DIR: &str = "db";
// TODO: change the ID for the production chain
pub const DEFAULT_CHAIN_ID: &str = "anoma-devchain-00000";

pub type Result<T> = std::result::Result<T, Error>;
const VALUE_AFTER_TABLE_ERROR_MSG: &str = r#"
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
    pub tendermint: PathBuf,
    pub db: PathBuf,
    pub address: SocketAddr,
    pub network: String,
}

impl Default for Ledger {
    fn default() -> Self {
        Self {
            // this two value are override when generating a default config in
            // config::generate(base_dir). There must be a better way ?
            tendermint: PathBuf::from(BASEDIR).join(TENDERMINT_DIR),
            db: PathBuf::from(BASEDIR).join(DB_DIR).join(DEFAULT_CHAIN_ID),
            address: SocketAddr::new(
                IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
                26658,
            ),
            network: String::from("mainnet"),
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
                39111,
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DiscoverPeer {
    pub max_discovery_peers: u64,
    pub kademlia: bool,
    pub mdns: bool,
    pub bootstrap_peers: HashSet<PeerAddress>, /* TODO add reserved_peers(explicit peers for gossipsub network, to not
                                                * be added to kademlia) */
}

impl Default for DiscoverPeer {
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
            address: Multiaddr::from_str("/ip4/0.0.0.0/tcp/20201").unwrap(),
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
    pub ledger: Option<Ledger>,
    pub intent_gossiper: Option<IntentGossiper>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            ledger: Some(Ledger::default()),
            intent_gossiper: Some(IntentGossiper::default()),
        }
    }
}

impl Config {
    // TODO try to check from any "config.*" file instead of only .toml
    pub fn read(base_dir_path: &Path) -> Result<Self> {
        let file_path = base_dir_path.join(FILENAME);
        let mut config = config::Config::new();
        config
            .merge(config::File::with_name(
                file_path.to_str().expect("uncorrect file"),
            ))
            .map_err(Error::ReadError)?;
        config.try_into().map_err(Error::DeserializationError)
    }

    pub fn generate(base_dir: &Path, replace: bool) -> Result<Self> {
        let mut config = Config::default();
        let mut ledger_cfg = config
            .ledger
            .as_mut()
            .expect("safe because default has ledger");
        ledger_cfg.db = base_dir.join(DB_DIR).join(DEFAULT_CHAIN_ID);
        ledger_cfg.tendermint = base_dir.join(TENDERMINT_DIR);
        config.write(base_dir, replace)?;
        Ok(config)
    }

    // TODO add format in config instead and serialize it to that format
    pub fn write(&self, base_dir: &Path, replace: bool) -> Result<()> {
        create_dir_all(&base_dir).map_err(Error::FileError)?;
        let file_path = base_dir.join(FILENAME);
        if file_path.exists() && !replace {
            Err(Error::AlreadyExistingConfig(file_path))
        } else {
            let mut file = File::create(file_path).map_err(Error::FileError)?;
            let toml = toml::ser::to_string(&self).map_err(|err| {
                if let toml::ser::Error::ValueAfterTable = err {
                    tracing::error!("{}", VALUE_AFTER_TABLE_ERROR_MSG);
                }
                Error::TomlError(err)
            })?;
            file.write_all(toml.as_bytes()).map_err(Error::WriteError)
        }
    }
}

#[cfg(any(test, feature = "testing"))]
impl IntentGossiper {
    pub fn default_with_address(
        ip: String,
        port: u32,
        peers_info: Vec<(String, u32, PeerId)>,
        mdns: bool,
        kademlia: bool,
    ) -> Self {
        let mut gossiper_config = IntentGossiper::default();
        let mut discover_config = DiscoverPeer::default();

        gossiper_config.address =
            Multiaddr::from_str(format!("/ip4/{}/tcp/{}", ip, port).as_str())
                .unwrap();

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
