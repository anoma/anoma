use std::collections::{HashMap, HashSet};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::path::PathBuf;
use std::str::FromStr;

use anoma::types::chain::ChainId;
use anoma::types::key::ed25519::Keypair;
use anoma::types::{address, token};
use borsh::BorshSerialize;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use rand::prelude::ThreadRng;
use rand::thread_rng;
use serde_json::json;
#[cfg(not(feature = "ABCI"))]
use tendermint::node::Id as TendermintNodeId;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_stable::node::Id as TendermintNodeId;

use crate::cli::{self, args};
use crate::config::genesis::genesis_config;
use crate::config::global::GlobalConfig;
use crate::config::{
    self, genesis, Config, IntentGossiper, PeerAddress, TendermintMode,
};
use crate::node::gossip;
use crate::node::ledger::tendermint_node;
use crate::wallet::Wallet;
use crate::wasm_loader;

pub const NET_ACCOUNTS_DIR: &str = "setup";
pub const NET_OTHER_ACCOUNTS_DIR: &str = "other";
/// Github URL prefix of released Anoma network configs
const RELEASE_PREFIX: &str =
    "https://github.com/heliaxdev/anoma-network-config/releases/download";

/// Configure Anoma to join an existing network. The chain must be released in
/// the <https://github.com/heliaxdev/anoma-network-config> repository.
pub async fn join_network(
    global_args: args::Global,
    args::JoinNetwork { chain_id }: args::JoinNetwork,
) {
    let release_filename = format!("{}.tar.gz", chain_id);
    let release_url =
        format!("{}/{}/{}", RELEASE_PREFIX, chain_id, release_filename);

    // Read or download the release archive
    println!("Downloading config release from {} ...", release_url);
    let release = download_file(release_url).await;

    // Decode and unpack the archive
    let mut decoder = GzDecoder::new(&release[..]);
    let mut tar = String::new();
    decoder.read_to_string(&mut tar).unwrap();
    let mut archive = tar::Archive::new(tar.as_bytes());
    archive.unpack(".").unwrap();

    // If the base-dir is not the default, move the network config into it
    let base_dir_name = global_args.base_dir.file_name();
    if let Some(base_dir_name) = base_dir_name {
        if base_dir_name != config::DEFAULT_BASE_DIR {
            tokio::fs::rename(".anoma", base_dir_name).await.unwrap();
        }
    }

    println!("Successfully configured for chain ID {}", chain_id);
}

/// Initialize a new test network with the given validators and faucet accounts.
pub fn init_network(
    global_args: args::Global,
    args::InitNetwork {
        genesis_path,
        wasm_checksums_path,
        chain_id_prefix,
        unsafe_dont_encrypt,
        consensus_timeout_commit,
        localhost,
        allow_duplicate_ip,
        dont_archive,
    }: args::InitNetwork,
) {
    let mut config = genesis_config::open_genesis_config(&genesis_path);

    // Update the WASM checksums
    let checksums =
        wasm_loader::Checksums::read_checksums_file(&wasm_checksums_path);
    config.wasm.iter_mut().for_each(|(name, config)| {
        // Find the sha256 from checksums.json
        let name = format!("{}.wasm", name);
        // Full name in format `{name}.{sha256}.wasm`
        let full_name = checksums.0.get(&name).unwrap();
        let hash = full_name
            .split_once(".")
            .unwrap()
            .1
            .split_once(".")
            .unwrap()
            .0;
        config.sha256 = genesis_config::HexString(hash.to_owned());
    });

    let temp_chain_id = chain_id_prefix.temp_chain_id();
    let temp_dir = global_args.base_dir.join(temp_chain_id.as_str());
    // The `temp_chain_id` gets renamed after we have chain ID
    let accounts_dir = temp_dir.join(NET_ACCOUNTS_DIR);
    // Base dir used in account sub-directories
    let accounts_temp_dir =
        PathBuf::from(config::DEFAULT_BASE_DIR).join(temp_chain_id.as_str());

    let mut rng: ThreadRng = thread_rng();

    let mut persistent_peers: Vec<TendermintAddress> =
        Vec::with_capacity(config.validator.len());
    // Intent gossiper config bootstrap peers where we'll add the address for
    // each validator's node
    let mut seed_peers: HashSet<PeerAddress> =
        HashSet::with_capacity(config.validator.len());
    let mut gossiper_configs: HashMap<String, IntentGossiper> =
        HashMap::with_capacity(config.validator.len());
    // Other accounts owned by one of the validators
    let mut validator_owned_accounts: HashMap<
        String,
        genesis_config::EstablishedAccountConfig,
    > = HashMap::default();

    // We need a temporary copy to be able to use this inside the validator
    // loop, which has mutable borrow on the config.
    let established_accounts = config.established.clone();
    // Iterate over each validator, generating keys and addresses
    config.validator.iter_mut().for_each(|(name, config)| {
        let validator_dir = accounts_dir.join(name);

        // Generate a node key
        let node_keypair = Keypair::generate(&mut rng);
        let node_pk: ed25519_dalek::PublicKey =
            node_keypair.public.clone().into();

        // Derive the node ID from the node key
        let node_id: TendermintNodeId = node_pk.into();

        // Convert and write the keypair into Tendermint node_key.json file
        let node_key: ed25519_dalek::Keypair = node_keypair.into();
        let tm_node_key = base64::encode(node_key.to_bytes());
        let tm_node_keypair_json = json!({
            "priv_key": {
                "type": "tendermint/PrivKeyEd25519",
                "value": tm_node_key,
            }
        });
        let chain_dir = validator_dir.join(&accounts_temp_dir);
        let tm_home_dir = chain_dir.join("tendermint");
        let tm_config_dir = tm_home_dir.join("config");
        fs::create_dir_all(&tm_config_dir)
            .expect("Couldn't create validator directory");
        let path = tm_config_dir.join("node_key.json");
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&path)
            .expect("Couldn't create validator node key file");
        serde_json::to_writer_pretty(file, &tm_node_keypair_json)
            .expect("Couldn't write validator node key file");
        tendermint_node::write_validator_state(&tm_home_dir);

        // Build the list of persistent peers from the validators' node IDs
        let peer = TendermintAddress::from_str(&format!(
            "{}@{}",
            node_id,
            config.net_address.as_ref().unwrap(),
        ))
        .expect("Validator address must be valid");
        persistent_peers.push(peer);
        // Add a Intent gossiper bootstrap peer from the validator's IP
        let mut gossiper_config = IntentGossiper::default();
        // Generate P2P identity
        let p2p_idenity = gossip::p2p::Identity::gen(&chain_dir);
        let peer_id = p2p_idenity.peer_id();
        let ledger_addr =
            SocketAddr::from_str(config.net_address.as_ref().unwrap()).unwrap();
        let ip = ledger_addr.ip().to_string();
        let first_port = ledger_addr.port();
        let intent_peer_address = libp2p::Multiaddr::from_str(
            format!("/ip4/{}/tcp/{}", ip, first_port + 3).as_str(),
        )
        .unwrap();

        gossiper_config.address = if localhost {
            intent_peer_address.clone()
        } else {
            libp2p::Multiaddr::from_str(
                format!("/ip4/0.0.0.0/tcp/{}", first_port + 3).as_str(),
            )
            .unwrap()
        };
        if let Some(discover) = gossiper_config.discover_peer.as_mut() {
            // Disable mDNS local network peer discovery on the validator nodes
            discover.mdns = false;
        }
        let intent_peer = PeerAddress {
            address: intent_peer_address,
            peer_id,
        };

        // Generate the consensus, account and reward keys
        // The `temp_chain_id` gets renamed after we have chain ID
        let mut wallet = Wallet::load_or_new(&chain_dir);
        let consensus_key_alias = format!("{}-consensus-key", name);
        println!("Generating validator {} consensus key...", name);
        let (_alias, consensus_keypair) =
            wallet.gen_key(Some(consensus_key_alias), unsafe_dont_encrypt);
        let account_key_alias = format!("{}-account-key", name);
        println!("Generating validator {} account key...", name);
        let (_alias, account_keypair) =
            wallet.gen_key(Some(account_key_alias), unsafe_dont_encrypt);
        let reward_key_alias = format!("{}-reward-key", name);
        println!(
            "Generating validator {} staking reward account key...",
            name
        );
        let (_alias, reward_keypair) =
            wallet.gen_key(Some(reward_key_alias), unsafe_dont_encrypt);
        // Add the validator public keys to genesis config
        config.consensus_public_key = Some(genesis_config::HexString(
            consensus_keypair.public.to_string(),
        ));
        config.account_public_key = Some(genesis_config::HexString(
            account_keypair.public.to_string(),
        ));
        config.staking_reward_public_key =
            Some(genesis_config::HexString(reward_keypair.public.to_string()));

        // Generate account and reward addresses
        let address = address::gen_established_address("validator account");
        let reward_address =
            address::gen_established_address("validator reward account");
        config.address = Some(address.to_string());
        config.staking_reward_address = Some(reward_address.to_string());

        // Write consensus key for Tendermint
        tendermint_node::write_validator_key(
            &tm_home_dir,
            &address,
            &consensus_keypair,
        );

        // Write keypairs to wallet
        wallet.add_address(name.clone(), address);
        wallet.add_address(format!("{}-reward", &name), reward_address);

        // Check if there's a matchmaker configured for this validator node
        match (
            &config.matchmaker_account,
            &config.matchmaker_code,
            &config.matchmaker_tx,
        ) {
            (Some(account), Some(mm_code), Some(tx_code)) => {
                if config.intent_gossip_seed.unwrap_or_default() {
                    eprintln!("A bootstrap node cannot run matchmakers");
                    cli::safe_exit(1)
                }
                match established_accounts.as_ref().and_then(|e| e.get(account))
                {
                    Some(matchmaker) => {
                        let mut matchmaker = matchmaker.clone();

                        init_established_account(
                            account,
                            &mut wallet,
                            &mut matchmaker,
                            unsafe_dont_encrypt,
                        );
                        validator_owned_accounts
                            .insert(account.clone(), matchmaker);

                        let ledger_address = TendermintAddress::from_str(
                            &format!("127.0.0.1:{}", first_port + 1),
                        )
                        .unwrap();
                        gossiper_config.matchmaker = Some(config::Matchmaker {
                            matchmaker: mm_code.clone().into(),
                            tx_code: tx_code.clone().into(),
                            ledger_address,
                            filter: None,
                        });
                    }
                    None => {
                        eprintln!(
                            "Misconfigured validator's matchmaker. No \
                             established account with alias {} found",
                            account
                        );
                        cli::safe_exit(1)
                    }
                }
            }
            (None, None, None) => {}
            _ => {
                eprintln!(
                    "Misconfigured validator's matchmaker. \
                     `matchmaker_account`, `matchmaker_code` and \
                     `matchmaker_tx` must be all or none present."
                );
                cli::safe_exit(1)
            }
        }

        // Store the gossip config
        gossiper_configs.insert(name.clone(), gossiper_config);
        if config.intent_gossip_seed.unwrap_or_default() {
            seed_peers.insert(intent_peer);
        }

        wallet.save().unwrap();
    });

    if seed_peers.is_empty() && config.validator.len() > 1 {
        tracing::warn!(
            "At least 1 validator with `intent_gossip_seed = true` is needed \
             to established connection between the intent gossiper nodes"
        );
    }

    // Create a wallet for all accounts other than validators
    let mut wallet =
        Wallet::load_or_new(&accounts_dir.join(NET_OTHER_ACCOUNTS_DIR));
    if let Some(established) = &mut config.established {
        established.iter_mut().for_each(|(name, config)| {
            match validator_owned_accounts.get(name) {
                Some(validator_owned) => {
                    *config = validator_owned.clone();
                }
                None => {
                    init_established_account(
                        name,
                        &mut wallet,
                        config,
                        unsafe_dont_encrypt,
                    );
                }
            }
        })
    }

    if let Some(token) = &mut config.token {
        token.iter_mut().for_each(|(name, config)| {
            if config.address.is_none() {
                let address = address::gen_established_address("token");
                config.address = Some(address.to_string());
                wallet.add_address(name.clone(), address);
            }
            if config.vp.is_none() {
                config.vp = Some("vp_token".to_string());
            }
        })
    }

    if let Some(implicit) = &mut config.implicit {
        implicit.iter_mut().for_each(|(name, config)| {
            if config.public_key.is_none() {
                println!(
                    "Generating implicit account {} key and address ...",
                    name
                );
                let (_alias, keypair) =
                    wallet.gen_key(Some(name.clone()), unsafe_dont_encrypt);
                let public_key =
                    genesis_config::HexString(keypair.public.to_string());
                config.public_key = Some(public_key);
            }
        })
    }

    // Make a copy of genesis config without validator net addresses to
    // `write_genesis_config`. Keep the original, because we still need the
    // addresses to configure validators.
    let mut config_clean = config.clone();
    config_clean
        .validator
        .iter_mut()
        .for_each(|(_name, config)| {
            config.net_address = None;
        });

    // Generate the chain ID first
    let genesis = genesis_config::load_genesis_config(config_clean.clone());
    let genesis_bytes = genesis.try_to_vec().unwrap();
    let chain_id = ChainId::from_genesis(chain_id_prefix, genesis_bytes);
    let chain_dir = global_args.base_dir.join(chain_id.as_str());
    let genesis_path = global_args
        .base_dir
        .join(format!("{}.toml", chain_id.as_str()));

    // Write the genesis file
    genesis_config::write_genesis_config(&config_clean, &genesis_path);

    // Add genesis addresses and save the wallet with other account keys
    wallet.add_genesis_addresses(config_clean.clone());
    wallet.save().unwrap();

    // Write the global config setting the default chain ID
    let global_config = GlobalConfig::new(chain_id.clone());
    global_config.write(&global_args.base_dir).unwrap();

    // Rename the generate chain config dir from `temp_chain_id` to `chain_id`
    std::fs::rename(&temp_dir, &chain_dir).unwrap();

    config.validator.iter().for_each(|(name, _config)| {
        let validator_dir = global_args
            .base_dir
            .join(chain_id.as_str())
            .join(NET_ACCOUNTS_DIR)
            .join(name)
            .join(config::DEFAULT_BASE_DIR);
        let temp_validator_chain_dir =
            validator_dir.join(temp_chain_id.as_str());
        let validator_chain_dir = validator_dir.join(&chain_id.as_str());
        fs::create_dir_all(&validator_chain_dir)
            .expect("Couldn't create validator directory");
        // Rename the generated directories for validators from `temp_chain_id`
        // to `chain_id`
        std::fs::rename(&temp_validator_chain_dir, &validator_chain_dir)
            .unwrap();
        // Write the genesis and global config into validator sub-dirs
        genesis_config::write_genesis_config(
            &config,
            validator_dir.join(format!("{}.toml", chain_id.as_str())),
        );
        global_config.write(validator_dir).unwrap();
        // Add genesis addresses to the validator's wallet
        let mut wallet = Wallet::load_or_new(&validator_chain_dir);
        wallet.add_genesis_addresses(config_clean.clone());
        wallet.save().unwrap();
    });

    // Generate the validators' ledger and intent gossip config
    config.validator.iter_mut().enumerate().for_each(
        |(ix, (name, validator_config))| {
            let accounts_dir = chain_dir.join(NET_ACCOUNTS_DIR);
            let validator_dir =
                accounts_dir.join(name).join(config::DEFAULT_BASE_DIR);
            let mut config = Config::load(
                &validator_dir,
                &chain_id,
                TendermintMode::Validator,
            );

            // Configure the ledger
            config.ledger.genesis_time = genesis.genesis_time.into();
            // In `config::Ledger`'s `base_dir`, `chain_id` and `tendermint`,
            // the paths are prefixed with `validator_dir` given in the first
            // parameter. We need to remove this prefix, because
            // these sub-directories will be moved to validators' root
            // directories.
            config.ledger.shell.base_dir = config::DEFAULT_BASE_DIR.into();
            // Add a ledger P2P persistent peers
            config.ledger.tendermint.p2p_persistent_peers = persistent_peers
                    .iter()
                    .enumerate()
                    .filter_map(|(index, peer)|
                        // we do not add the validator in its own persistent peer list
                        if index != ix  {
                            Some(peer.to_owned())
                        } else {
                            None
                        })
                    .collect();
            config.ledger.tendermint.consensus_timeout_commit =
                consensus_timeout_commit;
            config.ledger.tendermint.p2p_allow_duplicate_ip =
                allow_duplicate_ip;
            // Clear the net address from the config and use it to set ports
            let net_address = validator_config.net_address.take().unwrap();
            let first_port = SocketAddr::from_str(&net_address).unwrap().port();
            if !localhost {
                config
                    .ledger
                    .tendermint
                    .p2p_address
                    .set_ip(IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)));
            }
            config.ledger.tendermint.p2p_address.set_port(first_port);
            if !localhost {
                config
                    .ledger
                    .tendermint
                    .rpc_address
                    .set_ip(IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)));
            }
            config
                .ledger
                .tendermint
                .rpc_address
                .set_port(first_port + 1);
            config.ledger.shell.ledger_address.set_port(first_port + 2);
            // Validator node should turned off peer exchange reactor
            config.ledger.tendermint.p2p_pex = false;

            // Configure the intent gossiper
            config.intent_gossiper = gossiper_configs.remove(name).unwrap();
            config.intent_gossiper.seed_peers = seed_peers.clone();
            config.intent_gossiper.rpc = Some(config::RpcServer {
                address: SocketAddr::new(
                    IpAddr::V4(if localhost {
                        Ipv4Addr::new(127, 0, 0, 1)
                    } else {
                        Ipv4Addr::new(0, 0, 0, 0)
                    }),
                    first_port + 4,
                ),
            });

            config.write(&validator_dir, &chain_id, true).unwrap();
        },
    );

    // Update the ledger config persistent peers and save it
    let mut config = Config::load(
        &global_args.base_dir,
        &chain_id,
        TendermintMode::Validator,
    );
    config.ledger.tendermint.p2p_persistent_peers = persistent_peers;
    config.ledger.tendermint.consensus_timeout_commit =
        consensus_timeout_commit;
    config.ledger.tendermint.p2p_allow_duplicate_ip = allow_duplicate_ip;
    // Open P2P address
    if !localhost {
        config
            .ledger
            .tendermint
            .p2p_address
            .set_ip(IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)));
    }
    config.ledger.genesis_time = genesis.genesis_time.into();
    config.intent_gossiper.seed_peers = seed_peers;
    config
        .write(&global_args.base_dir, &chain_id, true)
        .unwrap();

    println!("Derived chain ID: {}", chain_id);
    println!(
        "Genesis file generated at {}",
        genesis_path.to_string_lossy()
    );

    // Create a release tarball for anoma-network-config
    if !dont_archive {
        let mut release = tar::Builder::new(Vec::new());
        let release_genesis_path = PathBuf::from(config::DEFAULT_BASE_DIR)
            .join(format!("{}.toml", chain_id.as_str()));
        release
            .append_path_with_name(genesis_path, release_genesis_path)
            .unwrap();
        let global_config_path = GlobalConfig::file_path(&global_args.base_dir);
        let release_global_config_path =
            GlobalConfig::file_path(config::DEFAULT_BASE_DIR);
        release
            .append_path_with_name(
                global_config_path,
                release_global_config_path,
            )
            .unwrap();
        let chain_config_path =
            Config::file_path(&global_args.base_dir, &chain_id);
        let release_chain_config_path =
            Config::file_path(config::DEFAULT_BASE_DIR, &chain_id);
        release
            .append_path_with_name(chain_config_path, release_chain_config_path)
            .unwrap();
        let release_wasm_checksums_path = "wasm/checksums.json";
        release
            .append_path_with_name(
                &wasm_checksums_path,
                release_wasm_checksums_path,
            )
            .unwrap();

        // Gzip tar release and write to file
        let release_file = format!("{}.tar.gz", chain_id);
        let compressed_file = File::create(&release_file).unwrap();
        let mut encoder =
            GzEncoder::new(compressed_file, Compression::default());
        encoder.write_all(&release.into_inner().unwrap()).unwrap();
        encoder.finish().unwrap();
        println!("Release archive created at {}", release_file);
    }
}

fn init_established_account(
    name: impl AsRef<str>,
    wallet: &mut Wallet,
    config: &mut genesis_config::EstablishedAccountConfig,
    unsafe_dont_encrypt: bool,
) {
    if config.address.is_none() {
        let address = address::gen_established_address("established");
        config.address = Some(address.to_string());
        wallet.add_address(name.as_ref().to_string(), address);
    }
    if config.public_key.is_none() {
        println!("Generating established account {} key...", name.as_ref());
        let (_alias, keypair) = wallet.gen_key(
            Some(format!("{}-key", name.as_ref())),
            unsafe_dont_encrypt,
        );
        let public_key = genesis_config::HexString(keypair.public.to_string());
        config.public_key = Some(public_key);
    }
    if config.vp.is_none() {
        config.vp = Some("vp_user".to_string());
    }
}

/// Initialize genesis validator's address, staking reward address,
/// consensus key, validator account key and staking rewards key and use
/// it in the ledger's node.
pub fn init_genesis_validator(
    global_args: args::Global,
    args::InitGenesisValidator {
        alias,
        chain_id,
        unsafe_dont_encrypt,
    }: args::InitGenesisValidator,
) {
    let chain_dir = global_args.base_dir.join(chain_id.as_str());
    let mut wallet = Wallet::load_or_new(&chain_dir);
    let config = Config::load(
        &global_args.base_dir,
        &chain_id,
        TendermintMode::Validator,
    );
    init_genesis_validator_aux(
        &mut wallet,
        &config,
        alias,
        unsafe_dont_encrypt,
    );
}

/// Initialize genesis validator's address, staking reward address,
/// consensus key, validator account key and staking rewards key and use
/// it in the ledger's node.
fn init_genesis_validator_aux(
    wallet: &mut Wallet,
    config: &Config,
    alias: String,
    unsafe_dont_encrypt: bool,
) -> genesis::Validator {
    // Generate validator address
    let validator_address =
        address::gen_established_address("genesis validator address");
    let validator_address_alias = alias.clone();
    if wallet
        .add_address(validator_address_alias.clone(), validator_address.clone())
        .is_none()
    {
        cli::safe_exit(1)
    }
    // Generate staking reward address
    let rewards_address =
        address::gen_established_address("genesis validator reward address");
    let rewards_address_alias = format!("{}-rewards", alias);
    if wallet
        .add_address(rewards_address_alias.clone(), rewards_address.clone())
        .is_none()
    {
        cli::safe_exit(1)
    }

    println!("Generating validator account key...");
    let (validator_key_alias, validator_key) = wallet.gen_key(
        Some(format!("{}-validator-key", alias)),
        unsafe_dont_encrypt,
    );
    println!("Generating consensus key...");
    let (consensus_key_alias, consensus_key) = wallet.gen_key(
        Some(format!("{}-consensus-key", alias)),
        unsafe_dont_encrypt,
    );
    println!("Generating staking reward account key...");
    let (rewards_key_alias, rewards_key) = wallet
        .gen_key(Some(format!("{}-rewards-key", alias)), unsafe_dont_encrypt);

    wallet.save().unwrap_or_else(|err| eprintln!("{}", err));

    let tendermint_home = &config.ledger.tendermint_dir();
    tendermint_node::write_validator_key(
        tendermint_home,
        &validator_address,
        &consensus_key,
    );
    tendermint_node::write_validator_state(tendermint_home);

    println!();
    println!("The validator's addresses and keys were stored in the wallet:");
    println!("  Validator address \"{}\"", validator_address_alias);
    println!("  Staking reward address \"{}\"", rewards_address_alias);
    println!("  Validator account key \"{}\"", validator_key_alias);
    println!("  Consensus key \"{}\"", consensus_key_alias);
    println!("  Staking reward key \"{}\"", rewards_key_alias);
    println!(
        "The ledger node has been setup to use this validator's address and \
         consensus key."
    );
    println!();
    let genesis_validator = genesis::Validator {
        pos_data: anoma::ledger::pos::GenesisValidator {
            address: validator_address,
            staking_reward_address: rewards_address,
            tokens: token::Amount::whole(200_000),
            consensus_key: consensus_key.public.clone(),
            staking_reward_key: rewards_key.public.clone(),
        },
        account_key: validator_key.public.clone(),
        non_staked_balance: token::Amount::whole(100_000),
        // TODO replace with https://github.com/anoma/anoma/issues/25)
        validator_vp_code_path: "wasm/vp_user.wasm".into(),
        // TODO: very fake hash
        validator_vp_sha256: [0; 32],
        reward_vp_code_path: "wasm/vp_user.wasm".into(),
        // TODO: very fake hash
        reward_vp_sha256: [0; 32],
    };
    println!("Validator account key {}", validator_key.public);
    println!("Consensus key {}", consensus_key.public);
    println!("Staking reward key {}", rewards_key.public);
    // TODO print in toml format after we have https://github.com/anoma/anoma/issues/425
    println!("Genesis validator config: {:#?}", genesis_validator);
    genesis_validator
}

async fn download_file(url: impl AsRef<str>) -> Vec<u8> {
    let url = url.as_ref();
    reqwest::get(url)
        .await
        .unwrap_or_else(|err| {
            eprintln!("File not found at {}. Error: {}", url, err);
            cli::safe_exit(1)
        })
        .bytes()
        .await
        .unwrap_or_else(|err| {
            eprintln!(
                "Failed to download file from {} with error: {}",
                url, err
            );
            cli::safe_exit(1)
        })
        .to_vec()
}
