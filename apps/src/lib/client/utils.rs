use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anoma::types::address;
use anoma::types::chain::ChainId;
use anoma::types::key::*;
use borsh::BorshSerialize;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use rand::prelude::ThreadRng;
use rand::thread_rng;
use serde_json::json;
use sha2::{Digest, Sha256};
#[cfg(not(feature = "ABCI"))]
use tendermint::node::Id as TendermintNodeId;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_stable::node::Id as TendermintNodeId;

use crate::cli::context::ENV_VAR_WASM_DIR;
use crate::cli::{self, args};
use crate::config::genesis::genesis_config;
use crate::config::genesis::genesis_config::HexString;
use crate::config::global::GlobalConfig;
use crate::config::{
    self, Config, IntentGossiper, PeerAddress, TendermintMode,
};
use crate::node::gossip;
use crate::node::ledger::tendermint_node;
use crate::wallet::Wallet;
use crate::wasm_loader;

pub const NET_ACCOUNTS_DIR: &str = "setup";
pub const NET_OTHER_ACCOUNTS_DIR: &str = "other";
/// Github URL prefix of released Anoma network configs
const ENV_VAR_NETWORK_CONFIGS_SERVER: &str = "ANOMA_NETWORK_CONFIGS_SERVER";
const DEFAULT_NETWORK_CONFIGS_SERVER: &str =
    "https://github.com/heliaxdev/anoma-network-config/releases/download";

/// We do pregenesis validator set up in this directory
const PREGENESIS_DIR: &str = "pregenesis";

/// Configure Anoma to join an existing network. The chain must be released in
/// the <https://github.com/heliaxdev/anoma-network-config> repository.
pub async fn join_network(
    global_args: args::Global,
    args::JoinNetwork {
        chain_id,
        genesis_validator,
    }: args::JoinNetwork,
) {
    use tokio::fs;

    let base_dir = &global_args.base_dir;
    let wasm_dir = global_args.wasm_dir.as_ref().cloned().or_else(|| {
        if let Ok(wasm_dir) = env::var(ENV_VAR_WASM_DIR) {
            let wasm_dir: PathBuf = wasm_dir.into();
            Some(wasm_dir)
        } else {
            None
        }
    });
    if let Some(wasm_dir) = wasm_dir.as_ref() {
        if wasm_dir.is_absolute() {
            eprintln!(
                "The arg `--wasm-dir` cannot be an absolute path. It is \
                 nested inside the chain directory."
            );
            cli::safe_exit(1);
        }
    }
    if let Err(err) = fs::canonicalize(base_dir).await {
        if err.kind() == std::io::ErrorKind::NotFound {
            // If the base-dir doesn't exist yet, create it
            fs::create_dir_all(base_dir).await.unwrap();
        }
    } else {
        // If the base-dir exists, check if it's already got this chain ID
        if fs::canonicalize(base_dir.join(chain_id.as_str()))
            .await
            .is_ok()
        {
            eprintln!("The chain directory for {} already exists.", chain_id);
            cli::safe_exit(1);
        }
    }
    let base_dir_full = fs::canonicalize(base_dir).await.unwrap();

    let release_filename = format!("{}.tar.gz", chain_id);
    let release_url = format!(
        "{}/{}",
        network_configs_url_prefix(&chain_id),
        release_filename
    );
    let cwd = env::current_dir().unwrap();

    // Read or download the release archive
    println!("Downloading config release from {} ...", release_url);
    let release = download_file(release_url).await;

    // Decode and unpack the archive
    let mut decoder = GzDecoder::new(&release[..]);
    let mut tar = String::new();
    decoder.read_to_string(&mut tar).unwrap();
    let mut archive = tar::Archive::new(tar.as_bytes());

    // If the base-dir is non-default, unpack the archive into a temp dir inside
    // first.
    let (unpack_dir, non_default_dir) =
        if base_dir_full != cwd.join(config::DEFAULT_BASE_DIR) {
            (base_dir.clone(), true)
        } else {
            (PathBuf::from_str(".").unwrap(), false)
        };
    archive.unpack(&unpack_dir).unwrap();

    let chain_dir = base_dir_full.join(chain_id.as_str());
    // Rename the base-dir from the default and rename wasm-dir, if non-default.
    if non_default_dir {
        // For compatibility for networks released with Anoma <= v0.4:
        // The old releases include the WASM directory at root path of the
        // archive. This has been moved into the chain directory, so if the
        // WASM dir is found at the old path, we move it to the new path.
        if let Ok(wasm_dir) =
            fs::canonicalize(unpack_dir.join(config::DEFAULT_WASM_DIR)).await
        {
            fs::rename(
                &wasm_dir,
                unpack_dir
                    .join(config::DEFAULT_BASE_DIR)
                    .join(chain_id.as_str())
                    .join(config::DEFAULT_WASM_DIR),
            )
            .await
            .unwrap();
        }

        // Move the chain dir
        fs::rename(
            unpack_dir
                .join(config::DEFAULT_BASE_DIR)
                .join(chain_id.as_str()),
            &chain_dir,
        )
        .await
        .unwrap();

        // Move the genesis file
        fs::rename(
            unpack_dir
                .join(config::DEFAULT_BASE_DIR)
                .join(format!("{}.toml", chain_id.as_str())),
            base_dir_full.join(format!("{}.toml", chain_id.as_str())),
        )
        .await
        .unwrap();

        // Move the global config
        fs::rename(
            unpack_dir
                .join(config::DEFAULT_BASE_DIR)
                .join(config::global::FILENAME),
            base_dir_full.join(config::global::FILENAME),
        )
        .await
        .unwrap();

        // Remove the default dir
        fs::remove_dir_all(unpack_dir.join(config::DEFAULT_BASE_DIR))
            .await
            .unwrap();
    }

    // Move wasm-dir and update config if it's non-default
    if let Some(wasm_dir) = wasm_dir.as_ref() {
        if wasm_dir.to_string_lossy() != config::DEFAULT_WASM_DIR {
            tokio::fs::rename(
                base_dir_full
                    .join(chain_id.as_str())
                    .join(config::DEFAULT_WASM_DIR),
                chain_dir.join(wasm_dir),
            )
            .await
            .unwrap();

            // Update the config
            let wasm_dir = wasm_dir.clone();
            let base_dir = base_dir.clone();
            let chain_id = chain_id.clone();
            tokio::task::spawn_blocking(move || {
                let mut config = Config::load(
                    &base_dir,
                    &chain_id,
                    global_args.mode.clone(),
                );
                config.wasm_dir = wasm_dir;
                config.write(&base_dir, &chain_id, true).unwrap();
            })
            .await
            .unwrap();
        }
    }

    if let Some(validator_alias) = genesis_validator {
        let genesis_file_path = global_args
            .base_dir
            .join(format!("{}.toml", chain_id.as_str()));
        let mut wallet =
            Wallet::load_or_new_from_genesis(&chain_dir, move || {
                genesis_config::open_genesis_config(genesis_file_path)
            });

        let address = wallet
            .find_address(&validator_alias)
            .unwrap_or_else(|| {
                eprintln!(
                    "Unable to find validator address for alias \
                     {validator_alias}"
                );
                cli::safe_exit(1)
            })
            .clone();

        let pregenesis_dir = base_dir.join(PREGENESIS_DIR);
        let mut pregenesis_wallet = Wallet::load(&pregenesis_dir)
            .unwrap_or_else(|| {
                eprintln!(
                    "No pre-genesis wallet found in {}",
                    pregenesis_dir.to_string_lossy()
                );
                cli::safe_exit(1)
            });

        let alias = validator_key_alias(&validator_alias);
        pregenesis_wallet.find_key(&alias).unwrap_or_else(|_err| {
            eprintln!("Missing validator key with alias {}", alias);
            cli::safe_exit(1)
        });

        let alias = consensus_key_alias(&validator_alias);
        let consensus_key =
            pregenesis_wallet.find_key(&alias).unwrap_or_else(|_err| {
                eprintln!("Missing consensus key with alias {}", alias);
                cli::safe_exit(1)
            });

        let alias = rewards_key_alias(&validator_alias);
        pregenesis_wallet.find_key(&alias).unwrap_or_else(|_err| {
            eprintln!("Missing rewards key with alias {}", alias);
            cli::safe_exit(1)
        });

        let alias = tendermint_node_key_alias(&validator_alias);
        let tendermint_node_key =
            pregenesis_wallet.find_key(&alias).unwrap_or_else(|_err| {
                eprintln!("Missing validator key with alias {}", alias);
                cli::safe_exit(1)
            });

        let tendermint_node_key: ed25519::SecretKey =
            tendermint_node_key.try_to_sk().unwrap_or_else(|_err| {
                eprintln!("Tendermint node key must be ed25519");
                cli::safe_exit(1)
            });

        let tm_home_dir = chain_dir.join("tendermint");

        // Write consensus key to tendermint home
        tendermint_node::write_validator_key(
            &tm_home_dir,
            &address,
            &*consensus_key,
        );

        // Write tendermint node key
        write_tendermint_node_key(&tm_home_dir, tendermint_node_key);

        // Pre-initialize tendermint validator state
        tendermint_node::write_validator_state(&tm_home_dir);

        // Extend the current wallet from the pre-genesis wallet.
        // This takes the validator keys to be usable in future commands (e.g.
        // to sign a tx from validator account using the account key).
        wallet.extend(pregenesis_wallet);

        wallet.set_validator_address(address);

        wallet.save().unwrap();

        // Update the config from the default non-validator settings to
        // validator settings
        let base_dir = base_dir.clone();
        let chain_id = chain_id.clone();
        tokio::task::spawn_blocking(move || {
            let mut config = Config::load(&base_dir, &chain_id, None);
            config.ledger.tendermint.tendermint_mode =
                TendermintMode::Validator;
            // Validator node should turned off peer exchange reactor
            config.ledger.tendermint.p2p_pex = false;
            config.write(&base_dir, &chain_id, true).unwrap();
        })
        .await
        .unwrap();
    }
    println!("Successfully configured for chain ID {}", chain_id);
}

/// Length of a Tendermint Node ID in bytes
const TENDERMINT_NODE_ID_LENGTH: usize = 20;

/// Derive Tendermint node ID from public key
fn id_from_pk(pk: &ed25519::PublicKey) -> TendermintNodeId {
    let digest = Sha256::digest(pk.try_to_vec().unwrap().as_slice());
    let mut bytes = [0u8; TENDERMINT_NODE_ID_LENGTH];
    bytes.copy_from_slice(&digest[..TENDERMINT_NODE_ID_LENGTH]);
    TendermintNodeId::new(bytes)
}

/// Initialize a new test network from the given configuration.
///
/// For any public keys that are not specified in the genesis configuration,
/// this command will generate them and place them in the "setup" directory
/// inside the chain-dir, so it can be used for testing (we're using it in the
/// e2e tests), dev/test-nets and public networks setup.
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
            .split_once('.')
            .unwrap()
            .1
            .split_once('.')
            .unwrap()
            .0;
        config.sha256 = Some(genesis_config::HexString(hash.to_owned()));
    });

    // The `temp_chain_id` gets renamed after we have chain ID.
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
    let mut gossiper_configs: HashMap<String, config::IntentGossiper> =
        HashMap::with_capacity(config.validator.len());
    let mut matchmaker_configs: HashMap<String, config::Matchmaker> =
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

        let chain_dir = validator_dir.join(&accounts_temp_dir);
        let tm_home_dir = chain_dir.join("tendermint");
        // ensures the whole directory hierarchy exists in case it is assumed
        // later on
        std::fs::create_dir_all(&tm_home_dir).unwrap();

        // Find or generate tendermint node key
        let node_pk = try_parse_public_key(
            format!("validator {name} Tendermint node key"),
            &config.tendermint_node_key,
        )
        .map(|pk| ed25519::PublicKey::try_from_pk(&pk).unwrap())
        .unwrap_or_else(|| {
            // Generate a node key
            let node_sk = ed25519::SigScheme::generate(&mut rng);

            let node_pk = write_tendermint_node_key(&tm_home_dir, node_sk);

            tendermint_node::write_validator_state(&tm_home_dir);

            node_pk
        });

        // Derive the node ID from the node key
        let node_id: TendermintNodeId = id_from_pk(&node_pk);

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

        // Generate account and reward addresses
        let address = address::gen_established_address("validator account");
        let reward_address =
            address::gen_established_address("validator reward account");
        config.address = Some(address.to_string());
        config.staking_reward_address = Some(reward_address.to_string());

        // Generate the consensus, account and reward keys, unless they're
        // pre-defined.
        let mut wallet = Wallet::load_or_new(&chain_dir);

        let consensus_pk = try_parse_public_key(
            format!("validator {name} consensus key"),
            &config.consensus_public_key,
        )
        .unwrap_or_else(|| {
            let alias = format!("{}-consensus-key", name);
            println!("Generating validator {} consensus key...", name);
            let (_alias, keypair) =
                wallet.gen_key(Some(alias), unsafe_dont_encrypt);

            // Write consensus key for Tendermint
            tendermint_node::write_validator_key(
                &tm_home_dir,
                &address,
                &keypair,
            );

            keypair.ref_to()
        });

        let account_pk = try_parse_public_key(
            format!("validator {name} account key"),
            &config.account_public_key,
        )
        .unwrap_or_else(|| {
            let alias = format!("{}-account-key", name);
            println!("Generating validator {} account key...", name);
            let (_alias, keypair) =
                wallet.gen_key(Some(alias), unsafe_dont_encrypt);
            keypair.ref_to()
        });

        let staking_reward_pk = try_parse_public_key(
            format!("validator {name} staking reward key"),
            &config.staking_reward_public_key,
        )
        .unwrap_or_else(|| {
            let alias = format!("{}-reward-key", name);
            println!(
                "Generating validator {} staking reward account key...",
                name
            );
            let (_alias, keypair) =
                wallet.gen_key(Some(alias), unsafe_dont_encrypt);
            keypair.ref_to()
        });

        let protocol_pk = try_parse_public_key(
            format!("validator {name} protocol key"),
            &config.protocol_public_key,
        )
        .unwrap_or_else(|| {
            let alias = format!("{}-protocol-key", name);
            println!("Generating validator {} protocol signing key...", name);
            let (_alias, keypair) =
                wallet.gen_key(Some(alias), unsafe_dont_encrypt);
            keypair.ref_to()
        });

        let dkg_pk = &config
            .dkg_public_key
            .as_ref()
            .map(|key| {
                key.to_dkg_public_key().unwrap_or_else(|err| {
                    let label = format!("validator {name} DKG key");
                    eprintln!("Invalid {label} key: {}", err);
                    cli::safe_exit(1)
                })
            })
            .unwrap_or_else(|| {
                println!(
                    "Generating validator {} DKG session keypair...",
                    name
                );

                let validator_keys = wallet
                    .gen_validator_keys(Some(protocol_pk.clone()))
                    .expect("Generating new validator keys should not fail");
                let pk = validator_keys.dkg_keypair.as_ref().unwrap().public();
                wallet.add_validator_data(address.clone(), validator_keys);
                pk
            });

        // Add the validator public keys to genesis config
        config.consensus_public_key =
            Some(genesis_config::HexString(consensus_pk.to_string()));
        config.account_public_key =
            Some(genesis_config::HexString(account_pk.to_string()));
        config.staking_reward_public_key =
            Some(genesis_config::HexString(staking_reward_pk.to_string()));

        config.protocol_public_key =
            Some(genesis_config::HexString(protocol_pk.to_string()));
        config.dkg_public_key =
            Some(genesis_config::HexString(dkg_pk.to_string()));

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

                        let matchmaker_config = config::Matchmaker {
                            matchmaker_path: Some(mm_code.clone().into()),
                            tx_code_path: Some(tx_code.clone().into()),
                        };
                        matchmaker_configs
                            .insert(name.clone(), matchmaker_config);
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
                    genesis_config::HexString(keypair.ref_to().to_string());
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
    let wasm_dir = global_args
        .wasm_dir
        .as_ref()
        .cloned()
        .or_else(|| {
            if let Ok(wasm_dir) = env::var(ENV_VAR_WASM_DIR) {
                let wasm_dir: PathBuf = wasm_dir.into();
                Some(wasm_dir)
            } else {
                None
            }
        })
        .unwrap_or_else(|| config::DEFAULT_WASM_DIR.into());
    if wasm_dir.is_absolute() {
        eprintln!(
            "The arg `--wasm-dir` cannot be an absolute path. It is nested \
             inside the chain directory."
        );
        cli::safe_exit(1);
    }

    // Write the genesis file
    genesis_config::write_genesis_config(&config_clean, &genesis_path);

    // Add genesis addresses and save the wallet with other account keys
    wallet.add_genesis_addresses(config_clean.clone());
    wallet.save().unwrap();

    // Write the global config setting the default chain ID
    let global_config = GlobalConfig::new(chain_id.clone());
    global_config.write(&global_args.base_dir).unwrap();

    // Rename the generate chain config dir from `temp_chain_id` to `chain_id`
    fs::rename(&temp_dir, &chain_dir).unwrap();

    // Copy the WASM checksums
    let wasm_dir_full = chain_dir.join(&wasm_dir);
    fs::create_dir_all(&wasm_dir_full).unwrap();
    fs::copy(
        &wasm_checksums_path,
        wasm_dir_full.join(config::DEFAULT_WASM_CHECKSUMS_FILE),
    )
    .unwrap();

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

        // Copy the WASM checksums
        let wasm_dir_full = validator_chain_dir.join(&wasm_dir);
        fs::create_dir_all(&wasm_dir_full).unwrap();
        fs::copy(
            &wasm_checksums_path,
            wasm_dir_full.join(config::DEFAULT_WASM_CHECKSUMS_FILE),
        )
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
                Some(TendermintMode::Validator),
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

            // Configure the intent gossiper, matchmaker (if any) and RPC
            config.intent_gossiper = gossiper_configs.remove(name).unwrap();
            config.intent_gossiper.seed_peers = seed_peers.clone();
            config.matchmaker =
                matchmaker_configs.remove(name).unwrap_or_default();
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
            config
                .intent_gossiper
                .matchmakers_server_addr
                .set_port(first_port + 5);

            config.write(&validator_dir, &chain_id, true).unwrap();
        },
    );

    // Update the ledger config persistent peers and save it
    let mut config = Config::load(&global_args.base_dir, &chain_id, None);
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
    config.ledger.tendermint.p2p_addr_book_strict = !localhost;
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
        let release_wasm_checksums_path =
            PathBuf::from(config::DEFAULT_BASE_DIR)
                .join(chain_id.as_str())
                .join(config::DEFAULT_WASM_DIR)
                .join(config::DEFAULT_WASM_CHECKSUMS_FILE);
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
        wallet.add_address(&name, address);
    }
    if config.public_key.is_none() {
        println!("Generating established account {} key...", name.as_ref());
        let (_alias, keypair) = wallet.gen_key(
            Some(format!("{}-key", name.as_ref())),
            unsafe_dont_encrypt,
        );
        let public_key =
            genesis_config::HexString(keypair.ref_to().to_string());
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
        unsafe_dont_encrypt,
    }: args::InitGenesisValidator,
) {
    let setup_dir = global_args.base_dir.join(PREGENESIS_DIR);
    let mut wallet = Wallet::load_or_new(&setup_dir);
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
    let (validator_key_alias, validator_key) =
        wallet.gen_key(Some(validator_key_alias(&alias)), unsafe_dont_encrypt);
    println!("Generating consensus key...");
    let (consensus_key_alias, consensus_key) =
        wallet.gen_key(Some(consensus_key_alias(&alias)), unsafe_dont_encrypt);
    println!("Generating staking reward account key...");
    let (rewards_key_alias, rewards_key) =
        wallet.gen_key(Some(rewards_key_alias(&alias)), unsafe_dont_encrypt);

    println!("Generating tendermint node key...");
    let (tendermint_node_alias, tendermint_node_key) = wallet
        .gen_key(Some(tendermint_node_key_alias(&alias)), unsafe_dont_encrypt);

    println!("Generating protocol key and DKG session key...");
    let validator_keys = wallet.gen_validator_keys(None).unwrap();
    let protocol_key = validator_keys.get_protocol_keypair().ref_to();
    let dkg_public_key = validator_keys
        .dkg_keypair
        .as_ref()
        .expect("DKG session keypair should exist.")
        .public();
    wallet.add_validator_data(validator_address.clone(), validator_keys);
    wallet.save().unwrap_or_else(|err| eprintln!("{}", err));

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
    println!("  Tendermint node key \"{}\"", tendermint_node_alias);
    println!();

    let validator_config = genesis_config::ValidatorConfig {
        consensus_public_key: Some(HexString(
            consensus_key.ref_to().to_string(),
        )),
        account_public_key: Some(HexString(validator_key.ref_to().to_string())),
        staking_reward_public_key: Some(HexString(
            rewards_key.ref_to().to_string(),
        )),
        protocol_public_key: Some(HexString(protocol_key.to_string())),
        dkg_public_key: Some(HexString(dkg_public_key.to_string())),
        tendermint_node_key: Some(HexString(
            tendermint_node_key.ref_to().to_string(),
        )),
        ..Default::default()
    };
    // this prints the validator block in the same way as the genesis config
    // TOML would look like

    let genesis_part = format!(
        "[validator.{alias}]\n{}",
        toml::to_string(&validator_config).unwrap()
    );
    println!("Your public partial pre-genesis TOML configuration:");
    println!();
    println!("{genesis_part}");

    let file_name = setup_dir.join(format!("{alias}-pre-genesis.toml"));
    fs::write(&file_name, genesis_part).unwrap_or_else(|err| {
        eprintln!(
            "Couldn't write partial pre-genesis file to {}. Failed with: {}",
            file_name.to_string_lossy(),
            err
        );
        cli::safe_exit(1)
    });
    println!();
    println!(
        "Pre-genesis TOML written to {}",
        file_name.to_string_lossy()
    );
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

fn try_parse_public_key(
    label: impl AsRef<str>,
    value: &Option<HexString>,
) -> Option<common::PublicKey> {
    let label = label.as_ref();
    value.as_ref().map(|key| {
        key.to_public_key().unwrap_or_else(|err| {
            eprintln!("Invalid {label} key: {}", err);
            cli::safe_exit(1)
        })
    })
}

fn network_configs_url_prefix(chain_id: &ChainId) -> String {
    std::env::var(ENV_VAR_NETWORK_CONFIGS_SERVER).unwrap_or_else(|_| {
        format!("{DEFAULT_NETWORK_CONFIGS_SERVER}/{chain_id}")
    })
}

fn validator_key_alias(alias: &str) -> String {
    format!("{alias}-validator-key")
}

fn consensus_key_alias(alias: &str) -> String {
    format!("{alias}-consensus-key")
}

fn rewards_key_alias(alias: &str) -> String {
    format!("{alias}-rewards-key")
}

fn tendermint_node_key_alias(alias: &str) -> String {
    format!("{alias}-tendermint-node-key")
}

fn write_tendermint_node_key(
    tm_home_dir: &Path,
    node_sk: ed25519::SecretKey,
) -> ed25519::PublicKey {
    let node_pk: ed25519::PublicKey = node_sk.ref_to();
    // Convert and write the keypair into Tendermint
    // node_key.json file
    let node_keypair =
        [node_sk.try_to_vec().unwrap(), node_pk.try_to_vec().unwrap()].concat();
    let tm_node_keypair_json = json!({
        "priv_key": {
            "type": "tendermint/PrivKeyEd25519",
            "value": base64::encode(node_keypair),
        }
    });
    let tm_config_dir = tm_home_dir.join("config");
    fs::create_dir_all(&tm_config_dir)
        .expect("Couldn't create validator directory");
    let node_key_path = tm_config_dir.join("node_key.json");
    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&node_key_path)
        .expect("Couldn't create validator node key file");
    serde_json::to_writer_pretty(file, &tm_node_keypair_json)
        .expect("Couldn't write validator node key file");
    node_pk
}
