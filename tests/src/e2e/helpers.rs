//! E2E test helpers

use std::str::FromStr;
use std::time::{Duration, Instant};

use anoma::types::address::Address;
use anoma::types::key::*;
use anoma::types::storage::Epoch;
use anoma_apps::config::{Config, TendermintMode};
use color_eyre::eyre::Result;
use eyre::eyre;

use super::setup::Test;
use crate::e2e::setup::{sleep, Bin, Who};
use crate::run;

/// Find the address of an account by its alias from the wallet
pub fn find_address(test: &Test, alias: impl AsRef<str>) -> Result<Address> {
    let mut find = run!(
        test,
        Bin::Wallet,
        &["address", "find", "--alias", alias.as_ref()],
        Some(1)
    )?;
    let (unread, matched) = find.exp_regex("Found address .*\n")?;
    let address_str = matched.trim().rsplit_once(' ').unwrap().1;
    let address = Address::from_str(address_str).map_err(|e| {
        eyre!(format!(
            "Address: {} parsed from {}, Error: {}\n\nOutput: {}",
            address_str, matched, e, unread
        ))
    })?;
    println!("Found {}", address);
    Ok(address)
}

/// Find the address of the intent gossiper node's RPC endpoint.
pub fn get_actor_rpc(test: &Test, who: &Who) -> String {
    let base_dir = test.get_base_dir(who);
    let tendermint_mode = match who {
        Who::NonValidator => TendermintMode::Full,
        Who::Validator(_) => TendermintMode::Validator,
    };
    let config =
        Config::load(&base_dir, &test.net.chain_id, Some(tendermint_mode));
    config.ledger.tendermint.rpc_address.to_string()
}

/// Find the address of the intent gossiper node's matchmakers server.
pub fn get_gossiper_mm_server(test: &Test, who: &Who) -> String {
    let base_dir = test.get_base_dir(who);
    let tendermint_mode = match who {
        Who::NonValidator => TendermintMode::Full,
        Who::Validator(_) => TendermintMode::Validator,
    };
    let config =
        Config::load(&base_dir, &test.net.chain_id, Some(tendermint_mode));
    config.intent_gossiper.matchmakers_server_addr.to_string()
}

/// Find the address of an account by its alias from the wallet
#[allow(dead_code)]
pub fn find_keypair(
    test: &Test,
    alias: impl AsRef<str>,
) -> Result<common::SecretKey> {
    let mut find = run!(
        test,
        Bin::Wallet,
        &[
            "key",
            "find",
            "--alias",
            alias.as_ref(),
            "--unsafe-show-secret"
        ],
        Some(1)
    )?;
    let (_unread, matched) = find.exp_regex("Public key: .*\n")?;
    let pk = matched.trim().rsplit_once(' ').unwrap().1;
    let (unread, matched) = find.exp_regex("Secret key: .*\n")?;
    let sk = matched.trim().rsplit_once(' ').unwrap().1;
    let key = format!("{}{}", sk, pk);
    common::SecretKey::from_str(&key).map_err(|e| {
        eyre!(format!(
            "Key: {} parsed from {}, Error: {}\n\nOutput: {}",
            key, matched, e, unread
        ))
    })
}

/// Find the address of an account by its alias from the wallet
pub fn find_voting_power(
    test: &Test,
    alias: impl AsRef<str>,
    ledger_address: &str,
) -> Result<u64> {
    let mut find = run!(
        test,
        Bin::Client,
        &[
            "voting-power",
            "--validator",
            alias.as_ref(),
            "--ledger-address",
            ledger_address
        ],
        Some(1)
    )?;
    let (unread, matched) = find.exp_regex("voting power: .*\n")?;
    let voting_power_str = matched.trim().rsplit_once(' ').unwrap().1;
    u64::from_str(voting_power_str).map_err(|e| {
        eyre!(format!(
            "Voting power: {} parsed from {}, Error: {}\n\nOutput: {}",
            voting_power_str, matched, e, unread
        ))
    })
}

/// Get the last committed epoch.
pub fn get_epoch(test: &Test, ledger_address: &str) -> Result<Epoch> {
    let mut find = run!(
        test,
        Bin::Client,
        &["epoch", "--ledger-address", ledger_address],
        Some(5)
    )?;
    let (unread, matched) = find.exp_regex("Last committed epoch: .*\n")?;
    let epoch_str = matched.trim().rsplit_once(' ').unwrap().1;
    let epoch = u64::from_str(epoch_str).map_err(|e| {
        eyre!(format!(
            "Epoch: {} parsed from {}, Error: {}\n\nOutput: {}",
            epoch_str, matched, e, unread
        ))
    })?;
    Ok(Epoch(epoch))
}

/// Sleep until the next epoch starts
pub fn epoch_sleep(
    test: &Test,
    ledger_address: &str,
    secs: u64,
) -> Result<Epoch> {
    let old_epoch = get_epoch(test, ledger_address)?;
    let start = Instant::now();
    let loop_timeout = Duration::new(secs, 0);
    loop {
        if Instant::now().duration_since(start) > loop_timeout {
            panic!("Timed out waiting for the next epoch");
        }
        let epoch = get_epoch(test, ledger_address)?;
        if epoch > old_epoch {
            break Ok(epoch);
        } else {
            sleep(15);
        }
    }
}
