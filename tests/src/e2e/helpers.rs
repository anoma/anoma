//! E2E test helpers

use std::str::FromStr;

use anoma::types::address::Address;
use anoma::types::key::ed25519::Keypair;
use anoma::types::storage::Epoch;
use color_eyre::eyre::Result;
use eyre::eyre;

use super::setup::Test;
use crate::e2e::setup::Bin;
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
    let address_str = matched.trim().rsplit_once(" ").unwrap().1;
    let address = Address::from_str(address_str).map_err(|e| {
        eyre!(format!(
            "Address: {} parsed from {}, Error: {}\n\nOutput: {}",
            address_str, matched, e, unread
        ))
    })?;
    println!("Found {}", address);
    Ok(address)
}

/// Find the address of an account by its alias from the wallet
#[allow(dead_code)]
pub fn find_keypair(test: &Test, alias: impl AsRef<str>) -> Result<Keypair> {
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
    let pk = matched.trim().rsplit_once(" ").unwrap().1;
    let (unread, matched) = find.exp_regex("Secret key: .*\n")?;
    let sk = matched.trim().rsplit_once(" ").unwrap().1;
    let key = format!("{}{}", sk, pk);
    Keypair::from_str(&key).map_err(|e| {
        eyre!(format!(
            "Key: {} parsed from {}, Error: {}\n\nOutput: {}",
            key, matched, e, unread
        ))
    })
}

/// Find the address of an account by its alias from the wallet
pub fn find_voting_power(test: &Test, alias: impl AsRef<str>) -> Result<u64> {
    let mut find = run!(
        test,
        Bin::Client,
        &["voting-power", "--validator", alias.as_ref()],
        Some(1)
    )?;
    let (unread, matched) = find.exp_regex("voting power: .*\n")?;
    let voting_power_str = matched.trim().rsplit_once(" ").unwrap().1;
    u64::from_str(voting_power_str).map_err(|e| {
        eyre!(format!(
            "Voting power: {} parsed from {}, Error: {}\n\nOutput: {}",
            voting_power_str, matched, e, unread
        ))
    })
}

/// Get the last committed epoch.
pub fn get_epoch(test: &Test) -> Result<Epoch> {
    let mut find = run!(test, Bin::Client, &["epoch"], Some(5))?;
    let (unread, matched) = find.exp_regex("Last committed epoch: .*\n")?;
    let epoch_str = matched.trim().rsplit_once(" ").unwrap().1;
    let epoch = u64::from_str(epoch_str).map_err(|e| {
        eyre!(format!(
            "Epoch: {} parsed from {}, Error: {}\n\nOutput: {}",
            epoch_str, matched, e, unread
        ))
    })?;
    Ok(Epoch(epoch))
}
