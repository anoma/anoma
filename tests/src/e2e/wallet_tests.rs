//! By default, these tests will run in release mode. This can be disabled
//! by setting environment variable `ANOMA_E2E_DEBUG=true`. For debugging,
//! you'll typically also want to set `RUST_BACKTRACE=1`, e.g.:
//!
//! ```ignore,shell
//! ANOMA_E2E_DEBUG=true RUST_BACKTRACE=1 cargo test e2e::wallet_tests -- --test-threads=1 --nocapture
//! ```
//!
//! To keep the temporary files created by a test, use env var
//! `ANOMA_E2E_KEEP_TEMP=true`.

use std::env;

use color_eyre::eyre::Result;

use super::setup;
use crate::e2e::setup::Bin;
use crate::run;

/// Test wallet key commands with an encrypted key:
/// 1. key gen
/// 2. key find
/// 3. key list
#[test]
fn wallet_encrypted_key_cmds() -> Result<()> {
    let test = setup::single_node_net()?;
    let key_alias = "test_key_1";
    let password = "VeRySeCuR3";

    // 1. key gen
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &["key", "gen", "--alias", key_alias],
        Some(20),
    )?;

    cmd.exp_string("Enter your encryption password:")?;
    cmd.send_line(password)?;
    cmd.exp_string(
        "To confirm, please enter the same encryption password once more: ",
    )?;
    cmd.send_line(password)?;
    cmd.exp_string(&format!(
        "Successfully added a key and an address with alias: \"{}\"",
        key_alias
    ))?;

    // 2. key find
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &["key", "find", "--alias", key_alias],
        Some(20),
    )?;

    cmd.exp_string("Enter decryption password:")?;
    cmd.send_line(password)?;
    cmd.exp_string("Public key hash:")?;
    cmd.exp_string("Public key:")?;

    // 3. key list
    let mut cmd = run!(test, Bin::Wallet, &["key", "list"], Some(20))?;
    cmd.exp_string(&format!("Alias \"{}\" (encrypted):", key_alias))?;

    Ok(())
}

/// Test wallet key commands with an encrypted key supplied via ENV:
/// 1. key gen
/// 2. key find
/// 3. key list
#[test]
fn wallet_encrypted_key_cmds_env_var() -> Result<()> {
    let test = setup::single_node_net()?;
    let key_alias = "test_key_1";
    let password = "VeRySeCuR3";

    env::set_var("ANOMA_WALLET_PASSWORD", password);

    // 1. key gen
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &["key", "gen", "--alias", key_alias],
        Some(20),
    )?;

    cmd.exp_string(&format!(
        "Successfully added a key and an address with alias: \"{}\"",
        key_alias
    ))?;

    // 2. key find
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &["key", "find", "--alias", key_alias],
        Some(20),
    )?;

    cmd.exp_string("Public key hash:")?;
    cmd.exp_string("Public key:")?;

    // 3. key list
    let mut cmd = run!(test, Bin::Wallet, &["key", "list"], Some(20))?;
    cmd.exp_string(&format!("Alias \"{}\" (encrypted):", key_alias))?;

    Ok(())
}

/// Test wallet key commands with an unencrypted key:
/// 1. key gen
/// 2. key find
/// 3. key list
#[test]
fn wallet_unencrypted_key_cmds() -> Result<()> {
    let test = setup::single_node_net()?;
    let key_alias = "test_key_1";

    // 1. key gen
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &["key", "gen", "--alias", key_alias, "--unsafe-dont-encrypt"],
        Some(20),
    )?;
    cmd.exp_string(&format!(
        "Successfully added a key and an address with alias: \"{}\"",
        key_alias
    ))?;

    // 2. key find
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &["key", "find", "--alias", key_alias],
        Some(20),
    )?;

    cmd.exp_string("Public key hash:")?;
    cmd.exp_string("Public key:")?;

    // 3. key list
    let mut cmd = run!(test, Bin::Wallet, &["key", "list"], Some(20))?;
    cmd.exp_string(&format!("Alias \"{}\" (not encrypted):", key_alias))?;

    Ok(())
}

/// Test wallet address commands:
/// 1. address gen
/// 2. address add
/// 3. address find
/// 4. address list
#[test]
fn wallet_address_cmds() -> Result<()> {
    let test = setup::single_node_net()?;
    let gen_address_alias = "test_address_1";
    let add_address_alias = "test_address_2";
    let add_address = "atest1v4ehgw36gs6yydf4xq6ngdpex5c5yw2zxgunqvfjgvurxv6ygsmr2dfcxfznxde4xuurw334uclqv3";

    // 1. address gen
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &[
            "address",
            "gen",
            "--alias",
            gen_address_alias,
            "--unsafe-dont-encrypt",
        ],
        Some(20),
    )?;
    cmd.exp_string(&format!(
        "Successfully added a key and an address with alias: \"{}\"",
        gen_address_alias
    ))?;

    // 2. address add
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &[
            "address",
            "add",
            "--address",
            add_address,
            "--alias",
            add_address_alias,
        ],
        Some(20),
    )?;
    cmd.exp_string(&format!(
        "Successfully added a key and an address with alias: \"{}\"",
        add_address_alias
    ))?;

    // 3. address find
    let mut cmd = run!(
        test,
        Bin::Wallet,
        &["address", "find", "--alias", gen_address_alias],
        Some(20),
    )?;
    cmd.exp_string("Found address")?;

    // 4. address list
    let mut cmd = run!(test, Bin::Wallet, &["address", "list"], Some(20))?;

    cmd.exp_string(&format!("\"{}\":", gen_address_alias))?;
    cmd.exp_string(&format!("\"{}\":", add_address_alias))?;

    Ok(())
}
