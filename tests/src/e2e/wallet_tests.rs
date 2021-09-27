use std::process::Command;

use assert_cmd::prelude::CommandCargoExt;
use color_eyre::eyre::Result;
use eyre::eyre;
use rexpect::session::spawn_command;
use tempfile::tempdir;

use super::setup;

const BIN: &str = "anomaw";

/// Test wallet key commands with an encrypted key:
/// 1. key gen
/// 2. key find
/// 3. key list
#[test]
fn wallet_encrypted_key_cmds() -> Result<()> {
    let dir = setup::working_dir();
    let base_dir = tempdir().unwrap();
    let key_alias = "test_key_1";
    let password = "VeRySeCuR3";

    // 1. key gen
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&["key", "gen", "--alias", key_alias]);

    let cmd_str = format!("{:?}", cmd);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string("Enter encryption password:")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    session.send_line(password).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string(&format!(
            "Successfully added a key and an address with alias: \"{}\"",
            key_alias
        ))
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    // 2. key find
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&["key", "find", "--alias", key_alias]);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string("Enter decryption password:")
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    session.send_line(password).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session.exp_string("Public key hash:").map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session.exp_string("Public key:").map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    // 3. key list
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&["key", "list"]);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string(&format!("Alias \"{}\" (encrypted):", key_alias))
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    Ok(())
}

/// Test wallet key commands with an unencrypted key:
/// 1. key gen
/// 2. key find
/// 3. key list
#[test]
fn wallet_unencrypted_key_cmds() -> Result<()> {
    let dir = setup::working_dir();
    let base_dir = tempdir().unwrap();
    let key_alias = "test_key_1";

    // 1. key gen
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&["key", "gen", "--alias", key_alias, "--unsafe-dont-encrypt"]);

    let cmd_str = format!("{:?}", cmd);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string(&format!(
            "Successfully added a key and an address with alias: \"{}\"",
            key_alias
        ))
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    // 2. key find
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&["key", "find", "--alias", key_alias]);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session.exp_string("Public key hash:").map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session.exp_string("Public key:").map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    // 3. key list
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&["key", "list"]);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string(&format!("Alias \"{}\" (not encrypted):", key_alias))
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    Ok(())
}

/// Test wallet address commands:
/// 1. address gen
/// 2. address add
/// 3. address find
/// 4. address list
#[test]
fn wallet_address_cmds() -> Result<()> {
    let dir = setup::working_dir();
    let base_dir = tempdir().unwrap();
    let gen_address_alias = "test_address_1";
    let add_address_alias = "test_address_2";
    let add_address = "atest1v4ehgw36gs6yydf4xq6ngdpex5c5yw2zxgunqvfjgvurxv6ygsmr2dfcxfznxde4xuurw334uclqv3";

    // 1. address gen
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&[
            "address",
            "gen",
            "--alias",
            gen_address_alias,
            "--unsafe-dont-encrypt",
        ]);

    let cmd_str = format!("{:?}", cmd);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string(&format!(
            "Successfully added a key and an address with alias: \"{}\"",
            gen_address_alias
        ))
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    // 2. address add
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&[
            "address",
            "add",
            "--address",
            add_address,
            "--alias",
            add_address_alias,
        ]);

    let cmd_str = format!("{:?}", cmd);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string(&format!(
            "Successfully added a key and an address with alias: \"{}\"",
            add_address_alias
        ))
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    // 3. address find
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&["address", "find", "--alias", gen_address_alias]);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session.exp_string("Found address").map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    // 4. address list
    let mut cmd = Command::cargo_bin(BIN)?;
    cmd.current_dir(&dir)
        .args(&["--base-dir", &base_dir.path().to_string_lossy()])
        .args(&["address", "list"]);

    let mut session = spawn_command(cmd, Some(20_000)).map_err(|e| {
        eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
    })?;

    session
        .exp_string(&format!("\"{}\":", gen_address_alias))
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    session
        .exp_string(&format!("\"{}\":", add_address_alias))
        .map_err(|e| {
            eyre!(format!("in command: {}\n\nReason: {}", cmd_str, e))
        })?;

    Ok(())
}
