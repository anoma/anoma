//! Anoma CLI.
//!
//! This CLI groups together the most commonly used commands inlined from the
//! node and the client. The other commands for the node, client and wallet can
//! be dispatched via `anoma node ...`, `anoma client ...` or `anoma wallet
//! ...`, respectively.

use std::env;
use std::process::Command;

use anoma_apps::cli;
use eyre::Result;

pub fn main() -> Result<()> {
    let (cmd, raw_sub_cmd) = cli::anoma_cli();
    handle_command(cmd, raw_sub_cmd)
}

fn handle_command(cmd: cli::cmds::Anoma, raw_sub_cmd: String) -> Result<()> {
    let args = env::args();

    let is_bin_sub_cmd = matches!(
        cmd,
        cli::cmds::Anoma::Node(_)
            | cli::cmds::Anoma::Client(_)
            | cli::cmds::Anoma::Wallet(_)
    );

    // Skip the first arg, which is the name of the binary
    let mut sub_args: Vec<String> = args.skip(1).collect();

    if is_bin_sub_cmd {
        // Because there may be global args before the `cmd`, we have to find it
        // before removing it.
        sub_args
            .iter()
            .position(|arg| arg == &raw_sub_cmd)
            .map(|e| sub_args.remove(e));
    }

    match cmd {
        cli::cmds::Anoma::Node(_)
        | cli::cmds::Anoma::Ledger(_)
        | cli::cmds::Anoma::Gossip(_)
        | cli::cmds::Anoma::Matchmaker(_) => {
            handle_subcommand("anoman", sub_args)
        }
        cli::cmds::Anoma::Client(_)
        | cli::cmds::Anoma::TxCustom(_)
        | cli::cmds::Anoma::TxTransfer(_)
        | cli::cmds::Anoma::TxUpdateVp(_)
        | cli::cmds::Anoma::TxInitNft(_)
        | cli::cmds::Anoma::TxMintNft(_)
        | cli::cmds::Anoma::TxInitProposal(_)
        | cli::cmds::Anoma::TxVoteProposal(_)
        | cli::cmds::Anoma::Intent(_) => handle_subcommand("anomac", sub_args),
        cli::cmds::Anoma::Wallet(_) => handle_subcommand("anomaw", sub_args),
    }
}

fn handle_subcommand(program: &str, mut sub_args: Vec<String>) -> Result<()> {
    let env_vars = env::vars_os();

    let cmd_name = if cfg!(feature = "dev") && env::var("CARGO").is_ok() {
        // When the command is ran from inside `cargo run`, we also want to
        // call the sub-command via `cargo run` to rebuild if necessary.
        // We do this by prepending the arguments with `cargo run` arguments.
        let mut cargo_args =
            vec!["run".to_string(), format!("--bin={}", program), "--".into()];
        cargo_args.append(&mut sub_args);
        sub_args = cargo_args;
        "cargo".into()
    } else {
        // Get the full path to the program to be inside the parent directory of
        // the current process
        let anoma_path = env::current_exe()?;
        anoma_path.parent().unwrap().join(program)
    };

    let mut cmd = Command::new(cmd_name);
    cmd.args(sub_args).envs(env_vars);
    exec_subcommand(program, cmd)
}

/// Borrowed and adapted from cargo's subcommand dispatch, which replaces the
/// current process with the sub-command:
/// <https://github.com/rust-lang/cargo/blob/94ca096afbf25f670e76e07dca754fcfe27134be/crates/cargo-util/src/process_builder.rs#L385>
///
/// Replaces the current process with the target process.
///
/// On Unix, this executes the process using the Unix syscall `execvp`, which
/// will block this process, and will only return if there is an error.
///
/// On Windows this isn't technically possible. Instead we emulate it to the
/// best of our ability. One aspect we fix here is that we specify a handler for
/// the Ctrl-C handler. In doing so (and by effectively ignoring it) we should
/// emulate proxying Ctrl-C handling to the application at hand, which will
/// either terminate or handle it itself. According to Microsoft's documentation
/// at <https://docs.microsoft.com/en-us/windows/console/ctrl-c-and-ctrl-break-signals>.
/// the Ctrl-C signal is sent to all processes attached to a terminal, which
/// should include our child process. If the child terminates then we'll reap
/// them in Cargo pretty quickly, and if the child handles the signal then we
/// won't terminate (and we shouldn't!) until the process itself later exits.
pub fn exec_subcommand(program: &str, cmd: Command) -> Result<()> {
    imp::exec_subcommand(program, cmd)
}

#[cfg(unix)]
mod imp {
    use std::os::unix::process::CommandExt;
    use std::process::Command;

    use eyre::{eyre, Result};

    pub fn exec_subcommand(program: &str, mut cmd: Command) -> Result<()> {
        let error = cmd.exec();
        Err(eyre!("Command {} failed with {}.", program, error))
    }
}
#[cfg(windows)]
mod imp {
    use std::process::Command;

    use eyre::{eyre, Result, WrapErr};
    use winapi::shared::minwindef::{BOOL, DWORD, FALSE, TRUE};
    use winapi::um::consoleapi::SetConsoleCtrlHandler;

    unsafe extern "system" fn ctrlc_handler(_: DWORD) -> BOOL {
        // Do nothing; let the child process handle it.
        TRUE
    }

    pub fn exec_subcommand(program: &str, mut cmd: Command) -> Result<()> {
        unsafe {
            if SetConsoleCtrlHandler(Some(ctrlc_handler), TRUE) == FALSE {
                return Err(eyre!("Could not set Ctrl-C handler."));
            }
        }

        let exit = cmd
            .status()
            .wrap_err_with(|| eyre!("Could not execute command {}", program))?;

        if exit.success() {
            Ok(())
        } else {
            Err(eyre!("Command {} failed.", program))
        }
    }
}
