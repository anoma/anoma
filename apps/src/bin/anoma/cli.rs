//! Anoma CLI.
//!
//! This CLI groups together the most commonly used commands inlined from the
//! node and the client. The other commands for the node, client and wallet can
//! be dispatched via `anoma node ...`, `anoma client ...` or `anoma wallet
//! ...`, respectively.

use std::env;
use std::process::Command;

use anoma_apps::cli;
use eyre::{eyre, Result};
#[cfg(not(target_os = "windows"))]
use signal_hook::consts::TERM_SIGNALS;
#[cfg(not(target_os = "windows"))]
use signal_hook::iterator::Signals;

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
        | cli::cmds::Anoma::Gossip(_) => handle_subcommand("anoman", sub_args),
        cli::cmds::Anoma::Client(_)
        | cli::cmds::Anoma::TxCustom(_)
        | cli::cmds::Anoma::TxTransfer(_)
        | cli::cmds::Anoma::TxUpdateVp(_)
        | cli::cmds::Anoma::Intent(_) => handle_subcommand("anomac", sub_args),
        cli::cmds::Anoma::Wallet(_) => handle_subcommand("anomaw", sub_args),
    }
}

fn handle_subcommand(
    program: &str,
    #[cfg(not(feature = "dev"))] sub_args: Vec<String>,
    #[cfg(feature = "dev")] mut sub_args: Vec<String>,
) -> Result<()> {
    let env_vars = env::vars_os();

    #[cfg(feature = "dev")]
    let cmd = if env::var("CARGO").is_ok() {
        // When the command is ran from inside `cargo run`, we also want to
        // call the sub-command via `cargo run` to rebuild if necessary.
        // We do this by prepending the arguments with `cargo run` arguments.
        let mut cargo_args =
            vec!["run".to_string(), format!("--bin={}", program), "--".into()];
        cargo_args.append(&mut sub_args);
        sub_args = cargo_args;
        "cargo"
    } else {
        program
    };
    #[cfg(not(feature = "dev"))]
    let cmd = program;

    let mut process = Command::new(cmd)
        .args(sub_args)
        .envs(env_vars)
        .spawn()
        .unwrap_or_else(|_| panic!("Couldn't run {} command.", cmd));

    loop {
        if let Ok(Some(exit_status)) = process.try_wait() {
            if exit_status.success() {
                break;
            } else {
                return Err(eyre!("{} command failed.", cmd));
            }
        }
        #[cfg(not(target_os = "windows"))]
        {
            let mut signals = Signals::new(TERM_SIGNALS).unwrap();
            kill_on_term_signal(process.id() as i32, &mut signals);
        }
        #[cfg(target_os = "windows")]
        kill_on_term_signal(process.id() as u32);
    }
    Ok(())
}

#[cfg(not(target_os = "windows"))]
fn kill_on_term_signal(pid: i32, signals: &mut Signals) {
    for sig in signals.pending() {
        if TERM_SIGNALS.contains(&sig) {
            tracing::info!("Anoma received termination signal");
            unsafe { libc::kill(pid, libc::SIGTERM) };
            break;
        }
    }
}

#[cfg(target_os = "windows")]
fn kill_on_term_signal(pid: u32) {
    use tokio::signal::ctrl_c;
    let _ = std::thread::spawn(move || {
        tokio_test::block_on(async move {
            loop {
                if ctrl_c().await.is_ok() {
                    tracing::info!("Anoma received termination signal");
                    break;
                }
            }
            unsafe {
                use winapi::um::processthreadsapi::{
                    OpenProcess, TerminateProcess,
                };
                use winapi::um::winnt::PROCESS_TERMINATE;
                let pc = OpenProcess(PROCESS_TERMINATE, 0, pid);
                if pc != std::ptr::null_mut() {
                    TerminateProcess(pc, 0);
                }
            };
        })
    });
}
