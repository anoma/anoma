//! Anoma node CLI.

#[cfg(feature = "dev")]
use std::path::Path;

use anoma_apps::config::Config;
use anoma_apps::node::{gossip, ledger};
use anoma_apps::{cli, config};
use eyre::{Context, Result};

pub fn main() -> Result<()> {
    let (cmd, ctx) = cli::anoma_node_cli();
    let base_dir = &ctx.global_args.base_dir;
    match cmd {
        cli::cmds::AnomaNode::Ledger(sub) => match sub {
            cli::cmds::Ledger::Run(_) => {
                let config = get_cfg(base_dir);
                let ledger_cfg = config.ledger.unwrap_or_default();
                ledger::run(ledger_cfg);
            }
            cli::cmds::Ledger::Reset(_) => {
                let config = get_cfg(base_dir);
                let ledger_cfg = config.ledger.unwrap_or_default();
                ledger::reset(ledger_cfg)
                    .wrap_err("Failed to reset Anoma node")?;
            }
        },
        cli::cmds::AnomaNode::Gossip(sub) => match sub {
            cli::cmds::Gossip::Run(cli::cmds::GossipRun(args)) => {
                let config = get_cfg(base_dir);
                let mut gossip_cfg = config.intent_gossiper.unwrap_or_default();
                cli::update_gossip_config(args, &mut gossip_cfg)
                    .expect("failed to update config with cli option");
                gossip::run(gossip_cfg).wrap_err(
                    "Failed to run gossip
            service",
                )?;
            }
        },
        cli::cmds::AnomaNode::Config(sub) => match sub {
            cli::cmds::Config::Gen(_) => {
                let gen_config = config::Config::generate(base_dir, false)
                    .wrap_err("Failed to generate the default config")?;
                tracing::debug!("Generated config {:?}", gen_config);
            }
        },
    }
    Ok(())
}

// for dev purpose this is useful so if the config change it automatically
// generate the default one
#[cfg(feature = "dev")]
fn get_cfg(base_dir: &Path) -> Config {
    match Config::read(base_dir) {
        Ok(config) => config,
        Err(err) => {
            tracing::error!(
                "Tried to read config in {} but failed with: {}",
                base_dir.display(),
                err
            );
            // generate(home,true) replace current config if it exists
            match config::Config::generate(base_dir, true) {
                Ok(config) => {
                    tracing::warn!(
                        "Generated default config in {}",
                        base_dir.display()
                    );
                    config
                }
                Err(err) => {
                    tracing::error!(
                        "Tried to generate config in {} but failed with: {}. \
                         Using default config (with new generated key)",
                        base_dir.display(),
                        err
                    );
                    config::Config::default()
                }
            }
        }
    }
}

#[cfg(not(feature = "dev"))]
fn get_cfg(home: String) -> Config {
    Config::read(&home).expect("Failed to read config file.")
}
