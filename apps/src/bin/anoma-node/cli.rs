//! Anoma node CLI.

use anoma_apps::node::{gossip, ledger};
use anoma_apps::{cli, config};
use eyre::{Context, Result};

pub fn main() -> Result<()> {
    let (cmd, mut ctx) = cli::anoma_node_cli();
    let base_dir = &ctx.global_args.base_dir;
    match cmd {
        cli::cmds::AnomaNode::Ledger(sub) => match sub {
            cli::cmds::Ledger::Run(_) => {
                ledger::run(ctx.config.ledger);
            }
            cli::cmds::Ledger::Reset(_) => {
                ledger::reset(ctx.config.ledger)
                    .wrap_err("Failed to reset Anoma node")?;
            }
        },
        cli::cmds::AnomaNode::Gossip(sub) => match sub {
            cli::cmds::Gossip::Run(cli::cmds::GossipRun(args)) => {
                let tx_source_address = ctx.get_opt(&args.tx_source_address);
                let tx_signing_key = ctx.get_opt_cached(&args.tx_signing_key);
                let config = ctx.config;
                let mut gossip_cfg = config.intent_gossiper;
                gossip_cfg.update(
                    args.addr,
                    args.rpc,
                    args.matchmaker_path,
                    args.tx_code_path,
                    args.ledger_addr,
                    args.filter_path,
                );
                gossip::run(gossip_cfg, tx_source_address, tx_signing_key)
                    .wrap_err(
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
