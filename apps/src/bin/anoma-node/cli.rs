//! Anoma node CLI.

use anoma_apps::cli;
use anoma_apps::cli::cmds;
use anoma_apps::node::{gossip, ledger};
use eyre::{Context, Result};

pub fn main() -> Result<()> {
    let (cmd, mut ctx) = cli::anoma_node_cli();
    match cmd {
        cmds::AnomaNode::Ledger(sub) => match sub {
            cmds::Ledger::Run(_) => {
                ledger::run(ctx.config.ledger);
            }
            cmds::Ledger::Reset(_) => {
                ledger::reset(ctx.config.ledger)
                    .wrap_err("Failed to reset Anoma node")?;
            }
        },
        cmds::AnomaNode::Gossip(sub) => match sub {
            cmds::Gossip::Run(cmds::GossipRun(args)) => {
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
                gossip::run(
                    gossip_cfg,
                    &config.ledger.wasm_dir,
                    tx_source_address,
                    tx_signing_key,
                )
                .wrap_err("Failed to run gossip service")?;
            }
        },
        cmds::AnomaNode::Config(sub) => match sub {
            cmds::Config::Gen(cmds::ConfigGen) => {
                // If the config doesn't exit, it gets generated in the context.
                // In here, we just need to overwrite the default chain ID, in
                // case it's been already set to a different value
                if let Some(chain_id) = ctx.global_args.chain_id.as_ref() {
                    ctx.global_config.default_chain_id = chain_id.clone();
                    ctx.global_config
                        .write(&ctx.global_args.base_dir)
                        .unwrap_or_else(|err| {
                            eprintln!("Error writing global config: {}", err);
                            cli::safe_exit(1)
                        });
                }
                tracing::debug!(
                    "Generated config and set default chain ID to {}",
                    &ctx.global_config.default_chain_id
                );
            }
        },
    }
    Ok(())
}
