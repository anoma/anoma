//! Anoma node CLI.

use anoma_apps::cli::{self, args, cmds};
use anoma_apps::node::{gossip, ledger, matchmaker};
use eyre::{Context, Result};

pub fn main() -> Result<()> {
    let (cmd, mut ctx) = cli::anoma_node_cli()?;
    if let Some(mode) = ctx.global_args.mode.clone() {
        ctx.config.ledger.tendermint.tendermint_mode = mode;
    }
    match cmd {
        cmds::AnomaNode::Ledger(sub) => match sub {
            cmds::Ledger::Run(_) => {
                let wasm_dir = ctx.wasm_dir();
                ledger::run(ctx.config.ledger, wasm_dir);
            }
            cmds::Ledger::Reset(_) => {
                ledger::reset(ctx.config.ledger)
                    .wrap_err("Failed to reset Anoma node")?;
            }
        },
        cmds::AnomaNode::Gossip(sub) => match sub {
            cmds::Gossip::Run(cmds::GossipRun(args::GossipRun {
                addr,
                rpc,
            })) => {
                let config = ctx.config;
                let mut gossip_cfg = config.intent_gossiper;
                gossip_cfg.update(addr, rpc);
                gossip::run(
                    gossip_cfg,
                    &config
                        .ledger
                        .shell
                        .base_dir
                        .join(ctx.global_config.default_chain_id.as_str()),
                )
                .wrap_err("Failed to run gossip service")?;
            }
        },
        cmds::AnomaNode::Matchmaker(cmds::Matchmaker(args::Matchmaker {
            intent_gossiper_addr,
            matchmaker_path,
            tx_code_path,
            ledger_addr,
            tx_signing_key,
            tx_source_address,
        })) => {
            let tx_signing_key = ctx.get_cached(&tx_signing_key);
            let tx_source_address = ctx.get(&tx_source_address);

            let wasm_dir = ctx.wasm_dir();
            let config = ctx.config;
            let mut mm_config = config.matchmaker;
            if matchmaker_path.is_some() {
                mm_config.matchmaker_path = matchmaker_path;
            }
            if tx_code_path.is_some() {
                mm_config.tx_code_path = tx_code_path;
            }

            matchmaker::run(
                mm_config,
                intent_gossiper_addr,
                ledger_addr,
                tx_signing_key,
                tx_source_address,
                wasm_dir,
            );
        }
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
