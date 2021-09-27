use anoma::types::{address, token};

use crate::cli::{self, args, Context};
use crate::config::genesis;
use crate::node::ledger::tendermint_node;

/// Initialize genesis validator's address, staking reward address,
/// consensus key, validator account key and staking rewards key and use
/// it in the ledger's node.",
pub fn init_genesis_validator(
    mut ctx: Context,
    args::InitGenesisValidator {
        alias,
        unsafe_dont_encrypt,
    }: args::InitGenesisValidator,
) {
    // Generate validator address
    let validator_address =
        address::gen_established_address("genesis validator address");
    let validator_address_alias = alias.clone();
    if !ctx
        .wallet
        .add_address(validator_address_alias.clone(), validator_address.clone())
    {
        cli::safe_exit(1)
    }
    // Generate staking reward address
    let rewards_address =
        address::gen_established_address("genesis validator reward address");
    let rewards_address_alias = format!("{}-rewards", alias);
    if !ctx
        .wallet
        .add_address(rewards_address_alias.clone(), rewards_address.clone())
    {
        cli::safe_exit(1)
    }

    println!("Generating validator account key...");
    let (validator_key_alias, validator_key) = ctx.wallet.gen_key(
        Some(format!("{}-validator-key", alias)),
        unsafe_dont_encrypt,
    );
    println!("Generating consensus key...");
    let (consensus_key_alias, consensus_key) = ctx.wallet.gen_key(
        Some(format!("{}-consensus-key", alias)),
        unsafe_dont_encrypt,
    );
    println!("Generating staking reward account key...");
    let (rewards_key_alias, rewards_key) = ctx
        .wallet
        .gen_key(Some(format!("{}-rewards-key", alias)), unsafe_dont_encrypt);

    ctx.wallet.save().unwrap_or_else(|err| eprintln!("{}", err));

    let tendermint_home = &ctx.config.ledger.tendermint;
    tendermint_node::write_validator_key(
        tendermint_home,
        &validator_address,
        &consensus_key,
    );
    tendermint_node::write_validator_state(tendermint_home);

    println!();
    println!("The validator's addresses and keys were stored in the wallet:");
    println!("  Validator address \"{}\"", validator_address_alias);
    println!("  Staking reward address \"{}\"", rewards_address_alias);
    println!("  Validator account key \"{}\"", validator_key_alias);
    println!("  Consensus key \"{}\"", consensus_key_alias);
    println!("  Staking reward key \"{}\"", rewards_key_alias);
    println!(
        "The ledger node has been setup to use this validator's address and \
         consensus key."
    );
    println!();
    let genesis_validator = genesis::Validator {
        pos_data: anoma::ledger::pos::GenesisValidator {
            address: validator_address,
            staking_reward_address: rewards_address,
            tokens: token::Amount::whole(200_000),
            consensus_key: consensus_key.public.clone(),
            staking_reward_key: rewards_key.public.clone(),
        },
        account_key: validator_key.public.clone(),
        non_staked_balance: token::Amount::whole(100_000),
        // TODO replace with https://github.com/anoma/anoma/issues/25)
        vp_code_path: "wasm/vp_user.wasm".into(),
    };
    println!("Validator account key {}", validator_key.public);
    println!("Consensus key {}", consensus_key.public);
    println!("Staking reward key {}", rewards_key.public);
    println!("{}", validator_key.public);
    // TODO print in toml format after we have https://github.com/anoma/anoma/issues/425
    println!("Genesis validator config: {:#?}", genesis_validator);
}
