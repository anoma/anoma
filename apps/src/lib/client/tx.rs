use std::borrow::Cow;
use std::convert::TryFrom;
use std::fs::File;

use anoma::ledger::governance::storage as gov_storage;
use anoma::ledger::pos::{BondId, Bonds, Unbonds};
use anoma::proto::Tx;
use anoma::types::address::{xan as m1t, Address};
use anoma::types::governance::{
    OfflineProposal, OfflineVote, Proposal, ProposalVote,
};
use anoma::types::key::*;
use anoma::types::nft::{self, Nft, NftToken};
use anoma::types::storage::Epoch;
use anoma::types::token::Amount;
use anoma::types::transaction::governance::{
    InitProposalData, VoteProposalData,
};
use anoma::types::transaction::nft::{CreateNft, MintNft};
use anoma::types::transaction::{
    hash_tx, pos, Fee, InitAccount, InitValidator, UpdateVp, WrapperTx,
};
use anoma::types::{address, token};
use anoma::{ledger, vm};
use async_std::io::{self, WriteExt};
use borsh::BorshSerialize;
use itertools::Either::*;
use jsonpath_lib as jsonpath;
use serde::Serialize;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::net::Address as TendermintAddress;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::endpoint::broadcast::tx_sync::Response;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::query::{EventType, Query};
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::{Client, HttpClient};
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::endpoint::broadcast::tx_sync::Response;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::query::{EventType, Query};
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::{Client, HttpClient};

use super::{rpc, signing};
use crate::cli::context::WalletAddress;
use crate::cli::{args, safe_exit, Context};
use crate::client::tendermint_websocket_client::{
    Error, TendermintWebsocketClient, WebSocketAddress,
};
#[cfg(not(feature = "ABCI"))]
use crate::node::ledger::events::{Attributes, EventType as TmEventType};
use crate::node::ledger::tendermint_node;

const TX_INIT_ACCOUNT_WASM: &str = "tx_init_account.wasm";
const TX_INIT_VALIDATOR_WASM: &str = "tx_init_validator.wasm";
const TX_INIT_PROPOSAL: &str = "tx_init_proposal.wasm";
const TX_VOTE_PROPOSAL: &str = "tx_vote_proposal.wasm";
const TX_UPDATE_VP_WASM: &str = "tx_update_vp.wasm";
const TX_TRANSFER_WASM: &str = "tx_transfer.wasm";
const TX_INIT_NFT: &str = "tx_init_nft.wasm";
const TX_MINT_NFT: &str = "tx_mint_nft.wasm";
const VP_USER_WASM: &str = "vp_user.wasm";
const TX_BOND_WASM: &str = "tx_bond.wasm";
const TX_UNBOND_WASM: &str = "tx_unbond.wasm";
const TX_WITHDRAW_WASM: &str = "tx_withdraw.wasm";
const VP_NFT: &str = "vp_nft.wasm";

/// Data needed for broadcasting a tx and
/// monitoring its progress on chain
///
/// Txs may be either a dry run or else
/// they should be encrypted and included
/// in a wrapper.
pub enum TxBroadcastData {
    DryRun(Tx),
    Wrapper {
        tx: Tx,
        wrapper_hash: String,
        decrypted_hash: Option<String>,
    },
}

pub async fn submit_custom(ctx: Context, args: args::TxCustom) {
    let tx_code = ctx.read_wasm(args.code_path);
    let data = args.data_path.map(|data_path| {
        std::fs::read(data_path).expect("Expected a file at given data path")
    });
    let tx = Tx::new(tx_code, data);
    let (ctx, initialized_accounts) = process_tx(ctx, &args.tx, tx, None).await;
    save_initialized_accounts(ctx, &args.tx, initialized_accounts).await;
}

pub async fn submit_update_vp(ctx: Context, args: args::TxUpdateVp) {
    let addr = ctx.get(&args.addr);

    // Check that the address is established and exists on chain
    match &addr {
        Address::Established(_) => {
            let exists =
                rpc::known_address(&addr, args.tx.ledger_address.clone()).await;
            if !exists {
                eprintln!("The address {} doesn't exist on chain.", addr);
                if !args.tx.force {
                    safe_exit(1)
                }
            }
        }
        Address::Implicit(_) => {
            eprintln!(
                "A validity predicate of an implicit address cannot be \
                 directly updated. You can use an established address for \
                 this purpose."
            );
            if !args.tx.force {
                safe_exit(1)
            }
        }
        Address::Internal(_) => {
            eprintln!(
                "A validity predicate of an internal address cannot be \
                 directly updated."
            );
            if !args.tx.force {
                safe_exit(1)
            }
        }
    }

    let vp_code = ctx.read_wasm(args.vp_code_path);
    // Validate the VP code
    if let Err(err) = vm::validate_untrusted_wasm(&vp_code) {
        eprintln!("Validity predicate code validation failed with {}", err);
        if !args.tx.force {
            safe_exit(1)
        }
    }

    let tx_code = ctx.read_wasm(TX_UPDATE_VP_WASM);

    let data = UpdateVp { addr, vp_code };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    process_tx(ctx, &args.tx, tx, Some(&args.addr)).await;
}

pub async fn submit_init_account(mut ctx: Context, args: args::TxInitAccount) {
    let public_key = ctx.get_cached(&args.public_key);
    let vp_code = args
        .vp_code_path
        .map(|path| ctx.read_wasm(path))
        .unwrap_or_else(|| ctx.read_wasm(VP_USER_WASM));
    // Validate the VP code
    if let Err(err) = vm::validate_untrusted_wasm(&vp_code) {
        eprintln!("Validity predicate code validation failed with {}", err);
        if !args.tx.force {
            safe_exit(1)
        }
    }

    let tx_code = ctx.read_wasm(TX_INIT_ACCOUNT_WASM);
    let data = InitAccount {
        public_key,
        vp_code,
    };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let (ctx, initialized_accounts) =
        process_tx(ctx, &args.tx, tx, Some(&args.source)).await;
    save_initialized_accounts(ctx, &args.tx, initialized_accounts).await;
}

pub async fn submit_init_validator(
    mut ctx: Context,
    args::TxInitValidator {
        tx: tx_args,
        source,
        account_key,
        consensus_key,
        rewards_account_key,
        protocol_key,
        validator_vp_code_path,
        rewards_vp_code_path,
        unsafe_dont_encrypt,
    }: args::TxInitValidator,
) {
    let alias = tx_args
        .initialized_account_alias
        .as_ref()
        .cloned()
        .unwrap_or_else(|| "validator".to_string());

    let validator_key_alias = format!("{}-key", alias);
    let consensus_key_alias = format!("{}-consensus-key", alias);
    let rewards_key_alias = format!("{}-rewards-key", alias);
    let account_key = ctx.get_opt_cached(&account_key).unwrap_or_else(|| {
        println!("Generating validator account key...");
        ctx.wallet
            .gen_key(Some(validator_key_alias.clone()), unsafe_dont_encrypt)
            .1
            .ref_to()
    });

    let consensus_key =
        ctx.get_opt_cached(&consensus_key).unwrap_or_else(|| {
            println!("Generating consensus key...");
            ctx.wallet
                .gen_key(Some(consensus_key_alias.clone()), unsafe_dont_encrypt)
                .1
        });

    let rewards_account_key =
        ctx.get_opt_cached(&rewards_account_key).unwrap_or_else(|| {
            println!("Generating staking reward account key...");
            ctx.wallet
                .gen_key(Some(rewards_key_alias.clone()), unsafe_dont_encrypt)
                .1
                .ref_to()
        });
    let protocol_key = ctx.get_opt_cached(&protocol_key);

    if protocol_key.is_none() {
        println!("Generating protocol signing key...");
    }
    // Generate the validator keys
    let validator_keys = ctx.wallet.gen_validator_keys(protocol_key).unwrap();
    let protocol_key = validator_keys.get_protocol_keypair().ref_to();
    let dkg_key = validator_keys
        .dkg_keypair
        .as_ref()
        .expect("DKG sessions keys should have been created")
        .public();

    ctx.wallet.save().unwrap_or_else(|err| eprintln!("{}", err));

    let validator_vp_code = validator_vp_code_path
        .map(|path| ctx.read_wasm(path))
        .unwrap_or_else(|| ctx.read_wasm(VP_USER_WASM));
    // Validate the validator VP code
    if let Err(err) = vm::validate_untrusted_wasm(&validator_vp_code) {
        eprintln!(
            "Validator validity predicate code validation failed with {}",
            err
        );
        if !tx_args.force {
            safe_exit(1)
        }
    }
    let rewards_vp_code = rewards_vp_code_path
        .map(|path| ctx.read_wasm(path))
        .unwrap_or_else(|| ctx.read_wasm(VP_USER_WASM));
    // Validate the rewards VP code
    if let Err(err) = vm::validate_untrusted_wasm(&rewards_vp_code) {
        eprintln!(
            "Staking reward account validity predicate code validation failed \
             with {}",
            err
        );
        if !tx_args.force {
            safe_exit(1)
        }
    }
    let tx_code = ctx.read_wasm(TX_INIT_VALIDATOR_WASM);

    let data = InitValidator {
        account_key,
        consensus_key: consensus_key.ref_to(),
        rewards_account_key,
        protocol_key,
        dkg_key,
        validator_vp_code,
        rewards_vp_code,
    };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");
    let tx = Tx::new(tx_code, Some(data));
    let (mut ctx, initialized_accounts) =
        process_tx(ctx, &tx_args, tx, Some(&source)).await;
    if !tx_args.dry_run {
        let (validator_address_alias, validator_address, rewards_address_alias) =
            match &initialized_accounts[..] {
                // There should be 2 accounts, one for the validator itself, one
                // for its staking reward address.
                [account_1, account_2] => {
                    // We need to find out which address is which
                    let (validator_address, rewards_address) =
                        if rpc::is_validator(account_1, tx_args.ledger_address)
                            .await
                        {
                            (account_1, account_2)
                        } else {
                            (account_2, account_1)
                        };

                    let validator_address_alias = match tx_args
                        .initialized_account_alias
                    {
                        Some(alias) => alias,
                        None => {
                            print!(
                                "Choose an alias for the validator address: "
                            );
                            io::stdout().flush().await.unwrap();
                            let mut alias = String::new();
                            io::stdin().read_line(&mut alias).await.unwrap();
                            alias.trim().to_owned()
                        }
                    };
                    let validator_address_alias =
                        if validator_address_alias.is_empty() {
                            println!(
                                "Empty alias given, using {} as the alias.",
                                validator_address.encode()
                            );
                            validator_address.encode()
                        } else {
                            validator_address_alias
                        };
                    if let Some(new_alias) = ctx.wallet.add_address(
                        validator_address_alias.clone(),
                        validator_address.clone(),
                    ) {
                        println!(
                            "Added alias {} for address {}.",
                            new_alias,
                            validator_address.encode()
                        );
                    }
                    let rewards_address_alias =
                        format!("{}-rewards", validator_address_alias);
                    if let Some(new_alias) = ctx.wallet.add_address(
                        rewards_address_alias.clone(),
                        rewards_address.clone(),
                    ) {
                        println!(
                            "Added alias {} for address {}.",
                            new_alias,
                            rewards_address.encode()
                        );
                    }
                    (
                        validator_address_alias,
                        validator_address.clone(),
                        rewards_address_alias,
                    )
                }
                _ => {
                    eprintln!("Expected two accounts to be created");
                    safe_exit(1)
                }
            };
        // add validator address and keys to the wallet
        ctx.wallet
            .add_validator_data(validator_address.clone(), validator_keys);
        ctx.wallet.save().unwrap_or_else(|err| eprintln!("{}", err));

        let tendermint_home = ctx.config.ledger.tendermint_dir();
        tendermint_node::write_validator_key(
            &tendermint_home,
            &validator_address,
            &consensus_key,
        );
        tendermint_node::write_validator_state(tendermint_home);

        println!();
        println!(
            "The validator's addresses and keys were stored in the wallet:"
        );
        println!("  Validator address \"{}\"", validator_address_alias);
        println!("  Staking reward address \"{}\"", rewards_address_alias);
        println!("  Validator account key \"{}\"", validator_key_alias);
        println!("  Consensus key \"{}\"", consensus_key_alias);
        println!("  Staking reward key \"{}\"", rewards_key_alias);
        println!(
            "The ledger node has been setup to use this validator's address \
             and consensus key."
        );
    } else {
        println!("Transaction dry run. No addresses have been saved.")
    }
}

pub async fn submit_transfer(ctx: Context, args: args::TxTransfer) {
    let source = ctx.get(&args.source);
    // Check that the source address exists on chain
    let source_exists =
        rpc::known_address(&source, args.tx.ledger_address.clone()).await;
    if !source_exists {
        eprintln!("The source address {} doesn't exist on chain.", source);
        if !args.tx.force {
            safe_exit(1)
        }
    }
    let target = ctx.get(&args.target);
    // Check that the target address exists on chain
    let target_exists =
        rpc::known_address(&target, args.tx.ledger_address.clone()).await;
    if !target_exists {
        eprintln!("The target address {} doesn't exist on chain.", target);
        if !args.tx.force {
            safe_exit(1)
        }
    }
    let token = ctx.get(&args.token);
    // Check that the token address exists on chain
    let token_exists =
        rpc::known_address(&token, args.tx.ledger_address.clone()).await;
    if !token_exists {
        eprintln!("The token address {} doesn't exist on chain.", token);
        if !args.tx.force {
            safe_exit(1)
        }
    }
    // Check source balance
    let balance_key = token::balance_key(&token, &source);
    let client = HttpClient::new(args.tx.ledger_address.clone()).unwrap();
    match rpc::query_storage_value::<token::Amount>(&client, &balance_key).await
    {
        Some(balance) => {
            if balance < args.amount {
                eprintln!(
                    "The balance of the source {} of token {} is lower than \
                     the amount to be transferred. Amount to transfer is {} \
                     and the balance is {}.",
                    source, token, args.amount, balance
                );
                if !args.tx.force {
                    safe_exit(1)
                }
            }
        }
        None => {
            eprintln!(
                "No balance found for the source {} of token {}",
                source, token
            );
            if !args.tx.force {
                safe_exit(1)
            }
        }
    }
    let tx_code = ctx.read_wasm(TX_TRANSFER_WASM);
    let transfer = token::Transfer {
        source,
        target,
        token,
        amount: args.amount,
    };
    tracing::debug!("Transfer data {:?}", transfer);
    let data = transfer
        .try_to_vec()
        .expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    process_tx(ctx, &args.tx, tx, Some(&args.source)).await;
}

pub async fn submit_init_nft(ctx: Context, args: args::NftCreate) {
    let file = File::open(&args.nft_data).expect("File must exist.");
    let nft: Nft = serde_json::from_reader(file)
        .expect("Couldn't deserialize nft data file");

    let vp_code = match &nft.vp_path {
        Some(path) => {
            std::fs::read(path).expect("Expected a file at given code path")
        }
        None => ctx.read_wasm(VP_NFT),
    };

    let signer = Some(WalletAddress::new(nft.creator.clone().to_string()));

    let data = CreateNft {
        tag: nft.tag.to_string(),
        creator: nft.creator,
        vp_code,
        keys: nft.keys,
        opt_keys: nft.opt_keys,
        tokens: nft.tokens,
    };

    let data = data.try_to_vec().expect(
        "Encoding transfer data to initialize a new account shouldn't fail",
    );

    let tx_code = ctx.read_wasm(TX_INIT_NFT);

    let tx = Tx::new(tx_code, Some(data));
    process_tx(ctx, &args.tx, tx, signer.as_ref()).await;
}

pub async fn submit_mint_nft(ctx: Context, args: args::NftMint) {
    let file = File::open(&args.nft_data).expect("File must exist.");
    let nft_tokens: Vec<NftToken> =
        serde_json::from_reader(file).expect("JSON was not well-formatted");

    let nft_creator_key = nft::get_creator_key(&args.nft_address);
    let client = HttpClient::new(args.tx.ledger_address.clone()).unwrap();
    let nft_creator_address =
        match rpc::query_storage_value::<Address>(&client, &nft_creator_key)
            .await
        {
            Some(addr) => addr,
            None => {
                eprintln!("No creator key found for {}", &args.nft_address);
                safe_exit(1);
            }
        };

    let signer = Some(WalletAddress::new(nft_creator_address.to_string()));

    let data = MintNft {
        address: args.nft_address,
        creator: nft_creator_address,
        tokens: nft_tokens,
    };

    let data = data.try_to_vec().expect(
        "Encoding transfer data to initialize a new account shouldn't fail",
    );

    let tx_code = ctx.read_wasm(TX_MINT_NFT);

    let tx = Tx::new(tx_code, Some(data));
    process_tx(ctx, &args.tx, tx, signer.as_ref()).await;
}

pub async fn submit_init_proposal(mut ctx: Context, args: args::InitProposal) {
    let file = File::open(&args.proposal_data).expect("File must exist.");
    let proposal: Proposal =
        serde_json::from_reader(file).expect("JSON was not well-formatted");

    let signer = WalletAddress::new(proposal.clone().author.to_string());

    if args.offline {
        let signer = ctx.get(&signer);
        let signing_key = signing::find_keypair(
            &mut ctx.wallet,
            &signer,
            args.tx.ledger_address.clone(),
        )
        .await;
        let offline_proposal =
            OfflineProposal::new(proposal, signer, &signing_key);
        let proposal_filename = "proposal".to_string();
        let out = File::create(&proposal_filename).unwrap();
        match serde_json::to_writer_pretty(out, &offline_proposal) {
            Ok(_) => {
                println!("Proposal created: {}.", proposal_filename);
            }
            Err(e) => {
                eprintln!("Error while creating proposal file: {}.", e);
                safe_exit(1)
            }
        }
    } else {
        let client = HttpClient::new(args.tx.ledger_address.clone()).unwrap();

        let tx_data: Result<InitProposalData, _> = proposal.clone().try_into();
        let init_proposal_data = if let Ok(data) = tx_data {
            data
        } else {
            eprintln!("Invalid data for init proposal transaction.");
            safe_exit(1)
        };

        let min_proposal_funds_key = gov_storage::get_min_proposal_fund_key();
        let min_proposal_funds: Amount =
            rpc::query_storage_value(&client, &min_proposal_funds_key)
                .await
                .unwrap();
        let balance = rpc::get_token_balance(&client, &m1t(), &proposal.author)
            .await
            .unwrap_or_default();
        if balance < min_proposal_funds {
            eprintln!(
                "Address {} doesn't have enough funds.",
                &proposal.author
            );
            safe_exit(1);
        }
        let min_proposal_funds_key = gov_storage::get_min_proposal_fund_key();
        let min_proposal_funds: Amount =
            rpc::query_storage_value(&client, &min_proposal_funds_key)
                .await
                .unwrap();

        let balance = rpc::get_token_balance(&client, &m1t(), &proposal.author)
            .await
            .unwrap_or_default();
        if balance < min_proposal_funds {
            eprintln!(
                "Address {} doesn't have enough funds.",
                &proposal.author
            );
            safe_exit(1);
        }

        let data = init_proposal_data
            .try_to_vec()
            .expect("Encoding proposal data shouldn't fail");
        let tx_code = ctx.read_wasm(TX_INIT_PROPOSAL);
        let tx = Tx::new(tx_code, Some(data));

        process_tx(ctx, &args.tx, tx, Some(&signer)).await;
    }
}

pub async fn submit_vote_proposal(mut ctx: Context, args: args::VoteProposal) {
    let signer = if let Some(addr) = &args.tx.signer {
        addr
    } else {
        eprintln!("Missing mandatory argument --signer.");
        safe_exit(1)
    };

    if args.offline {
        let signer = ctx.get(signer);
        let proposal_file_path =
            args.proposal_data.expect("Proposal file should exist.");
        let file = File::open(&proposal_file_path).expect("File must exist.");

        let proposal: OfflineProposal =
            serde_json::from_reader(file).expect("JSON was not well-formatted");
        let public_key = rpc::get_public_key(
            &proposal.address,
            args.tx.ledger_address.clone(),
        )
        .await
        .expect("Public key should exist.");
        if !proposal.check_signature(&public_key) {
            eprintln!("Proposal signature mismatch!");
            safe_exit(1)
        }

        let signing_key = signing::find_keypair(
            &mut ctx.wallet,
            &signer,
            args.tx.ledger_address.clone(),
        )
        .await;
        let offline_vote = OfflineVote::new(
            &proposal,
            args.vote,
            signer.clone(),
            &signing_key,
        );

        let proposal_vote_filename =
            format!("proposal-vote-{}", &signer.to_string());
        let out = File::create(&proposal_vote_filename).unwrap();
        match serde_json::to_writer_pretty(out, &offline_vote) {
            Ok(_) => {
                println!("Proposal vote created: {}.", proposal_vote_filename);
            }
            Err(e) => {
                eprintln!("Error while creating proposal vote file: {}.", e);
                safe_exit(1)
            }
        }
    } else {
        let client = HttpClient::new(args.tx.ledger_address.clone()).unwrap();

        let voter_address = ctx.get(signer);
        let proposal_id = args.proposal_id.unwrap();
        let proposal_start_epoch_key =
            gov_storage::get_voting_start_epoch_key(proposal_id);
        let proposal_start_epoch = rpc::query_storage_value::<Epoch>(
            &client,
            &proposal_start_epoch_key,
        )
        .await;

        match proposal_start_epoch {
            Some(epoch) => {
                let mut delegation_addresses = rpc::get_delegators_delegation(
                    &client,
                    &voter_address,
                    epoch,
                )
                .await;

                // Optimize by quering if a vote from a validator
                // is equal to ours. If so, we can avoid voting
                if !args.tx.force {
                    delegation_addresses = filter_delegations(
                        &client,
                        delegation_addresses,
                        proposal_id,
                        &args.vote,
                    )
                    .await;
                }

                let tx_data = VoteProposalData {
                    id: proposal_id,
                    vote: args.vote,
                    voter: voter_address,
                    delegations: delegation_addresses,
                };

                let data = tx_data
                    .try_to_vec()
                    .expect("Encoding proposal data shouldn't fail");
                let tx_code = ctx.read_wasm(TX_VOTE_PROPOSAL);
                let tx = Tx::new(tx_code, Some(data));

                process_tx(ctx, &args.tx, tx, Some(signer)).await;
            }
            None => {
                eprintln!("Proposal start epoch is not in the storage.")
            }
        }
    }
}

/// Removes validators whose vote corresponds to that of the delegator (needless
/// vote)
async fn filter_delegations(
    client: &HttpClient,
    mut delegation_addresses: Vec<Address>,
    proposal_id: u64,
    delegator_vote: &ProposalVote,
) -> Vec<Address> {
    let mut remove_indexes: Vec<usize> = vec![];

    for (index, validator_address) in delegation_addresses.iter().enumerate() {
        let vote_key = gov_storage::get_vote_proposal_key(
            proposal_id,
            validator_address.to_owned(),
            validator_address.to_owned(),
        );

        if let Some(validator_vote) =
            rpc::query_storage_value::<ProposalVote>(client, &vote_key).await
        {
            if &validator_vote == delegator_vote {
                remove_indexes.push(index);
            }
        }
    }

    for index in remove_indexes {
        delegation_addresses.swap_remove(index);
    }

    delegation_addresses
}

pub async fn submit_bond(ctx: Context, args: args::Bond) {
    let validator = ctx.get(&args.validator);
    // Check that the validator address exists on chain
    let is_validator =
        rpc::is_validator(&validator, args.tx.ledger_address.clone()).await;
    if !is_validator {
        eprintln!(
            "The address {} doesn't belong to any known validator account.",
            validator
        );
        if !args.tx.force {
            safe_exit(1)
        }
    }
    let source = ctx.get_opt(&args.source);
    // Check that the source address exists on chain
    if let Some(source) = &source {
        let source_exists =
            rpc::known_address(source, args.tx.ledger_address.clone()).await;
        if !source_exists {
            eprintln!("The source address {} doesn't exist on chain.", source);
            if !args.tx.force {
                safe_exit(1)
            }
        }
    }
    // Check bond's source (source for delegation or validator for self-bonds)
    // balance
    let bond_source = source.as_ref().unwrap_or(&validator);
    let balance_key = token::balance_key(&address::xan(), bond_source);
    let client = HttpClient::new(args.tx.ledger_address.clone()).unwrap();
    match rpc::query_storage_value::<token::Amount>(&client, &balance_key).await
    {
        Some(balance) => {
            if balance < args.amount {
                eprintln!(
                    "The balance of the source {} is lower than the amount to \
                     be transferred. Amount to transfer is {} and the balance \
                     is {}.",
                    bond_source, args.amount, balance
                );
                if !args.tx.force {
                    safe_exit(1)
                }
            }
        }
        None => {
            eprintln!("No balance found for the source {}", bond_source);
            if !args.tx.force {
                safe_exit(1)
            }
        }
    }
    let tx_code = ctx.read_wasm(TX_BOND_WASM);
    let bond = pos::Bond {
        validator,
        amount: args.amount,
        source,
    };
    let data = bond.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let default_signer = args.source.as_ref().unwrap_or(&args.validator);
    process_tx(ctx, &args.tx, tx, Some(default_signer)).await;
}

pub async fn submit_unbond(ctx: Context, args: args::Unbond) {
    let validator = ctx.get(&args.validator);
    // Check that the validator address exists on chain
    let is_validator =
        rpc::is_validator(&validator, args.tx.ledger_address.clone()).await;
    if !is_validator {
        eprintln!(
            "The address {} doesn't belong to any known validator account.",
            validator
        );
        if !args.tx.force {
            safe_exit(1)
        }
    }

    let source = ctx.get_opt(&args.source);
    let tx_code = ctx.read_wasm(TX_UNBOND_WASM);

    // Check the source's current bond amount
    let bond_source = source.clone().unwrap_or_else(|| validator.clone());
    let bond_id = BondId {
        source: bond_source.clone(),
        validator: validator.clone(),
    };
    let bond_key = ledger::pos::bond_key(&bond_id);
    let client = HttpClient::new(args.tx.ledger_address.clone()).unwrap();
    let bonds = rpc::query_storage_value::<Bonds>(&client, &bond_key).await;
    match bonds {
        Some(bonds) => {
            let mut bond_amount: token::Amount = 0.into();
            for bond in bonds.iter() {
                for delta in bond.deltas.values() {
                    bond_amount += *delta;
                }
            }
            if args.amount > bond_amount {
                eprintln!(
                    "The total bonds of the source {} is lower than the \
                     amount to be unbonded. Amount to unbond is {} and the \
                     total bonds is {}.",
                    bond_source, args.amount, bond_amount
                );
                if !args.tx.force {
                    safe_exit(1)
                }
            }
        }
        None => {
            eprintln!("No bonds found");
            if !args.tx.force {
                safe_exit(1)
            }
        }
    }

    let data = pos::Unbond {
        validator,
        amount: args.amount,
        source,
    };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let default_signer = args.source.as_ref().unwrap_or(&args.validator);
    process_tx(ctx, &args.tx, tx, Some(default_signer)).await;
}

pub async fn submit_withdraw(ctx: Context, args: args::Withdraw) {
    let epoch = rpc::query_epoch(args::Query {
        ledger_address: args.tx.ledger_address.clone(),
    })
    .await;

    let validator = ctx.get(&args.validator);
    // Check that the validator address exists on chain
    let is_validator =
        rpc::is_validator(&validator, args.tx.ledger_address.clone()).await;
    if !is_validator {
        eprintln!(
            "The address {} doesn't belong to any known validator account.",
            validator
        );
        if !args.tx.force {
            safe_exit(1)
        }
    }

    let source = ctx.get_opt(&args.source);
    let tx_code = ctx.read_wasm(TX_WITHDRAW_WASM);

    // Check the source's current unbond amount
    let bond_source = source.clone().unwrap_or_else(|| validator.clone());
    let bond_id = BondId {
        source: bond_source.clone(),
        validator: validator.clone(),
    };
    let bond_key = ledger::pos::unbond_key(&bond_id);
    let client = HttpClient::new(args.tx.ledger_address.clone()).unwrap();
    let unbonds = rpc::query_storage_value::<Unbonds>(&client, &bond_key).await;
    match unbonds {
        Some(unbonds) => {
            let mut unbonded_amount: token::Amount = 0.into();
            if let Some(unbond) = unbonds.get(epoch) {
                for delta in unbond.deltas.values() {
                    unbonded_amount += *delta;
                }
            }
            if unbonded_amount == 0.into() {
                eprintln!(
                    "There are no unbonded bonds ready to withdraw in the \
                     current epoch {}.",
                    epoch
                );
                if !args.tx.force {
                    safe_exit(1)
                }
            }
        }
        None => {
            eprintln!("No unbonded bonds found");
            if !args.tx.force {
                safe_exit(1)
            }
        }
    }

    let data = pos::Withdraw { validator, source };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");

    let tx = Tx::new(tx_code, Some(data));
    let default_signer = args.source.as_ref().unwrap_or(&args.validator);
    process_tx(ctx, &args.tx, tx, Some(default_signer)).await;
}

/// Sign a transaction with a given signing key or public key of a given signer.
/// If no explicit signer given, use the `default`. If no `default` is given,
/// panics.
///
/// If this is not a dry run, the tx is put in a wrapper and returned along with
/// hashes needed for monitoring the tx on chain.
///
/// If it is a dry run, it is not put in a wrapper, but returned as is.
async fn sign_tx(
    mut ctx: Context,
    tx: Tx,
    args: &args::Tx,
    default: Option<&WalletAddress>,
) -> (Context, TxBroadcastData) {
    let (tx, keypair) = if let Some(signing_key) = &args.signing_key {
        let signing_key = ctx.get_cached(signing_key);
        (tx.sign(&signing_key), signing_key)
    } else if let Some(signer) = args.signer.as_ref().or(default) {
        let signer = ctx.get(signer);
        let signing_key = signing::find_keypair(
            &mut ctx.wallet,
            &signer,
            args.ledger_address.clone(),
        )
        .await;
        (tx.sign(&signing_key), signing_key)
    } else {
        panic!(
            "All transactions must be signed; please either specify the key \
             or the address from which to look up the signing key."
        );
    };
    let epoch = rpc::query_epoch(args::Query {
        ledger_address: args.ledger_address.clone(),
    })
    .await;
    let broadcast_data = if args.dry_run {
        TxBroadcastData::DryRun(tx)
    } else {
        sign_wrapper(&ctx, args, epoch, tx, &keypair).await
    };
    (ctx, broadcast_data)
}

/// Create a wrapper tx from a normal tx. Get the hash of the
/// wrapper and its payload which is needed for monitoring its
/// progress on chain.
async fn sign_wrapper(
    ctx: &Context,
    args: &args::Tx,
    epoch: Epoch,
    tx: Tx,
    keypair: &common::SecretKey,
) -> TxBroadcastData {
    let tx = {
        WrapperTx::new(
            Fee {
                amount: args.fee_amount,
                token: ctx.get(&args.fee_token),
            },
            keypair,
            epoch,
            args.gas_limit.clone(),
            tx,
            // TODO: Actually use the fetched encryption key
            Default::default(),
        )
    };

    // We use this to determine when the wrapper tx makes it on-chain
    let wrapper_hash = if !cfg!(feature = "ABCI") {
        hash_tx(&tx.try_to_vec().unwrap()).to_string()
    } else {
        tx.tx_hash.to_string()
    };
    // We use this to determine when the decrypted inner tx makes it
    // on-chain
    let decrypted_hash = if !cfg!(feature = "ABCI") {
        Some(tx.tx_hash.to_string())
    } else {
        None
    };
    TxBroadcastData::Wrapper {
        tx: tx
            .sign(keypair)
            .expect("Wrapper tx signing keypair should be correct"),
        wrapper_hash,
        decrypted_hash,
    }
}

/// Submit transaction and wait for result. Returns a list of addresses
/// initialized in the transaction if any. In dry run, this is always empty.
async fn process_tx(
    ctx: Context,
    args: &args::Tx,
    tx: Tx,
    default_signer: Option<&WalletAddress>,
) -> (Context, Vec<Address>) {
    let (ctx, to_broadcast) = sign_tx(ctx, tx, args, default_signer).await;
    // NOTE: use this to print the request JSON body:

    // let request =
    // tendermint_rpc::endpoint::broadcast::tx_commit::Request::new(
    //     tx_bytes.clone().into(),
    // );
    // use tendermint_rpc::Request;
    // let request_body = request.into_json();
    // println!("HTTP request body: {}", request_body);

    if args.dry_run {
        if let TxBroadcastData::DryRun(tx) = to_broadcast {
            rpc::dry_run_tx(&args.ledger_address, tx.to_bytes()).await;
            (ctx, vec![])
        } else {
            panic!(
                "Expected a dry-run transaction, received a wrapper \
                 transaction instead"
            );
        }
    } else {
        // Either broadcast or submit transaction and collect result into
        // sum type
        let result = if args.broadcast_only {
            Left(broadcast_tx(args.ledger_address.clone(), &to_broadcast).await)
        } else {
            Right(submit_tx(args.ledger_address.clone(), to_broadcast).await)
        };
        // Return result based on executed operation, otherwise deal with
        // the encountered errors uniformly
        match result {
            Right(Ok(result)) => (ctx, result.initialized_accounts),
            Left(Ok(_)) => (ctx, Vec::default()),
            Right(Err(err)) | Left(Err(err)) => {
                eprintln!(
                    "Encountered error while broadcasting transaction: {}",
                    err
                );
                safe_exit(1)
            }
        }
    }
}

/// Save accounts initialized from a tx into the wallet, if any.
async fn save_initialized_accounts(
    mut ctx: Context,
    args: &args::Tx,
    initialized_accounts: Vec<Address>,
) {
    let len = initialized_accounts.len();
    if len != 0 {
        // Store newly initialized account addresses in the wallet
        println!(
            "The transaction initialized {} new account{}",
            len,
            if len == 1 { "" } else { "s" }
        );
        // Store newly initialized account addresses in the wallet
        let wallet = &mut ctx.wallet;
        for (ix, address) in initialized_accounts.iter().enumerate() {
            let encoded = address.encode();
            let alias: Cow<str> = match &args.initialized_account_alias {
                Some(initialized_account_alias) => {
                    if len == 1 {
                        // If there's only one account, use the
                        // alias as is
                        initialized_account_alias.into()
                    } else {
                        // If there're multiple accounts, use
                        // the alias as prefix, followed by
                        // index number
                        format!("{}{}", initialized_account_alias, ix).into()
                    }
                }
                None => {
                    print!("Choose an alias for {}: ", encoded);
                    io::stdout().flush().await.unwrap();
                    let mut alias = String::new();
                    io::stdin().read_line(&mut alias).await.unwrap();
                    alias.trim().to_owned().into()
                }
            };
            let alias = alias.into_owned();
            let added = wallet.add_address(alias.clone(), address.clone());
            match added {
                Some(new_alias) if new_alias != encoded => {
                    println!(
                        "Added alias {} for address {}.",
                        new_alias, encoded
                    );
                }
                _ => println!("No alias added for address {}.", encoded),
            };
        }
        if !args.dry_run {
            wallet.save().unwrap_or_else(|err| eprintln!("{}", err));
        } else {
            println!("Transaction dry run. No addresses have been saved.")
        }
    }
}

/// Broadcast a transaction to be included in the blockchain and checks that
/// the tx has been successfully included into the mempool of a validator
///
/// In the case of errors in any of those stages, an error message is returned
pub async fn broadcast_tx(
    address: TendermintAddress,
    to_broadcast: &TxBroadcastData,
) -> Result<Response, Error> {
    let (tx, wrapper_tx_hash, _decrypted_tx_hash) = match to_broadcast {
        TxBroadcastData::Wrapper {
            tx,
            wrapper_hash,
            decrypted_hash,
        } => (tx, wrapper_hash, decrypted_hash),
        _ => panic!("Cannot broadcast a dry-run transaction"),
    };
    let mut wrapper_tx_subscription = TendermintWebsocketClient::open(
        WebSocketAddress::try_from(address.clone())?,
        None,
    )?;

    let response = wrapper_tx_subscription
        .broadcast_tx_sync(tx.to_bytes().into())
        .await
        .map_err(|err| Error::Response(format!("{:?}", err)))?;

    wrapper_tx_subscription.close();

    if response.code == 0.into() {
        println!("Transaction added to mempool: {:?}", response);
        // Print the transaction identifiers to enable the extraction of
        // acceptance/application results later
        #[cfg(not(feature = "ABCI"))]
        {
            println!("Wrapper transaction hash: {:?}", wrapper_tx_hash);
            println!("Inner transaction hash: {:?}", _decrypted_tx_hash);
        }
        #[cfg(feature = "ABCI")]
        println!("Transaction hash: {:?}", wrapper_tx_hash);
        Ok(response)
    } else {
        Err(Error::Response(response.log.to_string()))
    }
}

/// Broadcast a transaction to be included in the blockchain.
///
/// Checks that
/// 1. The tx has been successfully included into the mempool of a validator
/// 2. The tx with encrypted payload has been included on the blockchain
/// 3. The decrypted payload of the tx has been included on the blockchain.
///
/// In the case of errors in any of those stages, an error message is returned
pub async fn submit_tx(
    address: TendermintAddress,
    to_broadcast: TxBroadcastData,
) -> Result<TxResponse, Error> {
    let (_, wrapper_hash, _decrypted_hash) = match &to_broadcast {
        TxBroadcastData::Wrapper {
            tx,
            wrapper_hash,
            decrypted_hash,
        } => (tx, wrapper_hash, decrypted_hash),
        _ => panic!("Cannot broadcast a dry-run transaction"),
    };
    let mut wrapper_tx_subscription = TendermintWebsocketClient::open(
        WebSocketAddress::try_from(address.clone())?,
        None,
    )?;

    // It is better to subscribe to the transaction before it is broadcast
    //
    // Note that the `applied.hash` key comes from a custom event
    // created by the shell
    #[cfg(not(feature = "ABCI"))]
    let query_key = "accepted.hash";
    #[cfg(feature = "ABCI")]
    let query_key = "applied.hash";
    let query = Query::from(EventType::NewBlock)
        .and_eq(query_key, wrapper_hash.as_str());
    wrapper_tx_subscription.subscribe(query)?;

    // If we are using ABCI++, we also subscribe to the event emitted
    // when the encrypted payload makes its way onto the blockchain
    #[cfg(not(feature = "ABCI"))]
    let mut decrypted_tx_subscription = {
        let mut decrypted_tx_subscription = TendermintWebsocketClient::open(
            WebSocketAddress::try_from(address.clone())?,
            None,
        )?;
        let query = Query::from(EventType::NewBlock)
            .and_eq("applied.hash", _decrypted_hash.as_ref().unwrap().as_str());
        decrypted_tx_subscription.subscribe(query)?;
        decrypted_tx_subscription
    };
    // Broadcast the supplied transaction
    broadcast_tx(address, &to_broadcast).await?;

    #[cfg(not(feature = "ABCI"))]
    let parsed = {
        let parsed = parse(
            wrapper_tx_subscription.receive_response()?,
            TmEventType::Accepted,
            wrapper_hash,
        );
        println!(
            "Transaction accepted with result: {}",
            serde_json::to_string_pretty(&parsed).unwrap()
        );
        // The transaction is now on chain. We wait for it to be decrypted
        // and applied
        if parsed.code == 0.to_string() {
            let parsed = parse(
                decrypted_tx_subscription.receive_response()?,
                TmEventType::Applied,
                _decrypted_hash.as_ref().unwrap(),
            );
            println!(
                "Transaction applied with result: {}",
                serde_json::to_string_pretty(&parsed).unwrap()
            );
            Ok(parsed)
        } else {
            Ok(parsed)
        }
    };

    #[cfg(feature = "ABCI")]
    let parsed = {
        let parsed = TxResponse::find_tx(
            wrapper_tx_subscription.receive_response()?,
            wrapper_hash,
        );
        println!(
            "Transaction applied with result: {}",
            serde_json::to_string_pretty(&parsed).unwrap()
        );
        Ok(parsed)
    };

    wrapper_tx_subscription.unsubscribe()?;
    wrapper_tx_subscription.close();
    #[cfg(not(feature = "ABCI"))]
    {
        decrypted_tx_subscription.unsubscribe()?;
        decrypted_tx_subscription.close();
    }

    parsed
}

#[derive(Debug, Serialize)]
pub struct TxResponse {
    pub info: String,
    pub log: String,
    pub height: String,
    pub hash: String,
    pub code: String,
    pub gas_used: String,
    pub initialized_accounts: Vec<Address>,
}

/// Parse the JSON payload received from a subscription
///
/// Searches for custom events emitted from the ledger and converts
/// them back to thin wrapper around a hashmap for further parsing.
#[cfg(not(feature = "ABCI"))]
fn parse(
    json: serde_json::Value,
    event_type: TmEventType,
    tx_hash: &str,
) -> TxResponse {
    let mut selector = jsonpath::selector(&json);
    let mut event =
        selector(&format!("$.events.[?(@.type=='{}')]", event_type))
            .unwrap()
            .iter()
            .filter_map(|event| {
                let attrs = Attributes::from(*event);
                match attrs.get("hash") {
                    Some(hash) if hash == tx_hash => Some(attrs),
                    _ => None,
                }
            })
            .collect::<Vec<Attributes>>()
            .remove(0);

    let info = event.take("info").unwrap();
    let log = event.take("log").unwrap();
    let height = event.take("height").unwrap();
    let hash = event.take("hash").unwrap();
    let code = event.take("code").unwrap();
    let gas_used = event.take("gas_used").unwrap_or_else(|| String::from("0"));
    let initialized_accounts = event.take("initialized_accounts");
    let initialized_accounts = match initialized_accounts {
        Some(values) => serde_json::from_str(&values).unwrap(),
        _ => vec![],
    };
    TxResponse {
        info,
        log,
        height,
        hash,
        code,
        gas_used,
        initialized_accounts,
    }
}

impl TxResponse {
    pub fn find_tx(json: serde_json::Value, tx_hash: &str) -> Self {
        let tx_hash_json = serde_json::Value::String(tx_hash.to_string());
        let mut selector = jsonpath::selector(&json);
        let mut index = 0;
        #[cfg(feature = "ABCI")]
        let evt_key = "applied";
        #[cfg(not(feature = "ABCI"))]
        let evt_key = "accepted";
        // Find the tx with a matching hash
        let hash = loop {
            if let Ok(hash) =
                selector(&format!("$.events.['{}.hash'][{}]", evt_key, index))
            {
                let hash = hash[0].clone();
                if hash == tx_hash_json {
                    break hash;
                } else {
                    index += 1;
                }
            } else {
                eprintln!(
                    "Couldn't find tx with hash {} in the event string {}",
                    tx_hash, json
                );
                safe_exit(1)
            }
        };
        let info =
            selector(&format!("$.events.['{}.info'][{}]", evt_key, index))
                .unwrap();
        let log = selector(&format!("$.events.['{}.log'][{}]", evt_key, index))
            .unwrap();
        let height =
            selector(&format!("$.events.['{}.height'][{}]", evt_key, index))
                .unwrap();
        let code =
            selector(&format!("$.events.['{}.code'][{}]", evt_key, index))
                .unwrap();
        let gas_used =
            selector(&format!("$.events.['{}.gas_used'][{}]", evt_key, index))
                .unwrap();
        let initialized_accounts = selector(&format!(
            "$.events.['{}.initialized_accounts'][{}]",
            evt_key, index
        ));
        let initialized_accounts = match initialized_accounts {
            Ok(values) if !values.is_empty() => {
                // In a response, the initialized accounts are encoded as e.g.:
                // ```
                // "applied.initialized_accounts": Array([
                //   String(
                //     "[\"atest1...\"]",
                //   ),
                // ]),
                // ...
                // So we need to decode the inner string first ...
                let raw: String =
                    serde_json::from_value(values[0].clone()).unwrap();
                // ... and then decode the vec from the array inside the string
                serde_json::from_str(&raw).unwrap()
            }
            _ => vec![],
        };
        TxResponse {
            info: serde_json::from_value(info[0].clone()).unwrap(),
            log: serde_json::from_value(log[0].clone()).unwrap(),
            height: serde_json::from_value(height[0].clone()).unwrap(),
            hash: serde_json::from_value(hash).unwrap(),
            code: serde_json::from_value(code[0].clone()).unwrap(),
            gas_used: serde_json::from_value(gas_used[0].clone()).unwrap(),
            initialized_accounts,
        }
    }
}
