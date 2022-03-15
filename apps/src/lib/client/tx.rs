use std::borrow::Cow;
use std::convert::TryFrom;

use anoma::ledger::pos::{BondId, Bonds, Unbonds};
use anoma::proto::Tx;
use anoma::types::address::Address;
use anoma::types::key::*;
use anoma::types::nft::{self, Nft, NftToken};
use anoma::types::transaction::nft::{CreateNft, MintNft};
use anoma::types::transaction::{
    pos, Fee, InitAccount, InitValidator, UpdateVp, WrapperTx,
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
use masp_primitives::transaction::Transaction;
use masp_primitives::primitives::Note;
use masp_primitives::consensus::TestNetwork;
use masp_primitives::consensus::BranchId;
use std::collections::HashSet;
use masp_primitives::note_encryption::*;
use masp_primitives::primitives::ViewingKey;
use std::collections::HashMap;
use masp_primitives::transaction::components::Amount;
use masp_primitives::transaction::builder::{self, *};
use rand_core::CryptoRng;
use rand_core::RngCore;
use zcash_primitives::merkle_tree::CommitmentTree;
use masp_primitives::sapling::Node;
use ff::PrimeField;
use zcash_primitives::merkle_tree::IncrementalWitness;
use masp_primitives::zip32::ExtendedSpendingKey;
use masp_primitives::primitives::Diversifier;
use rand_core::OsRng;
use std::fmt::Debug;
use masp_primitives::legacy::TransparentAddress;
use masp_primitives::transaction::components::OutPoint;
use masp_primitives::transaction::components::TxOut;
use masp_proofs::prover::LocalTxProver;
use sha2::Digest;
use group::cofactor::CofactorGroup;
use masp_primitives::zip32::ExtendedFullViewingKey;
use masp_primitives::asset_type::AssetType;

use super::{rpc, signing};
use crate::cli::context::WalletAddress;
use crate::cli::{args, safe_exit, Context};
#[cfg(not(feature = "ABCI"))]
use crate::client::tendermint_websocket_client::hash_tx;
use crate::client::tendermint_websocket_client::{
    Error, TendermintWebsocketClient, WebSocketAddress,
};
#[cfg(not(feature = "ABCI"))]
use crate::node::ledger::events::{Attributes, EventType as TmEventType};
use crate::node::ledger::tendermint_node;
use crate::std::fs::File;
use masp_primitives::transaction::TxId;
use anoma::types::token::{HEAD_TX_KEY, TX_KEY_PREFIX};
use anoma::types::storage::Key;
use crate::client::rpc::query_storage_value;
use anoma::types::storage::KeySeg;
use anoma::types::address::masp;

const TX_INIT_ACCOUNT_WASM: &str = "tx_init_account.wasm";
const TX_INIT_VALIDATOR_WASM: &str = "tx_init_validator.wasm";
const TX_UPDATE_VP_WASM: &str = "tx_update_vp.wasm";
const TX_TRANSFER_WASM: &str = "tx_transfer.wasm";
const TX_CREATE_NFT: &str = "tx_create_nft.wasm";
const TX_MINT_NFT_TOKEN: &str = "tx_mint_nft_tokens.wasm";
const VP_USER_WASM: &str = "vp_user.wasm";
const TX_BOND_WASM: &str = "tx_bond.wasm";
const TX_UNBOND_WASM: &str = "tx_unbond.wasm";
const TX_WITHDRAW_WASM: &str = "tx_withdraw.wasm";
const VP_NFT: &str = "vp_nft.wasm";

pub async fn submit_custom(ctx: Context, args: args::TxCustom) {
    let tx_code = ctx.read_wasm(args.code_path);
    let data = args.data_path.map(|data_path| {
        std::fs::read(data_path).expect("Expected a file at given data path")
    });
    let tx = Tx::new(tx_code, data);
    let (ctx, tx, keypair) = sign_tx(ctx, tx, &args.tx, None).await;
    let (ctx, initialized_accounts) =
        process_tx(ctx, &args.tx, tx, &keypair).await;
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
    let (ctx, tx, keypair) = sign_tx(ctx, tx, &args.tx, Some(&args.addr)).await;
    process_tx(ctx, &args.tx, tx, &keypair).await;
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
    let (ctx, tx, keypair) =
        sign_tx(ctx, tx, &args.tx, Some(&args.source)).await;
    let (ctx, initialized_accounts) =
        process_tx(ctx, &args.tx, tx, &keypair).await;
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
        validator_vp_code,
        rewards_vp_code,
    };
    let data = data.try_to_vec().expect("Encoding tx data shouldn't fail");
    let tx = Tx::new(tx_code, Some(data));
    let (ctx, tx, keypair) = sign_tx(ctx, tx, &tx_args, Some(&source)).await;

    let (mut ctx, initialized_accounts) =
        process_tx(ctx, &tx_args, tx, &keypair).await;
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

/// Make a ViewingKey that can view notes encrypted by given ExtendedSpendingKey

pub fn to_viewing_key(esk: &ExtendedSpendingKey) -> ViewingKey {
    ExtendedFullViewingKey::from(esk).fvk.vk
}

/// Generate a valid diversifier, i.e. one that has a diversified base. Return
/// also this diversified base.

pub fn find_valid_diversifier<R: RngCore + CryptoRng>(
    rng: &mut R
) -> (Diversifier, jubjub::SubgroupPoint) {
    let mut diversifier;
    let g_d;
    // Keep generating random diversifiers until one has a diversified base
    loop {
        let mut d = [0; 11];
        rng.fill_bytes(&mut d);
        diversifier = Diversifier(d);
        if let Some(val) = diversifier.g_d() {
            g_d = val;
            break;
        }
    }
    (diversifier, g_d)
}

/// Obtain a chronologically-ordered list of all accepted shielded transactions
/// from the ledger. The ledger conceptually stores transactions as a linked
/// list. More concretely, the HEAD_TX_KEY location stores the txid of the last
/// accepted transaction, each transaction stores the txid of the previous
/// transaction, and each transaction is stored at a key whose name is derived
/// from its txid.

pub async fn fetch_shielded_transfers(
    ledger_address: &TendermintAddress,
) -> Vec<Transaction> {
    let client = HttpClient::new(ledger_address.clone()).unwrap();
    // The address of the MASP account
    let masp_addr = masp();
    // Construct the key where last transaction pointer is stored
    let head_tx_key = Key::from(masp_addr.to_db_key())
        .push(&HEAD_TX_KEY.to_owned())
        .expect("Cannot obtain a storage key");
    // Query for the ID of the last accepted transaction
    let mut head_txid_opt = query_storage_value::<TxId>(
        client.clone(),
        head_tx_key,
    )
        .await;
    let mut shielded_txs = Vec::new();
    // While there are still more transactions in the linked list
    while let Some(head_txid) = head_txid_opt {
        // Construct the key for where the current transaction is stored
        let current_tx_key = Key::from(masp_addr.to_db_key())
            .push(&(TX_KEY_PREFIX.to_owned() + &head_txid.to_string()))
            .expect("Cannot obtain a storage key");
        // Obtain the current transaction and a pointer to the next
        let (current_tx, next_txid) = query_storage_value::<(Transaction, Option<TxId>)>(
            client.clone(),
            current_tx_key,
        ).await.unwrap();
        // Collect the current transaction
        shielded_txs.push(current_tx);
        head_txid_opt = next_txid;
    }
    // Since we iterated the transaction in reverse chronological order
    shielded_txs.reverse();
    shielded_txs
}

/// Represents the current state of the shielded pool from the perspective of
/// the chosen viewing keys.

pub struct TxContext {
    tx_pos: usize,
    tree: CommitmentTree<Node>,
    vks: HashMap<ViewingKey, HashSet<usize>>,
    nf_map: HashMap<[u8; 32], usize>,
    note_map: HashMap<usize, Note>,
    memo_map: HashMap<usize, Memo>,
    div_map: HashMap<usize, Diversifier>,
    witness_map: HashMap<usize, IncrementalWitness<Node>>,
    spents: HashSet<usize>,
}

/// Default implementation to ease construction of TxContexts. Derive cannot be
/// used here due to CommitmentTree not implementing Default.

impl Default for TxContext {
    fn default() -> TxContext {
        TxContext {
            tx_pos: usize::default(),
            tree: CommitmentTree::empty(),
            vks: HashMap::default(),
            nf_map: HashMap::default(),
            note_map: HashMap::default(),
            memo_map: HashMap::default(),
            div_map: HashMap::default(),
            witness_map: HashMap::default(),
            spents: HashSet::default(),
        }
    }
}

/// Applies the given transaction to the supplied context. More precisely, the
/// shielded transaction's outputs are added to the commitment tree. Newly
/// discovered notes are associated to the supplied viewing keys. Note
/// nullifiers are mapped to their originating notes. Note positions are
/// associated to notes, memos, and diversifiers. And the set of notes that we
/// have spent are updated. The witness map is maintained to make it easier to
/// construct note merkle paths in other code. See
/// https://zips.z.cash/protocol/protocol.pdf#scan

fn scan_tx(
    tx: &Transaction,
    ctx: &mut TxContext,
) -> Result<(), ()> {
    // Listen for notes sent to our viewing keys
    for so in &(*tx).shielded_outputs {
        // Create merkle tree leaf node from note commitment
        let node = Node::new(so.cmu.to_repr());
        // Update each merkle tree in the witness map with the latest addition
        for (_, witness) in ctx.witness_map.iter_mut() {
            witness.append(node)?;
        }
        let note_pos = ctx.tree.size();
        ctx.tree.append(node)?;
        // Finally, make it easier to construct merkle paths to this new note
        let witness = IncrementalWitness::<Node>::from_tree(&ctx.tree);
        ctx.witness_map.insert(note_pos, witness);
        // Let's try to see if any of our viewing keys can decrypt latest note
        for (vk, notes) in ctx.vks.iter_mut() {
            let decres = try_sapling_note_decryption::<TestNetwork>(
                0,
                &vk.ivk().0,
                &so.ephemeral_key.into_subgroup().unwrap(),
                &so.cmu,
                &so.enc_ciphertext
            );
            // So this current viewing key does decrypt this current note...
            if let Some((note, pa, memo)) = decres {
                // Add this note to list of notes decrypted by this viewing key
                notes.insert(note_pos);
                // Compute the nullifier now to quickly recognize when spent
                let nf = note.nf(vk, note_pos.try_into().unwrap());
                ctx.note_map.insert(note_pos, note);
                ctx.memo_map.insert(note_pos, memo);
                // The payment address' diversifier is required to spend note
                ctx.div_map.insert(note_pos, *pa.diversifier());
                ctx.nf_map.insert(nf.0.try_into().unwrap(), note_pos);
                break;
            }
        }
    }
    // Cancel out those of our notes that have been spent
    for ss in &(*tx).shielded_spends {
        // If the shielded spend's nullifier is in our map, then target note
        // is rendered unusable
        if let Some(note_pos) = ctx.nf_map.get(&ss.nullifier) {
            ctx.spents.insert(*note_pos);
        }
    }
    // To avoid state corruption caused by accidentaly replaying transaction
    ctx.tx_pos += 1;
    Ok(())
}

/// Make shielded components to embed within a Transfer object. If no shielded
/// payment address nor spending key is specified, then no shielded components
/// are produced. Otherwise a transaction containing nullifiers and/or note
/// commitments are produced. Dummy transparent UTXOs are sometimes used to make
/// transactions balanced, but it is understood that transparent account changes
/// are effected only by the amounts and signatures specified by the containing
/// Transfer object.

async fn gen_shielded_transfer(
    ctx: &Context,
    args: &args::TxTransfer,
    balance: token::Amount,
) -> Result<Option<(Transaction, TransactionMetadata)>, builder::Error> {
    // No shielded components are needed when neither source nor destination
    // are shielded
    if args.spending_key.is_none() && args.payment_address.is_none() {
        return Ok(None);
    }
    // We want to fund our transaction solely from supplied spending key
    let spending_keys = args.spending_key.clone().into_iter().collect();
    // Load the current shielded context given the spending key we possess
    let tx_ctx = load_shielded_context(
        &args.tx.ledger_address,
        &spending_keys,
        &vec![],
    ).await;
    // Context required for storing which notes are in the source's possesion
    let height = 0u32;
    let consensus_branch_id = BranchId::Sapling;
    let amt: u64 = args.amount.into();
    let memo: Option<Memo> = None;

    // Now we build up the transaction within this object
    let mut builder = Builder::<TestNetwork, OsRng>::new(height);
    let token_bytes = ctx.get(&args.token).try_to_vec().expect("token should serialize");
    // Generate the unique asset identifier from the unique token address
    let asset_type = AssetType::new(token_bytes.as_ref()).expect("unable to create asset type");
    // Transaction fees will be taken care of in the wrapper Transfer
    builder.set_fee(Amount::zero())?;
    // If there are shielded inputs
    if let Some(sk) = &args.spending_key {
        let mut val_acc = 0;
        // Retrieve the notes that can be spent by this key
        if let Some(avail_notes) = tx_ctx.vks.get(&to_viewing_key(&sk)) {
            for note_idx in avail_notes {
                // No more transaction inputs are required once we have met
                // the target amount
                if val_acc >= amt { break; }
                // Spent notes cannot contribute a new transaction's pool
                if tx_ctx.spents.contains(note_idx) { continue; }
                // Get note, merkle path, diversifier associated with this ID
                let note = tx_ctx.note_map.get(note_idx).unwrap().clone();
                // Note with distinct asset type cannot be used as input to this
                // transaction
                if note.asset_type != asset_type { continue; }
                let merkle_path = tx_ctx.witness_map.get(note_idx).unwrap().path().unwrap();
                let diversifier = tx_ctx.div_map.get(note_idx).unwrap();
                val_acc += note.value;
                // Commit this note to our transaction
                builder.add_sapling_spend(sk.clone(), *diversifier, note, merkle_path)?;
            }
        }
        // If there is change leftover send it back to this spending key
        if val_acc > amt {
            let vk = sk.expsk.proof_generation_key().to_viewing_key();
            let change_pa = vk.to_payment_address(find_valid_diversifier(&mut OsRng).0).unwrap();
            let change_amt = val_acc - amt;
            builder.add_sapling_output(
                Some(sk.expsk.ovk),
                change_pa.clone(),
                asset_type,
                change_amt,
                None
            )?;
        }
    } else {
        // Otherwise the input must be entirely transparent. So model the source
        // address' balance as the input UTXO
        let balance: u64 = balance.into();
        // We add a dummy UTXO to our transaction, but only the source of the
        // parent Transfer object is used to validate fund availability
        let secp_sk = secp256k1::SecretKey::from_slice(&[0xcd; 32]).expect("secret key");
        let secp_ctx = secp256k1::Secp256k1::<secp256k1::SignOnly>::gen_new();
        let secp_pk = secp256k1::PublicKey::from_secret_key(&secp_ctx, &secp_sk).serialize();
        let hash = ripemd160::Ripemd160::digest(&sha2::Sha256::digest(&secp_pk));
        let script = TransparentAddress::PublicKey(hash.into()).script();
        builder.add_transparent_input(
            secp_sk,
            OutPoint::new([0u8; 32], 0),
            TxOut {
                asset_type,
                value: balance,
                script_pubkey: script
            }
        )?;
        // If there is change leftover send it back to this transparent input
        if balance > amt {
            builder.add_transparent_output(
                &TransparentAddress::PublicKey([0u8; 20]),
                asset_type,
                balance - amt
            )?;
        }
    }
    // Now handle the outputs of this transaction
    // If there is a shielded output
    if let Some(pa) = args.payment_address {
        let ovk_opt = args.spending_key.as_ref().map(|x| x.expsk.ovk);
        builder.add_sapling_output(ovk_opt, pa.clone(), asset_type, amt, memo)?;
    } else {
        builder.add_transparent_output(
            &TransparentAddress::PublicKey([0u8; 20]),
            asset_type,
            amt
        )?;
    }
    // Build and return the constructed transaction
    builder.build(
        consensus_branch_id,
        &LocalTxProver::with_default_location().expect("unable to load MASP Parameters")
    ).map(|x| Some(x))
}

/// Load up the current state of the multi-asset shielded pool into a TxContext

pub async fn load_shielded_context(
    ledger_address: &TendermintAddress,
    sks: &Vec<ExtendedSpendingKey>,
    fvks: &Vec<ViewingKey>,
) -> TxContext {
    // Load all transactions accepted until this point
    let txs = fetch_shielded_transfers(ledger_address).await;
    let mut tx_ctx = TxContext::default();
    // Load the viewing keys corresponding to given spending keys into context
    for esk in sks {
        let vk = to_viewing_key(esk);
        tx_ctx.vks.entry(vk.into()).or_insert(HashSet::new());
    }
    for vk in fvks {
        tx_ctx.vks.entry(*vk).or_insert(HashSet::new());
    }
    // Apply the loaded transactions to our context
    while tx_ctx.tx_pos < txs.len() {
        scan_tx(&txs[tx_ctx.tx_pos], &mut tx_ctx).expect("transaction scanned");
    }
    tx_ctx
}

/// Compute the total unspent notes associated with the viewing key in the
/// context. If the key is not in the context, then we do not know the balance
/// and hence we return None.

pub fn compute_shielded_balance(tx_ctx: &TxContext, vk: &ViewingKey) -> Option<Amount> {
    // Cannot query the balance of a key that's not in the map
    if !tx_ctx.vks.contains_key(vk) {
        return None
    }
    let mut val_acc = Amount::zero();
    // Retrieve the notes that can be spent by this key
    if let Some(avail_notes) = tx_ctx.vks.get(vk) {
        for note_idx in avail_notes {
            // Spent notes cannot contribute a new transaction's pool
            if tx_ctx.spents.contains(note_idx) { continue; }
            // Get note associated with this ID
            let note = tx_ctx.note_map.get(note_idx).unwrap().clone();
            // Finally add value to multi-asset accumulator
            val_acc += Amount::from(note.asset_type, note.value)
                .expect("found note with invalid value or asset type");
        }
    }
    Some(val_acc)
}

pub async fn submit_transfer(ctx: Context, args: args::TxTransfer) {
    let source = ctx.get(&args.source);
    let target = ctx.get(&args.target);
    if (source == masp()) != args.spending_key.is_some() {
        // A spending key implies a transparent transfer from the MASP address
        eprintln!("Must either specify source address or a spending key");
        safe_exit(1)
    } else if (target == masp()) != args.payment_address.is_some() {
        // A payment address implies a transparent transfer to the MASP address
        eprintln!("Must either specify target address or a payment address");
        safe_exit(1)
    }
    // Check that the source address exists on chain
    let source_exists =
        rpc::known_address(&source, args.tx.ledger_address.clone()).await;
    if !source_exists {
        eprintln!("The source address {} doesn't exist on chain.", source);
        if !args.tx.force {
            safe_exit(1)
        }
    }
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
    let balance = match rpc::query_storage_value::<token::Amount>(client, balance_key).await {
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
            balance
        }
        None => {
            eprintln!(
                "No balance found for the source {} of token {}",
                source, token
            );
            if !args.tx.force {
                safe_exit(1)
            }
            0.into()
        }
    };
    
    let tx_code = ctx.read_wasm(TX_TRANSFER_WASM);
    let masp_addr = masp();
    // The non-MASP entity, if any, will be signer for shielded transactions
    let default_signer =
        if source == masp_addr && target == masp_addr {
            None
        } else if source == masp_addr {
            Some(&args.target)
        } else {
            Some(&args.source)
        };
    let transfer = token::Transfer {
        source,
        target,
        token,
        amount: args.amount,
        shielded: gen_shielded_transfer(&ctx, &args, balance).await.unwrap().map(|x| x.0),
    };
    tracing::debug!("Transfer data {:?}", transfer);
    let data = transfer
        .try_to_vec()
        .expect("Encoding tx data shouldn't fail");
    
    let tx = Tx::new(tx_code, Some(data));
    let (ctx, tx, keypair) =
        sign_tx(ctx, tx, &args.tx, default_signer).await;
    process_tx(ctx, &args.tx, tx, &keypair).await;
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

    let tx_code = ctx.read_wasm(TX_CREATE_NFT);

    let tx = Tx::new(tx_code, Some(data));

    let (ctx, tx, keypair) = sign_tx(ctx, tx, &args.tx, signer.as_ref()).await;

    process_tx(ctx, &args.tx, tx, &keypair).await;
}

pub async fn submit_mint_nft(ctx: Context, args: args::NftMint) {
    let file = File::open(&args.nft_data).expect("File must exist.");
    let nft_tokens: Vec<NftToken> =
        serde_json::from_reader(file).expect("JSON was not well-formatted");

    let nft_creator_key = nft::get_creator_key(&args.nft_address);
    let client = HttpClient::new(args.tx.ledger_address.clone()).unwrap();
    let nft_creator_address = match rpc::query_storage_value::<Address>(
        client,
        nft_creator_key,
    )
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

    let tx_code = ctx.read_wasm(TX_MINT_NFT_TOKEN);

    let tx = Tx::new(tx_code, Some(data));

    let (ctx, tx, keypair) = sign_tx(ctx, tx, &args.tx, signer.as_ref()).await;

    process_tx(ctx, &args.tx, tx, &keypair).await;
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
    match rpc::query_storage_value::<token::Amount>(client, balance_key).await {
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
    let (ctx, tx, keypair) =
        sign_tx(ctx, tx, &args.tx, Some(default_signer)).await;
    process_tx(ctx, &args.tx, tx, &keypair).await;
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
    let bonds =
        rpc::query_storage_value::<Bonds>(client.clone(), bond_key).await;
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
    let (ctx, tx, keypair) =
        sign_tx(ctx, tx, &args.tx, Some(default_signer)).await;
    process_tx(ctx, &args.tx, tx, &keypair).await;
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
    let unbonds =
        rpc::query_storage_value::<Unbonds>(client.clone(), bond_key).await;
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
    let (ctx, tx, keypair) =
        sign_tx(ctx, tx, &args.tx, Some(default_signer)).await;
    process_tx(ctx, &args.tx, tx, &keypair).await;
}

/// Sign a transaction with a given signing key or public key of a given signer.
/// If no explicit signer given, use the `default`. If no `default` is given,
/// returns unsigned transaction.
async fn sign_tx(
    mut ctx: Context,
    tx: Tx,
    args: &args::Tx,
    default: Option<&WalletAddress>,
) -> (Context, Tx, std::rc::Rc<common::SecretKey>) {
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

    (ctx, tx, keypair)
}

/// Submit transaction and wait for result. Returns a list of addresses
/// initialized in the transaction if any. In dry run, this is always empty.
async fn process_tx(
    ctx: Context,
    args: &args::Tx,
    tx: Tx,
    keypair: &common::SecretKey,
) -> (Context, Vec<Address>) {
    // NOTE: use this to print the request JSON body:

    // let request =
    // tendermint_rpc::endpoint::broadcast::tx_commit::Request::new(
    //     tx_bytes.clone().into(),
    // );
    // use tendermint_rpc::Request;
    // let request_body = request.into_json();
    // println!("HTTP request body: {}", request_body);

    if args.dry_run {
        rpc::dry_run_tx(&args.ledger_address, tx.to_bytes()).await;
        (ctx, vec![])
    } else {
        let epoch = rpc::query_epoch(args::Query {
            ledger_address: args.ledger_address.clone(),
        })
        .await;
        let tx = WrapperTx::new(
            Fee {
                amount: args.fee_amount,
                token: ctx.get(&args.fee_token),
            },
            keypair,
            epoch,
            args.gas_limit.clone(),
            tx,
        );
        // Either broadcast or submit transaction and collect result into
        // sum type
        let result = if args.broadcast_only {
            Left(broadcast_tx(args.ledger_address.clone(), tx, keypair).await)
        } else {
            Right(submit_tx(args.ledger_address.clone(), tx, keypair).await)
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

/// Extract the wrapper transaction's hash and, if in ABCI++ mode, the inner
/// transaction's hash. Useful for determining when parts of the given
/// tranasaction make it on-chain.
pub fn tx_hashes(tx: &WrapperTx) -> (String, String) {
    let tx_hash = tx.tx_hash.to_string();
    #[cfg(not(feature = "ABCI"))]
    return (hash_tx(&tx.try_to_vec().unwrap()).to_string(), tx_hash);
    // In the case of ABCI, proceed as if inner and wrapper transaction
    // are synonymous
    #[cfg(feature = "ABCI")]
    return (tx_hash.clone(), tx_hash);
}

/// Broadcast a transaction to be included in the blockchain and checks that
/// the tx has been successfully included into the mempool of a validator
///
/// In the case of errors in any of those stages, an error message is returned
pub async fn broadcast_tx(
    address: TendermintAddress,
    tx: WrapperTx,
    keypair: &common::SecretKey,
) -> Result<Response, Error> {
    // These can later be used to determine when parts of the tx make it
    // on-chain
    let (wrapper_tx_hash, _decrypted_tx_hash) = tx_hashes(&tx);

    let mut wrapper_tx_subscription = TendermintWebsocketClient::open(
        WebSocketAddress::try_from(address.clone())?,
        None,
    )?;

    // we sign all txs
    let tx = tx
        .sign(keypair)
        .expect("Signing of the wrapper transaction should not fail");
    let tx_bytes = tx.to_bytes();

    let response = wrapper_tx_subscription
        .broadcast_tx_sync(tx_bytes.into())
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

/// Submits a transaction to the blockchain.
///
/// Checks that
/// 1. The tx has been successfully included into the mempool of a validator
/// 2. The tx with encrypted payload has been included on the blockchain
/// 3. The decrypted payload of the tx has been included on the blockchain.
///
/// In the case of errors in any of those stages, an error message is returned
pub async fn submit_tx(
    address: TendermintAddress,
    tx: WrapperTx,
    keypair: &common::SecretKey,
) -> Result<TxResponse, Error> {
    let mut wrapper_tx_subscription = TendermintWebsocketClient::open(
        WebSocketAddress::try_from(address.clone())?,
        None,
    )?;

    // We use these to determine when parts of the tx make it on-chain
    let (wrapper_tx_hash, _decrypted_tx_hash) = tx_hashes(&tx);
    // It is better to subscribe to the transaction before it is broadcast
    //
    // Note that the `applied.hash` key comes from a custom event
    // created by the shell
    #[cfg(not(feature = "ABCI"))]
    let query_key = "accepted.hash";
    #[cfg(feature = "ABCI")]
    let query_key = "applied.hash";
    let query = Query::from(EventType::NewBlock)
        .and_eq(query_key, wrapper_tx_hash.as_str());
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
            .and_eq("applied.hash", _decrypted_tx_hash.as_str());
        decrypted_tx_subscription.subscribe(query)?;
        decrypted_tx_subscription
    };
    // Broadcast the supplied transaction
    broadcast_tx(address, tx, keypair).await?;

    #[cfg(feature = "ABCI")]
    let parsed = {
        let parsed = TxResponse::find_tx(
            wrapper_tx_subscription.receive_response()?,
            wrapper_tx_hash,
        );
        println!(
            "Transaction applied with result: {}",
            serde_json::to_string_pretty(&parsed).unwrap()
        );
        Ok(parsed)
    };
    #[cfg(not(feature = "ABCI"))]
    let parsed = {
        let parsed = parse(
            wrapper_tx_subscription.receive_response()?,
            TmEventType::Accepted,
            &wrapper_tx_hash.to_string(),
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
                _decrypted_tx_hash.as_str(),
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
        height,
        hash,
        code,
        gas_used,
        initialized_accounts,
    }
}

impl TxResponse {
    pub fn find_tx(json: serde_json::Value, tx_hash: String) -> Self {
        let tx_hash_json = serde_json::Value::String(tx_hash.clone());
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
            height: serde_json::from_value(height[0].clone()).unwrap(),
            hash: serde_json::from_value(hash).unwrap(),
            code: serde_json::from_value(code[0].clone()).unwrap(),
            gas_used: serde_json::from_value(gas_used[0].clone()).unwrap(),
            initialized_accounts,
        }
    }
}
