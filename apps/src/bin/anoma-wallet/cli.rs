//! Anoma Wallet CLI.

use std::fs::File;
use std::io::{self, Write};

use anoma::types::key::*;
use anoma::types::masp::{MaspValue, PaymentAddress};
use anoma_apps::cli;
use anoma_apps::cli::{args, cmds, Context};
use anoma_apps::client::tx::find_valid_diversifier;
use anoma_apps::wallet::{DecryptionError, FindKeyError};
use borsh::BorshSerialize;
use color_eyre::eyre::Result;
use itertools::sorted;
use masp_primitives::zip32::ExtendedFullViewingKey;
use rand_core::OsRng;

pub fn main() -> Result<()> {
    let (cmd, ctx) = cli::anoma_wallet_cli();
    match cmd {
        cmds::AnomaWallet::Key(sub) => match sub {
            cmds::WalletKey::Gen(cmds::KeyGen(args)) => {
                key_and_address_gen(ctx, args)
            }
            cmds::WalletKey::Find(cmds::KeyFind(args)) => key_find(ctx, args),
            cmds::WalletKey::List(cmds::KeyList(args)) => key_list(ctx, args),
            cmds::WalletKey::Export(cmds::Export(args)) => {
                key_export(ctx, args)
            }
        },
        cmds::AnomaWallet::Address(sub) => match sub {
            cmds::WalletAddress::Gen(cmds::AddressGen(args)) => {
                key_and_address_gen(ctx, args)
            }
            cmds::WalletAddress::Find(cmds::AddressFind(args)) => {
                address_find(ctx, args)
            }
            cmds::WalletAddress::List(cmds::AddressList) => address_list(ctx),
            cmds::WalletAddress::Add(cmds::AddressAdd(args)) => {
                address_add(ctx, args)
            }
        },
        cmds::AnomaWallet::Masp(sub) => match sub {
            cmds::WalletMasp::GenSpendKey(cmds::MaspGenSpendKey(args)) => {
                spending_key_gen(ctx, args)
            }
            cmds::WalletMasp::GenPayAddr(cmds::MaspGenPayAddr(args)) => {
                payment_address_gen(ctx, args)
            }
            cmds::WalletMasp::AddAddrKey(cmds::MaspAddAddrKey(args)) => {
                address_key_add(ctx, args)
            }
            cmds::WalletMasp::ListPayAddrs(cmds::MaspListPayAddrs) => {
                payment_addresses_list(ctx)
            }
            cmds::WalletMasp::ListKeys(cmds::MaspListKeys(args)) => {
                spending_keys_list(ctx, args)
            }
            cmds::WalletMasp::FindAddrKey(cmds::MaspFindAddrKey(args)) => {
                address_key_find(ctx, args)
            }
        },
    }
    Ok(())
}

/// Find shielded address or key
fn address_key_find(
    ctx: Context,
    args::AddrKeyFind {
        alias,
        unsafe_show_secret,
    }: args::AddrKeyFind,
) {
    let mut wallet = ctx.wallet;
    let alias = alias.to_lowercase();
    if let Ok(viewing_key) = wallet.find_viewing_key(&alias) {
        // Check if alias is a viewing key
        println!("Viewing key: {}", viewing_key);
        if unsafe_show_secret {
            // Check if alias is also a spending key
            match wallet.find_spending_key(&alias) {
                Ok(spending_key) => println!("Spending key: {}", spending_key),
                Err(FindKeyError::KeyNotFound) => {}
                Err(err) => eprintln!("{}", err),
            }
        }
    } else if let Some(payment_addr) = wallet.find_payment_addr(&alias) {
        // Failing that, check if alias is a payment address
        println!("Payment address: {}", payment_addr);
    } else {
        // Otherwise alias cannot be referring to any shielded value
        println!(
            "No shielded address or key with alias {} found. Use the commands \
             `masp list-addrs` and `masp list-keys` to see all the known \
             addresses and keys.",
            alias.to_lowercase()
        );
    }
}

/// List spending keys.
fn spending_keys_list(
    ctx: Context,
    args::MaspKeysList {
        decrypt,
        unsafe_show_secret,
    }: args::MaspKeysList,
) {
    let wallet = ctx.wallet;
    let known_view_keys = wallet.get_viewing_keys();
    let known_spend_keys = wallet.get_spending_keys();
    if known_view_keys.is_empty() {
        println!(
            "No known keys. Try `masp add --alias my-addr --value ...` to add \
             a new key to the wallet."
        );
    } else {
        let stdout = io::stdout();
        let mut w = stdout.lock();
        writeln!(w, "Known keys:").unwrap();
        for (alias, key) in known_view_keys {
            write!(w, "  Alias \"{}\"", alias).unwrap();
            let spending_key_opt = known_spend_keys.get(&alias);
            // If this alias is associated with a spending key, indicate whether
            // or not the spending key is encrypted
            if let Some(spending_key) = spending_key_opt {
                if spending_key.is_encrypted() {
                    writeln!(w, " (encrypted):")
                } else {
                    writeln!(w, " (not encrypted):")
                }
                .unwrap();
            } else {
                writeln!(w, ":").unwrap();
            }
            // Always print the corresponding viewing key
            writeln!(w, "    Viewing Key: {}", key).unwrap();
            // A subset of viewing keys will have corresponding spending keys.
            // Print those too if they are available and requested.
            if unsafe_show_secret {
                if let Some(spending_key) = spending_key_opt {
                    match spending_key.get(decrypt) {
                        // Here the spending key is unencrypted or successfully
                        // decrypted
                        Ok(spending_key) => {
                            writeln!(w, "    Spending key: {}", spending_key)
                                .unwrap();
                        }
                        // Here the key is encrypted but decryption has not been
                        // requested
                        Err(DecryptionError::NotDecrypting) if !decrypt => {
                            continue;
                        }
                        // Here the key is encrypted but incorrect password has
                        // been provided
                        Err(err) => {
                            writeln!(
                                w,
                                "    Couldn't decrypt the spending key: {}",
                                err
                            )
                            .unwrap();
                        }
                    }
                }
            }
        }
    }
}

/// List payment addresses.
fn payment_addresses_list(ctx: Context) {
    let wallet = ctx.wallet;
    let known_addresses = wallet.get_payment_addrs();
    if known_addresses.is_empty() {
        println!(
            "No known payment addresses. Try `masp gen-addr --alias my-addr` \
             to generate a new payment address."
        );
    } else {
        let stdout = io::stdout();
        let mut w = stdout.lock();
        writeln!(w, "Known payment addresses:").unwrap();
        for (alias, address) in sorted(known_addresses) {
            writeln!(w, "  \"{}\": {}", alias, address).unwrap();
        }
    }
}

/// Generate a spending key.
fn spending_key_gen(
    ctx: Context,
    args::MaspSpendKeyGen {
        alias,
        unsafe_dont_encrypt,
    }: args::MaspSpendKeyGen,
) {
    let mut wallet = ctx.wallet;
    let alias = alias.to_lowercase();
    let (alias, _key) = wallet.gen_spending_key(alias, unsafe_dont_encrypt);
    wallet.save().unwrap_or_else(|err| eprintln!("{}", err));
    println!(
        "Successfully added a spending key with alias: \"{}\"",
        alias
    );
}

/// Generate a shielded payment address from the given key.
fn payment_address_gen(
    mut ctx: Context,
    args::MaspPayAddrGen {
        alias,
        viewing_key,
        pin,
    }: args::MaspPayAddrGen,
) {
    let alias = alias.to_lowercase();
    let viewing_key =
        ExtendedFullViewingKey::from(ctx.get_cached(&viewing_key))
            .fvk
            .vk;
    let (div, _g_d) = find_valid_diversifier(&mut OsRng);
    let payment_addr = viewing_key
        .to_payment_address(div)
        .expect("a PaymentAddress");
    let mut wallet = ctx.wallet;
    let alias = wallet
        .insert_payment_addr(
            alias,
            PaymentAddress::from(payment_addr).pinned(pin),
        )
        .unwrap_or_else(|| {
            eprintln!("Payment address not added");
            cli::safe_exit(1);
        });
    wallet.save().unwrap_or_else(|err| eprintln!("{}", err));
    println!(
        "Successfully generated a payment address with the following alias: {}",
        alias,
    );
}

/// Add a viewing key, spending key, or payment address to wallet.
fn address_key_add(
    mut ctx: Context,
    args::MaspAddrKeyAdd {
        alias,
        value,
        unsafe_dont_encrypt,
    }: args::MaspAddrKeyAdd,
) {
    let alias = alias.to_lowercase();
    let (alias, typ) = match value {
        MaspValue::FullViewingKey(viewing_key) => {
            let alias = ctx
                .wallet
                .insert_viewing_key(alias, viewing_key)
                .unwrap_or_else(|| {
                    eprintln!("Viewing key not added");
                    cli::safe_exit(1);
                });
            (alias, "viewing key")
        }
        MaspValue::ExtendedSpendingKey(spending_key) => {
            let alias = ctx
                .wallet
                .encrypt_insert_spending_key(
                    alias,
                    spending_key,
                    unsafe_dont_encrypt,
                )
                .unwrap_or_else(|| {
                    eprintln!("Spending key not added");
                    cli::safe_exit(1);
                });
            (alias, "spending key")
        }
        MaspValue::PaymentAddress(payment_addr) => {
            let alias = ctx
                .wallet
                .insert_payment_addr(alias, payment_addr)
                .unwrap_or_else(|| {
                    eprintln!("Payment address not added");
                    cli::safe_exit(1);
                });
            (alias, "payment address")
        }
    };
    ctx.wallet.save().unwrap_or_else(|err| eprintln!("{}", err));
    println!(
        "Successfully added a {} with the following alias to wallet: {}",
        typ, alias,
    );
}

/// Generate a new keypair and derive implicit address from it and store them in
/// the wallet.
fn key_and_address_gen(
    ctx: Context,
    args::KeyAndAddressGen {
        alias,
        unsafe_dont_encrypt,
    }: args::KeyAndAddressGen,
) {
    let mut wallet = ctx.wallet;
    let (alias, _key) = wallet.gen_key(alias, unsafe_dont_encrypt);
    wallet.save().unwrap_or_else(|err| eprintln!("{}", err));
    println!(
        "Successfully added a key and an address with alias: \"{}\"",
        alias
    );
}

/// Find a keypair in the wallet store.
fn key_find(
    ctx: Context,
    args::KeyFind {
        public_key,
        alias,
        value,
        unsafe_show_secret,
    }: args::KeyFind,
) {
    let mut wallet = ctx.wallet;
    let found_keypair = match public_key {
        Some(pk) => wallet.find_key_by_pk(&pk),
        None => {
            let alias = alias.or(value);
            match alias {
                None => {
                    eprintln!(
                        "An alias, public key or public key hash needs to be \
                         supplied"
                    );
                    cli::safe_exit(1)
                }
                Some(alias) => wallet.find_key(alias.to_lowercase()),
            }
        }
    };
    match found_keypair {
        Ok(keypair) => {
            let pkh: PublicKeyHash = (&keypair.ref_to()).into();
            println!("Public key hash: {}", pkh);
            println!("Public key: {}", keypair.ref_to());
            if unsafe_show_secret {
                println!("Secret key: {}", keypair);
            }
        }
        Err(err) => {
            eprintln!("{}", err);
        }
    }
}

/// List all known keys.
fn key_list(
    ctx: Context,
    args::KeyList {
        decrypt,
        unsafe_show_secret,
    }: args::KeyList,
) {
    let wallet = ctx.wallet;
    let known_keys = wallet.get_keys();
    if known_keys.is_empty() {
        println!(
            "No known keys. Try `key gen --alias my-key` to generate a new \
             key."
        );
    } else {
        let stdout = io::stdout();
        let mut w = stdout.lock();
        writeln!(w, "Known keys:").unwrap();
        for (alias, (stored_keypair, pkh)) in known_keys {
            let encrypted = if stored_keypair.is_encrypted() {
                "encrypted"
            } else {
                "not encrypted"
            };
            writeln!(w, "  Alias \"{}\" ({}):", alias, encrypted).unwrap();
            if let Some(pkh) = pkh {
                writeln!(w, "    Public key hash: {}", pkh).unwrap();
            }
            match stored_keypair.get(decrypt) {
                Ok(keypair) => {
                    writeln!(w, "    Public key: {}", keypair.ref_to())
                        .unwrap();
                    if unsafe_show_secret {
                        writeln!(w, "    Secret key: {}", keypair).unwrap();
                    }
                }
                Err(DecryptionError::NotDecrypting) if !decrypt => {
                    continue;
                }
                Err(err) => {
                    writeln!(w, "    Couldn't decrypt the keypair: {}", err)
                        .unwrap();
                }
            }
        }
    }
}

/// Export a keypair to a file.
fn key_export(ctx: Context, args::KeyExport { alias }: args::KeyExport) {
    let mut wallet = ctx.wallet;
    wallet
        .find_key(alias.to_lowercase())
        .map(|keypair| {
            let file_data = keypair
                .try_to_vec()
                .expect("Encoding keypair shouldn't fail");
            let file_name = format!("key_{}", alias.to_lowercase());
            let mut file = File::create(&file_name).unwrap();

            file.write_all(file_data.as_ref()).unwrap();
            println!("Exported to file {}", file_name);
        })
        .unwrap_or_else(|err| {
            eprintln!("{}", err);
            cli::safe_exit(1)
        })
}

/// List all known addresses.
fn address_list(ctx: Context) {
    let wallet = ctx.wallet;
    let known_addresses = wallet.get_addresses();
    if known_addresses.is_empty() {
        println!(
            "No known addresses. Try `address gen --alias my-addr` to \
             generate a new implicit address."
        );
    } else {
        let stdout = io::stdout();
        let mut w = stdout.lock();
        writeln!(w, "Known addresses:").unwrap();
        for (alias, address) in sorted(known_addresses) {
            writeln!(w, "  \"{}\": {}", alias, address.to_pretty_string())
                .unwrap();
        }
    }
}

/// Find address by its alias.
fn address_find(ctx: Context, args: args::AddressFind) {
    let wallet = ctx.wallet;
    if let Some(address) = wallet.find_address(&args.alias) {
        println!("Found address {}", address.to_pretty_string());
    } else {
        println!(
            "No address with alias {} found. Use the command `address list` \
             to see all the known addresses.",
            args.alias.to_lowercase()
        );
    }
}

/// Add an address to the wallet.
fn address_add(ctx: Context, args: args::AddressAdd) {
    let mut wallet = ctx.wallet;
    if wallet
        .add_address(args.alias.clone().to_lowercase(), args.address)
        .is_none()
    {
        eprintln!("Address not added");
        cli::safe_exit(1);
    }
    wallet.save().unwrap_or_else(|err| eprintln!("{}", err));
    println!(
        "Successfully added a key and an address with alias: \"{}\"",
        args.alias.to_lowercase()
    );
}
