//! Helpers for making digital signatures using cryptographic keys from the
//! wallet.

use std::rc::Rc;

use anoma::types::address::{Address, ImplicitAddress};
use anoma::types::key::ed25519::Keypair;

use super::rpc;
use crate::cli;
use crate::wallet::Wallet;

/// Find the public key for the given address and try to load the keypair
/// for it from the wallet. Panics if the key cannot be found or loaded.
pub async fn find_keypair(
    wallet: &mut Wallet,
    addr: &Address,
    ledger_address: tendermint::net::Address,
) -> Rc<Keypair> {
    match addr {
        Address::Established(_) => {
            println!(
                "Looking-up public key of {} from the ledger...",
                addr.encode()
            );
            let public_key = rpc::get_public_key(addr, ledger_address)
                .await
                .unwrap_or_else(|| {
                    eprintln!(
                        "No public key found for the address {}",
                        addr.encode()
                    );
                    cli::safe_exit(1);
                });
            wallet.find_key_by_pk(&public_key).unwrap_or_else(|err| {
                eprintln!(
                    "Unable to load the keypair from the wallet for public \
                     key {}. Failed with: {}",
                    public_key, err
                );
                cli::safe_exit(1)
            })
        }
        Address::Implicit(ImplicitAddress::Ed25519(pkh)) => {
            wallet.find_key_by_pkh(pkh).unwrap_or_else(|err| {
                eprintln!(
                    "Unable to load the keypair from the wallet for the \
                     implicit address {}. Failed with: {}",
                    addr.encode(),
                    err
                );
                cli::safe_exit(1)
            })
        }
        Address::Internal(_) => {
            eprintln!(
                "Internal address {} doesn't have any signing keys.",
                addr
            );
            cli::safe_exit(1)
        }
    }
}
