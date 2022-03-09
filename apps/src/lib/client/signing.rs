//! Helpers for making digital signatures using cryptographic keys from the
//! wallet.

use std::rc::Rc;

use anoma::ledger::rpc;
use anoma::types::address::{Address, ImplicitAddress};
use anoma::types::key::*;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::net::Address as TendermintAddress;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::HttpClient;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::HttpClient;

use crate::cli;
use crate::wallet::Wallet;

/// Find the public key for the given address and try to load the keypair
/// for it from the wallet. Panics if the key cannot be found or loaded.
pub async fn find_keypair(
    wallet: &mut Wallet,
    addr: &Address,
    ledger_address: TendermintAddress,
) -> Rc<common::SecretKey> {
    let client = HttpClient::new(ledger_address).unwrap();
    match addr {
        Address::Established(_) => {
            println!(
                "Looking-up public key of {} from the ledger...",
                addr.encode()
            );
            let pk = rpc::get_public_key(client.clone(), addr).await.unwrap();
            let public_key = pk.unwrap_or_else(|| {
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
        Address::Implicit(ImplicitAddress(pkh)) => {
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
