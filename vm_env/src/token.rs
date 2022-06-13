use std::collections::BTreeSet;

use anoma::types::address::{masp, Address, InternalAddress};
use anoma::types::storage::{Key, KeySeg};
use anoma::types::token;
use masp_primitives::transaction::{Transaction, TxId};

/// Vp imports and functions.
pub mod vp {
    pub use anoma::types::token::*;

    use super::*;
    use crate::imports::vp;

    /// A token validity predicate.
    pub fn vp(
        token: &Address,
        keys_changed: &BTreeSet<Key>,
        verifiers: &BTreeSet<Address>,
    ) -> bool {
        let mut change: Change = 0;
        let all_checked = keys_changed.iter().all(|key| {
            match token::is_balance_key(token, key) {
                None => {
                    // deny any other keys
                    false
                }
                Some(owner) => {
                    // accumulate the change
                    let key = key.to_string();
                    let pre: Amount = match owner {
                        Address::Internal(InternalAddress::IbcMint) => {
                            Amount::max()
                        }
                        Address::Internal(InternalAddress::IbcBurn) => {
                            Amount::default()
                        }
                        _ => vp::read_pre(&key).unwrap_or_default(),
                    };
                    let post: Amount = match owner {
                        Address::Internal(
                            InternalAddress::IbcMint | InternalAddress::IbcBurn,
                        ) => vp::read_temp(&key).unwrap_or_default(),
                        _ => vp::read_post(&key).unwrap_or_default(),
                    };
                    let this_change = post.change() - pre.change();
                    change += this_change;
                    // make sure that the spender approved the transaction
                    if this_change < 0 {
                        return verifiers.contains(owner) || *owner == masp();
                    }
                    true
                }
            }
        });
        all_checked && change == 0
    }
}

/// Tx imports and functions.
pub mod tx {
    pub use anoma::types::token::*;

    use super::*;
    use crate::imports::tx;

    /// A token transfer that can be used in a transaction.
    pub fn transfer(
        src: &Address,
        dest: &Address,
        token: &Address,
        amount: Amount,
        key: &Option<String>,
        shielded: &Option<Transaction>,
    ) {
        let src_key = token::balance_key(token, src);
        let dest_key = token::balance_key(token, dest);
        let src_bal: Option<Amount> = tx::read(&src_key.to_string());
        let mut src_bal = src_bal.unwrap_or_else(|| match src {
            Address::Internal(InternalAddress::IbcMint) => Amount::max(),
            _ => {
                tx::log_string(format!("src {} has no balance", src));
                unreachable!()
            }
        });
        let mut dest_bal: Amount =
            tx::read(&dest_key.to_string()).unwrap_or_default();
        // Only make changes to transparent balances if asset is not being
        // transferred to self
        if src != dest {
            src_bal.spend(&amount);
            dest_bal.receive(&amount);
            match src {
                Address::Internal(InternalAddress::IbcMint) => {
                    tx::write_temp(&src_key.to_string(), src_bal)
                }
                Address::Internal(InternalAddress::IbcBurn) => {
                    tx::log_string("invalid transfer from the burn address");
                    unreachable!()
                }
                _ => tx::write(&src_key.to_string(), src_bal),
            }
            match dest {
                Address::Internal(InternalAddress::IbcMint) => {
                    tx::log_string("invalid transfer to the mint address");
                    unreachable!()
                }
                Address::Internal(InternalAddress::IbcBurn) => {
                    tx::write_temp(&dest_key.to_string(), dest_bal)
                }
                _ => tx::write(&dest_key.to_string(), dest_bal),
            }
        }
        // If this transaction has a shielded component, then handle it
        // separately
        if let Some(shielded) = shielded {
            let masp_addr = masp();
            tx::insert_verifier(&masp_addr);
            let head_tx_key = Key::from(masp_addr.to_db_key())
                .push(&HEAD_TX_KEY.to_owned())
                .expect("Cannot obtain a storage key");
            let prev_tx_id: Option<TxId> = tx::read(&head_tx_key.to_string());
            let new_tx_key = Key::from(masp_addr.to_db_key())
                .push(
                    &(TX_KEY_PREFIX.to_owned() + &shielded.txid().to_string()),
                )
                .expect("Cannot obtain a storage key");
            tx::write(&new_tx_key.to_string(), (shielded, prev_tx_id));
            tx::write(&head_tx_key.to_string(), shielded.txid());
            // If storage key has been supplied, then pin this transaction to it
            if let Some(key) = key {
                let pin_key = Key::from(masp_addr.to_db_key())
                    .push(&(PIN_KEY_PREFIX.to_owned() + key))
                    .expect("Cannot obtain a storage key");
                tx::write(
                    &pin_key.to_string(),
                    (shielded.txid(), tx::get_block_epoch()),
                );
            }
        }
    }
}
