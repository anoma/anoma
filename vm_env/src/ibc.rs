//! IBC functions for transactions.

pub use anoma::ledger::ibc::handler::IbcActions;
use anoma::types::address::Address;
use anoma::types::ibc::IbcEvent;
use anoma::types::storage::{BlockHeight, Key};
use anoma::types::time::Rfc3339String;
use anoma::types::token::Amount;

use crate::imports::tx;
use crate::token::tx::transfer;

/// This struct integrates and gives access to lower-level IBC functions.
pub struct Ibc;

impl IbcActions for Ibc {
    fn read_ibc_data(&self, key: &Key) -> Option<Vec<u8>> {
        tx::read_bytes(key.to_string())
    }

    fn write_ibc_data(&self, key: &Key, data: impl AsRef<[u8]>) {
        tx::write_bytes(key.to_string(), data)
    }

    fn delete_ibc_data(&self, key: &Key) {
        tx::delete(key.to_string())
    }

    fn emit_ibc_event(&self, event: IbcEvent) {
        tx::emit_ibc_event(&event)
    }

    fn transfer_token(
        &self,
        src: &Address,
        dest: &Address,
        token: &Address,
        amount: Amount,
    ) {
        transfer(src, dest, token, amount, &None)
    }

    fn get_height(&self) -> BlockHeight {
        tx::get_block_height()
    }

    fn get_header_time(&self) -> Rfc3339String {
        tx::get_block_time()
    }
}
