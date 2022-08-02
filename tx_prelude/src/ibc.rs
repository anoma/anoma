//! IBC lower-level functions for transactions.

pub use anoma::ledger::ibc::handler::{Error, IbcActions, Result};
use anoma::ledger::read::StorageRead;
use anoma::ledger::tx_env::TxEnv;
use anoma::types::address::Address;
pub use anoma::types::ibc::IbcEvent;
use anoma::types::storage::{BlockHeight, Key};
use anoma::types::time::Rfc3339String;
use anoma::types::token::Amount;

use crate::token::transfer;
use crate::Ctx;

// This is needed to use `ibc::Handler::Error` with `IbcActions` below
impl From<Error> for crate::Error {
    fn from(err: Error) -> Self {
        crate::Error::new(err)
    }
}

impl IbcActions for Ctx {
    type Error = crate::Error;

    fn read_ibc_data(
        &self,
        key: &Key,
    ) -> std::result::Result<Option<Vec<u8>>, Self::Error> {
        let data = self.read_bytes(key)?;
        Ok(data)
    }

    fn write_ibc_data(
        &mut self,
        key: &Key,
        data: impl AsRef<[u8]>,
    ) -> std::result::Result<(), Self::Error> {
        self.write_bytes(key, data)?;
        Ok(())
    }

    fn delete_ibc_data(
        &mut self,
        key: &Key,
    ) -> std::result::Result<(), Self::Error> {
        self.delete(key)?;
        Ok(())
    }

    fn emit_ibc_event(
        &mut self,
        event: IbcEvent,
    ) -> std::result::Result<(), Self::Error> {
        <Ctx as TxEnv>::emit_ibc_event(self, &event)?;
        Ok(())
    }

    fn transfer_token(
        &mut self,
        src: &Address,
        dest: &Address,
        token: &Address,
        amount: Amount,
    ) -> std::result::Result<(), Self::Error> {
        transfer(self, src, dest, token, amount)?;
        Ok(())
    }

    fn get_height(&self) -> std::result::Result<BlockHeight, Self::Error> {
        let val = self.get_block_height()?;
        Ok(val)
    }

    fn get_header_time(
        &self,
    ) -> std::result::Result<Rfc3339String, Self::Error> {
        let val = self.get_block_time()?;
        Ok(val)
    }
}
