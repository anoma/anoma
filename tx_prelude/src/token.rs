use anoma::types::address::{Address, InternalAddress};
use anoma::types::token;
pub use anoma::types::token::*;

use super::*;

/// A token transfer that can be used in a transaction.
pub fn transfer(
    ctx: &mut Ctx,
    src: &Address,
    dest: &Address,
    token: &Address,
    amount: Amount,
) -> TxResult {
    let src_key = token::balance_key(token, src);
    let dest_key = token::balance_key(token, dest);
    let src_bal: Option<Amount> = ctx.read(&src_key)?;
    let mut src_bal = src_bal.unwrap_or_else(|| match src {
        Address::Internal(InternalAddress::IbcMint) => Amount::max(),
        _ => {
            log_string(format!("src {} has no balance", src));
            unreachable!()
        }
    });
    src_bal.spend(&amount);
    let mut dest_bal: Amount = ctx.read(&dest_key)?.unwrap_or_default();
    dest_bal.receive(&amount);
    match src {
        Address::Internal(InternalAddress::IbcMint) => {
            ctx.write_temp(&src_key, src_bal)?;
        }
        Address::Internal(InternalAddress::IbcBurn) => {
            log_string("invalid transfer from the burn address");
            unreachable!()
        }
        _ => {
            ctx.write(&src_key, src_bal)?;
        }
    }
    match dest {
        Address::Internal(InternalAddress::IbcMint) => {
            log_string("invalid transfer to the mint address");
            unreachable!()
        }
        Address::Internal(InternalAddress::IbcBurn) => {
            ctx.write_temp(&dest_key, dest_bal)?;
        }
        _ => {
            ctx.write(&dest_key, dest_bal)?;
        }
    }
    Ok(())
}
