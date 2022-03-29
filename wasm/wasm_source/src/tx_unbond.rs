//! A tx for a PoS unbond that removes staked tokens from a self-bond or a
//! delegation to be withdrawn in or after unbonding epoch.

use anoma_tx_prelude::proof_of_stake::unbond_tokens;
use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let unbond =
        transaction::pos::Unbond::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();

    if let Err(err) =
        unbond_tokens(unbond.source.as_ref(), &unbond.validator, unbond.amount)
    {
        debug_log!("Unbonding failed with: {}", err);
        panic!()
    }
}
