//! A tx for a PoS bond that stakes tokens via a self-bond or delegation.

use anoma_tx_prelude::proof_of_stake::bond_tokens;
use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let bond =
        transaction::pos::Bond::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();

    if let Err(err) =
        bond_tokens(bond.source.as_ref(), &bond.validator, bond.amount)
    {
        debug_log!("Bond failed with: {}", err);
        panic!()
    }
}
