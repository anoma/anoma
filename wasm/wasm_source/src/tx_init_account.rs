//! A tx to initialize a new established address with a given public key and
//! a validity predicate.

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let signed = SignedTxData::try_from_slice(&tx_data[..]).unwrap();
    let tx_data =
        transaction::InitAccount::try_from_slice(&signed.data.unwrap()[..])
            .unwrap();
    debug_log!("apply_tx called to init a new established account");

    let address = init_account(&tx_data.vp_code);
    let pk_key = key::pk_key(&address);
    write(&pk_key.to_string(), &tx_data.public_key);
}
