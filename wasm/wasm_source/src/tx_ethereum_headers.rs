//! A tx for submitting an Ethereum header signed off on by validators

use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
    let tx = Tx::try_from(tx_data.unwrap()).unwrap();
    let EthereumHeaderUpdate {
        header,
        seen_by,
        voting_power,
        seen
    } = BorshDeserialize::deserialize(&mut tx.data.as_slice())
        .unwrap();
    let hash = header.hash();
    let header_key = get_header_key(&hash);
    let seen_by_key = get_seen_by_key(&hash);
    let voting_power_key = get_voting_power(&hash);
    let seen_key = get_seen_key(&hash);
    tx::write(header_key, header);
    tx::write(seen_by_key, seen_by);
    tx::write(voting_power_key, voting_power);
    tx::write(seen_key, seen);
    log_string("apply_tx called to update an Ethereum header");
}