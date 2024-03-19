use starknet_crypto::{poseidon_hash, poseidon_hash_single, poseidon_hash_many};
use starknet_ff::FieldElement;

#[rustler::nif(schedule = "DirtyCpu")]
fn poseidon_single(x: Vec<u8>) -> Vec<u8>{
    let mut padded_x = x;
    padded_x.resize(32, 0);
    let x_bytes: [u8; 32] = padded_x.as_slice().try_into().expect("Slice with incorrect length");
    let x_field = FieldElement::from_bytes_be(&x_bytes).unwrap();
    poseidon_hash_single(x_field).to_bytes_be().to_vec()
}

#[rustler::nif(schedule = "DirtyCpu")]
fn poseidon(x: Vec<u8>, y: Vec<u8>) -> Vec<u8>{
    let x_bytes: [u8; 32] = x.as_slice().try_into().expect("Slice with incorrect length");
    let x_field = FieldElement::from_bytes_be(&x_bytes).unwrap();
    let y_bytes: [u8; 32] = y.as_slice().try_into().expect("Slice with incorrect length");
    let y_field = FieldElement::from_bytes_be(&y_bytes).unwrap();
    poseidon_hash(x_field, y_field).to_bytes_be().to_vec()
}

#[rustler::nif(schedule = "DirtyCpu")]
fn poseidon_many(inputs: Vec<Vec<u8>>) -> Vec<u8>{
    let mut vec_fe = Vec::new();
    for i in inputs {
        let i_bytes: [u8; 32] = i.as_slice().try_into().expect("Slice with incorrect length");
        vec_fe.push(FieldElement::from_bytes_be(&i_bytes).unwrap())
    };
    let result_fe = poseidon_hash_many(&vec_fe);
    result_fe.to_bytes_be().to_vec()
}

rustler::init!("Elixir.Anoma.StarknetCrypto", [poseidon_single, poseidon, poseidon_many]);
