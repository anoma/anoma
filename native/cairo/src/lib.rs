use cairo_platinum_prover::air::{generate_cairo_proof, verify_cairo_proof};
use cairo_platinum_prover::cairo_layout::CairoLayout;
use cairo_platinum_prover::runner::run::generate_prover_args;
use stark_platinum_prover::proof::options::{ProofOptions, SecurityLevel};
use starknet_crypto::{poseidon_hash, poseidon_hash_single, poseidon_hash_many};
use starknet_ff::FieldElement;

#[rustler::nif(schedule = "DirtyCpu")]
fn cairo_run_and_prove(program_content: String) -> (Vec<u8>, Vec<u8>) {
    let (main_trace, pub_inputs) =
        generate_prover_args(program_content.as_bytes(), CairoLayout::Plain).unwrap();

    let proof_options = ProofOptions::new_secure(SecurityLevel::Conjecturable100Bits, 3);
    let proof = generate_cairo_proof(&main_trace, &pub_inputs, &proof_options).unwrap();

    let proof_bytes = bincode::serde::encode_to_vec(proof, bincode::config::standard()).unwrap();
    let pub_inputs =
        bincode::serde::encode_to_vec(&pub_inputs, bincode::config::standard()).unwrap();

    (proof_bytes, pub_inputs)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn cairo_verify(proof: Vec<u8>, public_input: Vec<u8>) -> bool {
    let proof_options = ProofOptions::new_secure(SecurityLevel::Conjecturable100Bits, 3);

    let (proof, _) =
        bincode::serde::decode_from_slice(&proof, bincode::config::standard()).unwrap();

    let (pub_inputs, _) =
        bincode::serde::decode_from_slice(&public_input, bincode::config::standard()).unwrap();

    verify_cairo_proof(&proof, &pub_inputs, &proof_options)
}

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

rustler::init!(
    "Elixir.Anoma.Cairo",
    [cairo_run_and_prove, cairo_verify, poseidon_single, poseidon, poseidon_many]
);
