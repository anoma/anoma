use cairo_platinum_prover::air::{generate_cairo_proof, verify_cairo_proof};
use cairo_platinum_prover::cairo_layout::CairoLayout;
use cairo_platinum_prover::runner::run::generate_prover_args;
use stark_platinum_prover::proof::options::{ProofOptions, SecurityLevel};

#[rustler::nif]
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

#[rustler::nif]
fn cairo_verify(proof: Vec<u8>, public_input: Vec<u8>) -> bool {
    let proof_options = ProofOptions::new_secure(SecurityLevel::Conjecturable100Bits, 3);

    let (proof, _) =
        bincode::serde::decode_from_slice(&proof, bincode::config::standard()).unwrap();

    let (pub_inputs, _) =
        bincode::serde::decode_from_slice(&public_input, bincode::config::standard()).unwrap();

    verify_cairo_proof(&proof, &pub_inputs, &proof_options)
}

rustler::init!(
    "Elixir.Anoma.Cairo",
    [cairo_run_and_prove, cairo_verify]
);
