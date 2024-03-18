use cairo_platinum_prover::{
    air::{generate_cairo_proof, verify_cairo_proof, PublicInputs},
    cairo_layout::CairoLayout,
    cairo_mem::CairoMemory,
    register_states::RegisterStates,
    execution_trace::build_main_trace,
    runner::run::generate_prover_args,
};
use stark_platinum_prover::proof::options::{ProofOptions, SecurityLevel};

#[rustler::nif(schedule = "DirtyCpu")]
fn cairo0_run_and_prove(program_content: String) -> (Vec<u8>, Vec<u8>) {
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
fn cairo_prove(trace: Vec<u8>, memory: Vec<u8>) -> (Vec<u8>, Vec<u8>) {
    // Generating the prover args
    let register_states = RegisterStates::from_bytes_le(&trace).unwrap();
    let memory = CairoMemory::from_bytes_le(&memory).unwrap();

    // data length
    let data_len = 0_usize;
    let mut pub_inputs = PublicInputs::from_regs_and_mem(&register_states, &memory, data_len);
    let main_trace = build_main_trace(&register_states, &memory, &mut pub_inputs);

    // Generating proof
    let proof_options = ProofOptions::new_secure(SecurityLevel::Conjecturable100Bits, 3);
    let proof = generate_cairo_proof(&main_trace, &pub_inputs, &proof_options).unwrap();

    // Encode proof and pub_inputs
    let proof_bytes = bincode::serde::encode_to_vec(proof, bincode::config::standard()).unwrap();
    let pub_input_bytes =
        bincode::serde::encode_to_vec(&pub_inputs, bincode::config::standard()).unwrap();

    (proof_bytes, pub_input_bytes)
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

rustler::init!("Elixir.Anoma.Cairo", [cairo0_run_and_prove, cairo_prove, cairo_verify]);
