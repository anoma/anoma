extern crate core;
use taiga_halo2::{self, note::Note, nullifier::Nullifier};

use rand::rngs::OsRng;
use taiga_halo2::circuit::vp_examples::TrivialValidityPredicateCircuit;
use taiga_halo2::constant::TAIGA_COMMITMENT_TREE_DEPTH;
use taiga_halo2::merkle_tree::MerklePath;
use taiga_halo2::note::{InputNoteProvingInfo, NoteCommitment, OutputNoteProvingInfo};
use taiga_halo2::proof::Proof;
use taiga_halo2::shielded_ptx::ShieldedPartialTransaction;

fn trivial_to_output(
    trivial: &TrivialValidityPredicateCircuit,
    note: &Note,
) -> OutputNoteProvingInfo {
    OutputNoteProvingInfo::new(*note, Box::new(trivial.clone()), vec![])
}

fn trivial_to_input(
    trivial: &TrivialValidityPredicateCircuit,
    note: &Note,
) -> InputNoteProvingInfo {
    let mut rng = OsRng;

    let merkle_path = MerklePath::random(&mut rng, TAIGA_COMMITMENT_TREE_DEPTH);

    InputNoteProvingInfo::new(*note, merkle_path, Box::new(trivial.clone()), vec![])
}

// On the Elixir side, abtrasct out the need for the Note, as we can
// simply find it, with some basic logic
#[rustler::nif(schedule = "DirtyCpu")]
fn build_transaction(
    inputs: Vec<(TrivialValidityPredicateCircuit, Note)>,
    outputs: Vec<(TrivialValidityPredicateCircuit, Note)>,
) -> Option<ShieldedPartialTransaction> {
    let rng = OsRng;
    let vec_inputs: Vec<_> = inputs
        .iter()
        .map(|(vp, n)| trivial_to_input(vp, n))
        .collect();
    let new_inputs = vec_inputs.try_into().ok()?;
    let vec_outputs: Vec<_> = outputs
        .iter()
        .map(|(vp, n)| trivial_to_output(vp, n))
        .collect();
    let new_outputs = vec_outputs.try_into().ok()?;
    let (ptx, _) = ShieldedPartialTransaction::build(new_inputs, new_outputs, rng);
    Some(ptx)
}

#[rustler::nif]
fn proof_new(bytes: Vec<u8>) -> Proof {
    Proof::new(bytes)
}

#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif]
fn note_to_rust(note: Note) -> Note {
    note
}

#[rustler::nif(schedule = "DirtyCpu")]
fn note_commitment(note: Note) -> NoteCommitment {
    note.commitment()
}

#[rustler::nif]
fn note_nullifier(note: Note) -> Option<Nullifier> {
    note.get_nf()
}

#[rustler::nif]
fn commitment_x(nc: NoteCommitment) -> pasta_curves::pallas::Base {
    nc.get_x()
}

#[rustler::nif]
fn note_random_input() -> Note {
    Note::random_padding_input_note(OsRng)
}

#[rustler::nif]
fn note_random_output() -> Note {
    Note::random_padding_output_note(OsRng, Nullifier::default())
}

rustler::init!(
    "Elixir.Taiga.Native",
    [
        add,
        proof_new,
        note_to_rust,
        note_random_input,
        note_random_output,
        note_commitment,
        note_nullifier,
        commitment_x,
        build_transaction
    ]
);
