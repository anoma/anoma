extern crate core;

use taiga_halo2::{self, note::Note, nullifier::Nullifier};

use rand::rngs::OsRng;
use taiga_halo2::proof::Proof;

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
        note_random_output
    ]
);
