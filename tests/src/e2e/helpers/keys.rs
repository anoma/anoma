use anoma::types::key::{common, ed25519, SecretKey, SigScheme};
use rand::prelude::ThreadRng;

pub fn random_secret_key() -> common::SecretKey {
    let mut rng: ThreadRng = rand::thread_rng();
    ed25519::SigScheme::generate(&mut rng).try_to_sk().unwrap()
}
