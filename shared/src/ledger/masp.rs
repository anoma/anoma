//! MASP verification wrappers.

use std::fs::File;

use crate::types::masp::*;
use bellman::groth16::{generate_random_parameters, prepare_verifying_key};
use masp_proofs::circuit::sapling::Spend;
use rand_new::SeedableRng;
use rand_xorshift::XorShiftRng;

/// Very bad test groth16 parameters
// XXX sadly, can't be a const; going to delete it anyway eventually
pub fn bad_groth_params() -> (
    bellman::groth16::Parameters<Bls12>,
    bellman::groth16::PreparedVerifyingKey<Bls12>,
) {
    let rng = &mut XorShiftRng::from_seed([
        0x59, 0x62, 0xbe, 0x3d, 0x76, 0x3d, 0x31, 0x8d, 0x17, 0xdb, 0x37, 0x32,
        0x54, 0x06, 0xbc, 0xe5,
    ]);
    let params = generate_random_parameters::<Bls12, Spend, XorShiftRng>(
        Spend {
            value_commitment: None,
            proof_generation_key: None,
            payment_address: None,
            commitment_randomness: None,
            ar: None,
            auth_path: vec![None; 32],
            anchor: None,
        },
        rng,
    )
    .unwrap();
    let vk = prepare_verifying_key(&params.vk);
    (params, vk)
}

/// Load some bad groth params.
pub fn load_groth_params() -> (
    bellman::groth16::Parameters<Bls12>,
    bellman::groth16::PreparedVerifyingKey<Bls12>,
) {
    let param_f = File::open("params.bin").unwrap();
    let params = bellman::groth16::Parameters::read(&param_f, false).unwrap();
    let vk = prepare_verifying_key(&params.vk);
    (params, vk)
}

/// Inflict some bad groth params on the world.
pub fn dump_groth_params() {
    let (params, _vk) = bad_groth_params();
    let mut param_f = File::create("params.bin").unwrap();
    params.write(&mut param_f).unwrap();
}

/// check_spend wrapper
pub fn check_spend(
    spend: &SpendDescription,
    ctx: &mut SaplingVerificationContext,
    parameters: &PreparedVerifyingKey<Bls12>,
) -> bool {
    ctx.check_spend(
        spend.cv,
        spend.anchor,
        &spend.nullifier,
        // TODO: should make this clone, or just use an ExtendedPoint?
        PublicKey(spend.rk.0.clone()),
        &spend.sighash_value,
        spend.spend_auth_sig,
        spend.zkproof.clone(),
        parameters,
    )
}

/// check_output wrapper
pub fn check_output(
    output: &OutputDescription,
    ctx: &mut SaplingVerificationContext,
    parameters: &PreparedVerifyingKey<Bls12>,
) -> bool {
    ctx.check_output(
        output.cv,
        output.cmu,
        output.epk,
        output.zkproof.clone(),
        parameters,
    )
}

/// Verify a shielded transaction.
pub fn verify_shielded_tx(
    transaction: &ShieldedTransaction,
    parameters: &PreparedVerifyingKey<Bls12>,
) -> bool {
    let mut ctx = SaplingVerificationContext::new();

    let spends_valid = transaction
        .spends
        .iter()
        .all(|spend| check_spend(spend, &mut ctx, parameters));
    let outputs_valid = transaction
        .outputs
        .iter()
        .all(|output| check_output(output, &mut ctx, parameters));

    if !(spends_valid && outputs_valid) {
        return false;
    }

    ctx.final_check(
        &transaction.assets_and_values,
        &transaction.sighash_value,
        transaction.binding_sig,
    )
}
