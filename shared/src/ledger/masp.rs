//! MASP verification wrappers.

use std::{fs::File, ops::Deref, env};

use bellman::groth16::{
    generate_random_parameters, prepare_verifying_key, PreparedVerifyingKey,
};
use bls12_381::Bls12;
use masp_primitives::{
    asset_type::AssetType,
    consensus::BranchId::Sapling,
    redjubjub::PublicKey,
    transaction::{
        components::{OutputDescription, SpendDescription},
        signature_hash_data, Transaction, SIGHASH_ALL,
    },
};
use masp_proofs::{
    circuit::sapling::Spend, sapling::SaplingVerificationContext,
};
use rand::SeedableRng;
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
    let homedir = env::var("HOME").unwrap();
    let param_f = File::open(homedir + "/.zcash-params/sapling-spend.params").unwrap();
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
    sighash: &[u8; 32],
    ctx: &mut SaplingVerificationContext,
    parameters: &PreparedVerifyingKey<Bls12>,
) -> bool {
    let zkproof =
        bellman::groth16::Proof::read(spend.zkproof.as_slice()).unwrap();
    ctx.check_spend(
        spend.cv,
        spend.anchor,
        &spend.nullifier,
        // TODO: should make this clone, or just use an ExtendedPoint?
        PublicKey(spend.rk.0.clone()),
        sighash,
        spend.spend_auth_sig.unwrap(),
        zkproof,
        parameters,
    )
}

/// check_output wrapper
pub fn check_output(
    output: &OutputDescription,
    ctx: &mut SaplingVerificationContext,
    parameters: &PreparedVerifyingKey<Bls12>,
) -> bool {
    let zkproof =
        bellman::groth16::Proof::read(output.zkproof.as_slice()).unwrap();
    ctx.check_output(
        output.cv,
        output.cmu,
        output.ephemeral_key,
        zkproof,
        parameters,
    )
}

/// Verify a shielded transaction.
pub fn verify_shielded_tx(
    transaction: &Transaction,
    parameters: &PreparedVerifyingKey<Bls12>,
) -> bool {
    let mut ctx = SaplingVerificationContext::new();
    let tx_data = transaction.deref();

    let sighash: [u8; 32] =
        signature_hash_data(&tx_data, Sapling, SIGHASH_ALL, None)
            .try_into()
            .unwrap();

    let spends_valid = tx_data
        .shielded_spends
        .iter()
        .all(|spend| check_spend(spend, &sighash, &mut ctx, parameters));
    let outputs_valid = tx_data
        .shielded_outputs
        .iter()
        .all(|output| check_output(output, &mut ctx, parameters));

    if !(spends_valid && outputs_valid) {
        return false;
    }

    let assets_and_values: Vec<(AssetType, i64)> = tx_data
        .vout
        .iter()
        .map(|o| (o.asset_type, o.value.try_into().unwrap()))
        .collect();

    ctx.final_check(
        assets_and_values.as_slice(),
        &sighash,
        tx_data.binding_sig.unwrap(),
    )
}
