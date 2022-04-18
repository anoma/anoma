//! MASP verification wrappers.

use std::{fs::File, ops::Deref};
use std::path::Path;

use bellman::groth16::{prepare_verifying_key, PreparedVerifyingKey};
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
use masp_proofs::sapling::SaplingVerificationContext;

/// Load Sapling params.
pub fn load_params(path: impl AsRef<Path>) -> (
    bellman::groth16::Parameters<Bls12>,
    bellman::groth16::PreparedVerifyingKey<Bls12>,
) {
    let param_f =
        File::open(path).unwrap();
    let params = bellman::groth16::Parameters::read(&param_f, false).unwrap();
    let vk = prepare_verifying_key(&params.vk);
    (params, vk)
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
pub fn verify_shielded_tx(transaction: &Transaction) -> bool {
    tracing::info!("entered verify_shielded_tx()");

    let mut ctx = SaplingVerificationContext::new();
    let tx_data = transaction.deref();

    let params_dir = crate::masp::ParamsDirectory(masp_proofs::default_params_folder().unwrap());
    let (_, spend_pvk) = load_params(params_dir.spend_path());
    let (_, output_pvk) = load_params(params_dir.output_path());

    let sighash: [u8; 32] =
        signature_hash_data(&tx_data, Sapling, SIGHASH_ALL, None)
            .try_into()
            .unwrap();

    tracing::info!("sighash computed");

    let spends_valid = tx_data
        .shielded_spends
        .iter()
        .all(|spend| check_spend(spend, &sighash, &mut ctx, &spend_pvk));
    let outputs_valid = tx_data
        .shielded_outputs
        .iter()
        .all(|output| check_output(output, &mut ctx, &output_pvk));

    if !(spends_valid && outputs_valid) {
        return false;
    }

    tracing::info!("passed spend/output verification");

    let assets_and_values: Vec<(AssetType, i64)> = tx_data
        .vout
        .iter()
        .map(|o| (o.asset_type, o.value.try_into().unwrap()))
        .collect();

    tracing::info!("accumulated {} assets/values", assets_and_values.len());

    /*
    ctx.final_check(
        assets_and_values.as_slice(),
        &sighash,
        tx_data.binding_sig.unwrap(),
    )
    */
    true
}
