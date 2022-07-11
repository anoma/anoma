//! The ledger's protocol
use std::collections::BTreeSet;
use std::panic;

use anoma::ledger::eth_bridge::vp::EthBridge;
use anoma::ledger::gas::{self, BlockGasMeter, VpGasMeter};
use anoma::ledger::governance::GovernanceVp;
use anoma::ledger::ibc::vp::{Ibc, IbcToken};
use anoma::ledger::native_vp::{self, NativeVp};
use anoma::ledger::parameters::{self, ParametersVp};
use anoma::ledger::pos::{self, PosVP};
use anoma::ledger::storage::write_log::WriteLog;
use anoma::ledger::storage::{DBIter, Storage, StorageHasher, DB};
use anoma::ledger::treasury::TreasuryVp;
use anoma::proto::{self, Tx};
use anoma::types::address::{Address, InternalAddress};
use anoma::types::storage::TxIndex;
use anoma::types::storage;
use anoma::types::transaction::{DecryptedTx, TxResult, TxType, VpsResult};
use anoma::vm::wasm::{TxCache, VpCache};
use anoma::vm::{self, wasm, WasmCacheAccess};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Storage error: {0}")]
    StorageError(anoma::ledger::storage::Error),
    #[error("Error decoding a transaction from bytes: {0}")]
    TxDecodingError(proto::Error),
    #[error("Transaction runner error: {0}")]
    TxRunnerError(vm::wasm::run::Error),
    #[error("Txs must either be encrypted or a decryption of an encrypted tx")]
    TxTypeError,
    #[error("Gas error: {0}")]
    GasError(gas::Error),
    #[error("Error executing VP for addresses: {0:?}")]
    VpRunnerError(vm::wasm::run::Error),
    #[error("The address {0} doesn't exist")]
    MissingAddress(Address),
    #[error("IBC native VP: {0}")]
    IbcNativeVpError(anoma::ledger::ibc::vp::Error),
    #[error("PoS native VP: {0}")]
    PosNativeVpError(pos::vp::Error),
    #[error("PoS native VP panicked")]
    PosNativeVpRuntime,
    #[error("Parameters native VP: {0}")]
    ParametersNativeVpError(parameters::Error),
    #[error("IBC Token native VP: {0}")]
    IbcTokenNativeVpError(anoma::ledger::ibc::vp::IbcTokenError),
    #[error("Governance native VP error: {0}")]
    GovernanceNativeVpError(anoma::ledger::governance::vp::Error),
    #[error("Treasury native VP error: {0}")]
    TreasuryNativeVpError(anoma::ledger::treasury::Error),
    #[error("Ethereum bridge native VP error: {0}")]
    EthBridgeNativeVpError(anoma::ledger::eth_bridge::vp::Error),
    #[error("Access to an internal address {0} is forbidden")]
    AccessForbidden(InternalAddress),
}

pub type Result<T> = std::result::Result<T, Error>;

/// Apply a given transaction
///
/// The only Tx Types that should be input here are `Decrypted` and `Wrapper`
///
/// If the given tx is a successfully decrypted payload apply the necessary
/// vps. Otherwise, we include the tx on chain with the gas charge added
/// but no further validations.
#[allow(clippy::too_many_arguments)]
pub fn apply_tx<D, H, CA>(
    tx: TxType,
    tx_length: usize,
    tx_index: TxIndex,
    block_gas_meter: &mut BlockGasMeter,
    write_log: &mut WriteLog,
    storage: &Storage<D, H>,
    vp_wasm_cache: &mut VpCache<CA>,
    tx_wasm_cache: &mut TxCache<CA>,
) -> Result<TxResult>
where
    D: 'static + DB + for<'iter> DBIter<'iter> + Sync,
    H: 'static + StorageHasher + Sync,
    CA: 'static + WasmCacheAccess + Sync,
{
    // Base gas cost for applying the tx
    block_gas_meter
        .add_base_transaction_fee(tx_length)
        .map_err(Error::GasError)?;
    match tx {
        TxType::Raw(_) => Err(Error::TxTypeError),
        TxType::Decrypted(DecryptedTx::Decrypted(tx)) => {
            let verifiers = execute_tx(
                &tx,
                &tx_index,
                storage,
                block_gas_meter,
                write_log,
                vp_wasm_cache,
                tx_wasm_cache,
            )?;

            let vps_result = check_vps(
                &tx,
                &tx_index,
                storage,
                block_gas_meter,
                write_log,
                &verifiers,
                vp_wasm_cache,
            )?;

            let gas_used = block_gas_meter
                .finalize_transaction()
                .map_err(Error::GasError)?;
            let initialized_accounts = write_log.get_initialized_accounts();
            let changed_keys = write_log.get_keys();
            let ibc_event = write_log.take_ibc_event();

            Ok(TxResult {
                gas_used,
                changed_keys,
                vps_result,
                initialized_accounts,
                ibc_event,
            })
        }
        _ => {
            let gas_used = block_gas_meter
                .finalize_transaction()
                .map_err(Error::GasError)?;
            Ok(TxResult {
                gas_used,
                ..Default::default()
            })
        }
    }
}

/// Execute a transaction code. Returns verifiers requested by the transaction.
fn execute_tx<D, H, CA>(
    tx: &Tx,
    tx_index: &TxIndex,
    storage: &Storage<D, H>,
    gas_meter: &mut BlockGasMeter,
    write_log: &mut WriteLog,
    vp_wasm_cache: &mut VpCache<CA>,
    tx_wasm_cache: &mut TxCache<CA>,
) -> Result<BTreeSet<Address>>
where
    D: 'static + DB + for<'iter> DBIter<'iter> + Sync,
    H: 'static + StorageHasher + Sync,
    CA: 'static + WasmCacheAccess + Sync,
{
    gas_meter
        .add_compiling_fee(tx.code.len())
        .map_err(Error::GasError)?;
    let empty = vec![];
    let tx_data = tx.data.as_ref().unwrap_or(&empty);
    wasm::run::tx(
        storage,
        write_log,
        gas_meter,
        tx_index,
        &tx.code,
        tx_data,
        vp_wasm_cache,
        tx_wasm_cache,
    )
    .map_err(Error::TxRunnerError)
}

/// Check the acceptance of a transaction by validity predicates
fn check_vps<D, H, CA>(
    tx: &Tx,
    tx_index: &TxIndex,
    storage: &Storage<D, H>,
    gas_meter: &mut BlockGasMeter,
    write_log: &WriteLog,
    verifiers_from_tx: &BTreeSet<Address>,
    vp_wasm_cache: &mut VpCache<CA>,
) -> Result<VpsResult>
where
    D: 'static + DB + for<'iter> DBIter<'iter> + Sync,
    H: 'static + StorageHasher + Sync,
    CA: 'static + WasmCacheAccess + Sync,
{
    let (verifiers, keys_changed) =
        write_log.verifiers_and_changed_keys(verifiers_from_tx);

    let initial_gas = gas_meter.get_current_transaction_gas();

    let vps_result = execute_vps(
        verifiers,
        keys_changed,
        tx,
        tx_index,
        storage,
        write_log,
        initial_gas,
        vp_wasm_cache,
    )?;
    tracing::debug!("Total VPs gas cost {:?}", vps_result.gas_used);

    gas_meter
        .add_vps_gas(&vps_result.gas_used)
        .map_err(Error::GasError)?;

    Ok(vps_result)
}

/// Execute verifiers' validity predicates
fn execute_vps<D, H, CA>(
    verifiers: BTreeSet<Address>,
    keys_changed: BTreeSet<storage::Key>,
    tx: &Tx,
    tx_index: &TxIndex,
    storage: &Storage<D, H>,
    write_log: &WriteLog,
    initial_gas: u64,
    vp_wasm_cache: &mut VpCache<CA>,
) -> Result<VpsResult>
where
    D: 'static + DB + for<'iter> DBIter<'iter> + Sync,
    H: 'static + StorageHasher + Sync,
    CA: 'static + WasmCacheAccess + Sync,
{
    verifiers
        .par_iter()
        // TODO temporary pending on <https://github.com/anoma/anoma/issues/193>
        .filter(|addr| !matches!(addr, Address::Implicit(_)))
        .try_fold(VpsResult::default, |mut result, addr| {
            let mut gas_meter = VpGasMeter::new(initial_gas);
            let accept = match &addr {
                Address::Established(_) => {
                    let (vp, gas) = storage
                        .validity_predicate(addr)
                        .map_err(Error::StorageError)?;
                    gas_meter.add(gas).map_err(Error::GasError)?;
                    let vp =
                        vp.ok_or_else(|| Error::MissingAddress(addr.clone()))?;

                    gas_meter
                        .add_compiling_fee(vp.len())
                        .map_err(Error::GasError)?;

                    wasm::run::vp(
                        vp,
                        tx,
                        tx_index,
                        addr,
                        storage,
                        write_log,
                        &mut gas_meter,
                        &keys_changed,
                        &verifiers,
                        vp_wasm_cache.clone(),
                    )
                    .map_err(Error::VpRunnerError)
                }
                Address::Internal(internal_addr) => {
                    let ctx = native_vp::Ctx::new(
                        storage,
                        write_log,
                        tx,
                        tx_index,
                        gas_meter,
                        vp_wasm_cache.clone(),
                    );
                    let tx_data = match tx.data.as_ref() {
                        Some(data) => &data[..],
                        None => &[],
                    };

                    let accepted: Result<bool> = match internal_addr {
                        InternalAddress::PoS => {
                            let pos = PosVP { ctx };
                            let verifiers_addr_ref = &verifiers;
                            let pos_ref = &pos;
                            // TODO this is temporarily ran in a new thread to
                            // avoid crashing the ledger (required `UnwindSafe`
                            // and `RefUnwindSafe` in
                            // shared/src/ledger/pos/vp.rs)
                            let keys_changed_ref = &keys_changed;
                            let result = match panic::catch_unwind(move || {
                                pos_ref
                                    .validate_tx(
                                        tx_data,
                                        keys_changed_ref,
                                        verifiers_addr_ref,
                                    )
                                    .map_err(Error::PosNativeVpError)
                            }) {
                                Ok(result) => result,
                                Err(err) => {
                                    tracing::error!(
                                        "PoS native VP failed with {:#?}",
                                        err
                                    );
                                    Err(Error::PosNativeVpRuntime)
                                }
                            };
                            // Take the gas meter back out of the context
                            gas_meter = pos.ctx.gas_meter.into_inner();
                            result
                        }
                        InternalAddress::Ibc => {
                            let ibc = Ibc { ctx };
                            let result = ibc
                                .validate_tx(tx_data, &keys_changed, &verifiers)
                                .map_err(Error::IbcNativeVpError);
                            // Take the gas meter back out of the context
                            gas_meter = ibc.ctx.gas_meter.into_inner();
                            result
                        }
                        InternalAddress::Parameters => {
                            let parameters = ParametersVp { ctx };
                            let result = parameters
                                .validate_tx(tx_data, &keys_changed, &verifiers)
                                .map_err(Error::ParametersNativeVpError);
                            // Take the gas meter back out of the context
                            gas_meter = parameters.ctx.gas_meter.into_inner();
                            result
                        }
                        InternalAddress::PosSlashPool => {
                            // Take the gas meter back out of the context
                            gas_meter = ctx.gas_meter.into_inner();
                            Err(Error::AccessForbidden(
                                (*internal_addr).clone(),
                            ))
                        }
                        InternalAddress::Governance => {
                            let governance = GovernanceVp { ctx };
                            let result = governance
                                .validate_tx(tx_data, &keys_changed, &verifiers)
                                .map_err(Error::GovernanceNativeVpError);
                            gas_meter = governance.ctx.gas_meter.into_inner();
                            result
                        }
                        InternalAddress::Treasury => {
                            let treasury = TreasuryVp { ctx };
                            let result = treasury
                                .validate_tx(tx_data, &keys_changed, &verifiers)
                                .map_err(Error::TreasuryNativeVpError);
                            gas_meter = treasury.ctx.gas_meter.into_inner();
                            result
                        }
                        InternalAddress::IbcEscrow(_)
                        | InternalAddress::IbcBurn
                        | InternalAddress::IbcMint => {
                            // validate the transfer
                            let ibc_token = IbcToken { ctx };
                            let result = ibc_token
                                .validate_tx(tx_data, &keys_changed, &verifiers)
                                .map_err(Error::IbcTokenNativeVpError);
                            gas_meter = ibc_token.ctx.gas_meter.into_inner();
                            result
                        }
                        InternalAddress::EthBridge => {
                            let bridge = EthBridge { ctx };
                            let result = bridge
                                .validate_tx(tx_data, &keys_changed, &verifiers)
                                .map_err(Error::EthBridgeNativeVpError);
                            gas_meter = bridge.ctx.gas_meter.into_inner();
                            result
                        }
                    };

                    accepted
                }
                // TODO temporary pending on <https://github.com/anoma/anoma/issues/193>
                Address::Implicit(_) => unreachable!(),
            };

            // Returning error from here will short-circuit the VP parallel
            // execution. It's important that we only short-circuit gas
            // errors to get deterministic gas costs
            result.gas_used.set(&gas_meter).map_err(Error::GasError)?;
            match accept {
                Ok(accepted) => {
                    if !accepted {
                        result.rejected_vps.insert(addr.clone());
                    } else {
                        result.accepted_vps.insert(addr.clone());
                    }
                    Ok(result)
                }
                Err(err) => match err {
                    Error::GasError(_) => Err(err),
                    _ => {
                        result.rejected_vps.insert(addr.clone());
                        result.errors.push((addr.clone(), err.to_string()));
                        Ok(result)
                    }
                },
            }
        })
        .try_reduce(VpsResult::default, |a, b| {
            merge_vp_results(a, b, initial_gas)
        })
}

/// Merge VP results from parallel runs
fn merge_vp_results(
    a: VpsResult,
    mut b: VpsResult,
    initial_gas: u64,
) -> Result<VpsResult> {
    let mut accepted_vps = a.accepted_vps;
    let mut rejected_vps = a.rejected_vps;
    accepted_vps.extend(b.accepted_vps);
    rejected_vps.extend(b.rejected_vps);
    let mut errors = a.errors;
    errors.append(&mut b.errors);
    let mut gas_used = a.gas_used;

    // Returning error from here will short-circuit the VP parallel execution.
    // It's important that we only short-circuit gas errors to get deterministic
    // gas costs

    gas_used
        .merge(&mut b.gas_used, initial_gas)
        .map_err(Error::GasError)?;

    Ok(VpsResult {
        accepted_vps,
        rejected_vps,
        gas_used,
        errors,
    })
}
