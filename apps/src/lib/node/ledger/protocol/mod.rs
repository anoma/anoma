//! The ledger's protocol
use std::collections::HashSet;
use std::convert::TryFrom;
use std::{fmt, panic};

use anoma::ledger::gas::{self, BlockGasMeter, VpGasMeter, VpsGas};
use anoma::ledger::ibc::{self, Ibc};
use anoma::ledger::native_vp::{self, NativeVp};
use anoma::ledger::parameters::{self, ParametersVp};
use anoma::ledger::pos::{self, PosVP};
use anoma::ledger::storage::write_log::WriteLog;
use anoma::proto::{self, Tx};
use anoma::types::address::{Address, InternalAddress};
use anoma::types::storage::Key;
use anoma::types::transaction::{process_tx, TxType};
use anoma::vm::{self, wasm};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use thiserror::Error;

use crate::node::ledger::storage::PersistentStorage;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Storage error: {0}")]
    StorageError(anoma::ledger::storage::Error),
    #[error("Error decoding a transaction from bytes: {0}")]
    TxDecodingError(proto::Error),
    #[error("Transaction runner error: {0}")]
    TxRunnerError(vm::wasm::run::Error),
    #[error("Gas error: {0}")]
    GasError(gas::Error),
    #[error("Error executing VP for addresses: {0:?}")]
    VpRunnerError(vm::wasm::run::Error),
    #[error("The address {0} doesn't exist")]
    MissingAddress(Address),
    #[error("IBC native VP: {0}")]
    IbcNativeVpError(ibc::Error),
    #[error("PoS native VP: {0}")]
    PosNativeVpError(pos::vp::Error),
    #[error("PoS native VP panicked")]
    PosNativeVpRuntime,
    #[error("Parameters native VP: {0}")]
    ParametersNativeVpError(parameters::Error),
    #[error("Access to an internal address {0} is forbidden")]
    AccessForbidden(InternalAddress),
}

pub type Result<T> = std::result::Result<T, Error>;

/// Transaction application result
#[derive(Clone, Debug, Default)]
pub struct TxResult {
    pub gas_used: u64,
    pub changed_keys: HashSet<Key>,
    pub vps_result: VpsResult,
    pub initialized_accounts: Vec<Address>,
}

impl TxResult {
    pub fn is_accepted(&self) -> bool {
        self.vps_result.rejected_vps.is_empty()
    }
}

/// Result of checking a transaction with validity predicates
#[derive(Clone, Debug)]
pub struct VpsResult {
    pub accepted_vps: HashSet<Address>,
    pub rejected_vps: HashSet<Address>,
    pub gas_used: VpsGas,
    pub errors: Vec<(Address, String)>,
}

impl Default for VpsResult {
    fn default() -> Self {
        Self {
            accepted_vps: HashSet::default(),
            rejected_vps: HashSet::default(),
            gas_used: VpsGas::default(),
            errors: Vec::default(),
        }
    }
}

/// Apply a given transaction
pub fn apply_tx(
    tx_bytes: &[u8],
    block_gas_meter: &mut BlockGasMeter,
    write_log: &mut WriteLog,
    storage: &PersistentStorage,
) -> Result<TxResult> {
    block_gas_meter
        .add_base_transaction_fee(tx_bytes.len())
        .map_err(Error::GasError)?;

    let tx = Tx::try_from(tx_bytes).map_err(Error::TxDecodingError)?;

    match process_tx(tx).unwrap() {
        TxType::Raw(tx) => {
            let verifiers =
                execute_tx(&tx, storage, block_gas_meter, write_log)?;

            let vps_result = check_vps(
                &tx,
                storage,
                block_gas_meter,
                write_log,
                &verifiers,
            )?;

            let gas_used = block_gas_meter
                .finalize_transaction()
                .map_err(Error::GasError)?;
            let initialized_accounts = write_log.get_initialized_accounts();
            let changed_keys = write_log.get_keys();

            Ok(TxResult {
                gas_used,
                changed_keys,
                vps_result,
                initialized_accounts,
            })
        }
        TxType::Wrapper(_) => {
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
fn execute_tx(
    tx: &Tx,
    storage: &PersistentStorage,
    gas_meter: &mut BlockGasMeter,
    write_log: &mut WriteLog,
) -> Result<HashSet<Address>> {
    gas_meter
        .add_compiling_fee(tx.code.len())
        .map_err(Error::GasError)?;
    let empty = vec![];
    let tx_data = tx.data.as_ref().unwrap_or(&empty);
    wasm::run::tx(storage, write_log, gas_meter, &tx.code, tx_data)
        .map_err(Error::TxRunnerError)
}

/// A validity predicate
enum Vp<'a> {
    Wasm(Vec<u8>),
    Native(&'a InternalAddress),
}

/// Check the acceptance of a transaction by validity predicates
fn check_vps(
    tx: &Tx,
    storage: &PersistentStorage,
    gas_meter: &mut BlockGasMeter,
    write_log: &WriteLog,
    verifiers_from_tx: &HashSet<Address>,
) -> Result<VpsResult> {
    let verifiers = write_log.verifiers_changed_keys(verifiers_from_tx);

    // collect the VPs for the verifiers
    let verifiers: Vec<(Address, HashSet<Key>, Vp)> = verifiers
        .iter()
        .map(|(addr, keys)| {
            let vp = match addr {
                Address::Internal(addr) => Vp::Native(addr),
                Address::Established(_) | Address::Implicit(_) => {
                    let (vp, gas) = storage
                        .validity_predicate(addr)
                        .map_err(Error::StorageError)?;
                    gas_meter.add(gas).map_err(Error::GasError)?;
                    let vp =
                        vp.ok_or_else(|| Error::MissingAddress(addr.clone()))?;

                    gas_meter
                        .add_compiling_fee(vp.len())
                        .map_err(Error::GasError)?;
                    Vp::Wasm(vp)
                }
            };

            Ok((addr.clone(), keys.clone(), vp))
        })
        .collect::<std::result::Result<_, _>>()?;

    let initial_gas = gas_meter.get_current_transaction_gas();

    let vps_result =
        execute_vps(verifiers, tx, storage, write_log, initial_gas)?;
    tracing::debug!("Total VPs gas cost {:?}", vps_result.gas_used);

    gas_meter
        .add_vps_gas(&vps_result.gas_used)
        .map_err(Error::GasError)?;

    Ok(vps_result)
}

/// Execute verifiers' validity predicates
fn execute_vps(
    verifiers: Vec<(Address, HashSet<Key>, Vp)>,
    tx: &Tx,
    storage: &PersistentStorage,
    write_log: &WriteLog,
    initial_gas: u64,
) -> Result<VpsResult> {
    let verifiers_addr = verifiers
        .iter()
        .map(|(addr, _, _)| addr)
        .cloned()
        .collect::<HashSet<_>>();

    verifiers
        .par_iter()
        .try_fold(VpsResult::default, |mut result, (addr, keys, vp)| {
            let mut gas_meter = VpGasMeter::new(initial_gas);
            let accept = match &vp {
                Vp::Wasm(vp) => wasm::run::vp(
                    vp,
                    tx,
                    addr,
                    storage,
                    write_log,
                    &mut gas_meter,
                    keys,
                    &verifiers_addr,
                )
                .map_err(Error::VpRunnerError),
                Vp::Native(internal_addr) => {
                    let ctx =
                        native_vp::Ctx::new(storage, write_log, tx, gas_meter);
                    let tx_data = match tx.data.as_ref() {
                        Some(data) => &data[..],
                        None => &[],
                    };

                    let accepted: Result<bool> = match internal_addr {
                        InternalAddress::PoS => {
                            let pos = PosVP { ctx };
                            let verifiers_addr_ref = &verifiers_addr;
                            let pos_ref = &pos;
                            // TODO this is temporarily ran in a new thread to
                            // avoid crashing the ledger (required `UnwindSafe`
                            // and `RefUnwindSafe` in
                            // shared/src/ledger/pos/vp.rs)
                            let result = match panic::catch_unwind(move || {
                                pos_ref
                                    .validate_tx(
                                        tx_data,
                                        keys,
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
                                .validate_tx(tx_data, keys, &verifiers_addr)
                                .map_err(Error::IbcNativeVpError);
                            // Take the gas meter back out of the context
                            gas_meter = ibc.ctx.gas_meter.into_inner();
                            result
                        }
                        InternalAddress::Parameters => {
                            let parameters = ParametersVp { ctx };
                            let result = parameters
                                .validate_tx(tx_data, keys, &verifiers_addr)
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
                    };

                    accepted
                }
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
    let accepted_vps = a.accepted_vps.union(&b.accepted_vps).cloned().collect();
    let rejected_vps = a.rejected_vps.union(&b.rejected_vps).cloned().collect();
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

impl fmt::Display for TxResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Transaction is {}. Gas used: {};{} VPs result: {}",
            if self.is_accepted() {
                "valid"
            } else {
                "invalid"
            },
            self.gas_used,
            iterable_to_string("Changed keys", self.changed_keys.iter()),
            self.vps_result,
        )
    }
}

impl fmt::Display for VpsResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            iterable_to_string("Accepted", self.accepted_vps.iter()),
            iterable_to_string("Rejected", self.rejected_vps.iter()),
            iterable_to_string(
                "Errors",
                self.errors
                    .iter()
                    .map(|(addr, err)| format!("{} in {}", err, addr))
            ),
        )
    }
}

fn iterable_to_string<T: fmt::Display>(
    label: &str,
    iter: impl Iterator<Item = T>,
) -> String {
    let mut iter = iter.peekable();
    if iter.peek().is_none() {
        "".into()
    } else {
        format!(
            " {}: {};",
            label,
            iter.map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}
