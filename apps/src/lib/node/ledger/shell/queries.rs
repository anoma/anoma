//! Shell methods for querying state
use std::cmp::max;

use anoma::ledger::parameters::Parameters;
use anoma::ledger::pos::PosParams;
use anoma::types::address::Address;
use anoma::types::storage::Key;
use anoma::types::token::{self, Amount};
use borsh::{BorshDeserialize, BorshSerialize};
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::crypto::{ProofOp, ProofOps};
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::google::protobuf;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::types::EvidenceParams;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::crypto::{ProofOp, ProofOps};
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::google::protobuf;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::types::EvidenceParams;

use super::*;
use crate::node::ledger::response;
use crate::node::ledger::rpc::PrefixValue;

impl<D, H> Shell<D, H>
where
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    /// Uses `path` in the query to forward the request to the
    /// right query method and returns the result (which may be
    /// the default if `path` is not a supported string.
    /// INVARIANT: This method must be stateless.
    pub fn query(&self, query: request::Query) -> response::Query {
        use rpc::Path;
        match Path::from_str(&query.path) {
            Ok(path) => match path {
                Path::DryRunTx => self.dry_run_tx(&query.data),
                Path::Epoch => {
                    let (epoch, _gas) = self.storage.get_last_epoch();
                    let value = anoma::ledger::storage::types::encode(&epoch);
                    response::Query {
                        value,
                        ..Default::default()
                    }
                }
                Path::Value(storage_key) => {
                    self.read_storage_value(&storage_key, query.prove)
                }
                Path::Prefix(storage_key) => {
                    self.read_storage_prefix(&storage_key, query.prove)
                }
                Path::HasKey(storage_key) => self.has_storage_key(&storage_key),
            },
            Err(err) => response::Query {
                code: 1,
                info: format!("RPC error: {}", err),
                ..Default::default()
            },
        }
    }

    /// Simple helper function for the ledger to get balances
    /// of the specified token at the specified address
    pub fn get_balance(
        &self,
        token: &Address,
        owner: &Address,
    ) -> std::result::Result<Amount, String> {
        let query_resp =
            self.read_storage_value(&token::balance_key(token, owner), false);
        if query_resp.code != 0 {
            Err(format!(
                "Unable to read token {} balance of the given address {}",
                token, owner
            ))
        } else {
            BorshDeserialize::try_from_slice(&query_resp.value[..]).map_err(
                |_| {
                    "Unable to deserialize the balance of the given address"
                        .into()
                },
            )
        }
    }

    /// Query to read a value from storage
    pub fn read_storage_value(
        &self,
        key: &Key,
        is_proven: bool,
    ) -> response::Query {
        match self.storage.read(key) {
            Ok((Some(value), _gas)) => {
                let proof_ops = if is_proven {
                    match self.storage.get_existence_proof(key, value.clone()) {
                        Ok(proof) => Some(proof.into()),
                        Err(err) => {
                            return response::Query {
                                code: 2,
                                info: format!("Storage error: {}", err),
                                ..Default::default()
                            };
                        }
                    }
                } else {
                    None
                };
                response::Query {
                    value,
                    proof_ops,
                    ..Default::default()
                }
            }
            Ok((None, _gas)) => {
                let proof_ops = if is_proven {
                    match self.storage.get_non_existence_proof(key) {
                        Ok(proof) => Some(proof.into()),
                        Err(err) => {
                            return response::Query {
                                code: 2,
                                info: format!("Storage error: {}", err),
                                ..Default::default()
                            };
                        }
                    }
                } else {
                    None
                };
                response::Query {
                    code: 1,
                    info: format!("No value found for key: {}", key),
                    proof_ops,
                    ..Default::default()
                }
            }
            Err(err) => response::Query {
                code: 2,
                info: format!("Storage error: {}", err),
                ..Default::default()
            },
        }
    }

    /// Query to read a range of values from storage with a matching prefix. The
    /// value in successful response is a [`Vec<PrefixValue>`] encoded with
    /// [`BorshSerialize`].
    pub fn read_storage_prefix(
        &self,
        key: &Key,
        is_proven: bool,
    ) -> response::Query {
        let (iter, _gas) = self.storage.iter_prefix(key);
        let mut iter = iter.peekable();
        if iter.peek().is_none() {
            response::Query {
                code: 1,
                info: format!("No value found for key: {}", key),
                ..Default::default()
            }
        } else {
            let values: std::result::Result<
                Vec<PrefixValue>,
                anoma::types::storage::Error,
            > = iter
                .map(|(key, value, _gas)| {
                    let key = Key::parse(key)?;
                    Ok(PrefixValue { key, value })
                })
                .collect();
            match values {
                Ok(values) => {
                    let proof_ops = if is_proven {
                        let mut ops = vec![];
                        for PrefixValue { key, value } in &values {
                            match self
                                .storage
                                .get_existence_proof(key, value.clone())
                            {
                                Ok(p) => {
                                    let mut cur_ops: Vec<ProofOp> = p
                                        .ops
                                        .into_iter()
                                        .map(|op| op.into())
                                        .collect();
                                    ops.append(&mut cur_ops);
                                }
                                Err(err) => {
                                    return response::Query {
                                        code: 2,
                                        info: format!("Storage error: {}", err),
                                        ..Default::default()
                                    };
                                }
                            }
                        }
                        // ops is not empty in this case
                        Some(ProofOps { ops })
                    } else {
                        None
                    };
                    let value = values.try_to_vec().unwrap();
                    response::Query {
                        value,
                        proof_ops,
                        ..Default::default()
                    }
                }
                Err(err) => response::Query {
                    code: 1,
                    info: format!(
                        "Error parsing a storage key {}: {}",
                        key, err
                    ),
                    ..Default::default()
                },
            }
        }
    }

    /// Query to check if a storage key exists.
    fn has_storage_key(&self, key: &Key) -> response::Query {
        match self.storage.has_key(key) {
            Ok((has_key, _gas)) => response::Query {
                value: has_key.try_to_vec().unwrap(),
                ..Default::default()
            },
            Err(err) => response::Query {
                code: 2,
                info: format!("Storage error: {}", err),
                ..Default::default()
            },
        }
    }

    pub fn get_evidence_params(
        &self,
        protocol_params: &Parameters,
        pos_params: &PosParams,
    ) -> EvidenceParams {
        // Minimum number of epochs before tokens are unbonded and can be
        // withdrawn
        let len_before_unbonded = max(pos_params.unbonding_len as i64 - 1, 0);
        let max_age_num_blocks: i64 =
            protocol_params.epoch_duration.min_num_of_blocks as i64
                * len_before_unbonded;
        let min_duration_secs =
            protocol_params.epoch_duration.min_duration.0 as i64;
        let max_age_duration = Some(protobuf::Duration {
            seconds: min_duration_secs * len_before_unbonded,
            nanos: 0,
        });
        EvidenceParams {
            max_age_num_blocks,
            max_age_duration,
            ..EvidenceParams::default()
        }
    }
}
