//! Shell methods for querying state

use anoma::types::address::Address;
use anoma::types::key;
use anoma::types::key::dkg_session_keys::DkgPublicKey;
use anoma::types::storage::{Key, PrefixValue};
use anoma::types::token::{self, Amount};
use borsh::{BorshDeserialize, BorshSerialize};
use ferveo_common::TendermintValidator;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::crypto::{ProofOp, ProofOps};
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::crypto::{ProofOp, ProofOps};

use super::*;
use crate::node::ledger::response;

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
        let height = match query.height {
            0 => self.storage.get_block_height().0,
            1.. => BlockHeight(query.height as u64),
            _ => {
                return response::Query {
                    code: 1,
                    info: format!(
                        "The query height is invalid: {}",
                        query.height
                    ),
                    ..Default::default()
                };
            }
        };
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
                    self.read_storage_value(&storage_key, height, query.prove)
                }
                Path::Prefix(storage_key) => {
                    self.read_storage_prefix(&storage_key, height, query.prove)
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
        let height = self.storage.get_block_height().0;
        let query_resp = self.read_storage_value(
            &token::balance_key(token, owner),
            height,
            false,
        );
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
        height: BlockHeight,
        is_proven: bool,
    ) -> response::Query {
        match self.storage.read_with_height(key, height) {
            Ok((Some(value), _gas)) => {
                let proof_ops = if is_proven {
                    match self.storage.get_existence_proof(
                        key,
                        value.clone(),
                        height,
                    ) {
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
                    match self.storage.get_non_existence_proof(key, height) {
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
        height: BlockHeight,
        is_proven: bool,
    ) -> response::Query {
        if height != self.storage.get_block_height().0 {
            return response::Query {
                code: 2,
                info: format!(
                    "Prefix read works with only the latest height: height {}",
                    height
                ),
                ..Default::default()
            };
        }
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
                            match self.storage.get_existence_proof(
                                key,
                                value.clone(),
                                height,
                            ) {
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

    /// Lookup data about a validator from their protocol signing key
    #[allow(dead_code)]
    pub fn get_validator_from_protocol_pk(
        &self,
        pk: &key::common::PublicKey,
    ) -> Option<TendermintValidator<EllipticCurve>> {
        let pk_bytes = pk
            .try_to_vec()
            .expect("Serializing public key should not fail");
        // get the current epoch
        let (current_epoch, _) = self.storage.get_current_epoch();
        // get the active validator set
        self.storage
            .read_validator_set()
            .get(current_epoch)
            .expect("Validators for the next epoch should be known")
            .active
            .iter()
            .find(|validator| {
                let pk_key = key::protocol_pk_key(&validator.address);
                match self.storage.read(&pk_key) {
                    Ok((Some(bytes), _)) => bytes == pk_bytes,
                    _ => false,
                }
            })
            .map(|validator| {
                let dkg_key =
                    key::dkg_session_keys::dkg_pk_key(&validator.address);
                let bytes = self
                    .storage
                    .read(&dkg_key)
                    .expect("Validator should have public dkg key")
                    .0
                    .expect("Validator should have public dkg key");
                let dkg_publickey =
                    &<DkgPublicKey as BorshDeserialize>::deserialize(
                        &mut bytes.as_ref(),
                    )
                    .expect(
                        "DKG public key in storage should be deserializable",
                    );
                TendermintValidator {
                    power: validator.voting_power.into(),
                    address: validator.address.to_string(),
                    public_key: dkg_publickey.into(),
                }
            })
    }
}
