//! Client RPC queries

use std::clone::Clone;
use std::collections::HashMap;
use std::convert::TryInto;
use std::marker::Sync;

use borsh::BorshDeserialize;
use itertools::Itertools;
#[cfg(not(feature = "ABCI"))]
use tendermint::abci::Code;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::query::Query;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::Client;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::Order;
use tendermint_rpc_abci::endpoint::abci_query::AbciQuery;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::query::Query;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::Client;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::Order;
#[cfg(feature = "ABCI")]
use tendermint_stable::abci::Code;

use crate::ledger::pos::types::{Epoch as PosEpoch, VotingPower};
use crate::ledger::pos::{self, BondId, Bonds, Slash, Slashes, Unbonds};
use crate::types::address::Address;
use crate::types::key::*;
pub use crate::types::rpc::{
    BalanceQueryResult, BondQueryResult, Path, QueryError, SlashQueryResult,
    TendermintEventType, TxQueryResult, TxResponse,
};
use crate::types::storage::{Epoch, Key, PrefixValue};
use crate::types::token::Amount;
use crate::types::transaction::Hash;
use crate::types::{address, token};

#[allow(missing_docs)]
pub type Result<T> = std::result::Result<T, QueryError>;
/// Represents a query for an event pertaining to the specified transaction
#[derive(Debug, Clone)]
struct TxEventQuery {
    hash: Hash,
    event_type: TendermintEventType,
}

impl TxEventQuery {
    /// The event type to which this event query pertains
    fn event_type(&self) -> &'static str {
        self.event_type.into()
    }

    /// The transaction to which this event query pertains
    fn tx_hash(&self) -> String {
        format!("{}", self.hash)
    }
}

/// Transaction event queries are semantically a subset of general queries
impl From<TxEventQuery> for Query {
    fn from(tx_query: TxEventQuery) -> Self {
        Query::default().and_eq(
            format!("{}.hash", tx_query.event_type),
            format!("{}", tx_query.hash),
        )
    }
}

/// Query the epoch of the last committed block.
pub async fn query_epoch<C>(client: C) -> Result<Epoch>
where
    C: Client + Sync,
{
    let path = Path::Epoch;
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .map_err(QueryError::ABCIQueryError)?;

    match response.code {
        Code::Ok => Ok(Epoch::try_from_slice(&response.value[..])?),
        Code::Err(err) => Err(QueryError::Format(response.info, err)),
    }
}

/// Query token balance(s)
///
/// Arguments owner and token are Options, the function will produce a result
/// based on the effective values of these arguments.
///
/// Cases (token, owner):
///     Some, Some: returns the balance of token for owner
///     None, Some: returns the balances of all the tokens owned by owner
///     Some, None: returns the balances of token for all the users owning token
///     None, None: returns the balances of all the tokens for all the users
pub async fn query_balance<C>(
    client: C,
    token: Option<Address>,
    owner: Option<Address>,
) -> Result<BalanceQueryResult>
where
    C: Client + Clone + Sync,
{
    let tokens = address::tokens();
    let mut result = BalanceQueryResult::default();

    match (token, owner) {
        (Some(token), Some(owner)) => {
            let key = token::balance_key(&token, &owner);

            if let Some(balance) =
                query_storage_value::<C, Amount>(client, key).await?
            {
                result.insert(owner.clone(), token, balance);
            }
        }
        (None, Some(owner)) => {
            for (token, _) in tokens {
                let key = token::balance_key(&token, &owner);

                if let Some(balance) =
                    query_storage_value::<C, Amount>(client.clone(), key)
                        .await?
                {
                    result.insert(owner.clone(), token, balance);
                }
            }
        }
        (Some(token), None) => {
            let key = token::balance_prefix(&token);

            if let Some(balances) =
                query_storage_prefix::<C, Amount>(client, key).await?
            {
                for (key, balance) in balances {
                    if let Some(owner) = token::is_any_token_balance_key(&key) {
                        result.insert(owner.to_owned(), token.clone(), balance);
                    }
                }
            }
        }
        (None, None) => {
            for (token, _) in tokens {
                let key = token::balance_prefix(&token);
                if let Some(balances) =
                    query_storage_prefix::<C, Amount>(client.clone(), key)
                        .await?
                {
                    for (key, balance) in balances {
                        if let Some(owner) =
                            token::is_any_token_balance_key(&key)
                        {
                            result.insert(
                                owner.to_owned(),
                                token.clone(),
                                balance,
                            );
                        }
                    }
                }
            }
        }
    }

    Ok(result)
}

/// Query bonds of multiple entities. If owner is Some then find owner's bonds
/// to any validator else find all the bonds on the chain
pub async fn query_bonds<C>(
    client: C,
    owner: Option<Address>,
) -> Result<Vec<(BondId, Bonds)>>
where
    C: Client + Sync,
{
    let bonds_prefix = match owner {
        Some(owner) => pos::bonds_for_source_prefix(&owner),
        None => pos::bonds_prefix(),
    };

    let bonds = query_storage_prefix::<C, Bonds>(client, bonds_prefix).await?;

    let mut res = vec![];

    if let Some(bonds) = bonds {
        for (key, bond) in bonds {
            if let Some(bond_id) = pos::is_bond_key(&key) {
                res.push((bond_id, bond));
            }
        }
    }

    Ok(res)
}

/// Query unbonds of multiple entities. If owner is Some then find owner's
/// unbonds to any validator else find all the unbonds on the chain
pub async fn query_unbonds<C>(
    client: C,
    owner: Option<Address>,
) -> Result<Vec<(BondId, Unbonds)>>
where
    C: Client + Sync,
{
    let unbonds_prefix = match owner {
        Some(owner) => pos::unbonds_for_source_prefix(&owner),
        None => pos::unbonds_prefix(),
    };

    let unbonds =
        query_storage_prefix::<C, Unbonds>(client, unbonds_prefix).await?;

    let mut res = vec![];

    if let Some(unbonds) = unbonds {
        for (key, bond) in unbonds {
            if let Some(bond_id) = pos::is_unbond_key(&key) {
                res.push((bond_id, bond));
            }
        }
    }

    Ok(res)
}

/// Query bonds Amount.
/// Arguments owner and validator are Options, the function will produce a
/// result based on the effective values of these arguments.
///
/// Cases (owner, validator):
///     Some, Some: returns the bonds for owner on validator
///     None, Some: returns all the bonds on validator
///     Some, None: returns all the bonds of owner on any validator
///     None, None: returns all the bonds on chain
///
/// Returns a [`BondQueryResult`].
pub async fn query_bonds_amount<C>(
    client: C,
    owner: Option<Address>,
    validator: Option<Address>,
) -> Result<BondQueryResult>
where
    C: Client + Clone + Sync,
{
    let epoch = query_epoch(client.clone()).await?;
    let mut result = BondQueryResult::default();

    match validator {
        Some(validator) => {
            let bond_id = match owner {
                Some(owner) => BondId {
                    source: owner,
                    validator,
                },
                None => BondId {
                    source: validator.clone(),
                    validator,
                },
            };

            let mut result = BondQueryResult::default();
            // Find owner's delegations to the given validator
            let bonds = query_storage_value::<C, Bonds>(
                client.clone(),
                pos::bond_key(&bond_id),
            )
            .await?;
            // Find owner's unbonded delegations from the given
            // validator
            let unbonds = query_storage_value::<C, Unbonds>(
                client.clone(),
                pos::unbond_key(&bond_id),
            )
            .await?;
            // Find validator's slashes, if any
            let slashes_res =
                query_slashes(client, Some(bond_id.validator.clone())).await?;
            let slashes = match slashes_res.get(&bond_id.validator) {
                Some(v) => v.to_owned(),
                None => Vec::new(),
            };

            if let Some(bonds) = &bonds {
                let (t, a) =
                    process_bonds_query(bonds, &slashes, epoch, None, None);
                result.bonds = t;
                result.active = a;
            }

            if let Some(unbonds) = &unbonds {
                let (t, w) =
                    process_unbonds_query(unbonds, &slashes, epoch, None, None);
                result.unbonds = t;
                result.withdrawable = w;
            }
        }
        None => {
            let bonds = query_bonds(client.clone(), owner.clone()).await?;
            let unbonds = query_unbonds(client.clone(), owner).await?;

            for (bond_id, bond) in bonds {
                // Find validator's slashes, if any
                let slashes_res = query_slashes(
                    client.clone(),
                    Some(bond_id.validator.clone()),
                )
                .await?;
                let slashes = match slashes_res.get(&bond_id.validator) {
                    Some(v) => v.to_owned(),
                    None => Vec::new(),
                };

                let (tot, tot_active) = process_bonds_query(
                    &bond,
                    &slashes,
                    epoch,
                    Some(result.bonds),
                    Some(result.active),
                );
                result.bonds += tot;
                result.active += tot_active;
            }

            for (bond_id, unbonds) in unbonds {
                // Find validator's slashes, if any
                let slashes_res = query_slashes(
                    client.clone(),
                    Some(bond_id.validator.clone()),
                )
                .await?;
                let slashes = match slashes_res.get(&bond_id.validator) {
                    Some(v) => v.to_owned(),
                    None => Vec::new(),
                };

                let (tot, tot_withdrawable) = process_unbonds_query(
                    &unbonds,
                    &slashes,
                    epoch,
                    Some(result.unbonds),
                    Some(result.withdrawable),
                );
                result.unbonds += tot;
                result.withdrawable += tot_withdrawable;
            }
        }
    }
    Ok(result)
}

/// Query PoS voting power
/// If validator is Some then returns the voting power of that specific address,
/// otherwise returns the total voting power.
pub async fn query_voting_power<C, E>(
    client: C,
    validator: Option<&Address>,
    epoch: Option<E>,
) -> Result<Option<VotingPower>>
where
    C: Client + Clone + Sync,
    E: Into<Epoch>,
{
    let epoch = match epoch {
        Some(epoch) => epoch.into(),
        None => query_epoch(client.clone()).await?,
    };

    match validator {
        Some(validator) => {
            // Find voting power for the given validator
            let voting_power_key = pos::validator_voting_power_key(validator);
            let voting_powers = query_storage_value::<
                C,
                pos::ValidatorVotingPowers,
            >(client, voting_power_key)
            .await?;
            match voting_powers.and_then(|data| data.get(epoch)) {
                Some(voting_power_delta) => {
                    let voting_power: VotingPower =
                        voting_power_delta.try_into()?;
                    Ok(Some(voting_power))
                }
                None => Ok(None),
            }
        }
        None => {
            // Find total voting power
            let total_voting_power_key = pos::total_voting_power_key();
            let total_voting_powers =
                query_storage_value::<C, pos::TotalVotingPowers>(
                    client,
                    total_voting_power_key,
                )
                .await?
                .ok_or(QueryError::UnsetVotingPower)?;

            match total_voting_powers.get(epoch) {
                Some(total_voting_power_delta) => {
                    let total_voting_power =
                        total_voting_power_delta.try_into()?;
                    Ok(Some(total_voting_power))
                }
                None => Ok(None),
            }
        }
    }
}

/// Query PoS slashes
/// If validator is Some then returns the slashes for it, otherwise returns
/// the slashes for all the validators.
pub async fn query_slashes<C>(
    client: C,
    validator: Option<Address>,
) -> Result<SlashQueryResult>
where
    C: Client + Sync,
{
    let mut result = SlashQueryResult::default();

    match validator {
        Some(validator) => {
            // Find slashes for the given validator
            let slashes_key = pos::validator_slashes_key(&validator);
            let slashes: Option<Slashes> =
                query_storage_value::<C, Slashes>(client, slashes_key).await?;

            if let Some(slashes) = slashes {
                result.insert(validator, slashes);
            }
        }
        None => {
            // Iterate slashes for all validators
            let slashes_prefix = pos::slashes_prefix();
            let slashes =
                query_storage_prefix::<C, Slashes>(client, slashes_prefix)
                    .await?;

            if let Some(slashes) = slashes {
                for (slashes_key, slashes) in slashes {
                    if let Some(validator) =
                        pos::is_validator_slashes_key(&slashes_key)
                    {
                        result.insert(validator.to_owned(), slashes);
                    }
                }
            }
        }
    }

    Ok(result)
}

/// Dry run a transaction
pub async fn dry_run_tx<C>(client: C, tx_bytes: Vec<u8>) -> Result<AbciQuery>
where
    C: Client + Sync,
{
    let path = Path::DryRunTx;
    client
        .abci_query(Some(path.into()), tx_bytes, None, false)
        .await
        .map_err(QueryError::ABCIQueryError)
}

/// Get account's public key stored in its storage sub-space
pub async fn get_public_key<C>(
    client: C,
    address: &Address,
) -> Result<Option<common::PublicKey>>
where
    C: Client + Sync,
{
    let key = pk_key(address);
    query_storage_value(client, key).await
}

/// Check if the given address is a known validator.
pub async fn is_validator<C>(client: C, address: &Address) -> Result<bool>
where
    C: Client + Sync,
{
    // Check if there's any validator state
    let key = pos::validator_state_key(address);
    // We do not need to decode it
    let state: Option<pos::ValidatorStates> =
        query_storage_value(client, key).await?;
    // If there is, then the address is a validator
    Ok(state.is_some())
}

/// Check if the address exists on chain. Established address exists if it has a
/// stored validity predicate. Implicit and internal addresses always return
/// true.
pub async fn known_address<C>(client: C, address: &Address) -> Result<bool>
where
    C: Client + Sync,
{
    match address {
        Address::Established(_) => {
            // Established account exists if it has a VP
            let key = Key::validity_predicate(address);
            query_has_storage_key(client, key).await
        }
        Address::Implicit(_) | Address::Internal(_) => Ok(true),
    }
}

/// Accumulate slashes starting from `epoch_start` until (optionally)
/// `withdraw_epoch` (non included) and apply them to the token amount `delta`.
fn apply_slashes(
    slashes: &[Slash],
    mut delta: Amount,
    epoch_start: PosEpoch,
    withdraw_epoch: Option<PosEpoch>,
) -> Amount {
    for slash in slashes {
        if slash.epoch >= epoch_start
            && slash.epoch < withdraw_epoch.unwrap_or_else(|| u64::MAX.into())
        {
            let raw_delta: u64 = delta.into();
            let current_slashed = Amount::from(slash.rate * raw_delta);
            delta -= current_slashed;
        }
    }

    delta
}

/// Process the result of a bonds query to determine total bonds
/// and total active bonds. This includes taking into account
/// an aggregation of slashes since the start of the given epoch.
fn process_bonds_query(
    bonds: &Bonds,
    slashes: &[Slash],
    epoch: Epoch,
    total: Option<Amount>,
    total_active: Option<Amount>,
) -> (Amount, Amount) {
    let mut total_active = total_active.unwrap_or_else(|| 0.into());
    let mut current_total: Amount = 0.into();

    for bond in bonds.iter() {
        for (epoch_start, &(mut delta)) in bond.deltas.iter().sorted() {
            delta = apply_slashes(slashes, delta, *epoch_start, None);
            current_total += delta;

            if epoch > (*epoch_start).into() {
                total_active += delta;
            }
        }
    }
    let total = total.unwrap_or_else(|| 0.into()) + current_total;

    (total, total_active)
}

/// Process the result of an unbonds query to determine total bonds
/// and total withdrawable bonds. This includes taking into account
/// an aggregation of slashes since the start of the given epoch up
/// until the withdrawal epoch.
fn process_unbonds_query(
    unbonds: &Unbonds,
    slashes: &[Slash],
    epoch: Epoch,
    total: Option<Amount>,
    total_withdrawable: Option<Amount>,
) -> (Amount, Amount) {
    let mut withdrawable = total_withdrawable.unwrap_or_else(|| 0.into());
    let mut current_total: Amount = 0.into();

    for deltas in unbonds.iter() {
        for ((epoch_start, epoch_end), &(mut delta)) in
            deltas.deltas.iter().sorted()
        {
            let withdraw_epoch = *epoch_end + 1_u64;
            delta = apply_slashes(
                slashes,
                delta,
                *epoch_start,
                Some(withdraw_epoch),
            );
            current_total += delta;
            if epoch > (*epoch_end).into() {
                withdrawable += delta;
            }
        }
    }
    let total = total.unwrap_or_else(|| 0.into()) + current_total;

    (total, withdrawable)
}

/// Query a storage value and decode it with [`BorshDeserialize`].
pub async fn query_storage_value<C, T>(client: C, key: Key) -> Result<Option<T>>
where
    C: Client + Sync,
    T: BorshDeserialize,
{
    let path = Path::Value(key);
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .map_err(QueryError::ABCIQueryError)?;

    match response.code {
        Code::Ok => match T::try_from_slice(&response.value[..]) {
            Ok(value) => Ok(Some(value)),
            Err(err) => Err(QueryError::Decoding(err)),
        },
        Code::Err(err) if err == 1 => Ok(None),
        Code::Err(err) => Err(QueryError::Format(response.info, err)),
    }
}

/// Query a range of storage values with a matching prefix and decode them with
/// [`BorshDeserialize`]. Returns an iterator of the storage keys paired with
/// their associated values.
async fn query_storage_prefix<C, T>(
    client: C,
    key: Key,
) -> Result<Option<impl Iterator<Item = (Key, T)>>>
where
    C: Client + Sync,
    T: BorshDeserialize,
{
    let path = Path::Prefix(key);
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .map_err(QueryError::ABCIQueryError)?;

    match response.code {
        Code::Ok => {
            match Vec::<PrefixValue>::try_from_slice(&response.value[..]) {
                Ok(values) => {
                    let decode = |PrefixValue { key, value }: PrefixValue| {
                        match T::try_from_slice(&value[..]) {
                            Err(_) => None,
                            Ok(value) => Some((key, value)),
                        }
                    };
                    Ok(Some(values.into_iter().filter_map(decode)))
                }
                Err(err) => Err(QueryError::Decoding(err)),
            }
        }
        Code::Err(err) if err == 1 => Ok(None),
        Code::Err(err) => Err(QueryError::Format(response.info, err)),
    }
}

/// Query to check if the given storage key exists.
async fn query_has_storage_key<C>(client: C, key: Key) -> Result<bool>
where
    C: Client + Sync,
{
    let path = Path::HasKey(key);
    let data = vec![];
    let response = client
        .abci_query(Some(path.into()), data, None, false)
        .await
        .map_err(QueryError::ABCIQueryError)?;

    match response.code {
        Code::Ok => match bool::try_from_slice(&response.value[..]) {
            Ok(value) => Ok(value),
            Err(err) => Err(QueryError::Decoding(err)),
        },
        Code::Err(err) => Err(QueryError::Format(response.info, err)),
    }
}

/// Lookup the full response accompanying the specified transaction event
async fn query_tx_response<C>(
    client: C,
    tx_query: TxEventQuery,
) -> Result<TxResponse>
where
    C: Client + Sync,
{
    // Find all blocks that apply a transaction with the specified hash
    let blocks = client
        .block_search(Query::from(tx_query.clone()), 1, 255, Order::Ascending)
        .await?
        .blocks;

    // Get the block results corresponding to a block to which
    // the specified transaction belongs
    let block = &blocks
        .get(0)
        .ok_or_else(|| QueryError::BlockNotFound(tx_query.hash.clone()))?
        .block;

    let response_block_results = client
        .block_results(block.header.height)
        .await
        .map_err(|_| QueryError::BlockNotFound(tx_query.hash.clone()))?;

    // Search for the event where the specified transaction is
    // applied to the blockchain
    let query_event_opt =
        response_block_results.end_block_events.and_then(|events| {
            (&events)
                .iter()
                .find(|event| {
                    event.type_str == tx_query.event_type()
                        && (&event.attributes).iter().any(|tag| {
                            tag.key.as_ref() == "hash"
                                && tag.value.as_ref() == tx_query.tx_hash()
                        })
                })
                .cloned()
        });
    let query_event =
        query_event_opt.ok_or(QueryError::EventNotFound(tx_query.hash))?;

    // Reformat the event attributes so as to ease value extraction
    let event_map: HashMap<&str, &str> = (&query_event.attributes)
        .iter()
        .map(|tag| (tag.key.as_ref(), tag.value.as_ref()))
        .collect();

    // Summarize the transaction results that we were searching for
    let result = TxResponse {
        info: event_map["info"].to_string(),
        log: event_map["log"].to_string(),
        height: serde_json::from_str(event_map["height"])?,
        hash: Hash::try_from(event_map["hash"].as_bytes())?,
        code: serde_json::from_str(event_map["code"])?,
        gas_used: serde_json::from_str(event_map["gas_used"])?,
        initialized_accounts: serde_json::from_str(
            event_map["initialized_accounts"],
        )
        .unwrap_or_default(),
    };

    Ok(result)
}

/// Lookup the results of applying the specified transaction to the
/// blockchain.
pub async fn query_tx_result<C, T>(
    client: C,
    tx_hash: T,
) -> Result<TxQueryResult>
where
    C: Client + Clone + Sync,
    T: AsRef<[u8]>,
{
    // First try looking up application event pertaining to given hash.
    let hash = Hash::try_from(tx_hash.as_ref())?;

    let tx_response = query_tx_response(
        client.clone(),
        TxEventQuery {
            hash: hash.clone(),
            event_type: TendermintEventType::Applied,
        },
    )
    .await;

    match tx_response {
        Ok(tx_response) => Ok(TxQueryResult {
            response: tx_response,
            event_type: TendermintEventType::Applied,
        }),
        Err(e) => {
            // If this fails then instead look for an acceptance event (only
            // ABCI++)
            #[cfg(not(feature = "ABCI"))]
            {
                let tx_response = query_tx_response(
                    client,
                    TxEventQuery {
                        hash: hash.clone(),
                        event_type: TendermintEventType::Accepted,
                    },
                )
                .await?;
                return Ok(TxQueryResult {
                    response: tx_response,
                    event_type: TendermintEventType::Accepted,
                });
            }
            // For ABCI simply return the error
            #[cfg(feature = "ABCI")]
            Err(e)
        }
    }
}

#[cfg(test)]
mod test_rpc {
    use std::collections::HashMap;

    use anoma_proof_of_stake::epoched::EpochedDelta;

    use crate::ledger::pos::types::{
        BasisPoints, Bond, Epoch as PosEpoch, Slash, SlashType, Unbond,
    };
    use crate::ledger::rpc;
    use crate::types::storage::Epoch;
    use crate::types::token::Amount;

    struct RpcTestCtx {
        delta: Amount,
        epochs: Vec<PosEpoch>,
        slashes: Vec<Slash>,
    }

    fn test_prelude() -> RpcTestCtx {
        let delta = Amount::from(1_000);
        let e1 = PosEpoch::from(1);
        let e2 = PosEpoch::from(2);
        let e3 = PosEpoch::from(5);

        let s1 = Slash {
            epoch: e1,
            block_height: 100,
            r#type: SlashType::DuplicateVote,
            rate: BasisPoints::new(20),
        };

        let s2 = Slash {
            epoch: e2,
            block_height: 121,
            r#type: SlashType::DuplicateVote,
            rate: BasisPoints::new(15),
        };

        let s3 = Slash {
            epoch: e3,
            block_height: 157,
            r#type: SlashType::LightClientAttack,
            rate: BasisPoints::new(75),
        };

        RpcTestCtx {
            delta,
            epochs: vec![e1, e2, e3],
            slashes: vec![s1, s2, s3],
        }
    }

    #[test]
    fn test_apply_slashes() {
        let ctx = test_prelude();

        // No withdraw epoch
        let mut res =
            rpc::apply_slashes(&ctx.slashes, ctx.delta, ctx.epochs[0], None);
        assert_eq!(res, Amount::from(990));

        // Withdraw epoch
        res = rpc::apply_slashes(
            &ctx.slashes,
            ctx.delta,
            ctx.epochs[0],
            Some(ctx.epochs[1]),
        );
        assert_eq!(res, Amount::from(998));

        // Exclude first slash
        res = rpc::apply_slashes(&ctx.slashes, ctx.delta, ctx.epochs[1], None);
        assert_eq!(res, Amount::from(992));

        // Case null initial delta
        res = rpc::apply_slashes(
            &ctx.slashes,
            Amount::from(0),
            ctx.epochs[0],
            None,
        );
        assert_eq!(res, Amount::from(0));
    }

    #[test]
    fn test_process_bonds() {
        let ctx = test_prelude();

        let mut map = HashMap::new();

        for epoch in ctx.epochs {
            map.insert(epoch, ctx.delta);
        }

        let bonds = EpochedDelta::init_at_genesis(
            Bond { deltas: map },
            PosEpoch::from(0),
        );

        // Consider all Slashes
        let (total, total_active) = rpc::process_bonds_query(
            &bonds,
            &ctx.slashes,
            Epoch(8),
            None,
            None,
        );
        assert_eq!(total, Amount::from(2_975));
        assert_eq!(total_active, Amount::from(2_975));

        // Smaller Epoch than slashes
        let (total, total_active) = rpc::process_bonds_query(
            &bonds,
            &ctx.slashes,
            Epoch(2),
            None,
            None,
        );
        assert_eq!(total, Amount::from(2_975));
        assert_eq!(total_active, Amount::from(990));

        // Non-zero initial total and total_active
        let (total, total_active) = rpc::process_bonds_query(
            &bonds,
            &ctx.slashes,
            Epoch(2),
            Some(Amount::from(15)),
            Some(Amount::from(3)),
        );
        assert_eq!(total, Amount::from(2_975 + 15));
        assert_eq!(total_active, Amount::from(990 + 3));
    }

    #[test]
    fn test_process_unbonds() {
        let ctx = test_prelude();

        let mut map = HashMap::new();

        for epoch in ctx.epochs {
            map.insert((epoch, epoch + 10u64), ctx.delta);
        }

        let unbonds = EpochedDelta::init_at_genesis(
            Unbond { deltas: map },
            PosEpoch::from(0),
        );

        // Epoch less than withdrawable epoch
        let (total, total_active) = rpc::process_unbonds_query(
            &unbonds,
            &ctx.slashes,
            Epoch(8),
            None,
            None,
        );
        assert_eq!(total, Amount::from(2_975));
        assert_eq!(total_active, Amount::from(0));

        // Epoch greather than first twos withdrawable epoch
        let (total, total_active) = rpc::process_unbonds_query(
            &unbonds,
            &ctx.slashes,
            Epoch(13),
            None,
            None,
        );
        assert_eq!(total, Amount::from(2_975));
        assert_eq!(total_active, Amount::from(1_982));

        // Non-zero initial total and total_active
        let (total, total_active) = rpc::process_unbonds_query(
            &unbonds,
            &ctx.slashes,
            Epoch(100),
            Some(Amount::from(15)),
            Some(Amount::from(3)),
        );
        assert_eq!(total, Amount::from(2_975 + 15));
        assert_eq!(total_active, Amount::from(2_975 + 3));
    }
}
