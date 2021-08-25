//! Proof-of-Stake integration as a native validity predicate

use std::collections::HashSet;

pub use anoma_proof_of_stake;
pub use anoma_proof_of_stake::parameters::PosParams;
pub use anoma_proof_of_stake::types::{
    self, TotalVotingPowers, ValidatorStates, ValidatorVotingPowers,
};
use anoma_proof_of_stake::validation::validate;
use anoma_proof_of_stake::{validation, PoSBase, PoSReadOnly};
use borsh::BorshDeserialize;
use itertools::Itertools;
use thiserror::Error;

use super::storage::types::{decode, encode};
use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::{self, Address, InternalAddress};
use crate::types::storage::{DbKeySeg, Epoch, Key, KeySeg};
use crate::types::{key, token};

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
}

/// PoS functions result
pub type Result<T> = std::result::Result<T, Error>;

/// Address of the PoS account implemented as a native VP
const ADDRESS: Address = Address::Internal(InternalAddress::PoS);

/// Address of the staking token (XAN)
pub fn staking_token_address() -> Address {
    address::xan()
}

/// Proof-of-Stake VP
pub struct PoS<'a, DB, H>
where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H>,
}

/// Initialize storage in the genesis block.
pub fn init_genesis_storage<'a, DB, H>(
    storage: &mut Storage<DB, H>,
    params: &'a PosParams,
    validators: impl Iterator<Item = &'a GenesisValidator> + Clone + 'a,
    current_epoch: Epoch,
) where
    DB: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    storage
        .init_genesis(params, validators, current_epoch)
        .expect("Initialize PoS genesis storage")
}

/// Alias for a PoS type with the same name with concrete type parameters
pub type ValidatorConsensusKeys =
    anoma_proof_of_stake::types::ValidatorConsensusKeys<
        key::ed25519::PublicKey,
    >;

/// Alias for a PoS type with the same name with concrete type parameters
pub type ValidatorTotalDeltas =
    anoma_proof_of_stake::types::ValidatorTotalDeltas<token::Change>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type Bonds = anoma_proof_of_stake::types::Bonds<token::Amount>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type Unbonds = anoma_proof_of_stake::types::Unbonds<token::Amount>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type ValidatorSets = anoma_proof_of_stake::types::ValidatorSets<Address>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type BondId = anoma_proof_of_stake::types::BondId<Address>;

/// Alias for a PoS type with the same name with concrete type parameters
pub type GenesisValidator = anoma_proof_of_stake::types::GenesisValidator<
    Address,
    token::Amount,
    key::ed25519::PublicKey,
>;

impl<'a, DB, H> NativeVp for PoS<'a, DB, H>
where
    DB: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::PoS;

    fn validate_tx(
        &self,
        _tx_data: &[u8],
        keys_changed: &HashSet<Key>,
        _verifiers: &HashSet<Address>,
    ) -> Result<bool> {
        use validation::Data;
        use validation::DataUpdate::{self, *};
        use validation::ValidatorUpdate::*;

        let mut changes: Vec<DataUpdate<_, _, _>> = vec![];
        let current_epoch = self.ctx.get_block_epoch()?;
        for key in keys_changed {
            if is_params_key(key) {
                // TODO parameters changes are not yet implemented
                return Ok(false);
            } else if is_validator_set_key(key) {
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    ValidatorSets::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    ValidatorSets::try_from_slice(&bytes[..]).ok()
                });
                changes.push(ValidatorSet(Data { pre, post }));
            } else if let Some(validator) =
                is_validator_staking_reward_address_key(key)
            {
                let pre = self
                    .ctx
                    .read_pre(key)?
                    .and_then(|bytes| Address::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_post(key)?
                    .and_then(|bytes| Address::try_from_slice(&bytes[..]).ok());
                changes.push(Validator {
                    address: validator.clone(),
                    update: StakingRewardAddress(Data { pre, post }),
                });
            } else if let Some(validator) = is_validator_total_deltas_key(key) {
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    ValidatorTotalDeltas::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    ValidatorTotalDeltas::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Validator {
                    address: validator.clone(),
                    update: TotalDeltas(Data { pre, post }),
                });
            } else if let Some(validator) = is_validator_voting_power_key(key) {
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    ValidatorVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    ValidatorVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Validator {
                    address: validator.clone(),
                    update: VotingPowerUpdate(Data { pre, post }),
                });
            } else if let Some(owner) =
                token::is_balance_key(&staking_token_address(), key)
            {
                if owner != &Address::Internal(Self::ADDR) {
                    continue;
                }
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    token::Amount::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    token::Amount::try_from_slice(&bytes[..]).ok()
                });
                changes.push(Balance(Data { pre, post }));
            } else if let Some(bond_id) = is_bond_key(key) {
                let pre = self
                    .ctx
                    .read_pre(key)?
                    .and_then(|bytes| Bonds::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_post(key)?
                    .and_then(|bytes| Bonds::try_from_slice(&bytes[..]).ok());
                changes.push(Bond {
                    id: bond_id.clone(),
                    data: Data { pre, post },
                });
            } else if let Some(unbond_id) = is_unbond_key(key) {
                let pre = self
                    .ctx
                    .read_pre(key)?
                    .and_then(|bytes| Unbonds::try_from_slice(&bytes[..]).ok());
                let post = self
                    .ctx
                    .read_post(key)?
                    .and_then(|bytes| Unbonds::try_from_slice(&bytes[..]).ok());
                changes.push(Unbond {
                    id: unbond_id.clone(),
                    data: Data { pre, post },
                });
            } else if is_total_voting_power_key(key) {
                let pre = self.ctx.read_pre(key)?.and_then(|bytes| {
                    TotalVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                let post = self.ctx.read_post(key)?.and_then(|bytes| {
                    TotalVotingPowers::try_from_slice(&bytes[..]).ok()
                });
                changes.push(TotalVotingPower(Data { pre, post }));
            } else {
                tracing::info!("PoS unrecognized key change {} rejected", key);
                return Ok(false);
            }
        }

        let params = self.read_params();
        let errors = validate(&params, changes, current_epoch);
        Ok(if errors.is_empty() {
            true
        } else {
            tracing::info!(
                "PoS validation errors:\n - {}",
                errors.iter().format("\n - ")
            );
            false
        })
    }
}

const PARAMS_STORAGE_KEY: &str = "params";
const VALIDATOR_STORAGE_PREFIX: &str = "validator";
const VALIDATOR_ADDRESS_RAW_HASH: &str = "address_raw_hash";
const VALIDATOR_STAKING_REWARD_ADDRESS_STORAGE_KEY: &str =
    "staking_reward_address";
const VALIDATOR_CONSENSUS_KEY_STORAGE_KEY: &str = "consensus_key";
const VALIDATOR_STATE_STORAGE_KEY: &str = "state";
const VALIDATOR_TOTAL_DELTAS_STORAGE_KEY: &str = "total_deltas";
const VALIDATOR_VOTING_POWER_STORAGE_KEY: &str = "voting_power";
const BOND_STORAGE_KEY: &str = "bond";
const UNBOND_STORAGE_KEY: &str = "unbond";
const VALIDATOR_SET_STORAGE_KEY: &str = "validator_set";
const TOTAL_VOTING_POWER_STORAGE_KEY: &str = "total_voting_power";

/// Storage key for PoS parameters.
pub fn params_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&PARAMS_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for PoS parameters?
pub fn is_params_key(key: &Key) -> bool {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(key)]
            if addr == &ADDRESS && key == PARAMS_STORAGE_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Storage key prefix for validator data.
fn validator_prefix(validator: &Address) -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&VALIDATOR_STORAGE_PREFIX.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&validator.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator's address raw hash for look-up from raw hash of an
/// address to address.
pub fn validator_address_raw_hash_key(raw_hash: impl AsRef<str>) -> Key {
    let raw_hash = raw_hash.as_ref().to_owned();
    Key::from(ADDRESS.to_db_key())
        .push(&VALIDATOR_ADDRESS_RAW_HASH.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&raw_hash)
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator's staking reward address.
pub fn validator_staking_reward_address_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_STAKING_REWARD_ADDRESS_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for validator's staking reward address?
pub fn is_validator_staking_reward_address_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), DbKeySeg::AddressSeg(validator), DbKeySeg::StringSeg(key)]
            if addr == &ADDRESS
                && prefix == VALIDATOR_STORAGE_PREFIX
                && key == VALIDATOR_STAKING_REWARD_ADDRESS_STORAGE_KEY =>
        {
            Some(validator)
        }
        _ => None,
    }
}

/// Storage key for validator's consensus key.
pub fn validator_consensus_key_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_CONSENSUS_KEY_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for validator's consensus key?
pub fn is_validator_consensus_key_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), DbKeySeg::AddressSeg(validator), DbKeySeg::StringSeg(key)]
            if addr == &ADDRESS
                && prefix == VALIDATOR_STORAGE_PREFIX
                && key == VALIDATOR_CONSENSUS_KEY_STORAGE_KEY =>
        {
            Some(validator)
        }
        _ => None,
    }
}

/// Storage key for validator's state.
pub fn validator_state_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_STATE_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for validator's state?
pub fn is_validator_state_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), DbKeySeg::AddressSeg(validator), DbKeySeg::StringSeg(key)]
            if addr == &ADDRESS
                && prefix == VALIDATOR_STORAGE_PREFIX
                && key == VALIDATOR_STATE_STORAGE_KEY =>
        {
            Some(validator)
        }
        _ => None,
    }
}

/// Storage key for validator's total deltas.
pub fn validator_total_deltas_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_TOTAL_DELTAS_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for validator's total deltas?
pub fn is_validator_total_deltas_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), DbKeySeg::AddressSeg(validator), DbKeySeg::StringSeg(key)]
            if addr == &ADDRESS
                && prefix == VALIDATOR_STORAGE_PREFIX
                && key == VALIDATOR_TOTAL_DELTAS_STORAGE_KEY =>
        {
            Some(validator)
        }
        _ => None,
    }
}

/// Storage key for validator's voting power.
pub fn validator_voting_power_key(validator: &Address) -> Key {
    validator_prefix(validator)
        .push(&VALIDATOR_VOTING_POWER_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for validator's voting power?
pub fn is_validator_voting_power_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), DbKeySeg::AddressSeg(validator), DbKeySeg::StringSeg(key)]
            if addr == &ADDRESS
                && prefix == VALIDATOR_STORAGE_PREFIX
                && key == VALIDATOR_VOTING_POWER_STORAGE_KEY =>
        {
            Some(validator)
        }
        _ => None,
    }
}

/// Storage key prefix for all bonds.
pub fn bonds_prefix() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&BOND_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key prefix for all bonds of the given source address.
pub fn bonds_for_source_prefix(source: &Address) -> Key {
    bonds_prefix()
        .push(&source.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Storage key for a bond with the given ID (source and validator).
pub fn bond_key(bond_id: &BondId) -> Key {
    bonds_for_source_prefix(&bond_id.source)
        .push(&bond_id.validator.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for a bond?
pub fn is_bond_key(key: &Key) -> Option<BondId> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), DbKeySeg::AddressSeg(source), DbKeySeg::AddressSeg(validator)]
            if addr == &ADDRESS && prefix == BOND_STORAGE_KEY =>
        {
            Some(BondId {
                source: source.clone(),
                validator: validator.clone(),
            })
        }
        _ => None,
    }
}

/// Storage key prefix for all unbonds.
pub fn unbonds_prefix() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&UNBOND_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key prefix for all unbonds of the given source address.
pub fn unbonds_for_source_prefix(source: &Address) -> Key {
    unbonds_prefix()
        .push(&source.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Storage key for an unbond with the given ID (source and validator).
pub fn unbond_key(bond_id: &BondId) -> Key {
    unbonds_for_source_prefix(&bond_id.source)
        .push(&bond_id.validator.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for a unbond?
pub fn is_unbond_key(key: &Key) -> Option<BondId> {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(prefix), DbKeySeg::AddressSeg(source), DbKeySeg::AddressSeg(validator)]
            if addr == &ADDRESS && prefix == UNBOND_STORAGE_KEY =>
        {
            Some(BondId {
                source: source.clone(),
                validator: validator.clone(),
            })
        }
        _ => None,
    }
}

/// Storage key for validator set (active and inactive).
pub fn validator_set_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&VALIDATOR_SET_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for a validator set?
pub fn is_validator_set_key(key: &Key) -> bool {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(key)]
            if addr == &ADDRESS && key == VALIDATOR_SET_STORAGE_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Storage key for total voting power.
pub fn total_voting_power_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&TOTAL_VOTING_POWER_STORAGE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for total voting power?
pub fn is_total_voting_power_key(key: &Key) -> bool {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(key)]
            if addr == &ADDRESS && key == TOTAL_VOTING_POWER_STORAGE_KEY =>
        {
            true
        }
        _ => false,
    }
}

impl<D, H> PoSReadOnly for PoS<'_, D, H>
where
    D: 'static + storage::DB + for<'iter> storage::DBIter<'iter>,
    H: 'static + StorageHasher,
{
    type Address = Address;
    type PublicKey = key::ed25519::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = ADDRESS;

    fn staking_token_address() -> Self::Address {
        staking_token_address()
    }

    fn read_params(&self) -> PosParams {
        let value = self.ctx.read_pre(&params_key()).unwrap().unwrap();
        decode(value).unwrap()
    }

    fn read_validator_staking_reward_address(
        &self,
        key: &Self::Address,
    ) -> Option<Self::Address> {
        let value = self
            .ctx
            .read_pre(&validator_staking_reward_address_key(key))
            .unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys> {
        let value = self
            .ctx
            .read_pre(&validator_consensus_key_key(key))
            .unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorStates> {
        let value = self.ctx.read_pre(&validator_state_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorTotalDeltas> {
        let value =
            self.ctx.read_pre(&validator_total_deltas_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorVotingPowers> {
        let value =
            self.ctx.read_pre(&validator_voting_power_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_bond(&self, key: &BondId) -> Option<Bonds> {
        let value = self.ctx.read_pre(&bond_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_unbond(&self, key: &BondId) -> Option<Unbonds> {
        let value = self.ctx.read_pre(&unbond_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_set(&self) -> ValidatorSets {
        let value = self.ctx.read_pre(&validator_set_key()).unwrap().unwrap();
        decode(value).unwrap()
    }

    fn read_total_voting_power(&self) -> TotalVotingPowers {
        let value = self
            .ctx
            .read_pre(&total_voting_power_key())
            .unwrap()
            .unwrap();
        decode(value).unwrap()
    }
}

impl<D, H> PoSBase for Storage<D, H>
where
    D: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    type Address = Address;
    type PublicKey = key::ed25519::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = Address::Internal(InternalAddress::PoS);

    fn staking_token_address() -> Self::Address {
        staking_token_address()
    }

    fn read_params(&self) -> PosParams {
        let (value, _gas) = self.read(&params_key()).unwrap();
        decode(value.unwrap()).unwrap()
    }

    fn read_validator_address_raw_hash(
        &self,
        raw_hash: impl AsRef<str>,
    ) -> Option<Self::Address> {
        let key = validator_address_raw_hash_key(raw_hash);
        let (value, _gas) = self.read(&params_key()).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_set(&self) -> ValidatorSets {
        let (value, _gas) = self.read(&validator_set_key()).unwrap();
        decode(value.unwrap()).unwrap()
    }

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys> {
        let (value, _gas) =
            self.read(&validator_consensus_key_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn write_params(&mut self, params: &PosParams) {
        self.write(&params_key(), encode(params)).unwrap();
    }

    fn write_validator_address_raw_hash(&mut self, address: &Self::Address) {
        let raw_hash = address.raw_hash().unwrap();
        self.write(&validator_address_raw_hash_key(raw_hash), encode(address))
            .unwrap();
    }

    fn write_validator_staking_reward_address(
        &mut self,
        key: &Self::Address,
        value: &Self::Address,
    ) {
        self.write(&validator_staking_reward_address_key(key), encode(value))
            .unwrap();
    }

    fn write_validator_consensus_key(
        &mut self,
        key: &Self::Address,
        value: &ValidatorConsensusKeys,
    ) {
        self.write(&validator_consensus_key_key(key), encode(value))
            .unwrap();
    }

    fn write_validator_state(
        &mut self,
        key: &Self::Address,
        value: &ValidatorStates,
    ) {
        self.write(&validator_state_key(key), encode(value))
            .unwrap();
    }

    fn write_validator_total_deltas(
        &mut self,
        key: &Self::Address,
        value: &ValidatorTotalDeltas,
    ) {
        self.write(&validator_total_deltas_key(key), encode(value))
            .unwrap();
    }

    fn write_validator_voting_power(
        &mut self,
        key: &Self::Address,
        value: &ValidatorVotingPowers,
    ) {
        self.write(&validator_voting_power_key(key), encode(value))
            .unwrap();
    }

    fn write_bond(&mut self, key: &BondId, value: &Bonds) {
        self.write(&bond_key(key), encode(value)).unwrap();
    }

    fn write_validator_set(&mut self, value: &ValidatorSets) {
        self.write(&validator_set_key(), encode(value)).unwrap();
    }

    fn write_total_voting_power(&mut self, value: &TotalVotingPowers) {
        self.write(&total_voting_power_key(), encode(value))
            .unwrap();
    }

    fn init_staking_reward_account(
        &mut self,
        address: &Self::Address,
        pk: &Self::PublicKey,
    ) {
        let user_vp =
            std::fs::read("wasm/vp_user.wasm").expect("cannot load user VP");
        // The staking reward accounts are setup with a user VP
        self.write(&Key::validity_predicate(address), user_vp.to_vec())
            .unwrap();

        // Write the public key
        let pk_key = key::ed25519::pk_key(address);
        self.write(&pk_key, encode(pk)).unwrap();
    }

    fn credit_tokens(
        &mut self,
        token: &Self::Address,
        target: &Self::Address,
        amount: Self::TokenAmount,
    ) {
        let key = token::balance_key(token, target);
        let new_balance = match self
            .read(&key)
            .expect("Unable to read token balance for PoS system")
        {
            (Some(balance), _gas) => {
                let balance: Self::TokenAmount =
                    decode(balance).unwrap_or_default();
                balance + amount
            }
            _ => amount,
        };
        self.write(&key, encode(&new_balance))
            .expect("Unable to write token balance for PoS system");
    }
}

impl From<Epoch> for anoma_proof_of_stake::types::Epoch {
    fn from(epoch: Epoch) -> Self {
        let epoch: u64 = epoch.into();
        anoma_proof_of_stake::types::Epoch::from(epoch)
    }
}

impl From<anoma_proof_of_stake::types::Epoch> for Epoch {
    fn from(epoch: anoma_proof_of_stake::types::Epoch) -> Self {
        let epoch: u64 = epoch.into();
        Epoch(epoch)
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}
