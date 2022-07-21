//! Proof-of-Stake storage keys and storage integration via [`PosBase`] trait.

use anoma_proof_of_stake::parameters::PosParams;
use anoma_proof_of_stake::types::{
    TotalVotingPowers, ValidatorStates, ValidatorVotingPowers,
};
use anoma_proof_of_stake::{types, PosBase};

use super::{
    BondId, Bonds, ValidatorConsensusKeys, ValidatorSets, ValidatorTotalDeltas,
    ADDRESS,
};
use crate::ledger::storage::types::{decode, encode};
use crate::ledger::storage::{self, Storage, StorageHasher};
use crate::types::address::Address;
use crate::types::storage::{DbKeySeg, Key, KeySeg};
use crate::types::{key, token};

const PARAMS_STORAGE_KEY: &str = "params";
const VALIDATOR_STORAGE_PREFIX: &str = "validator";
const VALIDATOR_ADDRESS_RAW_HASH: &str = "address_raw_hash";
const VALIDATOR_STAKING_REWARD_ADDRESS_STORAGE_KEY: &str =
    "staking_reward_address";
const VALIDATOR_CONSENSUS_KEY_STORAGE_KEY: &str = "consensus_key";
const VALIDATOR_STATE_STORAGE_KEY: &str = "state";
const VALIDATOR_TOTAL_DELTAS_STORAGE_KEY: &str = "total_deltas";
const VALIDATOR_VOTING_POWER_STORAGE_KEY: &str = "voting_power";
const SLASHES_PREFIX: &str = "slash";
const BOND_STORAGE_KEY: &str = "bond";
const UNBOND_STORAGE_KEY: &str = "unbond";
const VALIDATOR_SET_STORAGE_KEY: &str = "validator_set";
const TOTAL_VOTING_POWER_STORAGE_KEY: &str = "total_voting_power";

/// Is the given key a PoS storage key?
pub fn is_pos_key(key: &Key) -> bool {
    match &key.segments.get(0) {
        Some(DbKeySeg::AddressSeg(addr)) => addr == &ADDRESS,
        _ => false,
    }
}

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

/// Is storage key for validator's address raw hash?
pub fn is_validator_address_raw_hash_key(key: &Key) -> Option<&str> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(raw_hash),
        ] if addr == &ADDRESS && prefix == VALIDATOR_ADDRESS_RAW_HASH => {
            Some(raw_hash)
        }
        _ => None,
    }
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
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::AddressSeg(validator),
            DbKeySeg::StringSeg(key),
        ] if addr == &ADDRESS
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
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::AddressSeg(validator),
            DbKeySeg::StringSeg(key),
        ] if addr == &ADDRESS
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
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::AddressSeg(validator),
            DbKeySeg::StringSeg(key),
        ] if addr == &ADDRESS
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
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::AddressSeg(validator),
            DbKeySeg::StringSeg(key),
        ] if addr == &ADDRESS
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
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::AddressSeg(validator),
            DbKeySeg::StringSeg(key),
        ] if addr == &ADDRESS
            && prefix == VALIDATOR_STORAGE_PREFIX
            && key == VALIDATOR_VOTING_POWER_STORAGE_KEY =>
        {
            Some(validator)
        }
        _ => None,
    }
}

/// Storage prefix for slashes.
pub fn slashes_prefix() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&SLASHES_PREFIX.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Storage key for validator's slashes.
pub fn validator_slashes_key(validator: &Address) -> Key {
    slashes_prefix()
        .push(&validator.to_db_key())
        .expect("Cannot obtain a storage key")
}

/// Is storage key for validator's slashes?
pub fn is_validator_slashes_key(key: &Key) -> Option<&Address> {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::AddressSeg(validator),
        ] if addr == &ADDRESS && prefix == SLASHES_PREFIX => Some(validator),
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
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::AddressSeg(source),
            DbKeySeg::AddressSeg(validator),
        ] if addr == &ADDRESS && prefix == BOND_STORAGE_KEY => Some(BondId {
            source: source.clone(),
            validator: validator.clone(),
        }),
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
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::AddressSeg(source),
            DbKeySeg::AddressSeg(validator),
        ] if addr == &ADDRESS && prefix == UNBOND_STORAGE_KEY => Some(BondId {
            source: source.clone(),
            validator: validator.clone(),
        }),
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

/// Get validator address from bond key
pub fn get_validator_address_from_bond(key: &Key) -> Option<Address> {
    match key.get_at(3) {
        Some(segment) => match segment {
            DbKeySeg::AddressSeg(addr) => Some(addr.clone()),
            DbKeySeg::StringSeg(_) => None,
        },
        None => None,
    }
}

impl<D, H> PosBase for Storage<D, H>
where
    D: storage::DB + for<'iter> storage::DBIter<'iter>,
    H: StorageHasher,
{
    type Address = Address;
    type PublicKey = key::common::PublicKey;
    type TokenAmount = token::Amount;
    type TokenChange = token::Change;

    const POS_ADDRESS: Self::Address = super::ADDRESS;
    const POS_SLASH_POOL_ADDRESS: Self::Address = super::SLASH_POOL_ADDRESS;

    fn staking_token_address() -> Self::Address {
        super::staking_token_address()
    }

    fn read_pos_params(&self) -> PosParams {
        let (value, _gas) = self.read(&params_key()).unwrap();
        decode(value.unwrap()).unwrap()
    }

    fn read_validator_address_raw_hash(
        &self,
        raw_hash: impl AsRef<str>,
    ) -> Option<Self::Address> {
        let (value, _gas) = self
            .read(&validator_address_raw_hash_key(raw_hash))
            .unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_consensus_key(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorConsensusKeys> {
        let (value, _gas) =
            self.read(&validator_consensus_key_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_state(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorStates> {
        let (value, _gas) = self.read(&validator_state_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_total_deltas(
        &self,
        key: &Self::Address,
    ) -> Option<types::ValidatorTotalDeltas<Self::TokenChange>> {
        let (value, _gas) =
            self.read(&validator_total_deltas_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_voting_power(
        &self,
        key: &Self::Address,
    ) -> Option<ValidatorVotingPowers> {
        let (value, _gas) =
            self.read(&validator_voting_power_key(key)).unwrap();
        value.map(|value| decode(value).unwrap())
    }

    fn read_validator_slashes(&self, key: &Self::Address) -> types::Slashes {
        let (value, _gas) = self.read(&validator_slashes_key(key)).unwrap();
        value
            .map(|value| decode(value).unwrap())
            .unwrap_or_default()
    }

    fn read_validator_set(&self) -> ValidatorSets {
        let (value, _gas) = self.read(&validator_set_key()).unwrap();
        decode(value.unwrap()).unwrap()
    }

    fn read_total_voting_power(&self) -> TotalVotingPowers {
        let (value, _gas) = self.read(&total_voting_power_key()).unwrap();
        decode(value.unwrap()).unwrap()
    }

    fn write_pos_params(&mut self, params: &PosParams) {
        self.write(&params_key(), encode(params)).unwrap();
    }

    fn write_validator_address_raw_hash(
        &mut self,
        address: &Self::Address,
        consensus_key: &Self::PublicKey,
    ) {
        let raw_hash = key::tm_consensus_key_raw_hash(consensus_key);
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

    fn write_validator_slash(
        &mut self,
        validator: &Self::Address,
        value: types::Slash,
    ) {
        let mut slashes = self.read_validator_slashes(validator);
        slashes.push(value);
        self.write(&validator_slashes_key(validator), encode(&slashes))
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
        // let user_vp =
        //     std::fs::read("wasm/vp_user.wasm").expect("cannot load user VP");
        // // The staking reward accounts are setup with a user VP
        // self.write(&Key::validity_predicate(address), user_vp.to_vec())
        //     .unwrap();

        // Write the public key
        let pk_key = key::pk_key(address);
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

    fn transfer(
        &mut self,
        token: &Self::Address,
        amount: Self::TokenAmount,
        src: &Self::Address,
        dest: &Self::Address,
    ) {
        let src_key = token::balance_key(token, src);
        let dest_key = token::balance_key(token, dest);
        if let (Some(src_balance), _gas) = self
            .read(&src_key)
            .expect("Unable to read token balance for PoS system")
        {
            let mut src_balance: Self::TokenAmount =
                decode(src_balance).unwrap_or_default();
            if src_balance < amount {
                tracing::error!(
                    "PoS system transfer error, the source doesn't have \
                     sufficient balance. It has {}, but {} is required",
                    src_balance,
                    amount
                );
                return;
            }
            src_balance.spend(&amount);
            let (dest_balance, _gas) = self.read(&dest_key).unwrap_or_default();
            let mut dest_balance: Self::TokenAmount = dest_balance
                .and_then(|b| decode(b).ok())
                .unwrap_or_default();
            dest_balance.receive(&amount);
            self.write(&src_key, encode(&src_balance))
                .expect("Unable to write token balance for PoS system");
            self.write(&dest_key, encode(&dest_balance))
                .expect("Unable to write token balance for PoS system");
        } else {
            tracing::error!(
                "PoS system transfer error, the source has no balance"
            );
        }
    }
}
