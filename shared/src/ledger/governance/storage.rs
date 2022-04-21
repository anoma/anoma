use super::ADDRESS;
use crate::types::address::Address;
use crate::types::storage::{DbKeySeg, Key, KeySeg};

const PROPOSAL_PREFIX: &str = "proposal";
const PROPOSAL_VOTE: &str = "vote";
const PROPOSAL_AUTHOR: &str = "author";
const PROPOSAL_CONTENT: &str = "content";
const PROPOSAL_START_EPOCH: &str = "start_epoch";
const PROPOSAL_END_EPOCH: &str = "end_epoch";
const PROPOSAL_GRACE_EPOCH: &str = "grace_epoch";
const PROPOSAL_FUNDS: &str = "funds";
const PROPOSAL_CODE: &str = "proposal_code";
const PROPOSAL_COMMITTING_EPOCH: &str = "epoch";

const MIN_PROPOSAL_FUND_KEY: &str = "min_fund";
const MAX_PROPOSAL_CODE_SIZE_KEY: &str = "max_code_size";
const MIN_PROPOSAL_PERIOD_KEY: &str = "min_period";
const MAX_PROPOSAL_CONTENT_SIZE_KEY: &str = "max_content";
const MIN_GRACE_EPOCH_KEY: &str = "min_grace_epoch";
const COUNTER_KEY: &str = "counter";

/// Check if key is inside governance address space
pub fn is_governance_key(key: &Key) -> bool {
    matches!(&key.segments[0], DbKeySeg::AddressSeg(addr) if addr == &ADDRESS)
}

/// Check if a key is a vote key
pub fn is_vote_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(id),
            DbKeySeg::StringSeg(vote),
            DbKeySeg::AddressSeg(_validator_address),
            DbKeySeg::AddressSeg(_address),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && vote == PROPOSAL_VOTE =>
        {
            id.parse::<u64>().is_ok()
        }
        _ => false,
    }
}

/// Check if key is author key
pub fn is_author_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(id),
            DbKeySeg::StringSeg(author),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && author == PROPOSAL_AUTHOR =>
        {
            id.parse::<u64>().is_ok()
        }
        _ => false,
    }
}

/// Check if key is proposal key
pub fn is_proposal_code_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(id),
            DbKeySeg::StringSeg(proposal_code),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && proposal_code == PROPOSAL_CODE =>
        {
            id.parse::<u64>().is_ok()
        }
        _ => false,
    }
}

/// Check if key is grace epoch key
pub fn is_grace_epoch_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(id),
            DbKeySeg::StringSeg(grace_epoch),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && grace_epoch == PROPOSAL_GRACE_EPOCH =>
        {
            id.parse::<u64>().is_ok()
        }
        _ => false,
    }
}

/// Check if key is content key
pub fn is_content_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(id),
            DbKeySeg::StringSeg(content),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && content == PROPOSAL_CONTENT =>
        {
            id.parse::<u64>().is_ok()
        }
        _ => false,
    }
}

/// Check if key is balance key
pub fn is_balance_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(id),
            DbKeySeg::StringSeg(funds),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && funds == PROPOSAL_FUNDS =>
        {
            id.parse::<u64>().is_ok()
        }
        _ => false,
    }
}

/// Check if key is start epoch key
pub fn is_start_epoch_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(id),
            DbKeySeg::StringSeg(start_epoch),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && start_epoch == PROPOSAL_START_EPOCH =>
        {
            id.parse::<u64>().is_ok()
        }
        _ => false,
    }
}

/// Check if key is epoch key
pub fn is_end_epoch_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(id),
            DbKeySeg::StringSeg(end_epoch),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && end_epoch == PROPOSAL_END_EPOCH =>
        {
            id.parse::<u64>().is_ok()
        }
        _ => false,
    }
}

/// Check if key is counter key
pub fn is_counter_key(key: &Key) -> bool {
    match &key.segments[..] {
        [DbKeySeg::AddressSeg(addr), DbKeySeg::StringSeg(counter)]
            if addr == &ADDRESS && counter == COUNTER_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Check if key is a proposal fund parameter key
pub fn is_min_proposal_fund_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(min_funds_param),
        ] if addr == &ADDRESS && min_funds_param == MIN_PROPOSAL_FUND_KEY => {
            true
        }
        _ => false,
    }
}

/// Check if key is a proposal max content parameter key
pub fn is_max_content_size_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(max_content_size_param),
        ] if addr == &ADDRESS
            && max_content_size_param == MAX_PROPOSAL_CONTENT_SIZE_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Check if key is a max proposal size key
pub fn is_max_proposal_code_size_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(max_content_size_param),
        ] if addr == &ADDRESS
            && max_content_size_param == MAX_PROPOSAL_CONTENT_SIZE_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Check if key is a min proposal period param key
pub fn is_min_proposal_period_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(min_proposal_period_param),
        ] if addr == &ADDRESS
            && min_proposal_period_param == MIN_PROPOSAL_PERIOD_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Check if key is a min grace epoch key
pub fn is_min_grace_epoch_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(prefix),
            DbKeySeg::StringSeg(epoch_prefix),
            DbKeySeg::StringSeg(_epoch),
            DbKeySeg::StringSeg(_id),
        ] if addr == &ADDRESS
            && prefix == PROPOSAL_PREFIX
            && epoch_prefix == PROPOSAL_COMMITTING_EPOCH =>
        {
            true
        }
        _ => false,
    }
}

/// Check if key is a commit proposal key
pub fn is_commit_proposal_key(key: &Key) -> bool {
    match &key.segments[..] {
        [
            DbKeySeg::AddressSeg(addr),
            DbKeySeg::StringSeg(min_grace_epoch_param),
        ] if addr == &ADDRESS
            && min_grace_epoch_param == MIN_GRACE_EPOCH_KEY =>
        {
            true
        }
        _ => false,
    }
}

/// Check if key is parameter key
pub fn is_parameter_key(key: &Key) -> bool {
    is_min_proposal_fund_key(key)
        || is_max_content_size_key(key)
        || is_max_proposal_code_size_key(key)
        || is_min_proposal_period_key(key)
        || is_min_grace_epoch_key(key)
}

/// Check if key is start epoch or end epoch key
pub fn is_start_or_end_epoch_key(key: &Key) -> bool {
    is_end_epoch_key(key) || is_start_epoch_key(key)
}

/// Get governance prefix key
pub fn proposal_prefix() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&PROPOSAL_PREFIX.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get key for the minimum proposal fund
pub fn get_min_proposal_fund_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&MIN_PROPOSAL_FUND_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get maximum proposal code size key
pub fn get_max_proposal_code_size_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&MAX_PROPOSAL_CODE_SIZE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get minimum proposal period key
pub fn get_min_proposal_period_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&MIN_PROPOSAL_PERIOD_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get maximum proposal content key
pub fn get_max_proposal_content_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&MAX_PROPOSAL_CONTENT_SIZE_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get min grace epoch proposal key
pub fn get_min_proposal_grace_epoch_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&MIN_GRACE_EPOCH_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get key of proposal ids counter
pub fn get_counter_key() -> Key {
    Key::from(ADDRESS.to_db_key())
        .push(&COUNTER_KEY.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get key of proposal content
pub fn get_content_key(id: u64) -> Key {
    proposal_prefix()
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
        .push(&PROPOSAL_CONTENT.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get key of proposal author
pub fn get_author_key(id: u64) -> Key {
    proposal_prefix()
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
        .push(&PROPOSAL_AUTHOR.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get key of proposal voting start epoch
pub fn get_voting_start_epoch_key(id: u64) -> Key {
    proposal_prefix()
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
        .push(&PROPOSAL_START_EPOCH.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get key of proposal voting end epoch
pub fn get_voting_end_epoch_key(id: u64) -> Key {
    proposal_prefix()
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
        .push(&PROPOSAL_END_EPOCH.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get key of proposal funds
pub fn get_funds_key(id: u64) -> Key {
    proposal_prefix()
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
        .push(&PROPOSAL_FUNDS.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get proposal grace epoch key
pub fn get_grace_epoch_key(id: u64) -> Key {
    proposal_prefix()
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
        .push(&PROPOSAL_GRACE_EPOCH.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get proposal code key
pub fn get_proposal_code_key(id: u64) -> Key {
    proposal_prefix()
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
        .push(&PROPOSAL_CODE.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get the committing proposal key
pub fn get_committing_proposals_key(id: u64, epoch: u64) -> Key {
    proposal_prefix()
        .push(&PROPOSAL_COMMITTING_EPOCH.to_owned())
        .expect("Cannot obtain a storage key")
        .push(&epoch.to_string())
        .expect("Cannot obtain a storage key")
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
}

/// Get proposal vote prefix key
pub fn get_proposal_prefix_key(id: u64) -> Key {
    proposal_prefix()
        .push(&id.to_string())
        .expect("Cannot obtain a storage key")
        .push(&PROPOSAL_VOTE.to_owned())
        .expect("Cannot obtain a storage key")
}

/// Get proposal code key
pub fn get_vote_proposal_key(
    id: u64,
    voter_address: Address,
    delegation_address: Address,
) -> Key {
    get_proposal_prefix_key(id)
        .push(&delegation_address)
        .expect("Cannot obtain a storage key")
        .push(&voter_address)
        .expect("Cannot obtain a storage key")
}

/// Get proposal id from key
pub fn get_id(key: &Key) -> Option<u64> {
    match key.get_at(2) {
        Some(id) => match id {
            DbKeySeg::AddressSeg(_) => None,
            DbKeySeg::StringSeg(res) => res.parse::<u64>().ok(),
        },
        None => None,
    }
}

/// Get the delegation address from vote key
pub fn get_vote_delegation_address(key: &Key) -> Option<&Address> {
    match key.get_at(4) {
        Some(addr) => match addr {
            DbKeySeg::AddressSeg(res) => Some(res),
            DbKeySeg::StringSeg(_) => None,
        },
        None => None,
    }
}

/// Get voter address from vote key
pub fn get_voter_address(key: &Key) -> Option<&Address> {
    match key.get_at(5) {
        Some(addr) => match addr {
            DbKeySeg::AddressSeg(res) => Some(res),
            DbKeySeg::StringSeg(_) => None,
        },
        None => None,
    }
}
