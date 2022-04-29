//! Files defyining the types used in governance.

use std::collections::BTreeMap;
use std::fmt::{self, Display};
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::address::Address;
use super::hash::Hash;
use super::key::common::{self, Signature};
use super::key::SigScheme;
use super::storage::Epoch;
use super::transaction::governance::InitProposalData;

#[derive(
    Debug,
    Clone,
    PartialEq,
    BorshSerialize,
    BorshDeserialize,
    Serialize,
    Deserialize,
    Eq,
)]
/// The vote for a proposal
pub enum ProposalVote {
    /// Yes
    Yay,
    /// No
    Nay,
}

impl ProposalVote {
    /// Check if a vote is yay
    pub fn is_yay(&self) -> bool {
        match self {
            ProposalVote::Yay => true,
            ProposalVote::Nay => false,
        }
    }
}

impl Display for ProposalVote {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProposalVote::Yay => write!(f, "yay"),
            ProposalVote::Nay => write!(f, "nay"),
        }
    }
}

#[allow(missing_docs)]
#[derive(Debug, Error)]
pub enum ProposalVoteParseError {
    #[error("Invalid vote. Vote shall be yay or nay.")]
    InvalidVote,
}

impl FromStr for ProposalVote {
    type Err = ProposalVoteParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq("yay") {
            Ok(ProposalVote::Yay)
        } else if s.eq("nay") {
            Ok(ProposalVote::Nay)
        } else {
            Err(ProposalVoteParseError::InvalidVote)
        }
    }
}

/// The result of a proposal
pub enum TallyResult {
    /// Proposal was accepted
    Passed,
    /// Proposal was rejected
    Rejected,
    /// Proposal result is unknown
    Unknown,
}

impl fmt::Display for TallyResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TallyResult::Passed => write!(f, "passed"),
            TallyResult::Rejected => write!(f, "rejected"),
            TallyResult::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(
    Debug, Clone, BorshSerialize, BorshDeserialize, Serialize, Deserialize,
)]
/// The proposal structure
pub struct Proposal {
    /// The proposal id
    pub id: Option<u64>,
    /// The proposal content
    pub content: BTreeMap<String, String>,
    /// The proposal author address
    pub author: Address,
    /// The epoch from which voting is allowed
    pub voting_start_epoch: Epoch,
    /// The epoch from which voting is stopped
    pub voting_end_epoch: Epoch,
    /// The epoch from which this changes are executed
    pub grace_epoch: Epoch,
    /// The code containing the storage changes
    pub proposal_code: Option<Vec<u8>>,
}

impl Display for Proposal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "id: {:?}, author: {:?}", self.id, self.author)
    }
}

#[allow(missing_docs)]
#[derive(Debug, Error)]
pub enum ProposalError {
    #[error("Invalid proposal data.")]
    InvalidProposalData,
}

impl TryFrom<Proposal> for InitProposalData {
    type Error = ProposalError;

    fn try_from(proposal: Proposal) -> Result<Self, Self::Error> {
        Ok(InitProposalData {
            id: proposal.id,
            content: proposal.content.try_to_vec().unwrap(),
            author: proposal.author,
            voting_start_epoch: proposal.voting_start_epoch,
            voting_end_epoch: proposal.voting_end_epoch,
            grace_epoch: proposal.grace_epoch,
            proposal_code: proposal.proposal_code,
        })
    }
}

#[derive(
    Debug, Clone, BorshSerialize, BorshDeserialize, Serialize, Deserialize,
)]
/// The offline proposal structure
pub struct OfflineProposal {
    /// The proposal data
    pub data: Proposal,
    /// The signature over proposal data
    pub signature: Signature,
    /// The public key to check the signature
    pub pubkey: common::PublicKey,
}

impl OfflineProposal {
    /// Create an offline proposal with a signature
    pub fn new(
        proposal: Proposal,
        pubkey: common::PublicKey,
        signing_key: &common::SecretKey,
    ) -> Self {
        let proposal_data = serde_json::to_vec(&proposal)
            .expect("Conversion to bytes shouldn't fail.");
        let proposal_data_hash = Hash::sha256(&proposal_data);
        let signature =
            common::SigScheme::sign(signing_key, &proposal_data_hash);
        Self {
            data: proposal,
            signature,
            pubkey,
        }
    }

    /// Check whether the signature is valid or not
    pub fn check_signature(&self) -> bool {
        let proposal_data = serde_json::to_vec(&self.data)
            .expect("Conversion to bytes shouldn't fail.");
        let proposal_data_hash = Hash::sha256(&proposal_data);
        common::SigScheme::verify_signature(
            &self.pubkey,
            &proposal_data_hash,
            &self.signature,
        )
        .is_ok()
    }
}

#[derive(
    Debug, Clone, BorshSerialize, BorshDeserialize, Serialize, Deserialize,
)]
/// The offline proposal structure
pub struct OfflineVote {
    /// The proposal data hash
    pub proposal_hash: Hash,
    /// The proposal vote
    pub vote: ProposalVote,
    /// The signature over proposal data
    pub signature: Signature,
    /// The public key to check the signature
    pub pubkey: common::PublicKey,
}

impl OfflineVote {
    /// Create an offline vote for a proposal
    pub fn new(
        proposal: &OfflineProposal,
        vote: ProposalVote,
        pubkey: common::PublicKey,
        signing_key: &common::SecretKey,
    ) -> Self {
        let proposal_data = serde_json::to_vec(proposal)
            .expect("Conversion to bytes shouldn't fail.");
        let proposal_hash = Hash::sha256(&proposal_data);
        let proposal_hash_data = proposal_hash
            .try_to_vec()
            .expect("Conversion to bytes shouldn't fail.");
        let proposal_vote_data = vote
            .try_to_vec()
            .expect("Conversion to bytes shouldn't fail.");
        let signature = common::SigScheme::sign(
            signing_key,
            &[proposal_hash_data, proposal_vote_data].concat(),
        );
        Self {
            proposal_hash,
            vote,
            signature,
            pubkey,
        }
    }

    /// Check whether the signature is valid or not
    pub fn check_signature(&self) -> bool {
        let proposal_hash_data = self
            .proposal_hash
            .try_to_vec()
            .expect("Conversion to bytes shouldn't fail.");
        let proposal_vote_data = self
            .vote
            .try_to_vec()
            .expect("Conversion to bytes shouldn't fail.");
        common::SigScheme::verify_signature(
            &self.pubkey,
            &[proposal_hash_data, proposal_vote_data].concat(),
            &self.signature,
        )
        .is_ok()
    }
}
