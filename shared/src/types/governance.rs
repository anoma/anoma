//! Files defyining the types used in governance.

use std::collections::HashMap;
use std::fmt::Display;
use std::str::FromStr;

use borsh::{BorshDeserialize, BorshSerialize};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::address::Address;
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

#[derive(
    Debug, Clone, BorshSerialize, BorshDeserialize, Serialize, Deserialize,
)]
/// The proposal structure
pub struct Proposal {
    /// The proposal id
    pub id: Option<u64>,
    /// The proposal content
    pub content: HashMap<String, String>,
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
}

impl OfflineProposal {
    /// Create an offline proposal with a signature
    pub fn new(data: Proposal, keypair: &common::SecretKey) -> Self {
        let to_sign = serde_json::to_vec(&data)
            .expect("Conversion to bytes shouldn't fail.");
        let signature = common::SigScheme::sign(keypair, &to_sign);
        Self { data, signature }
    }
}
