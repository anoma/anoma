use std::collections::VecDeque;
use std::rc::Rc;

use anoma::ledger::storage::{StorageHasher, DB, DBIter};
use anoma::types::address::Address;
use anoma::types::key::dkg_session_keys::DkgKeypair;
use anoma::types::transaction::WrapperTx;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use ark_std::rand::SeedableRng;
use borsh::{BorshDeserialize, BorshSerialize};
use ferveo::dkg::{Params as DkgParams, PubliclyVerifiableDkg};
use ferveo::{TendermintValidator, ValidatorSet};

use super::Shell;
use crate::wallet::ValidatorData;

pub type DkgStateMachine =
    PubliclyVerifiableDkg<anoma::types::transaction::EllipticCurve>;

#[derive(Debug)]
pub(super) enum ShellMode {
    Validator {
        data: ValidatorData,
        next_dkg_keypair: Option<DkgKeypair>,
    },
    Full,
    Seed,
}

impl ShellMode {
    /// Get the validator address if ledger is in validator mode
    pub fn get_validator_address(&self) -> Option<&Address> {
        match &self {
            ShellMode::Validator { data, .. } => Some(&data.address),
            _ => None,
        }
    }

    /// Get the queued DKG pair if it exists
    pub fn get_next_dkg_keypair(&self) -> Option<&DkgKeypair> {
        match &self {
            ShellMode::Validator {
                next_dkg_keypair, ..
            } => next_dkg_keypair.as_ref(),
            _ => None,
        }
    }
}

#[derive(Default, Debug, Clone, BorshDeserialize, BorshSerialize)]
/// Wrapper txs to be decrypted in the next block proposal
pub(super) struct TxQueue {
    /// Index of next wrapper_tx to fetch from storage
    next_wrapper: usize,
    /// The actual wrappers
    queue: VecDeque<WrapperTx>,
}

impl TxQueue {
    /// Add a new wrapper at the back of the queue
    pub fn push(&mut self, wrapper: WrapperTx) {
        self.queue.push_back(wrapper);
    }

    /// Remove the wrapper at the head of the queue
    pub fn pop(&mut self) -> Option<WrapperTx> {
        self.queue.pop_front()
    }

    /// Iterate lazily over the queue
    #[allow(dead_code)]
    pub fn next(&mut self) -> Option<&WrapperTx> {
        let next = self.queue.get(self.next_wrapper);
        if self.next_wrapper < self.queue.len() {
            self.next_wrapper += 1;
        }
        next
    }

    /// Reset the iterator to the head of the queue
    pub fn rewind(&mut self) {
        self.next_wrapper = 0;
    }

    /// Get an iterator over the queue
    #[allow(dead_code)]
    pub fn iter(&self) -> impl std::iter::Iterator<Item = &WrapperTx> {
        self.queue.iter()
    }

    /// Check if there are any txs in the queue
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}

/// Holds the DKG state machine
#[derive(Debug)]
pub(super) struct DkgInstance {
    pub state_machine: DkgStateMachine,
}

impl Default for DkgInstance {
    fn default() -> Self {
        let rng = &mut ark_std::rand::prelude::StdRng::from_entropy();
        let validator = TendermintValidator {
            power: 0,
            address: "".into(),
        };
        DkgInstance {
            state_machine: DkgStateMachine::new(
                ValidatorSet::new(vec![validator.clone()]),
                DkgParams {
                    tau: 0,
                    security_threshold: 0,
                    total_weight: 0,
                },
                validator,
                rng,
            )
            .expect("Constructing default DKG should not fail"),
        }
    }
}

impl borsh::ser::BorshSerialize for DkgInstance {
    fn serialize<W: std::io::Write>(
        &self,
        writer: &mut W,
    ) -> std::io::Result<()> {
        let buf = Vec::<u8>::new();
        let bytes = CanonicalSerialize::serialize(&self.state_machine, buf)
            .map_err(|e| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, e)
            })?;
        BorshSerialize::serialize(&bytes, writer)
    }
}

impl borsh::de::BorshDeserialize for DkgInstance {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        let state_machine: Vec<u8> = BorshDeserialize::deserialize(buf)?;
        Ok(DkgInstance {
            state_machine: CanonicalDeserialize::deserialize(&*state_machine)
                .map_err(|err| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, err)
            })?,
        })
    }
}

/// Used to queue up state transactions that
/// require validation from multiple code paths
pub(super) struct ActionQueue<'a, T, D, H>
where
    T: FnOnce(&'a mut Shell<D, H>) -> (),
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    shell: Rc<&'a mut Shell<D, H>>,
    queue: Vec<T>,
}

impl<'a, T, D, H> ActionQueue<'a, T, D, H>
where
    T: FnOnce(&'a mut Shell<D, H>) -> (),
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    /// create a new queue from a single action
    pub fn new(shell: Rc<&'a mut Shell<D, H>>) -> Self {
        Self {
            shell,
            queue: vec![],
        }
    }

    /// Add a new action to the queue
    pub fn enqueue(&mut self, action: T) {
        self.queue.push(action)
    }

    /// Retrieve the last action in the queue
    pub fn pop(&mut self) -> Option<T> {
        self.queue.pop()
    }

    /// Apply the last function
    pub fn apply_last(&'a mut self) {
        if let Some(action) = self.queue.pop() {
            action(
                Rc::get_mut(&mut self.shell)
                    .expect("No other Rc pointer to shell should exist"),
            )
        }
    }

    /// Apply all functions in the queue
    pub fn apply_all(&'a mut self) {
        for action in self.queue.drain(0..).rev() {
            action(
                Rc::get_mut(&mut self.shell)
                    .expect("No other Rc pointer to shell should exist"),
            )
        };
    }
}
