use anoma::ledger::storage::{DBIter, StorageHasher, DB};
use anoma::types::address::Address;
use anoma::types::key::dkg_session_keys::DkgKeypair;
use anoma::types::transaction::EllipticCurve;
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use ark_std::rand::SeedableRng;
use borsh::{BorshDeserialize, BorshSerialize};
use ferveo::dkg::{Params as DkgParams, PubliclyVerifiableDkg};
use tokio::sync::mpsc::UnboundedSender;

use super::{Shell, TendermintValidator, ValidatorSet};
use crate::wallet::ValidatorData;

pub type DkgStateMachine = PubliclyVerifiableDkg<EllipticCurve>;

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub(super) enum ShellMode {
    Validator {
        data: ValidatorData,
        next_dkg_keypair: Option<DkgKeypair>,
        dkg: DkgInstance,
        broadcast_sender: UnboundedSender<Vec<u8>>,
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

    /// Set the flag on the DKG state machine that a new epoch
    /// began and it should be updated
    pub fn set_update_dkg(&mut self) {
        if let ShellMode::Validator { dkg, .. } = self {
            dkg.update = true;
        }
    }
}

/// Holds the DKG state machine
#[derive(Debug)]
pub(super) struct DkgInstance {
    pub state_machine: DkgStateMachine,
    pub update: bool,
}

impl Default for DkgInstance {
    fn default() -> Self {
        let rng = &mut ark_std::rand::prelude::StdRng::from_entropy();
        let keypair = ferveo_common::Keypair::new(rng);
        let validator = TendermintValidator {
            power: 0,
            address: "".into(),
            public_key: keypair.public(),
        };
        DkgInstance {
            state_machine: DkgStateMachine::new(
                ValidatorSet::new(vec![validator.clone()]),
                DkgParams {
                    tau: 0,
                    security_threshold: 0,
                    total_weight: 0,
                    retry_after: 2,
                },
                validator,
                keypair,
            )
            .expect("Constructing default DKG should not fail"),
            update: false,
        }
    }
}

impl borsh::ser::BorshSerialize for DkgInstance {
    fn serialize<W: std::io::Write>(
        &self,
        writer: &mut W,
    ) -> std::io::Result<()> {
        let mut buf = Vec::<u8>::new();
        CanonicalSerialize::serialize(&self.state_machine, &mut buf).map_err(
            |e| std::io::Error::new(std::io::ErrorKind::InvalidData, e),
        )?;
        BorshSerialize::serialize(&(buf, self.update), writer)
    }
}

impl borsh::de::BorshDeserialize for DkgInstance {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        let (state_machine, update): (Vec<u8>, bool) =
            BorshDeserialize::deserialize(buf)?;
        Ok(DkgInstance {
            state_machine: CanonicalDeserialize::deserialize(&*state_machine)
                .map_err(|err| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, err)
            })?,
            update,
        })
    }
}

/// Used to queue up state transactions that
/// require validation from multiple code paths
pub(super) struct ActionQueue<T, D, H>
where
    T: FnOnce(&mut Shell<D, H>),
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    queue: Vec<T>,
    phantom_db: std::marker::PhantomData<D>,
    phantom_hasher: std::marker::PhantomData<H>,
}

impl<T, D, H> ActionQueue<T, D, H>
where
    T: FnOnce(&mut Shell<D, H>),
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    /// create a new queue from a single action
    pub fn new() -> Self {
        Self {
            queue: vec![],
            phantom_db: Default::default(),
            phantom_hasher: Default::default(),
        }
    }

    /// Add a new action to the queue
    pub fn enqueue(&mut self, action: T) {
        self.queue.push(action)
    }

    /// Apply all functions in the queue
    pub fn apply_all(&mut self, shell: &mut Shell<D, H>) {
        for action in self.queue.drain(0..).rev() {
            action(shell)
        }
    }
}

impl<T, D, H> Default for ActionQueue<T, D, H>
where
    T: FnOnce(&mut Shell<D, H>),
    D: DB + for<'iter> DBIter<'iter> + Sync + 'static,
    H: StorageHasher + Sync + 'static,
{
    fn default() -> Self {
        Self::new()
    }
}
