mod filter;
pub mod matchmaker;
mod mempool;

use std::path::Path;
use std::rc::Rc;

use anoma::proto::Intent;
use anoma::types::address::Address;
use anoma::types::key::ed25519::Keypair;
use matchmaker::Matchmaker;
use thiserror::Error;
use tokio::sync::mpsc::{Receiver, Sender};

use self::matchmaker::MatchmakerMessage;

// TODO split Error and Result type in two, one for Result/Error that can only
// happens locally and the other that can happens locally and in the network
#[derive(Error, Debug)]
pub enum Error {
    #[error("Error while decoding intent: {0}")]
    Decode(prost::DecodeError),
    #[error("Error initializing the matchmaker: {0}")]
    MatchmakerInit(matchmaker::Error),
    #[error("Error running the matchmaker: {0}")]
    Matchmaker(matchmaker::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

/// The gossip intent app is mainly useful for the moment when the matchmaker is
/// activated
#[derive(Debug, Default)]
pub struct GossipIntent {
    pub matchmaker: Option<Matchmaker>,
    pub mm_sender: Option<Sender<MatchmakerMessage>>,
    pub mm_receiver: Option<Receiver<MatchmakerMessage>>,
}

impl GossipIntent {
    /// Create a new gossip intent app with a matchmaker, if enabled.
    pub fn new(
        config: &crate::config::IntentGossiper,
        wasm_dir: impl AsRef<Path>,
        tx_source_address: Option<Address>,
        tx_signing_key: Option<Rc<Keypair>>,
    ) -> Result<Self> {
        if let (
            Some(matchmaker),
            Some(tx_source_address),
            Some(tx_signing_key),
        ) = (&config.matchmaker, tx_source_address, tx_signing_key)
        {
            let (mm, mm_sender, mm_receiver) = Matchmaker::new(
                matchmaker,
                wasm_dir,
                tx_source_address,
                tx_signing_key,
            )
            .map_err(Error::MatchmakerInit)?;
            Ok(Self {
                matchmaker: Some(mm),
                mm_sender: Some(mm_sender),
                mm_receiver: Some(mm_receiver),
            })
        } else {
            Ok(Self::default())
        }
    }

    /// Apply the matchmaker logic on a new intent. Return `Ok(true) if a
    /// transaction have been crafted or if there's no matchmaker.
    async fn apply_matchmaker(&mut self, intent: Intent) -> Result<bool> {
        if let Some(matchmaker) = self.matchmaker.as_mut() {
            matchmaker
                .try_match_intent(&intent)
                .await
                .map_err(Error::Matchmaker)
        } else {
            Ok(true)
        }
    }

    // Apply the logic to a new intent. It only tries to apply the matchmaker if
    // this one exists. If no matchmaker then returns true.
    pub async fn apply_intent(&mut self, intent: Intent) -> Result<bool> {
        self.apply_matchmaker(intent).await
    }

    /// pass the matchmaker message to the matchmaker. If no matchmaker is
    /// define then fail. This case should never happens because only when a
    /// matchmaker exists that it can send message.
    pub async fn handle_mm_message(&mut self, mm_message: MatchmakerMessage) {
        match self.matchmaker.as_mut() {
            Some(mm) => mm.handle_mm_message(mm_message).await,
            None => {
                tracing::error!(
                    "cannot handle mesage {:?} because no matchmaker started",
                    mm_message
                )
            }
        }
    }
}
