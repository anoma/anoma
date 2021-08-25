mod filter;
mod matchmaker;
mod mempool;

use anoma::proto::Intent;
use matchmaker::Matchmaker;
use thiserror::Error;
use tokio::sync::mpsc::Receiver;

use crate::types::MatchmakerMessage;

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
#[derive(Debug)]
pub struct GossipIntent {
    pub matchmaker: Option<Matchmaker>,
}

impl GossipIntent {
    /// Create a new gossip intent app based on the config given in parameter.
    pub fn new(
        config: &crate::config::IntentGossiper,
    ) -> Result<(Self, Option<Receiver<MatchmakerMessage>>)> {
        let (matchmaker, matchmaker_event_receiver) = if let Some(matchmaker) =
            &config.matchmaker
        {
            let (matchmaker, matchmaker_event_receiver) =
                Matchmaker::new(matchmaker).map_err(Error::MatchmakerInit)?;
            (Some(matchmaker), Some(matchmaker_event_receiver))
        } else {
            (None, None)
        };
        Ok((Self { matchmaker }, matchmaker_event_receiver))
    }

    /// Apply the matchmaker logic on a new intent. Return Some(Ok(True)) if a
    /// transaction have been crafted.
    fn apply_matchmaker(&mut self, intent: Intent) -> Option<Result<bool>> {
        self.matchmaker.as_mut().map(|matchmaker| {
            matchmaker
                .try_match_intent(&intent)
                .map_err(Error::Matchmaker)
        })
    }

    // Apply the logic to a new intent. It only tries to apply the matchmaker if
    // this one exists. If no matchmaker then returns true.
    pub fn apply_intent(&mut self, intent: Intent) -> Result<bool> {
        self.apply_matchmaker(intent).unwrap_or(Ok(true))
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
