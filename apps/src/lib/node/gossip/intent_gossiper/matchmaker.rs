use std::sync::{Arc, Mutex};

use anoma::gossip::mm::MmHost;
use anoma::proto::{Intent, IntentId, Tx};
use anoma::vm::wasm;
use tendermint::net;
use thiserror::Error;
use tokio::sync::mpsc::{channel, Receiver, Sender};

use super::filter::Filter;
use super::mempool::{self, IntentMempool};
use crate::client::tx::broadcast_tx;
use crate::types::MatchmakerMessage;
use crate::{config, wallet};

/// A matchmaker receive intents and tries to find a match with previously
/// received intent.
#[derive(Debug)]
pub struct Matchmaker {
    /// All valid and received intent are saved in this mempool
    mempool: IntentMempool,
    /// Possible filter that filter any received intent.
    filter: Option<Filter>,
    matchmaker_code: Vec<u8>,
    /// The code of the transaction that is going to be send to a ledger.
    tx_code: Vec<u8>,
    /// the matchmaker's state as arbitrary bytes
    state: Vec<u8>,
    /// The ledger address to send any crafted transaction to
    ledger_address: net::Address,
    // TODO this doesn't have to be a mutex as it's just a Sender which is
    // thread-safe
    wasm_host: Arc<Mutex<WasmHost>>,
}

#[derive(Debug)]
struct WasmHost(Sender<MatchmakerMessage>);

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to add intent to mempool: {0}")]
    MempoolFailed(mempool::Error),
    #[error("Failed to run matchmaker prog: {0}")]
    RunnerFailed(wasm::run::Error),
    #[error("Failed to read file: {0}")]
    FileFailed(std::io::Error),
    #[error("Failed to create filter: {0}")]
    FilterInit(super::filter::Error),
    #[error("Failed to run filter: {0}")]
    Filter(super::filter::Error),
}

type Result<T> = std::result::Result<T, Error>;

impl MmHost for WasmHost {
    /// Send a message from the guest program to remove value from the mempool
    fn remove_intents(&self, intents_id: std::collections::HashSet<Vec<u8>>) {
        self.0
            .try_send(MatchmakerMessage::RemoveIntents(intents_id))
            .expect("Sending matchmaker message")
    }

    /// Send a message from the guest program to inject a new transaction to the
    /// ledger
    fn inject_tx(&self, tx_data: Vec<u8>) {
        self.0
            .try_send(MatchmakerMessage::InjectTx(tx_data))
            .expect("Sending matchmaker message")
    }

    /// Send a message from the guest program to update the matchmaker state
    fn update_state(&self, state: Vec<u8>) {
        self.0
            .try_send(MatchmakerMessage::UpdateState(state))
            .expect("Sending matchmaker message")
    }
}

impl Matchmaker {
    /// Create a new matchmaker based on the parameter config.
    pub fn new(
        config: &config::Matchmaker,
    ) -> Result<(Self, Receiver<MatchmakerMessage>)> {
        // TODO: find a good number or maybe unlimited channel ?
        let (inject_mm_message, receiver_mm_message) = channel(100);
        let matchmaker_code =
            std::fs::read(&config.matchmaker).map_err(Error::FileFailed)?;
        let tx_code =
            std::fs::read(&config.tx_code).map_err(Error::FileFailed)?;
        let filter = config
            .filter
            .as_ref()
            .map(Filter::from_file)
            .transpose()
            .map_err(Error::FilterInit)?;

        Ok((
            Self {
                mempool: IntentMempool::new(),
                filter,
                matchmaker_code,
                tx_code,
                state: Vec::new(),
                ledger_address: config.ledger_address.clone(),
                wasm_host: Arc::new(Mutex::new(WasmHost(inject_mm_message))),
            },
            receiver_mm_message,
        ))
    }

    /// Tries to apply the filter or returns true if no filter is define
    fn apply_filter(&self, intent: &Intent) -> Result<bool> {
        self.filter
            .as_ref()
            .map(|f| f.validate(intent))
            .transpose()
            .map(|v| v.unwrap_or(true))
            .map_err(Error::Filter)
    }

    /// add the intent to the matchmaker mempool and tries to find a match for
    /// that intent
    pub fn try_match_intent(&mut self, intent: &Intent) -> Result<bool> {
        if self.apply_filter(intent)? {
            self.mempool
                .put(intent.clone())
                .map_err(Error::MempoolFailed)?;
            Ok(wasm::run::matchmaker(
                &self.matchmaker_code.clone(),
                &self.state,
                &intent.id().0,
                &intent.data,
                self.wasm_host.clone(),
            )
            .map_err(Error::RunnerFailed)
            .unwrap())
        } else {
            Ok(false)
        }
    }

    pub async fn handle_mm_message(&mut self, mm_message: MatchmakerMessage) {
        match mm_message {
            MatchmakerMessage::InjectTx(tx_data) => {
                let tx_code = self.tx_code.clone();
                let keypair = wallet::defaults::matchmaker_keypair();
                let tx = Tx::new(tx_code, Some(tx_data)).sign(&keypair);
                let tx_bytes = tx.to_bytes();

                let response =
                    broadcast_tx(self.ledger_address.clone(), tx_bytes).await;
                println!("{:#?}", response);
            }
            MatchmakerMessage::RemoveIntents(intents_id) => {
                intents_id.into_iter().for_each(|intent_id| {
                    self.mempool.remove(&IntentId::from(intent_id));
                });
            }
            MatchmakerMessage::UpdateState(mm_data) => {
                self.state = mm_data;
            }
        }
    }
}
