use std::rc::Rc;

use anoma::gossip::mm::MmHost;
use anoma::proto::{Intent, IntentId, Tx};
use anoma::types::address::Address;
use anoma::types::intent::{IntentTransfers, MatchedExchanges};
use anoma::types::key::ed25519::Keypair;
use anoma::vm::wasm;
use borsh::{BorshDeserialize, BorshSerialize};
use tendermint::net;
use thiserror::Error;
use tokio::sync::mpsc::{channel, Receiver, Sender};

use super::filter::Filter;
use super::mempool::{self, IntentMempool};
use crate::client::tx::broadcast_tx;
use crate::config;
use crate::types::MatchmakerMessage;

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
    /// The WASM host allows the WASM runtime to send messages back to this
    /// matchmaker
    wasm_host: WasmHost,
    /// A source address for transactions created from intents.
    tx_source_address: Address,
    /// A keypair that will be used to sign transactions.
    tx_signing_key: Rc<Keypair>,
}

#[derive(Clone, Debug)]
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
        tx_source_address: Address,
        tx_signing_key: Rc<Keypair>,
    ) -> Result<(Self, Sender<MatchmakerMessage>, Receiver<MatchmakerMessage>)>
    {
        // TODO: find a good number or maybe unlimited channel ?
        let (sender, receiver) = channel(100);
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
                wasm_host: WasmHost(sender.clone()),
                tx_source_address,
                tx_signing_key,
            },
            sender,
            receiver,
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
                let matches =
                    MatchedExchanges::try_from_slice(&tx_data[..]).unwrap();
                let intent_transfers = IntentTransfers {
                    matches,
                    source: self.tx_source_address.clone(),
                };
                let tx_data = intent_transfers.try_to_vec().unwrap();
                let tx =
                    Tx::new(tx_code, Some(tx_data)).sign(&self.tx_signing_key);
                let tx_bytes = tx.to_bytes();

                let response =
                    broadcast_tx(self.ledger_address.clone(), tx_bytes).await;
                match response {
                    Ok(tx_response) => {
                        tracing::info!(
                            "Injected transaction from matchmaker with \
                             result: {:#?}",
                            tx_response
                        );
                    }
                    Err(err) => {
                        tracing::error!(
                            "Matchmaker error in submitting a transaction to \
                             the ledger: {}",
                            err
                        );
                    }
                }
            }
            MatchmakerMessage::RemoveIntents(intents_id) => {
                intents_id.into_iter().for_each(|intent_id| {
                    self.mempool.remove(&IntentId::from(intent_id));
                });
            }
            MatchmakerMessage::UpdateState(mm_data) => {
                self.state = mm_data;
            }
            MatchmakerMessage::ApplyIntent(intent, response_sender) => {
                let result =
                    self.try_match_intent(&intent).unwrap_or_else(|err| {
                        tracing::error!(
                            "Matchmaker error in applying intent {}",
                            err
                        );
                        false
                    });
                response_sender.send(result).unwrap_or_else(|err| {
                    tracing::error!(
                        "Matchmaker error in sending back intent result {}",
                        err
                    )
                });
            }
        }
    }
}
