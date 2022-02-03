use std::net::ToSocketAddrs;
use std::sync::{Arc, RwLock};

use anoma::proto::{Intent, IntentId};

use super::mempool::IntentMempool;
use super::rpc::matchmakers::{
    MsgFromClient, MsgFromServer, ServerDialer, ServerListener,
};

/// A server for connected matchmakers that can receive intents from the intent
/// gossiper node and send back the results from their filter, if any, or from
/// trying to match them.
#[derive(Debug, Default)]
pub struct MatchmakersServer {
    /// A node listener and its abort receiver. These are consumed once the
    /// listener is started with [`MatchmakersServer::listen`].
    listener: Option<ServerListener>,
    /// Known intents mempool, shared with [`IntentGossiper`].
    mempool: Arc<RwLock<IntentMempool>>,
}

/// Intent gossiper handle can be cloned and is thread safe.
#[derive(Clone, Debug)]
pub struct IntentGossiper {
    /// Known intents mempool, shared with [`MatchmakersServer`].
    mempool: Arc<RwLock<IntentMempool>>,
    /// A dialer can send messages to the connected matchmaker
    dialer: ServerDialer,
}

impl MatchmakersServer {
    /// Create a new gossip intent app with a matchmaker, if enabled.
    pub fn new_pair(
        matchmakers_server_addr: impl ToSocketAddrs,
    ) -> (Self, IntentGossiper) {
        // Prepare a server for matchmakers connections
        let (listener, dialer) =
            ServerListener::new_pair(matchmakers_server_addr);

        let mempool = Arc::new(RwLock::new(IntentMempool::default()));
        let intent_gossiper = IntentGossiper {
            mempool: mempool.clone(),
            dialer,
        };
        (
            Self {
                listener: Some(listener),
                mempool,
            },
            intent_gossiper,
        )
    }

    pub async fn listen(mut self) {
        self.listener
            .take()
            .unwrap()
            .listen(|msg| match msg {
                MsgFromClient::InvalidIntent { id } => {
                    let id = IntentId(id);
                    // Remove matched intents from mempool
                    tracing::info!("Removing matched intent ID {}", id);
                    let mut w_mempool = self.mempool.write().unwrap();
                    w_mempool.remove(&id);
                }
                MsgFromClient::IntentConstraintsTooComplex { id } => {
                    let id = IntentId(id);
                    tracing::info!(
                        "Intent ID {} has constraints that are too complex \
                         for a connected matchmaker",
                        id
                    );
                }
                MsgFromClient::IgnoredIntent { id } => {
                    let id = IntentId(id);
                    tracing::info!(
                        "Intent ID {} ignored by a connected matchmaker",
                        id
                    );
                }
                MsgFromClient::Matched { intent_ids } => {
                    // Remove matched intents from mempool
                    let mut w_mempool = self.mempool.write().unwrap();
                    for id in intent_ids {
                        let id = IntentId(id);
                        tracing::info!("Removing matched intent ID {}", id);
                        w_mempool.remove(&id);
                    }
                }
                MsgFromClient::Unmatched { id } => {
                    let id = IntentId(id);
                    tracing::info!("No match found for intent ID {}", id);
                }
            })
            .await
    }
}

impl IntentGossiper {
    // Apply the logic to a new intent. It only tries to apply the matchmaker if
    // this one exists. If no matchmaker then returns true.
    pub async fn add_intent(&mut self, intent: Intent) {
        let id = intent.id();

        let r_mempool = self.mempool.read().unwrap();
        let is_known = r_mempool.contains(&id);
        drop(r_mempool);
        if !is_known {
            let mut w_mempool = self.mempool.write().unwrap();
            w_mempool.insert(intent.clone());
        }

        tracing::info!(
            "Sending intent ID {} to connected matchmakers, if any",
            id
        );
        self.dialer.send(MsgFromServer::AddIntent {
            id: id.0,
            data: intent.data,
        })
    }
}
