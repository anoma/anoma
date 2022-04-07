use std::collections::HashMap;
use std::ffi::OsString;

use anoma::types::ethereum_headers::EthereumHeader;
#[cfg(feature = "eth-fullnode")]
use futures_lite::{Stream, StreamExt};
use thiserror::Error;
use tokio::sync::mpsc::UnboundedSender;
use web3::types::{Log, H160};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to start Ethereum fullnode: {0}")]
    StartUp(std::io::Error),
    #[error("{0}")]
    Runtime(String),
    #[error("{0}")]
    Web3(web3::error::Error),
    #[error("Ethereum subscription stream unexpectedly terminated")]
    TerminatedSubscription,
    #[error(
        "The receiver of the Ethereum relayer messages unexpectedly dropped"
    )]
    RelayerReceiverDropped,
    #[error("Encountered an invalid Ethereum header")]
    InvalidHeader,
    #[error(
        "Could not read Ethereum network to connect to from env var: {0:?}"
    )]
    EthereumNetwork(OsString),
}

pub type Result<T> = std::result::Result<T, Error>;

/// A struct of new Ethereum headers as well as logs from
/// smart contracts we are subscribe to. This are streamed
/// from the Ethereum fullnode and sent via a channel to
/// the ledger.
#[derive(Default)]
pub struct EthPollResult {
    pub new_header: Option<EthereumHeader>,
    pub new_logs: HashMap<H160, Log>,
    pub error: Option<String>,
}

/// Run the Ethereum fullnode as well as a relayer
/// that sends RPC streams to the ledger. If either
/// stops or an abort signal is sent, these processes
/// are halted.
pub async fn run(
    url: &str,
    smart_contract_addresses: Vec<Vec<H160>>,
    mut sender: UnboundedSender<EthPollResult>,
    abort_recv: tokio::sync::oneshot::Receiver<
        tokio::sync::oneshot::Sender<()>,
    >,
) -> Result<()> {
    // start up the ethereum node.
    let mut ethereum_node = EthereumNode::new(url, &mut sender).await?;

    tokio::select! {
        // run the ethereum fullnode
        status = ethereum_node.wait() => status,
        // wait for an abort signal
        resp_sender = abort_recv => {
            match resp_sender {
                Ok(resp_sender) => {
                    tracing::info!("Shutting down Ethereum fullnode...");
                    ethereum_node.kill().await;
                    resp_sender.send(()).unwrap();
                },
                Err(err) => {
                    tracing::error!("The Ethereum abort sender has unexpectedly dropped: {}", err);
                    tracing::info!("Shutting down Ethereum fullnode...");
                    ethereum_node.kill().await;
                }
            }
            Ok(())
        }
        // run the relayer
        relayer_resp = ethereum_channel::run(
            url,
            smart_contract_addresses,
            sender,
        ) => {
            ethereum_node.kill().await;
            relayer_resp
        }
    }
}

#[cfg(feature = "eth-fullnode")]
/// Tools for running a geth fullnode process
pub mod eth_fullnode {
    use std::convert::TryInto;

    use futures_lite::StreamExt;
    use tokio::process::{Child, Command};
    use tokio::sync::mpsc::UnboundedSender;
    use web3::api::{EthSubscribe, Namespace};
    use web3::transports::WebSocket;
    use web3::types::SyncState;

    use super::{Error, EthPollResult, Result};

    /// A handle to a running geth process
    pub struct EthereumNode {
        process: Child,
    }

    /// Read from environment variable which Ethereum
    /// network to connect to. Defaults to mainnet if
    /// no variable is set.
    ///
    /// Returns an error if the env var is defined but not
    /// a valid unicode
    fn get_eth_network() -> Result<Option<String>> {
        match std::env::var("ETHEREUM_NETWORK") {
            Ok(path) => {
                tracing::info!("Connecting to Ethereum network: {}", &path);
                Ok(Some(path))
            }
            Err(std::env::VarError::NotPresent) => {
                tracing::info!("Connecting to Ethereum mainnet");
                Ok(None)
            }
            Err(std::env::VarError::NotUnicode(msg)) => {
                Err(Error::EthereumNetwork(msg))
            }
        }
    }

    impl EthereumNode {
        /// Starts the geth process and returns a handle to it.
        ///
        /// First looks up which network to connect to from an env var.
        /// It then starts the process and waits for it to finish
        /// syncing.
        pub async fn new(
            url: &str,
            sender: &mut UnboundedSender<EthPollResult>,
        ) -> Result<EthereumNode> {
            // the geth fullnode process
            let network = get_eth_network()?;
            let args = match &network {
                Some(network) => vec![
                    "--syncmode",
                    "snap",
                    network.as_str(),
                    "--ws",
                    "--ws.api",
                    "eth",
                ],
                None => vec!["--syncmode", "snap", "--ws", "--ws.api", "eth"],
            };
            let ethereum_node = Command::new("geth")
                .args(&args)
                .kill_on_drop(true)
                .spawn()
                .map_err(Error::StartUp)?;
            tracing::info!("Ethereum fullnode started");

            // it takes a brief amount of time to open up the websocket on
            // geth's end
            std::thread::sleep(std::time::Duration::from_secs(5));

            // we now wait for the full node to sync
            let websocket = WebSocket::new(url).await.map_err(Error::Web3)?;
            let mut sync = EthSubscribe::new(websocket.clone())
                .subscribe_syncing()
                .await
                .map_err(Error::Web3)?;
            let mut headers = EthSubscribe::new(websocket)
                .subscribe_new_heads()
                .await
                .map_err(Error::Web3)?;

            loop {
                match sync.next().await {
                    Some(Ok(sync_state)) => match sync_state {
                        SyncState::Syncing(info) => {
                            tracing::info!(
                                "Syncing Ethereum, at block: {}. Estimated \
                                 highest block: {}",
                                info.current_block,
                                info.highest_block,
                            );
                        }
                        SyncState::NotSyncing => {
                            tracing::info!("Finished syncing");
                            break;
                        }
                    },
                    Some(Err(err)) => {
                        tracing::error!(
                            "Encountered an error while syncing: {}",
                            err
                        );
                    }
                    _ => {}
                }
            }
            let _ = sync.unsubscribe().await;
            // send the latest block header seen to the ledger
            // so that it knows which epoch Ethereum is in.
            // Necessary for the ethash algorithm to verify headers
            let header = loop {
                if let Some(Ok(header)) = headers.next().await {
                    break header;
                }
            };
            let _ = headers.unsubscribe().await;
            if sender
                .send(EthPollResult {
                    new_header: Some(header.try_into().unwrap()),
                    ..Default::default()
                })
                .is_err()
            {
                panic!("The channel from the Ethereum unexpectedly dropped");
            }

            Ok(Self {
                process: ethereum_node,
            })
        }

        /// Wait for the process to finish. If it does,
        /// return the status.
        pub async fn wait(&mut self) -> Result<()> {
            match self.process.wait().await {
                Ok(status) => {
                    if status.success() {
                        Ok(())
                    } else {
                        Err(Error::Runtime(status.to_string()))
                    }
                }
                Err(err) => Err(Error::Runtime(err.to_string())),
            }
        }

        /// Stop the geth process
        pub async fn kill(&mut self) {
            self.process.kill().await.unwrap();
        }
    }
}

#[cfg(feature = "eth-fullnode")]
pub use eth_fullnode::EthereumNode;

#[cfg(not(feature = "eth-fullnode"))]
/// tools for running a mock ethereum fullnode process
pub mod mock_eth_fullnode {
    use anoma::types::hash::Hash;
    use tokio::sync::mpsc::UnboundedSender;

    use super::{EthPollResult, EthereumHeader, Result};

    pub struct EthereumNode;

    impl EthereumNode {
        pub async fn new(
            _: &str,
            sender: &mut UnboundedSender<EthPollResult>,
        ) -> Result<EthereumNode> {
            let header = EthereumHeader {
                hash: Hash([0; 32]),
                parent_hash: Hash([0; 32]),
                number: 0,
                difficulty: 0.into(),
                nonce: Default::default(),
                mix_hash: Hash([0; 32]),
                state_root: Hash([0; 32]),
                transactions_root: Hash([0; 32]),
            };
            if sender
                .send(EthPollResult {
                    new_header: Some(header),
                    ..Default::default()
                })
                .is_err()
            {
                panic!("The channel from the Ethereum unexpectedly dropped");
            }
            Ok(Self {})
        }

        pub async fn wait(&mut self) -> Result<()> {
            std::future::pending().await
        }

        pub async fn kill(&mut self) {}
    }
}

#[cfg(not(feature = "eth-fullnode"))]
pub use mock_eth_fullnode::EthereumNode;

/// Tools for polling the Geth fullnode asynchronously
#[cfg(feature = "eth-fullnode")]
pub mod ethereum_poller {
    use std::convert::TryInto;
    use std::pin::Pin;

    use futures::task::{Context, Poll};
    use web3::api::{EthSubscribe, Namespace, SubscriptionStream};
    use web3::transports::ws::WebSocket;
    use web3::types::{BlockHeader, FilterBuilder};

    use super::*;

    /// An async stream for polling the Ethereum fullnode
    /// via RPC. It polls the following endpoints:
    ///
    ///  * sync: Checks if the fullnode is finished syncing
    ///  * headers: Checks for new ethereum block headers
    ///  * logs: Checks for logs from ethereum smart contracts whose address
    ///    were provided as input
    ///
    /// If the fullnode is syncing, we return Poll::Pending. Otherwise
    /// we eagerly return any new headers and logs.
    #[cfg(feature = "eth-fullnode")]
    pub struct EthereumPoller {
        /// Subscription for getting new block headers
        header_subscription: SubscriptionStream<WebSocket, BlockHeader>,
        /// Subscription for the logs of provided smart contract addresses
        log_subscriptions: Vec<SubscriptionStream<WebSocket, Log>>,
    }

    #[cfg(feature = "eth-fullnode")]
    impl EthereumPoller {
        /// Starts a new set of subscription streams.
        ///  * `url` should point to the websocket endpoint of the ethereum
        ///    fullnode

        ///  * `smart_contract_addresses` are the address of smart contracts
        ///    whose logs we wish to see
        ///
        /// We start three subscriptions after opening the websocket and create
        /// filters for the logs.
        pub async fn new(
            url: &str,
            smart_contract_addresses: Vec<Vec<H160>>,
        ) -> Result<Self> {
            let websocket = WebSocket::new(url).await.map_err(Error::Web3)?;
            let eth_subscriber = EthSubscribe::new(websocket);
            let mut log_subscriptions = vec![];
            for address in smart_contract_addresses {
                let filter = FilterBuilder::default().address(address).build();
                log_subscriptions.push(
                    eth_subscriber
                        .subscribe_logs(filter)
                        .await
                        .map_err(Error::Web3)?,
                );
            }

            Ok(Self {
                header_subscription: eth_subscriber
                    .subscribe_new_heads()
                    .await
                    .map_err(Error::Web3)?,
                log_subscriptions,
            })
        }
    }

    impl Stream for EthereumPoller {
        type Item = EthPollResult;

        fn poll_next(
            mut self: Pin<&mut Self>,
            cx: &mut Context,
        ) -> Poll<Option<Self::Item>> {
            let mut eth_poll_result = EthPollResult::default();
            let mut pending = true;

            // try to poll the next ethereum header
            match self.header_subscription.poll_next(cx) {
                Poll::Ready(Some(Ok(header))) => {
                    if let Ok(header) = header.try_into() {
                        eth_poll_result.new_header = Some(header);
                        pending = false;
                    }
                }
                Poll::Ready(Some(Err(err))) => {
                    eth_poll_result.error =
                        Some(format!("Error in Ethereum header: {:?}", err));
                    pending = false;
                }
                Poll::Ready(None) => return Poll::Ready(None),
                _ => {}
            }

            // poll each log subscription
            for log_subscription in self.log_subscriptions.iter_mut() {
                // try to poll the next log from the Ethereum smart contract
                // address
                match log_subscription.poll_next(cx) {
                    Poll::Ready(Some(Ok(log))) => {
                        eth_poll_result.new_logs.insert(log.address, log);
                        pending = false;
                    }
                    Poll::Ready(Some(Err(err))) => {
                        eth_poll_result.error =
                            Some(format!("Error in Ethereum log: {:?}", err));
                        pending = false;
                    }
                    Poll::Ready(None) => return Poll::Ready(None),
                    _ => {}
                }
            }

            // if any poll returned a result, return it
            if !pending {
                Poll::Ready(Some(eth_poll_result))
            } else {
                Poll::Pending
            }
        }
    }
}

#[cfg(feature = "eth-fullnode")]
pub use ethereum_poller::EthereumPoller;

/// Tools for mocking responses from the Ethereum fullnode
/// Used for testing
#[cfg(not(feature = "eth-fullnode"))]
mod mock_eth_poller {
    use anoma::types::hash::Hash;
    use rand::Rng;

    use super::*;

    /// A mock struct that sends fake Ethereum headers
    /// to the ledger. Used for testing.
    pub struct MockEthereumPoller {
        /// used so that parent hash is correct in mocked Ethereum header
        pub last_hash: Hash,

        /// used so that block number is correct in mocked Ethereum header
        pub last_number: u64,
    }

    impl MockEthereumPoller {
        /// We start from a random hash and block number 0
        /// We use a mock header verifier that checks the first
        /// 10 bytes are all zero.
        pub async fn new(
            _url: &str,
            _smart_contract_addresses: Vec<Vec<H160>>,
        ) -> Result<Self> {
            let mut hash = rand::thread_rng().gen::<[u8; 32]>();
            let _ = (0..10).map(|i| hash[i] = 0).collect::<()>();
            Ok(Self {
                last_hash: Hash(hash),
                last_number: 0,
            })
        }

        /// Each time we poll, we update the hash with a
        /// new random hash and increment the block number
        /// by 1.
        ///
        /// We force the first 10 bytes of the random hash
        /// to be zero. This is the criteria the mock verifier uses.
        ///
        /// This way the parent_hash field is always the hash of
        /// the previously seen hash.
        ///
        /// We send a new header once every 2 seconds.
        pub async fn next(&mut self) -> Option<EthPollResult> {
            std::thread::sleep(std::time::Duration::from_secs(2));
            let mut parent_hash = rand::thread_rng().gen::<[u8; 32]>();
            let _ = (0..10).map(|i| parent_hash[i] = 0).collect::<()>();
            let mut parent_hash = Hash(parent_hash);

            std::mem::swap(&mut self.last_hash, &mut parent_hash);
            let new_header = EthereumHeader {
                hash: self.last_hash.clone(),
                parent_hash,
                number: self.last_number,
                difficulty: 0.into(),
                nonce: Default::default(),
                mix_hash: Hash([0; 32]),
                state_root: Hash([0; 32]),
                transactions_root: Hash([0; 32]),
            };
            self.last_number += 1;
            Some(EthPollResult {
                new_header: Some(new_header),
                new_logs: Default::default(),
                error: None,
            })
        }
    }
}

#[cfg(not(feature = "eth-fullnode"))]
pub use mock_eth_poller::MockEthereumPoller as EthereumPoller;

/// Runs the process the relays the results of polling the
/// ethereum subscription streams.
pub mod ethereum_channel {
    use super::*;

    /// Creates a new poller given the websocket url and the smart contract
    /// addresses whose logs we wish to monitor.
    ///
    /// Runs until the abort signal is received, sending any data pulled from
    /// the stream over a channel to the ledger.
    pub async fn run(
        url: &str,
        smart_contract_addresses: Vec<Vec<H160>>,
        sender: UnboundedSender<EthPollResult>,
    ) -> Result<()> {
        let mut eth_poller =
            EthereumPoller::new(url, smart_contract_addresses).await?;
        loop {
            match eth_poller.next().await {
                Some(poll_result) => sender
                    .send(poll_result)
                    .or(Err(Error::RelayerReceiverDropped))?,
                None => {
                    return Err(Error::TerminatedSubscription) as Result<()>;
                }
            };
        }
    }
}
