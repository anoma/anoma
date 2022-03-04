use std::collections::HashMap;
use std::pin::Pin;

use futures::task::{Context, Poll};
use futures_lite::{Stream, StreamExt};
use thiserror::Error;
use tokio::process::Command;
use tokio::sync::mpsc::UnboundedSender;
use web3::api::{EthSubscribe, Namespace, SubscriptionStream};
use web3::transports::ws::WebSocket;
use web3::types::{BlockHeader, FilterBuilder, Log, SyncState, H160};

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
}

pub type Result<T> = std::result::Result<T, Error>;

/// A struct of new Ethereum headers as well as logs from
/// smart contracts we are subscribe to. This are streamed
/// from the Ethereum fullnode and sent via a channel to
/// the ledger.
#[derive(Default)]
pub struct EthPollResult {
    new_header: Option<BlockHeader>,
    new_logs: HashMap<H160, Log>,
}

/// Run the Ethereum fullnode as well as a relayer
/// that sends RPC streams to the ledger. If either
/// stops or an abort signal is sent, these processes
/// are halted.
pub async fn run(
    url: &str,
    smart_contract_addresses: Vec<Vec<H160>>,
    sender: UnboundedSender<EthPollResult>,
    abort_recv: tokio::sync::oneshot::Receiver<
        tokio::sync::oneshot::Sender<()>,
    >,
) -> Result<()> {
    // the geth fullnode process
    let mut ethereum_node = Command::new("geth")
        .args(&["--syncmode", "snap", "--ws", "--ws.api", "web3"])
        .kill_on_drop(true)
        .spawn()
        .map_err(Error::StartUp)?;
    tracing::info!("Ethereum fullnode started");
    // a channel for shutting down the relayer
    let (ethereum_abort_sender, ethereum_abort_recv) =
        tokio::sync::oneshot::channel();
    tokio::select! {
        // run the ethereum fullnode
        status = ethereum_node.wait() => {
            match status {
                Ok(status) => {
                    if status.success() {
                        Ok(())
                    } else {
                        Err(Error::Runtime(status.to_string()))
                    }
                },
                Err(err) => {
                    Err(Error::Runtime(err.to_string()))
                }
            }
        },
        // wait for an abort signal
        resp_sender = abort_recv => {
            match resp_sender {
                Ok(resp_sender) => {
                    tracing::info!("Shutting down Ethereum fullnode...");
                    ethereum_node.kill().await.unwrap();
                    ethereum_abort_sender.send(()).unwrap();
                    resp_sender.send(()).unwrap();

                },
                Err(err) => {
                    tracing::error!("The Ethereum abort sender has unexpectedly dropped: {}", err);
                    tracing::info!("Shutting down Ethereum fullnode...");
                    ethereum_abort_sender.send(()).unwrap();
                    ethereum_node.kill().await.unwrap();
                }
            }
            Ok(())
        }
        // run the relayer
        relayer_resp = ethereum_relayer::run(
            url,
            smart_contract_addresses,
            sender,
            ethereum_abort_recv,
        ) => {
            ethereum_node.kill().await.unwrap();
            relayer_resp
        }
    }
}

/// An async stream for polling the Ethereum fullnode
/// via RPC. It polls the following endpoints:
///
///  * sync: Checks if the fullnode is finished syncing
///  * headers: Checks for new ethereum block headers
///  * logs: Checks for logs from ethereum smart contracts whose address were
///    provided as input
///
/// If the fullnode is syncing, we return Poll::Pending. Otherwise
/// we eagerly return any new headers and logs.
pub struct EthereumPoller {
    /// The subscription for checking if the fullnode is synced
    sync_subscription: SubscriptionStream<WebSocket, SyncState>,
    /// Subscription for getting new block headers
    header_subscription: SubscriptionStream<WebSocket, BlockHeader>,
    /// Subscription for the logs of provided smart contract addresses
    log_subscriptions: Vec<SubscriptionStream<WebSocket, Log>>,
}

impl EthereumPoller {
    /// Starts a new set of subscription streams.
    ///  * `url` should point to the websocket endpoint of the ethereum fullnode
    ///  * `smart_contract_addresses` are the address of smart contracts whose
    ///    logs we wish to see
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
            sync_subscription: eth_subscriber
                .subscribe_syncing()
                .await
                .map_err(Error::Web3)?,
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
        match self.sync_subscription.poll_next(cx) {
            // we first check that we are done syncing
            Poll::Ready(Some(Ok(SyncState::NotSyncing))) => {
                let mut eth_poll_result = EthPollResult::default();
                let mut pending = true;

                // try to poll the next ethereum header
                match self.header_subscription.poll_next(cx) {
                    Poll::Ready(Some(Ok(header))) => {
                        eth_poll_result.new_header = Some(header);
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
            _ => Poll::Pending,
        }
    }
}

/// Runs the process the relays the results of polling the
/// ethereum subscription streams.
mod ethereum_relayer {
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
        mut abort_recv: tokio::sync::oneshot::Receiver<()>,
    ) -> Result<()> {
        let mut eth_poller =
            EthereumPoller::new(url, smart_contract_addresses).await?;
        loop {
            if abort_recv.try_recv().is_ok() {
                return Ok(());
            }
            match eth_poller.next().await {
                Some(poll_result) => sender
                    .send(poll_result)
                    .or(Err(Error::RelayerReceiverDropped))?,
                None => return Err(Error::TerminatedSubscription),
            }
        }
    }
}
