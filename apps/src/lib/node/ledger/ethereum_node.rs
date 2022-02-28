use thiserror::Error;
use tokio::process::Command;
use web3::api::{EthSubscribe, SubscriptionStream, Namespace};
use web3::transports::ws::WebSocket;
use web3::types::{BlockHeader, Log, SyncState};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to start Ethereum fullnode: {0}")]
    StartUp(std::io::Error),
    #[error("{0}")]
    Runtime(String),
    #[error("{0}")]
    Web3(web3::error::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

/// Run the Ethereum fullnode
pub async fn run(
    abort_recv: tokio::sync::oneshot::Receiver<
        tokio::sync::oneshot::Sender<()>,
        >,
) -> Result<()> {
    let ethereum_node = Command::new("geth")
        .args(&["--syncmode", "snap", "--ws", "--ws.api", "web3"])
        .kill_on_drop(true)
        .spawn()
        .map_err(Error::Init)?;
    tracing::info!("Ethereum fullnode started");

    tokio::select! {
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
        resp_sender = abort_recv => {
            match resp_sender {
                Ok(resp_sender) => {
                    tracing::info!("Shutting down Ethereum fullnode...");
                    ethereum_node.kill().await.unwrap();
                    resp_sender.send(()).unwrap();
                },
                Err(err) => {
                    tracing::error!("The Ethereum abort sender has unexpectedly dropped: {}", err);
                    tracing::info!("Shutting down Ethereum fullnode...");
                    tendermint_node.kill().await.unwrap();
                }
            }
            Ok(())
        }
    }
}

pub struct EthereumRelayer {
    sync_subscription: SubscriptionStream<WebSocket, SyncState>,
    header_subscription: SubscriptionStream<WebSocket, BlockHeader>,
    logs_subscription: SubscriptionStream<WebSocket, Log>,
}

impl EthereumRelayer {

    pub async fn new(url: &str) -> Result<Self> {
        let websocket = WebSocket::new(url)
            .await
            .map_err(Error::Web3)?;
        let eth_subscriber = EthSubscribe::new(websocket);
        Ok(Self {
            sync_subscription: eth_subscriber
                .subscribe_syncing()
                .await
                .map_err(Error::Web3)?,
            header_subscription: eth_subscriber
                .subscribe_new_heads()
                .await
                .map_err(Error::Web3)?,
            logs_subscription: eth_subscriber
                .subscribe_logs()
                .await
                .map_err(Error::Web3)?,
        })
    }
}