#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::{Client, HttpClient};
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::{Client, HttpClient};
use tokio::sync::mpsc::UnboundedReceiver;

/// A service for broadcasting txs via an HTTP client.
/// The receiver is for receiving message payloads for other services
/// to be broadcast.
pub struct Broadcaster {
    client: HttpClient,
    receiver: UnboundedReceiver<Vec<u8>>,
}

impl Broadcaster {
    /// Create a new broadcaster that will send Http messages
    /// over the given url.
    pub fn new(url: &str, receiver: UnboundedReceiver<Vec<u8>>) -> Self {
        Self {
            client: HttpClient::new(format!("http://{}", url).as_str())
                .unwrap(),
            receiver,
        }
    }

    /// Loop forever, braodcasting messages that have been received
    /// by the receiver
    async fn run_loop(&mut self) {
        loop {
            if let Some(msg) = self.receiver.recv().await {
                let _ = self.client.broadcast_tx_sync(msg.into()).await;
            }
        }
    }

    /// Loop until an abort signal is received, forwarding messages over
    /// the HTTP client as they are received from the receiver.
    pub async fn run(
        &mut self,
        abort_recv: tokio::sync::oneshot::Receiver<()>,
    ) {
        tracing::info!("Starting broadcaster.");
        tokio::select! {
            _ = self.run_loop() => {
                tracing::error!("Broadcaster unexpectedly shut down.");
                tracing::info!("Shutting down broadcaster...");
            },
            resp_sender = abort_recv => {
                match resp_sender {
                    Ok(_) => {
                        tracing::info!("Shutting down broadcaster...");
                    },
                    Err(err) => {
                        tracing::error!("The broadcaster abort sender has unexpectedly dropped: {}", err);
                        tracing::info!("Shutting down broadcaster...");
                    }
                }
            }
        }
    }
}
