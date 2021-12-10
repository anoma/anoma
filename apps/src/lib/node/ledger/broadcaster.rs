#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::{Client, HttpClient};
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::{Client, HttpClient};
use tokio::sync::mpsc::error::TryRecvError;
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
    pub fn new(
        url: std::net::SocketAddr,
        receiver: UnboundedReceiver<Vec<u8>>,
    ) -> Self {
        Self {
            client: HttpClient::new(url.to_string().as_str()).unwrap(),
            receiver,
        }
    }

    /// Loop forever, forwarding messages over the HTTP client as
    /// they are received from the receiver.
    ///
    /// When shutting down the broadcaster, we stop polling the future
    /// generated here. Note that this means the broadcaster
    /// drop method is not called
    pub async fn run(&mut self) -> Result<(), TryRecvError> {
        loop {
            if let Some(msg) = self.receiver.recv().await {
                let _ = self.client.broadcast_tx_sync(msg.into()).await;
            }
        }
    }
}

/// Run the broadcaster and return an error if it unexpectedly panics
/// in order to gracefully shut down the other services
pub async fn run(mut broadcaster: Broadcaster) -> Result<(), &'static str> {
    let join_handle =
        std::thread::spawn(move || async move { broadcaster.run().await });
    join_handle
        .join()
        .map_err(|_| "Broadcaster shut down unexpectedly. Shutting down node.")
        .map(|_| ())
}
