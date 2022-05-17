//! Helpers for constructing and submitting transactions without having to go
//! via anomac.

use std::time::Duration;

use anoma_apps::client::tendermint_websocket_client::{
    TendermintWebsocketClient, WebSocketAddress,
};
#[cfg(not(feature = "ABCI"))]
use anoma_apps::tendermint::node::info::ListenAddress;
#[cfg(not(feature = "ABCI"))]
use anoma_apps::tendermint_config::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use anoma_apps::tendermint_config_abci::net::Address as TendermintAddress;
#[cfg(feature = "ABCI")]
use anoma_apps::tendermint_stable::node::info::ListenAddress;
use color_eyre::eyre::Result;

const DEFAULT_CONNECTION_TIMEOUT: Duration = Duration::from_secs(60);

pub(crate) fn get_tm_websocket_client(
    rpc_addr: &str,
) -> Result<TendermintWebsocketClient> {
    let listen_addr = ListenAddress::new(rpc_addr.to_string());
    let remote_addr =
        TendermintAddress::from_listen_address(&listen_addr).unwrap();
    let websocket_addr = WebSocketAddress::try_from(remote_addr)?;
    let client = TendermintWebsocketClient::open(
        websocket_addr,
        Some(DEFAULT_CONNECTION_TIMEOUT),
    )?;
    Ok(client)
}
