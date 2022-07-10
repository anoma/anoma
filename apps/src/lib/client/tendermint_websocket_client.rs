use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::net::TcpStream;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use async_trait::async_trait;
#[cfg(not(feature = "ABCI"))]
use tendermint_config::net::Address;
#[cfg(feature = "ABCI")]
use tendermint_config_abci::net::Address;
#[cfg(not(feature = "ABCI"))]
use tendermint_rpc::{
    Client, Error as RpcError, Request, Response, SimpleRequest,
};
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::query::Query;
#[cfg(feature = "ABCI")]
use tendermint_rpc_abci::{
    Client, Error as RpcError, Request, Response, SimpleRequest,
};
use thiserror::Error;
use tokio::time::Instant;
use websocket::result::WebSocketError;
use websocket::{ClientBuilder, Message, OwnedMessage};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Could not convert into websocket address: {0:?}")]
    Address(Address),
    #[error("Websocket Error: {0:?}")]
    Websocket(WebSocketError),
    #[error("Failed to subscribe to the event: {0}")]
    Subscribe(String),
    #[error("Failed to unsubscribe to the event: {0}")]
    Unsubscribe(String),
    #[error("Unexpected response from query: {0:?}")]
    UnexpectedResponse(OwnedMessage),
    #[error("More then one subscription at a time is not supported")]
    AlreadySubscribed,
    #[error("Cannot wait on a response if not subscribed to an event")]
    NotSubscribed,
    #[error("Received an error response: {0}")]
    Response(String),
    #[error("Encountered JSONRPC request/response without an id")]
    MissingId,
    #[error("Connection timed out")]
    ConnectionTimeout,
}

type Json = serde_json::Value;

/// Module that brings in the basic building blocks from tendermint_rpc
/// and adds the necessary functionality and wrappers to them.
mod rpc_types {
    use std::collections::HashMap;
    use std::fmt;
    use std::str::FromStr;

    use serde::{de, Deserialize, Serialize, Serializer};
    #[cfg(not(feature = "ABCI"))]
    use tendermint_rpc::method::Method;
    #[cfg(not(feature = "ABCI"))]
    use tendermint_rpc::query::{EventType, Query};
    #[cfg(not(feature = "ABCI"))]
    use tendermint_rpc::{request, response};
    #[cfg(feature = "ABCI")]
    use tendermint_rpc_abci::method::Method;
    #[cfg(feature = "ABCI")]
    use tendermint_rpc_abci::query::{EventType, Query};
    #[cfg(feature = "ABCI")]
    use tendermint_rpc_abci::{request, response};

    use super::Json;

    #[derive(Debug, Deserialize, Serialize)]
    pub struct RpcRequest {
        #[serde(skip_serializing)]
        method: Method,
        params: HashMap<String, String>,
    }

    #[derive(Debug, Deserialize, Serialize)]
    pub enum SubscribeType {
        Subscribe,
        Unsubscribe,
    }

    #[derive(Debug, Deserialize, Serialize)]
    pub struct RpcSubscription(
        #[serde(skip_serializing)] pub SubscribeType,
        #[serde(serialize_with = "serialize_query")]
        #[serde(deserialize_with = "deserialize_query")]
        pub Query,
    );

    pub(super) fn serialize_query<S>(
        query: &Query,
        serialize: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serialize.serialize_str(&query.to_string())
    }

    pub(super) fn deserialize_query<'de, D>(
        deserializer: D,
    ) -> Result<Query, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct QueryVisitor;

        impl<'de> de::Visitor<'de> for QueryVisitor {
            type Value = Query;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str(
                    "a string of params from a valid Tendermint RPC query",
                )
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match EventType::from_str(v) {
                    Ok(event) => Ok(Query::from(event)),
                    Err(error) => {
                        Err(de::Error::custom(format!("{:?}", error)))
                    }
                }
            }
        }
        deserializer.deserialize_any(QueryVisitor)
    }

    /// This type is required by the tendermint_rs traits but we
    /// cannot use it due to a bug in the RPC responses from
    /// tendermint
    #[derive(Debug, Deserialize, Serialize)]
    #[serde(transparent)]
    pub struct RpcResponse(pub Json);

    impl response::Response for RpcResponse {}

    impl request::Request for RpcRequest {
        type Response = RpcResponse;

        fn method(&self) -> Method {
            self.method
        }
    }

    impl request::Request for RpcSubscription {
        type Response = RpcResponse;

        fn method(&self) -> Method {
            match self.0 {
                SubscribeType::Subscribe => Method::Subscribe,
                SubscribeType::Unsubscribe => Method::Unsubscribe,
            }
        }
    }
}

pub struct WebSocketAddress {
    host: String,
    port: u16,
}

impl TryFrom<Address> for WebSocketAddress {
    type Error = Error;

    fn try_from(value: Address) -> Result<Self, Self::Error> {
        match value {
            Address::Tcp { host, port, .. } => Ok(Self { host, port }),
            _ => Err(Error::Address(value)),
        }
    }
}

impl Display for WebSocketAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ws://{}:{}/websocket", self.host, self.port)
    }
}
#[cfg(feature = "ABCI")]
use rpc_types::{RpcResponse, RpcSubscription, SubscribeType};

/// We need interior mutability since the `perform` method of the `Client`
/// trait from `tendermint_rpc` only takes `&self` as an argument
/// Furthermore, TendermintWebsocketClient must be `Send` since it will be
/// used in async methods
type Websocket = Arc<Mutex<websocket::sync::client::Client<TcpStream>>>;
type ResponseQueue = Arc<Mutex<HashMap<String, String>>>;

#[cfg(feature = "ABCI")]
struct Subscription {
    id: String,
    query: Query,
}

pub struct TendermintWebsocketClient {
    websocket: Websocket,
    #[cfg(feature = "ABCI")]
    subscribed: Option<Subscription>,
    received_responses: ResponseQueue,
    connection_timeout: Duration,
}

impl TendermintWebsocketClient {
    /// Open up a new websocket given a specified URL.
    /// If no `connection_timeout` is given, defaults to 5 minutes.
    pub fn open(
        url: WebSocketAddress,
        connection_timeout: Option<Duration>,
    ) -> Result<Self, Error> {
        match ClientBuilder::new(&url.to_string())
            .unwrap()
            .connect_insecure()
        {
            Ok(websocket) => Ok(Self {
                websocket: Arc::new(Mutex::new(websocket)),
                #[cfg(feature = "ABCI")]
                subscribed: None,
                received_responses: Arc::new(Mutex::new(HashMap::new())),
                connection_timeout: connection_timeout
                    .unwrap_or_else(|| Duration::new(300, 0)),
            }),
            Err(inner) => Err(Error::Websocket(inner)),
        }
    }

    /// Shutdown the client. Can still be reused afterwards
    pub fn close(&mut self) {
        // Even in the case of errors, this will be shutdown
        let _ = self.websocket.lock().unwrap().shutdown();
        #[cfg(feature = "ABCI")]
        {
            self.subscribed = None;
        }
        self.received_responses.lock().unwrap().clear();
    }

    /// Subscribes to an event specified by the query argument.
    #[cfg(feature = "ABCI")]
    pub fn subscribe(&mut self, query: Query) -> Result<(), Error> {
        // We do not support more than one subscription currently
        // This can be fixed by correlating on ids later
        if self.subscribed.is_some() {
            return Err(Error::AlreadySubscribed);
        }
        // send the subscription request
        let message = RpcSubscription(SubscribeType::Subscribe, query.clone())
            .into_json();
        let msg_id = get_id(&message).unwrap();

        self.websocket
            .lock()
            .unwrap()
            .send_message(&Message::text(&message))
            .map_err(Error::Websocket)?;

        // check that the request was received and a success message returned
        match self.process_response(|_| Error::Subscribe(message), None) {
            Ok(_) => {
                self.subscribed = Some(Subscription { id: msg_id, query });
                Ok(())
            }
            Err(err) => Err(err),
        }
    }

    /// Receive a response from the subscribed event or
    /// process the response if it has already been received
    #[cfg(feature = "ABCI")]
    pub fn receive_response(&self) -> Result<Json, Error> {
        if let Some(Subscription { id, .. }) = &self.subscribed {
            let response = self.process_response(
                Error::Response,
                self.received_responses.lock().unwrap().remove(id),
            )?;
            Ok(response)
        } else {
            Err(Error::NotSubscribed)
        }
    }

    /// Unsubscribe from the currently subscribed event
    /// Note that even if an error is returned, the client
    /// will return to an unsubscribed state
    #[cfg(feature = "ABCI")]
    pub fn unsubscribe(&mut self) -> Result<(), Error> {
        match self.subscribed.take() {
            Some(Subscription { query, .. }) => {
                // send the subscription request
                let message =
                    RpcSubscription(SubscribeType::Unsubscribe, query)
                        .into_json();

                self.websocket
                    .lock()
                    .unwrap()
                    .send_message(&Message::text(&message))
                    .map_err(Error::Websocket)?;
                // empty out the message queue. Should be empty already
                self.received_responses.lock().unwrap().clear();
                // check that the request was received and a success message
                // returned
                match self
                    .process_response(|_| Error::Unsubscribe(message), None)
                {
                    Ok(_) => Ok(()),
                    Err(err) => Err(err),
                }
            }
            _ => Err(Error::NotSubscribed),
        }
    }

    /// Process the next response received and handle any exceptions that
    /// may have occurred. Takes a function to map response to an error
    /// as a parameter.
    ///
    /// Optionally, the response may have been received earlier while
    /// handling a different request. In that case, we process it
    /// now.
    #[cfg(feature = "ABCI")]
    fn process_response<F>(
        &self,
        f: F,
        received: Option<String>,
    ) -> Result<Json, Error>
    where
        F: FnOnce(String) -> Error,
    {
        let resp = match received {
            Some(resp) => OwnedMessage::Text(resp),
            None => {
                let mut websocket = self.websocket.lock().unwrap();
                let start = Instant::now();
                loop {
                    if Instant::now().duration_since(start)
                        > self.connection_timeout
                    {
                        tracing::error!(
                            "Websocket connection timed out while waiting for \
                             response"
                        );
                        return Err(Error::ConnectionTimeout);
                    }
                    match websocket.recv_message().map_err(Error::Websocket)? {
                        text @ OwnedMessage::Text(_) => break text,
                        OwnedMessage::Ping(data) => {
                            tracing::debug!(
                                "Received websocket Ping, sending Pong"
                            );
                            websocket
                                .send_message(&OwnedMessage::Pong(data))
                                .unwrap();
                            continue;
                        }
                        OwnedMessage::Pong(_) => {
                            tracing::debug!(
                                "Received websocket Pong, ignoring"
                            );
                            continue;
                        }
                        other => return Err(Error::UnexpectedResponse(other)),
                    }
                }
            }
        };
        match resp {
            OwnedMessage::Text(raw) => RpcResponse::from_string(raw)
                .map(|v| v.0)
                .map_err(|e| f(e.to_string())),
            other => Err(Error::UnexpectedResponse(other)),
        }
    }
}

#[async_trait]
impl Client for TendermintWebsocketClient {
    async fn perform<R>(&self, request: R) -> Result<R::Response, RpcError>
    where
        R: SimpleRequest,
    {
        // send the subscription request
        // Return an empty response if the request fails to send
        let req_json = request.into_json();
        let req_id = get_id(&req_json).unwrap();
        if let Err(error) = self
            .websocket
            .lock()
            .unwrap()
            .send_message(&Message::text(&req_json))
        {
            tracing::info! {
                "Unable to send request: {}\nReceived Error: {:?}",
                &req_json,
                error
            };
            return <R as Request>::Response::from_string("");
        }

        // Return the response if text is returned, else return empty response
        let mut websocket = self.websocket.lock().unwrap();
        let start = Instant::now();
        loop {
            let duration = Instant::now().duration_since(start);
            if duration > self.connection_timeout {
                tracing::error!(
                    "Websocket connection timed out while waiting for response"
                );
                return Err(RpcError::web_socket_timeout(duration));
            }
            let response = match websocket
                .recv_message()
                .expect("Failed to receive message from websocket")
            {
                OwnedMessage::Text(resp) => resp,
                OwnedMessage::Ping(data) => {
                    tracing::debug!("Received websocket Ping, sending Pong");
                    websocket.send_message(&OwnedMessage::Pong(data)).unwrap();
                    continue;
                }
                OwnedMessage::Pong(_) => {
                    tracing::debug!("Received websocket Pong, ignoring");
                    continue;
                }
                other => {
                    tracing::info! {
                        "Received unexpected response to query: {}\nReceived {:?}",
                        &req_json,
                        other
                    };
                    String::from("")
                }
            };
            // Check that we did not accidentally get a response for a
            // subscription. If so, store it for later
            if let Ok(resp_id) = get_id(&response) {
                if resp_id != req_id {
                    self.received_responses
                        .lock()
                        .unwrap()
                        .insert(resp_id, response);
                } else {
                    return <R as Request>::Response::from_string(response);
                }
            } else {
                // got an invalid response, just return nothing
                return <R as Request>::Response::from_string(response);
            };
        }
    }
}

fn get_id(req_json: &str) -> Result<String, Error> {
    if let serde_json::Value::Object(req) =
        serde_json::from_str(req_json).unwrap()
    {
        req.get("id").ok_or(Error::MissingId).map(|v| v.to_string())
    } else {
        Err(Error::MissingId)
    }
}

/// The TendermintWebsocketClient has a basic state machine for ensuring
/// at most one subscription at a time. These tests cover that it
/// works as intended.
///
/// Furthermore, since a client can handle a subscription and a
/// simple request simultaneously, we must test that the correct
/// responses are give for each of the corresponding requests
#[cfg(all(test, feature = "ABCI"))]
mod test_tendermint_websocket_client {
    use std::time::Duration;

    use anoma::types::transaction::hash_tx as hash_tx_bytes;
    use serde::{Deserialize, Serialize};
    #[cfg(feature = "ABCI")]
    use tendermint_rpc_abci::endpoint::abci_info::AbciInfo;
    #[cfg(feature = "ABCI")]
    use tendermint_rpc_abci::query::{EventType, Query};
    #[cfg(feature = "ABCI")]
    use tendermint_rpc_abci::Client;
    #[cfg(feature = "ABCI")]
    use tendermint_stable::abci::transaction;
    use websocket::sync::Server;
    use websocket::{Message, OwnedMessage};

    use crate::client::tendermint_websocket_client::{
        TendermintWebsocketClient, WebSocketAddress,
    };

    #[derive(Debug, Deserialize, Serialize)]
    #[serde(rename_all = "snake_case")]
    pub enum ReqType {
        Subscribe,
        Unsubscribe,
        AbciInfo,
    }

    #[derive(Debug, Deserialize, Serialize)]
    pub struct RpcRequest {
        pub jsonrpc: String,
        pub id: String,
        pub method: ReqType,
        pub params: Option<Vec<String>>,
    }

    fn address() -> WebSocketAddress {
        WebSocketAddress {
            host: "localhost".into(),
            port: 26657,
        }
    }

    #[derive(Default)]
    struct Handle {
        subscription_id: Option<String>,
    }

    impl Handle {
        /// Mocks responses to queries. Fairly arbitrary with just enough
        /// variety to test the TendermintWebsocketClient state machine and
        /// message synchronization
        fn handle(&mut self, msg: String) -> Vec<String> {
            let id = super::get_id(&msg).unwrap();
            let request: RpcRequest = serde_json::from_str(&msg).unwrap();
            match request.method {
                ReqType::Unsubscribe => {
                    self.subscription_id = None;
                    vec![format!(
                        r#"{{"jsonrpc": "2.0", "id": {}, "error": "error"}}"#,
                        id
                    )]
                }
                ReqType::Subscribe => {
                    self.subscription_id = Some(id);
                    let id = self.subscription_id.as_ref().unwrap();
                    if request.params.unwrap()[0]
                        == Query::from(EventType::NewBlock).to_string()
                    {
                        vec![format!(
                            r#"{{"jsonrpc": "2.0", "id": {}, "error": "error"}}"#,
                            id
                        )]
                    } else {
                        vec![format!(
                            r#"{{"jsonrpc": "2.0", "id": {}, "result": {{}}}}"#,
                            id
                        )]
                    }
                }
                ReqType::AbciInfo => {
                    // Mock a subscription result returning on the wire before
                    // the simple request result
                    let info = AbciInfo {
                        last_block_app_hash: transaction::Hash::new(
                            hash_tx_bytes("Testing".as_bytes()).0,
                        )
                        .as_ref()
                        .into(),
                        ..AbciInfo::default()
                    };
                    let resp = serde_json::to_string(&info).unwrap();
                    if let Some(prev_id) = self.subscription_id.take() {
                        vec![
                            format!(
                                r#"{{"jsonrpc": "2.0", "id": {}, "result": {{"subscription": "result!"}}}}"#,
                                prev_id
                            ),
                            format!(
                                r#"{{"jsonrpc": "2.0", "id": {}, "result": {{"response": {}}}}}"#,
                                id, resp
                            ),
                        ]
                    } else {
                        vec![format!(
                            r#"{{"jsonrpc": "2.0", "id": {}, "result": {{"response": {}}}}}"#,
                            id, resp
                        )]
                    }
                }
            }
        }
    }

    /// A mock tendermint node. This is just a basic websocket server
    /// TODO: When the thread drops from scope, we may get an ignorable
    /// panic as we did not shut the loop down. But we should.
    fn start() {
        let node = Server::bind("localhost:26657").unwrap();
        for connection in node.filter_map(Result::ok) {
            std::thread::spawn(move || {
                let mut handler = Handle::default();
                let mut client = connection.accept().unwrap();
                loop {
                    for resp in match client.recv_message().unwrap() {
                        OwnedMessage::Text(msg) => handler.handle(msg),
                        _ => panic!("Unexpected request"),
                    } {
                        let msg = Message::text(resp);
                        let _ = client.send_message(&msg);
                    }
                }
            });
        }
    }

    /// Test that we cannot subscribe to a new event
    /// if we have an active subscription
    #[test]
    fn test_subscribe_twice() {
        std::thread::spawn(start);
        // need to make sure that the mock tendermint node has time to boot up
        std::thread::sleep(std::time::Duration::from_secs(1));
        let mut rpc_client = TendermintWebsocketClient::open(
            address(),
            Some(Duration::new(10, 0)),
        )
        .expect("Client could not start");
        // Check that subscription was successful
        rpc_client.subscribe(Query::from(EventType::Tx)).unwrap();
        assert_eq!(
            rpc_client.subscribed.as_ref().expect("Test failed").query,
            Query::from(EventType::Tx)
        );
        // Check that we cannot subscribe while we still have an active
        // subscription
        assert!(rpc_client.subscribe(Query::from(EventType::Tx)).is_err());
    }

    /// Test that even if there is an error on the protocol layer,
    /// the client still unsubscribes and returns control
    #[test]
    fn test_unsubscribe_even_on_protocol_error() {
        std::thread::spawn(start);
        // need to make sure that the mock tendermint node has time to boot up
        std::thread::sleep(std::time::Duration::from_secs(1));
        let mut rpc_client = TendermintWebsocketClient::open(
            address(),
            Some(Duration::new(10, 0)),
        )
        .expect("Client could not start");
        // Check that subscription was successful
        rpc_client.subscribe(Query::from(EventType::Tx)).unwrap();
        assert_eq!(
            rpc_client.subscribed.as_ref().expect("Test failed").query,
            Query::from(EventType::Tx)
        );
        // Check that unsubscribe was successful even though it returned an
        // error
        assert!(rpc_client.unsubscribe().is_err());
        assert!(rpc_client.subscribed.is_none());
    }

    /// Test that if we unsubscribe from an event, we can
    /// reuse the client to subscribe to a new event
    #[test]
    fn test_subscribe_after_unsubscribe() {
        std::thread::spawn(start);
        // need to make sure that the mock tendermint node has time to boot up
        std::thread::sleep(std::time::Duration::from_secs(1));
        let mut rpc_client = TendermintWebsocketClient::open(
            address(),
            Some(Duration::new(10, 0)),
        )
        .expect("Client could not start");
        // Check that subscription was successful
        rpc_client.subscribe(Query::from(EventType::Tx)).unwrap();
        assert_eq!(
            rpc_client.subscribed.as_ref().expect("Test failed").query,
            Query::from(EventType::Tx)
        );
        // Check that unsubscribe was successful
        let _ = rpc_client.unsubscribe();
        assert!(rpc_client.subscribed.as_ref().is_none());
        // Check that we can now subscribe to new event
        rpc_client.subscribe(Query::from(EventType::Tx)).unwrap();
        assert_eq!(
            rpc_client.subscribed.expect("Test failed").query,
            Query::from(EventType::Tx)
        );
    }

    /// In this test we first subscribe to an event and then
    /// make a simple request.
    ///
    /// The mock node is set up so that while the request is waiting
    /// for its response, it receives the response for the subscription.
    ///
    /// This test checks that methods correctly return the correct
    /// responses.
    #[test]
    fn test_subscription_returns_before_request_handled() {
        std::thread::spawn(start);
        // need to make sure that the mock tendermint node has time to boot up
        std::thread::sleep(std::time::Duration::from_secs(1));
        let mut rpc_client = TendermintWebsocketClient::open(
            address(),
            Some(Duration::new(10, 0)),
        )
        .expect("Client could not start");
        // Check that subscription was successful
        rpc_client.subscribe(Query::from(EventType::Tx)).unwrap();
        assert_eq!(
            rpc_client.subscribed.as_ref().expect("Test failed").query,
            Query::from(EventType::Tx)
        );
        // Check that there are no pending subscription responses
        assert!(rpc_client.received_responses.lock().unwrap().is_empty());
        // If the wrong response is returned, json deserialization will fail the
        // test
        let _ =
            tokio_test::block_on(rpc_client.abci_info()).expect("Test failed");
        // Check that we received the subscription response and it has been
        // stored
        assert!(
            rpc_client
                .received_responses
                .lock()
                .unwrap()
                .contains_key(&rpc_client.subscribed.as_ref().unwrap().id)
        );

        // check that we receive the expected response to the subscription
        let response = rpc_client.receive_response().expect("Test failed");
        assert_eq!(response.to_string(), r#"{"subscription":"result!"}"#);
        // Check that there are no pending subscription responses
        assert!(rpc_client.received_responses.lock().unwrap().is_empty());
    }
}
