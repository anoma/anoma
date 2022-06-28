#[cfg(not(feature = "ABCI"))]
mod tm_jsonrpc {
    use std::convert::TryFrom;
    use std::fmt::{Display, Formatter};
    use std::ops::{Deref, DerefMut};

    use curl::easy::{Easy2, Handler, WriteError};
    use serde::{Deserialize, Serialize};
    use tendermint_config::net::Address as TendermintAddress;
    use tendermint_rpc::query::Query;

    use crate::client::tendermint_rpc_types::{
        parse, Error, EventParams, EventReply, TxResponse,
    };

    /// Maximum number of times we try to send a curl request
    const MAX_SEND_ATTEMPTS: u8 = 10;
    /// Number of events we request from the events log
    const NUM_EVENTS: u64 = 10;

    pub struct JsonRpcAddress<'a> {
        host: &'a str,
        port: u16,
    }

    impl<'a> TryFrom<&'a TendermintAddress> for JsonRpcAddress<'a> {
        type Error = Error;

        fn try_from(value: &'a TendermintAddress) -> Result<Self, Self::Error> {
            match value {
                TendermintAddress::Tcp { host, port, .. } => Ok(Self {
                    host: host.as_str(),
                    port: *port,
                }),
                _ => Err(Error::Address(value.to_string())),
            }
        }
    }

    impl<'a> Display for JsonRpcAddress<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}:{}", self.host, self.port)
        }
    }

    /// The body of a json rpc request
    #[derive(Serialize)]
    pub struct Request {
        /// Method name
        pub method: String,
        /// parameters to give the method
        params: EventParams,
        /// ID of the request
        id: u8,
    }

    impl From<EventParams> for Request {
        fn from(params: EventParams) -> Self {
            Request {
                method: "events".into(),
                params,
                id: 1,
            }
        }
    }

    /// The response we get back from Tendermint
    #[derive(Serialize, Deserialize)]
    pub struct Response {
        /// JSON-RPC version
        jsonrpc: String,
        /// Identifier included in request
        id: u8,
        /// Results of request (if successful)
        result: Option<EventReply>,
        /// Error message if unsuccessful
        error: Option<tendermint_rpc::response_error::ResponseError>,
    }

    impl Response {
        /// Convert the response into a result type
        pub fn into_result(self) -> Result<EventReply, Error> {
            if let Some(e) = self.error {
                Err(Error::Rpc(e))
            } else if let Some(result) = self.result {
                Ok(result)
            } else {
                Err(Error::MalformedJson)
            }
        }
    }

    /// Holds bytes returned in response to curl request
    #[derive(Default)]
    pub struct Collector(Vec<u8>);

    impl Handler for Collector {
        fn write(&mut self, data: &[u8]) -> Result<usize, WriteError> {
            self.0.extend_from_slice(data);
            Ok(data.len())
        }
    }

    /// The RPC client
    pub struct Client<'a> {
        /// The actual curl client
        inner: Easy2<Collector>,
        /// Url to send requests to
        url: &'a str,
        /// The request body
        request: Request,
        /// The hash of the tx whose corresponding event is being searched for.
        hash: &'a str,
    }

    impl<'a> Deref for Client<'a> {
        type Target = Easy2<Collector>;

        fn deref(&self) -> &Self::Target {
            &self.inner
        }
    }

    impl<'a> DerefMut for Client<'a> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.inner
        }
    }

    impl<'a> Client<'a> {
        /// Create a new client
        pub fn new(url: &'a str, request: Request, hash: &'a str) -> Self {
            let mut client = Self {
                inner: Easy2::new(Collector::default()),
                url,
                request,
                hash,
            };
            client.initialize();
            client
        }

        /// Send a request to Tendermint
        ///
        /// Takes the 10 newest block header events and searches for
        /// the relevant event among them.
        pub fn send(&mut self) -> Result<TxResponse, Error> {
            // send off the request
            // this loop is here because if commit timeouts
            // become too long, sometimes we get back empty responses.
            for attempt in 0..MAX_SEND_ATTEMPTS {
                match self.perform() {
                    Ok(()) => break,
                    Err(err) => {
                        tracing::debug!(?attempt, response = ?err, "attempting request")
                    }
                }
            }
            if self.get_ref().0.is_empty() {
                return Err(Error::Send);
            }

            // deserialize response
            let response: Response =
                serde_json::from_slice(self.get_ref().0.as_slice())
                    .map_err(Error::Deserialize)?;
            let response = response.into_result()?;
            // search for the event in the response and return
            // it if found. Else request the next chunk of results
            parse(response, self.hash)
                .ok_or_else(|| Error::NotFound(self.hash.to_string()))
        }

        /// Initialize the curl client from the fields of `Client`
        fn initialize(&mut self) {
            self.inner.reset();
            let url = self.url;
            self.url(url).unwrap();
            self.post(true).unwrap();

            // craft the body of the request
            let request_body = serde_json::to_string(&self.request).unwrap();
            self.post_field_size(request_body.as_bytes().len() as u64)
                .unwrap();
            // update the request and serialize to bytes
            let data = serde_json::to_string(&self.request).unwrap();
            let data = data.as_bytes();
            self.post_fields_copy(data).unwrap();
        }
    }

    /// Given a query looking for a particular Anoma event,
    /// query the Tendermint's jsonrpc endpoint for the events
    /// log. Returns the appropriate event if found in the log.
    pub async fn fetch_event(
        address: &str,
        filter: Query,
        tx_hash: &str,
    ) -> Result<TxResponse, Error> {
        // craft the body of the request
        let request = Request::from(EventParams::new(
            filter,
            NUM_EVENTS,
            std::time::Duration::from_secs(60),
        ));
        // construct a curl client
        let mut client = Client::new(address, request, tx_hash);
        // perform the request
        client.send()
    }

    #[cfg(test)]
    mod test_rpc_types {
        use serde_json::json;

        use super::*;
        use crate::client::tendermint_rpc_types::{EventData, EventItem};

        /// Test that we correctly parse the response from Tendermint
        #[test]
        fn test_parse_response() {
            let resp = r#"
            {
                "jsonrpc":"2.0",
                "id":1,
                "result":{
                    "items": [{
                        "cursor":"16f1b066717b4261-0060",
                        "event":"NewRoundStep",
                        "data":{
                            "type":"tendermint/event/RoundState",
                            "value":{
                                "height":"17416",
                                "round":0,
                                "step":"RoundStepCommit"
                            }
                        }
                    }],
                    "more":true,
                    "oldest":"16f1b065029b23d0-0001",
                    "newest":"16f1b066717b4261-0060"
                }
            }"#;
            let response: Response =
                serde_json::from_str(resp).expect("Test failed");
            let items = response.into_result().expect("Test failed").items;
            assert_eq!(
                items,
                vec![EventItem {
                    cursor: String::from("16f1b066717b4261-0060").into(),
                    event: "NewRoundStep".to_string(),
                    data: EventData {
                        r#type: "tendermint/event/RoundState".to_string(),
                        value: json!({
                            "height":"17416",
                            "round":0,
                            "step":"RoundStepCommit"
                        }),
                    }
                }]
            )
        }
    }
}

#[cfg(not(feature = "ABCI"))]
pub use tm_jsonrpc::*;
