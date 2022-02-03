//! This module provides connection between an intent gossiper node (the server)
//! and matchmakers (clients) over WebSocket.
//!
//! Both the server and the client can asynchronously listen for new messages
//! and send messages to the other side.

use std::collections::HashSet;
use std::fmt::Debug;
use std::net::{SocketAddr, ToSocketAddrs};
use std::sync::atomic::{self, AtomicBool};
use std::sync::{Arc, RwLock};

use borsh::{BorshDeserialize, BorshSerialize};
use derivative::Derivative;
use message_io::network::{Endpoint, ResourceId, ToRemoteAddr, Transport};
use message_io::node::{self, NodeHandler, NodeListener};

use crate::cli;

/// Message from intent gossiper to a matchmaker
#[derive(Clone, Debug, PartialEq, Eq, BorshSerialize, BorshDeserialize)]
pub enum MsgFromServer {
    /// Try to match an intent
    AddIntent { id: Vec<u8>, data: Vec<u8> },
}

/// Message from a matchmaker to intent gossiper
#[derive(Clone, Debug, PartialEq, Eq, BorshSerialize, BorshDeserialize)]
pub enum MsgFromClient {
    /// The intent is invalid and hence it shouldn't be gossiped
    InvalidIntent { id: Vec<u8> },
    /// The intent constraints are too complex for this matchmaker, gossip it
    IntentConstraintsTooComplex { id: Vec<u8> },
    /// The matchmaker doesn't care about this intent, gossip it
    IgnoredIntent { id: Vec<u8> },
    /// Intents were matched into a tx. Remove the matched intents from mempool
    /// if the tx gets applied.
    Matched { intent_ids: HashSet<Vec<u8>> },
    /// An intent was accepted and added, but no match found yet. Gossip it
    Unmatched { id: Vec<u8> },
}

/// Intent gossiper server listener handles connections from [`ClientDialer`]s.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct ServerListener {
    /// The address on which the server is listening
    pub address: SocketAddr,
    /// The accepted client connections, shared with the [`ServerDialer`]
    clients: Arc<RwLock<HashSet<Endpoint>>>,
    /// A node listener and its abort receiver. These are consumed once the
    /// listener is started with [`ServerListener::listen`].
    #[derivative(Debug = "ignore")]
    listener: Option<(NodeListener<()>, tokio::sync::mpsc::Receiver<()>)>,
}

/// Intent gossiper server dialer can send messages to the connected
/// [`ClientListener`]s.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ServerDialer {
    /// The connection handler
    #[derivative(Debug = "ignore")]
    handler: NodeHandler<()>,
    /// Connection resource ID
    resource_id: ResourceId,
    /// The accepted client connections, shared with the [`ServerListener`]
    clients: Arc<RwLock<HashSet<Endpoint>>>,
    /// A message to abort the server must be sent to stop the
    /// [`ServerListener`]. This message will be sent on [`ServerDialer`]'s
    /// `drop` call.
    abort_send: tokio::sync::mpsc::Sender<()>,
}

/// Server events are used internally by the async [`ServerListener`].
#[derive(Clone, Debug)]
enum ServerEvent {
    /// New endpoint has been accepted by a listener and considered ready to
    /// use. The event contains the resource id of the listener that
    /// accepted this connection.
    Accepted(Endpoint, ResourceId),
    /// Input message received by the network.
    Message(Endpoint, MsgFromClient),
    /// This event is only dispatched when a connection is lost.
    Disconnected(Endpoint),
}

/// Matchmaker client listener handles a connection from [`ServerDialer`].
#[derive(Derivative)]
#[derivative(Debug)]
pub struct ClientListener {
    /// The connection handler
    #[derivative(Debug = "ignore")]
    handler: NodeHandler<()>,
    /// The server connection endpoint
    server: Endpoint,
    /// The address on which the client is listening
    local_addr: SocketAddr,
    /// The client listener. This is consumed once the listener is started with
    /// [`ClientListener::listen`].
    #[derivative(Debug = "ignore")]
    listener: Option<NodeListener<()>>,
    /// Server connection status
    is_connected: Arc<AtomicBool>,
}

/// Matchmaker client dialer can send messages to the connected
/// [`ServerListener`].
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ClientDialer {
    /// The address on which the client is listening
    pub local_addr: SocketAddr,
    /// The server address
    server: Endpoint,
    /// The connection handler
    #[derivative(Debug = "ignore")]
    handler: NodeHandler<()>,
    /// Server connection status
    is_connected: Arc<AtomicBool>,
}

impl ServerListener {
    /// Create a new intent gossiper node server. Returns a listener and
    /// a dialer that can be used to send messages to clients and to shut down
    /// the server.
    pub fn new_pair(address: impl ToSocketAddrs) -> (Self, ServerDialer) {
        let clients: Arc<RwLock<HashSet<Endpoint>>> = Default::default();
        let (handler, listener) = node::split::<()>();

        let (resource_id, address) = match handler
            .network()
            .listen(Transport::Ws, &address)
        {
            Ok((resource_id, real_addr)) => {
                tracing::info!("Matchmakers server running at {}", real_addr);
                (resource_id, real_addr)
            }
            Err(err) => {
                eprintln!(
                    "The matchmakers server cannot listen at {:?}: {}",
                    address.to_socket_addrs().unwrap().collect::<Vec<_>>(),
                    err
                );
                cli::safe_exit(1);
            }
        };

        let (abort_send, abort_recv) = tokio::sync::mpsc::channel::<()>(1);

        (
            Self {
                address,
                clients: clients.clone(),
                listener: Some((listener, abort_recv)),
            },
            ServerDialer {
                handler,
                clients,
                resource_id,
                abort_send,
            },
        )
    }

    /// Start the server listener and call `on_msg` on every received message.
    /// The listener can be stopped early by [`ServerDialer::shutdown`].
    pub async fn listen(mut self, mut on_msg: impl FnMut(MsgFromClient)) {
        // Open a channel for events received from the async listener
        let (send, mut recv) = tokio::sync::mpsc::unbounded_channel();

        // This is safe because `listen` consumes `self` created by
        // [`ServerListener::new_pair`]
        let (listener, mut abort_recv) = self.listener.take().unwrap();

        tracing::debug!("Starting intent gossiper matchmakers server...");

        // Start the async listener that will send server events over the
        // channel
        let _task = listener.for_each_async(move |event| {
            match event.network() {
                message_io::network::NetEvent::Message(
                    endpoint,
                    mut msg_bytes,
                ) => match MsgFromClient::deserialize(&mut msg_bytes) {
                    Ok(msg) => {
                        let _ = send.send(ServerEvent::Message(endpoint, msg));
                    }
                    Err(err) => {
                        tracing::error!(
                            "Couldn't decode a msg from matchmaker {}: {}",
                            endpoint,
                            err
                        );
                    }
                },
                message_io::network::NetEvent::Accepted(endpoint, id) => {
                    tracing::info!(
                        "Accepted connection from matchmaker {}",
                        endpoint
                    );
                    let _ = send.send(ServerEvent::Accepted(endpoint, id));
                }
                message_io::network::NetEvent::Disconnected(endpoint) => {
                    tracing::info!("Matchmaker disconnected: {}", endpoint);
                    let _ = send.send(ServerEvent::Disconnected(endpoint));
                }
                message_io::network::NetEvent::Connected(endpoint, status) => {
                    // Server only gets `NetEvent::Accepted` from connected
                    // clients
                    tracing::error!(
                        "Unexpected server `NetEvent::Connected` with \
                         endpoint {}, status {}",
                        endpoint,
                        status
                    );
                }
            }
        });

        tracing::debug!("Intent gossiper matchmakers server is ready.");

        // Process the server events
        loop {
            tokio::select! {
                _ = abort_recv.recv() => {
                    tracing::debug!("Shutting down intent gossiper matchmakers server.");
                    return;
                },
                event = recv.recv() => if let Some(event) = event {
                    match event {
                        ServerEvent::Message(endpoint, msg) => {
                            tracing::debug!(
                                "Received msg from matchmaker {}: {:?}",
                                endpoint,
                                msg
                            );
                            on_msg(msg);
                        }
                        ServerEvent::Accepted(endpoint, _id) => {
                            let mut clients = self.clients.write().unwrap();
                            if !clients.insert(endpoint) {
                                tracing::warn!(
                                    "Accepted matchmaker already known {}",
                                    endpoint
                                )
                            }
                        }
                        ServerEvent::Disconnected(endpoint) => {
                            let mut clients = self.clients.write().unwrap();
                            if !clients.remove(&endpoint) {
                                tracing::warn!(
                                    "Disconnected matchmaker unknown endpoint {}",
                                    endpoint
                                )
                            }
                        }
                    }
                }
            }
        }
    }
}

impl ServerDialer {
    /// Broadcast a message to all connected matchmaker clients
    pub fn send(&mut self, msg: MsgFromServer) {
        let net = self.handler.network();
        for client in self.clients.read().unwrap().iter() {
            let msg_bytes = msg.try_to_vec().unwrap();
            let status = net.send(*client, &msg_bytes);
            tracing::info!(
                "Sent msg {:?} to {} with status {:?}",
                msg,
                client,
                status
            );
        }
    }

    /// Is the server listener ready to start handling incoming connections?
    pub fn is_ready(&self) -> bool {
        self.handler
            .network()
            .is_ready(self.resource_id)
            .unwrap_or_default()
    }

    /// Force shut-down the [`ServerListener`] associated with this dialer.
    pub fn shutdown(&mut self) {
        self.handler.stop();
        // Send a message to abort and ignore the result
        let _ = self.abort_send.blocking_send(());
    }
}

impl ClientListener {
    /// Create a new matchmaker client. Returns a listener and a dialer that
    /// can be used to send messages to the server and to shut down the client.
    pub fn new_pair(server_addr: impl ToRemoteAddr) -> (Self, ClientDialer) {
        let server_addr = server_addr.to_remote_addr().unwrap();
        // Not using message-io signals
        let (handler, listener) = node::split::<()>();

        let (server, local_addr) = match handler
            .network()
            .connect(Transport::Ws, server_addr.clone())
        {
            Ok(res) => res,
            Err(err) => {
                eprintln!(
                    "Cannot listen at {} for matchmakers server: {}",
                    server_addr, err,
                );
                cli::safe_exit(1);
            }
        };
        tracing::info!("Matchmaker client running at {}", local_addr);

        let is_connected = Arc::new(AtomicBool::new(false));

        (
            Self {
                server,
                local_addr,
                listener: Some(listener),
                is_connected: is_connected.clone(),
                handler: handler.clone(),
            },
            ClientDialer {
                server,
                local_addr,
                handler,
                is_connected,
            },
        )
    }

    /// Start the client listener and call `on_msg` on every received message.
    /// The listener can be stopped early by [`ClientDialer::shutdown`].
    pub fn listen(mut self, mut on_msg: impl FnMut(MsgFromServer)) {
        // This is safe because `listen` consumes `self`
        let listener = self.listener.take().unwrap();

        // Start the blocking listener that will call `on_msg` on every message
        let server_addr = self.server.addr();
        let local_addr_port = self.local_addr.port();

        tracing::debug!("Matchmakers client is ready.");

        listener.for_each(move |event| {
            tracing::debug!("Client event {:#?}", event);
            match event {
                node::NodeEvent::Network(net_event) => match net_event {
                    message_io::network::NetEvent::Message(
                        endpoint,
                        mut msg_bytes,
                    ) => match MsgFromServer::deserialize(&mut msg_bytes) {
                        Ok(msg) => {
                            on_msg(msg);
                        }
                        Err(err) => {
                            tracing::error!(
                                "Couldn't decode a msg from intent gossiper \
                                 {}: {}",
                                endpoint,
                                err
                            );
                        }
                    },
                    message_io::network::NetEvent::Connected(
                        _endpoint,
                        established,
                    ) => {
                        if established {
                            tracing::info!(
                                "Connected to the server at {}. The client is \
                                 identified by local port: {}",
                                server_addr,
                                local_addr_port
                            );
                        } else {
                            tracing::error!(
                                "Cannot connect to the server at {}",
                                server_addr
                            )
                        }
                        self.is_connected
                            .store(established, atomic::Ordering::SeqCst);
                    }
                    message_io::network::NetEvent::Disconnected(endpoint) => {
                        tracing::info!("Disconnected from {}", endpoint);
                        self.is_connected
                            .store(false, atomic::Ordering::SeqCst);
                        // Exit on disconnect, a user of this client can
                        // implement retry logic
                        self.handler.stop();
                    }
                    message_io::network::NetEvent::Accepted(endpoint, _) => {
                        // Client only gets `NetEvent::Connected` from connected
                        // clients
                        tracing::error!(
                            "Unexpected client `NetEvent::Accepted` with \
                             endpoint {}",
                            endpoint
                        );
                    }
                },
                node::NodeEvent::Signal(()) => {
                    // unused
                }
            }
        });

        tracing::debug!("Matchmakers client is shutting down.");
    }
}

impl ClientDialer {
    /// Send a message to the intent gossiper server
    pub fn send(&mut self, msg: MsgFromClient) {
        let net = self.handler.network();
        let msg_bytes = msg.try_to_vec().unwrap();
        let status = net.send(self.server, &msg_bytes);
        tracing::info!(
            "Sent msg {:?} to {} with status {:?}",
            msg,
            self.server,
            status
        );
    }

    /// Is the client connected?
    pub fn is_connected(&self) -> bool {
        self.is_connected.load(atomic::Ordering::SeqCst)
    }

    /// Force shut-down the [`ClientListener`] associated with this dialer.
    pub fn shutdown(&mut self) {
        self.handler.stop();
    }
}

impl Drop for ServerDialer {
    fn drop(&mut self) {
        self.shutdown();
    }
}

impl Drop for ClientDialer {
    fn drop(&mut self) {
        self.shutdown();
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::sync::atomic;

    use itertools::Itertools;
    use proptest::prelude::*;
    use proptest::prop_state_machine;
    use proptest::state_machine::{AbstractStateMachine, StateMachineTest};
    use proptest::test_runner::Config;
    use test_log::test;

    use super::*;

    prop_state_machine! {
        #![proptest_config(Config {
            // Instead of the default 256, we only run 10 because otherwise it
            // takes too long
            cases: 10,
            // 10 second timeout
            timeout: 10_000,
            .. Config::default()
        })]
        #[test]
        /// A `StateMachineTest` implemented on `AbstractState`
        fn connections_state_machine_test(sequential 1..20 => AbstractState);
    }

    /// Abstract representation of a state of a server and client(s)
    #[derive(Clone, Debug)]
    struct AbstractState {
        // true == running
        server: bool,
        clients: HashSet<ClientId>,
    }

    /// State of a concrete server and client(s) implementation
    #[derive(Default)]
    struct ConcreteState {
        server: Option<TestServer>,
        clients: HashMap<ClientId, TestClient>,
    }

    /// State machine transitions
    #[derive(Clone, Debug)]
    enum Transition {
        StartServer,
        StopServer,
        StartClient(ClientId),
        StopClient(ClientId),
        ServerMsg(MsgFromServer),
        ClientMsg(ClientId, MsgFromClient),
    }

    type ClientId = usize;

    struct TestServer {
        /// The address of the server (assigned dynamically)
        address: SocketAddr,
        /// Runtime for the async listener
        rt: tokio::runtime::Runtime,
        #[allow(dead_code)]
        /// Task that runs the async server listener
        listener_handle: tokio::task::JoinHandle<()>,
        /// A server dialer can send messages to clients
        dialer: ServerDialer,
        /// Messages received by the `listener` from clients are forwarded
        /// to this receiver, to be checked by the test.
        msgs_recv: std::sync::mpsc::Receiver<MsgFromClient>,
    }

    struct TestClient {
        /// A client dialer can send messages to the server
        dialer: ClientDialer,
        /// A thread that runs the client listener
        listener_handle: std::thread::JoinHandle<()>,
        /// Messages received by the `listener` from the server are forwarded
        /// to this receiver, to be checked by the test.
        msgs_recv: std::sync::mpsc::Receiver<MsgFromServer>,
    }

    impl StateMachineTest for AbstractState {
        type Abstract = Self;
        type ConcreteState = ConcreteState;

        fn init_test(
            _initial_state: <Self::Abstract as AbstractStateMachine>::State,
        ) -> Self::ConcreteState {
            ConcreteState::default()
        }

        fn apply_concrete(
            mut state: Self::ConcreteState,
            transition: <Self::Abstract as AbstractStateMachine>::Transition,
        ) -> Self::ConcreteState {
            match transition {
                Transition::StartServer => {
                    // Assign port dynamically
                    let (listener, dialer) =
                        ServerListener::new_pair("127.0.0.1:0");
                    let address = listener.address;
                    let (msgs_send, msgs_recv) = std::sync::mpsc::channel();
                    // Run the listener, we need an async runtime
                    let rt = tokio::runtime::Runtime::new().unwrap();
                    let listener_handle = rt.spawn(async move {
                        listener
                            .listen(move |msg| {
                                msgs_send.send(msg).unwrap();
                            })
                            .await;
                    });

                    // Wait for the server to be ready
                    while !dialer.is_ready() {
                        println!("Waiting for the server to be ready");
                    }

                    state.server = Some(TestServer {
                        address,
                        rt,
                        dialer,
                        listener_handle,
                        msgs_recv,
                    })
                }
                Transition::StopServer => {
                    // For the server, we have to send abort signal and drop
                    // the dialer
                    let mut server = state.server.take().unwrap();
                    server.dialer.shutdown();
                    server
                        .rt
                        .shutdown_timeout(std::time::Duration::from_secs(2));
                    drop(server.dialer);

                    if !state.clients.is_empty() {
                        println!(
                            "The server is waiting for all the clients to \
                             stop..."
                        );
                        while state.clients.values().any(|client| {
                            client
                                .dialer
                                .is_connected
                                .load(atomic::Ordering::SeqCst)
                        }) {}
                        // Stop the clients
                        for (id, client) in
                            std::mem::take(&mut state.clients).into_iter()
                        {
                            // Ask the client to stop
                            client.dialer.handler.stop();
                            println!("Asking client {} listener to stop", id);
                            // Wait for it to actually stop
                            client.listener_handle.join().unwrap();
                            println!("Client {} listener stopped", id);
                        }
                        println!("Clients stopped");
                    }
                }
                Transition::StartClient(id) => {
                    let server_addr = state.server.as_ref().unwrap().address;
                    let (listener, dialer) =
                        ClientListener::new_pair(server_addr);
                    let (msgs_send, msgs_recv) = std::sync::mpsc::channel();
                    let listener_handle = std::thread::spawn(move || {
                        listener.listen(|msg| {
                            msgs_send.send(msg).unwrap();
                        })
                    });

                    // If there is a server running ...
                    if let Some(server) = state.server.as_ref() {
                        // ... wait for the client to connect ...
                        while !dialer.is_connected() {}
                        // ... and for the server to accept it
                        while !server.dialer.clients.read().unwrap().iter().any(
                            |client| {
                                // Client's address is added once it's accepted
                                client.addr() == dialer.local_addr
                            },
                        ) {}
                    }

                    state.clients.insert(
                        id,
                        TestClient {
                            dialer,
                            listener_handle,
                            msgs_recv,
                        },
                    );
                }
                Transition::StopClient(id) => {
                    // Remove the client
                    let client = state.clients.remove(&id).unwrap();
                    // Ask the client to stop
                    client.dialer.handler.stop();
                    // Wait for it to actually stop
                    client.listener_handle.join().unwrap();
                }
                Transition::ServerMsg(msg) => {
                    state.server.as_mut().unwrap().dialer.send(msg.clone());

                    // Post-condition: every client must receive the msg
                    for client in state.clients.values() {
                        let recv_msg = client.msgs_recv.recv().unwrap();
                        assert_eq!(msg, recv_msg);
                    }
                }
                Transition::ClientMsg(id, msg) => {
                    let client = state.clients.get_mut(&id).unwrap();
                    client.dialer.send(msg.clone());

                    // Post-condition:
                    // If there is a server running ...
                    if let Some(server) = state.server.as_mut() {
                        // ... it must receive the msg
                        let recv_msg = server.msgs_recv.recv().unwrap();
                        assert_eq!(msg, recv_msg);
                    }
                }
            }
            state
        }

        fn test_sequential(
            initial_state: <Self::Abstract as AbstractStateMachine>::State,
            transitions: Vec<
                <Self::Abstract as AbstractStateMachine>::Transition,
            >,
        ) {
            let mut state = Self::init_test(initial_state);
            for transition in transitions {
                state = Self::apply_concrete(state, transition);
                Self::invariants(&state);
            }

            // Shutdown the server gracefully
            if let Some(mut server) = state.server {
                server.dialer.shutdown();
                server
                    .rt
                    .shutdown_timeout(std::time::Duration::from_secs(4));
            }
            // Shutdown any clients too
            if !state.clients.is_empty() {
                println!(
                    "The server is waiting for all the clients to stop..."
                );
                while state.clients.values().any(|client| {
                    client.dialer.is_connected.load(atomic::Ordering::SeqCst)
                }) {}
                println!("Clients stopped");
            }
        }
    }

    impl AbstractStateMachine for AbstractState {
        type State = Self;
        type Transition = Transition;

        fn init_state() -> BoxedStrategy<Self::State> {
            Just(Self {
                server: false,
                clients: HashSet::default(),
            })
            .boxed()
        }

        fn transitions(state: &Self::State) -> BoxedStrategy<Self::Transition> {
            use Transition::*;
            if state.clients.is_empty() {
                prop_oneof![
                    Just(StartServer),
                    Just(StopServer),
                    (0..4_usize).prop_map(StartClient),
                    arb_msg_from_server().prop_map(ServerMsg),
                ]
                .boxed()
            } else {
                let ids: Vec<_> =
                    state.clients.iter().sorted().cloned().collect();
                let arb_id = proptest::sample::select(ids);
                prop_oneof![
                    Just(StartServer),
                    Just(StopServer),
                    (0..4_usize).prop_map(StartClient),
                    arb_msg_from_server().prop_map(ServerMsg),
                    arb_id.clone().prop_map(StopClient),
                    arb_id.prop_flat_map(|id| arb_msg_from_client()
                        .prop_map(move |msg| { ClientMsg(id, msg) })),
                ]
                .boxed()
            }
        }

        fn preconditions(
            state: &Self::State,
            transition: &Self::Transition,
        ) -> bool {
            match transition {
                Transition::StartServer => !state.server,
                Transition::StopServer => state.server,
                Transition::StartClient(id) => {
                    // only start clients if the server is running and this
                    // client ID is not running
                    state.server && !state.clients.contains(id)
                }
                Transition::StopClient(id) => {
                    // stop only if this client is running
                    state.clients.contains(id)
                }
                Transition::ServerMsg(_) => {
                    // can send only if the server is running
                    state.server
                }
                Transition::ClientMsg(id, _) => {
                    // can send only if the server and this client is running
                    state.server && state.clients.contains(id)
                }
            }
        }

        fn apply_abstract(
            mut state: Self::State,
            transition: &Self::Transition,
        ) -> Self::State {
            match transition {
                Transition::StartServer => {
                    state.server = true;
                }
                Transition::StopServer => {
                    state.server = false;
                    // Clients should disconnect and stop
                    state.clients = Default::default();
                }
                Transition::StartClient(id) => {
                    state.clients.insert(*id);
                }
                Transition::StopClient(id) => {
                    state.clients.remove(id);
                }
                Transition::ServerMsg(_msg) => {
                    // no change
                }
                Transition::ClientMsg(_id, _msg) => {
                    // no change
                }
            }
            state
        }
    }

    prop_compose! {
        /// Generate an arbitrary MsgFromServer
        fn arb_msg_from_server()
            (id in proptest::collection::vec(any::<u8>(), 1..100),
            data in proptest::collection::vec(any::<u8>(), 1..100))
        -> MsgFromServer {
            MsgFromServer::AddIntent { id, data }
        }
    }

    /// Generate an arbitrary MsgFromClient
    fn arb_msg_from_client() -> impl Strategy<Value = MsgFromClient> {
        let arb_intent_id = proptest::collection::vec(any::<u8>(), 1..100);
        let invalid_intent = arb_intent_id
            .clone()
            .prop_map(|id| MsgFromClient::InvalidIntent { id });
        let intent_too_complex = arb_intent_id
            .clone()
            .prop_map(|id| MsgFromClient::IntentConstraintsTooComplex { id });
        let ignored_intent = arb_intent_id
            .clone()
            .prop_map(|id| MsgFromClient::IgnoredIntent { id });
        let unmatched_intent = arb_intent_id
            .clone()
            .prop_map(|id| MsgFromClient::Unmatched { id });
        let matched_intent =
            proptest::collection::hash_set(arb_intent_id, 1..10).prop_map(
                move |intent_ids| MsgFromClient::Matched { intent_ids },
            );
        prop_oneof![
            invalid_intent,
            intent_too_complex,
            ignored_intent,
            matched_intent,
            unmatched_intent,
        ]
    }
}
