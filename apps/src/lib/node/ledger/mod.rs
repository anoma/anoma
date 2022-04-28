mod broadcaster;
mod ethereum_node;
pub mod events;
pub mod protocol;
pub mod rpc;
mod shell;
mod shims;
pub mod storage;
pub mod tendermint_node;

use std::convert::TryInto;
use std::net::SocketAddr;
use std::path::PathBuf;

use byte_unit::Byte;
use futures::future::TryFutureExt;
use once_cell::unsync::Lazy;
use sysinfo::{RefreshKind, System, SystemExt};
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::abci::CheckTxType;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::CheckTxType;
use tower::ServiceBuilder;
#[cfg(not(feature = "ABCI"))]
use tower_abci::{response, split, Server};
#[cfg(feature = "ABCI")]
use tower_abci_old::{response, split, Server};

use self::shims::abcipp_shim::AbciService;
use crate::config::utils::num_of_threads;
use crate::config::TendermintMode;
use crate::node::ledger::broadcaster::Broadcaster;
use crate::node::ledger::shell::{Error, MempoolTxType, Shell};
use crate::node::ledger::shims::abcipp_shim::AbcippShim;
use crate::node::ledger::shims::abcipp_shim_types::shim::{Request, Response};
use crate::{config, wasm_loader};

/// Env. var to set a number of Tokio RT worker threads
const ENV_VAR_TOKIO_THREADS: &str = "ANOMA_TOKIO_THREADS";

/// Env. var to set a number of Rayon global worker threads
const ENV_VAR_RAYON_THREADS: &str = "ANOMA_RAYON_THREADS";

// Until ABCI++ is ready, the shim provides the service implementation.
// We will add this part back in once the shim is no longer needed.
//```
// impl Service<Request> for Shell {
//     type Error = Error;
//     type Future =
//         Pin<Box<dyn Future<Output = Result<Response, BoxError>> + Send +
// 'static>>;    type Response = Response;
//
//     fn poll_ready(
//         &mut self,
//         _cx: &mut Context<'_>,
//     ) -> Poll<Result<(), Self::Error>> {
//         Poll::Ready(Ok(()))
//     }
//```

impl Shell {
    fn call(&mut self, req: Request) -> Result<Response, Error> {
        match req {
            Request::InitChain(init) => {
                self.init_chain(init).map(Response::InitChain)
            }
            Request::Info(_) => Ok(Response::Info(self.last_state())),
            Request::Query(query) => Ok(Response::Query(self.query(query))),
            #[cfg(not(feature = "ABCI"))]
            Request::PrepareProposal(block) => {
                Ok(Response::PrepareProposal(self.prepare_proposal(block)))
            }
            Request::VerifyHeader(_req) => {
                Ok(Response::VerifyHeader(self.verify_header(_req)))
            }
            #[cfg(not(feature = "ABCI"))]
            Request::ProcessProposal(block) => {
                Ok(Response::ProcessProposal(self.process_proposal(block)))
            }
            #[cfg(feature = "ABCI")]
            Request::DeliverTx(deliver_tx) => {
                Ok(Response::DeliverTx(self.process_and_decode_block(deliver_tx)))
            },
            #[cfg(not(feature = "ABCI"))]
            Request::RevertProposal(_req) => {
                Ok(Response::RevertProposal(self.revert_proposal(_req)))
            }
            #[cfg(not(feature = "ABCI"))]
            Request::ExtendVote(_req) => {
                Ok(Response::ExtendVote(self.extend_vote(_req)))
            }
            #[cfg(not(feature = "ABCI"))]
            Request::VerifyVoteExtension(_req) => Ok(
                Response::VerifyVoteExtension(self.verify_vote_extension(_req)),
            ),
            Request::FinalizeBlock(finalize) => {
                self.finalize_block(finalize).map(Response::FinalizeBlock)
            }
            Request::Commit(_) => Ok(Response::Commit(self.commit())),
            Request::Flush(_) => Ok(Response::Flush(Default::default())),
            Request::Echo(msg) => Ok(Response::Echo(response::Echo {
                message: msg.message,
            })),
            Request::CheckTx(tx) => {
                let r#type = match CheckTxType::from_i32(tx.r#type)
                    .expect("received unexpected CheckTxType from ABCI")
                {
                    CheckTxType::New => MempoolTxType::NewTransaction,
                    CheckTxType::Recheck => MempoolTxType::RecheckTransaction,
                };
                Ok(Response::CheckTx(self.mempool_validate(&*tx.tx, r#type)))
            }
            Request::ListSnapshots(_) => {
                Ok(Response::ListSnapshots(Default::default()))
            }
            Request::OfferSnapshot(_) => {
                Ok(Response::OfferSnapshot(Default::default()))
            }
            Request::LoadSnapshotChunk(_) => {
                Ok(Response::LoadSnapshotChunk(Default::default()))
            }
            Request::ApplySnapshotChunk(_) => {
                Ok(Response::ApplySnapshotChunk(Default::default()))
            }
        }
    }
}

/// Run the ledger with an async runtime
pub fn run(config: config::Ledger, wasm_dir: PathBuf) {
    let logical_cores = num_cpus::get();
    tracing::info!("Available logical cores: {}", logical_cores);

    let rayon_threads = num_of_threads(
        ENV_VAR_RAYON_THREADS,
        // If not set, default to half of logical CPUs count
        logical_cores / 2,
    );
    tracing::info!("Using {} threads for Rayon.", rayon_threads);

    let tokio_threads = num_of_threads(
        ENV_VAR_TOKIO_THREADS,
        // If not set, default to half of logical CPUs count
        logical_cores / 2,
    );
    tracing::info!("Using {} threads for Tokio.", tokio_threads);

    // Configure number of threads for rayon (used in `par_iter` when running
    // VPs)
    rayon::ThreadPoolBuilder::new()
        .num_threads(rayon_threads)
        .thread_name(|i| format!("ledger-rayon-worker-{}", i))
        .build_global()
        .unwrap();

    // Start tokio runtime with the `run_aux` function
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(tokio_threads)
        .thread_name("ledger-tokio-worker")
        // Enable time and I/O drivers
        .enable_all()
        .build()
        .unwrap()
        .block_on(run_aux(config, wasm_dir));
}

/// Resets the tendermint_node state and removes database files
pub fn reset(config: config::Ledger) -> Result<(), shell::Error> {
    shell::reset(config)
}

/// Runs three concurrent tasks: A tendermint node, a shell which contains an
/// ABCI, server for talking to the tendermint node, and a broadcaster so that
/// the ledger may submit txs to the chain. All must be alive for correct
/// functioning.
async fn run_aux(config: config::Ledger, wasm_dir: PathBuf) {
    // Prefetch needed wasm artifacts
    wasm_loader::pre_fetch_wasm(&wasm_dir).await;

    // Find the system available memory
    let available_memory_bytes = Lazy::new(|| {
        let sys = System::new_with_specifics(RefreshKind::new().with_memory());
        let available_memory_bytes = sys.available_memory() * 1024;
        tracing::info!(
            "Available memory: {}",
            Byte::from_bytes(available_memory_bytes as u128)
                .get_appropriate_unit(true)
        );
        available_memory_bytes
    });

    // Find the VP WASM compilation cache size
    let vp_wasm_compilation_cache =
        match config.shell.vp_wasm_compilation_cache_bytes {
            Some(vp_wasm_compilation_cache) => {
                tracing::info!(
                    "VP WASM compilation cache size set from the configuration"
                );
                vp_wasm_compilation_cache
            }
            None => {
                tracing::info!(
                    "VP WASM compilation cache size not configured, using 1/6 \
                     of available memory."
                );
                *available_memory_bytes / 6
            }
        };
    tracing::info!(
        "VP WASM compilation cache size: {}",
        Byte::from_bytes(vp_wasm_compilation_cache as u128)
            .get_appropriate_unit(true)
    );

    // Find the tx WASM compilation cache size
    let tx_wasm_compilation_cache =
        match config.shell.tx_wasm_compilation_cache_bytes {
            Some(tx_wasm_compilation_cache) => {
                tracing::info!(
                    "Tx WASM compilation cache size set from the configuration"
                );
                tx_wasm_compilation_cache
            }
            None => {
                tracing::info!(
                    "Tx WASM compilation cache size not configured, using 1/6 \
                     of available memory."
                );
                *available_memory_bytes / 6
            }
        };
    tracing::info!(
        "Tx WASM compilation cache size: {}",
        Byte::from_bytes(tx_wasm_compilation_cache as u128)
            .get_appropriate_unit(true)
    );

    // Setup DB cache, it must outlive the DB instance that's in the shell
    let block_cache_size_bytes = match config.shell.block_cache_bytes {
        Some(block_cache_bytes) => {
            tracing::info!("Block cache set from the configuration.",);
            block_cache_bytes
        }
        None => {
            tracing::info!(
                "Block cache size not configured, using 1/3 of available \
                 memory."
            );
            *available_memory_bytes / 3
        }
    };
    tracing::info!(
        "RocksDB block cache size: {}",
        Byte::from_bytes(block_cache_size_bytes as u128)
            .get_appropriate_unit(true)
    );
    let db_cache =
        rocksdb::Cache::new_lru_cache(block_cache_size_bytes as usize).unwrap();

    let tendermint_dir = config.tendermint_dir();
    let ethereum_url = config.ethereum_url();
    let ledger_address = config.shell.ledger_address.to_string();
    let rpc_address = config.tendermint.rpc_address.to_string();
    let chain_id = config.chain_id.clone();
    let genesis_time = config
        .genesis_time
        .clone()
        .try_into()
        .expect("expected RFC3339 genesis_time");
    let tendermint_config = config.tendermint.clone();

    // Channel for signalling shut down from the shell or from Tendermint
    let (abort_send, abort_recv) =
        tokio::sync::mpsc::unbounded_channel::<&'static str>();
    // Channels for the Ethereum relayer to send new Ethereum block headers
    // and smart contract logs to the ledger
    let (eth_relayer_sender, mut eth_relayer_receiver) =
        tokio::sync::mpsc::unbounded_channel();
    // Channels for validators to send protocol txs to be broadcast to the
    // broadcaster service
    let (broadcaster_sender, broadcaster_receiver) =
        tokio::sync::mpsc::unbounded_channel();

    let (ethereum_node, ethereum_height, broadcaster) = if matches!(
        config.tendermint.tendermint_mode,
        TendermintMode::Validator
    ) {
        // Start Ethereum fullnode
        // Channel for signalling shut down to Tendermint process
        let (eth_abort_send, eth_abort_recv) =
            tokio::sync::oneshot::channel::<tokio::sync::oneshot::Sender<()>>();
        let abort_send_for_eth = abort_send.clone();
        let ethereum_node = tokio::spawn(async move {
            // On panic or exit, the `Drop` of `AbortSender` will send abort
            // message
            let aborter = Aborter {
                sender: abort_send_for_eth,
                who: "Ethereum",
            };

            let res = ethereum_node::run(
                &ethereum_url,
                vec![],
                eth_relayer_sender,
                eth_abort_recv,
            )
            .map_err(Error::Ethereum)
            .await;
            tracing::info!("Ethereum fullnode is no longer running.");

            drop(aborter);
            res
        });
        // wait for the Ethereum fullnode to finish syncing.
        let ethereum_height = eth_relayer_receiver
            .recv()
            .await
            .unwrap()
            .new_header
            .expect("A synced full node should return an Ethereum header")
            .number;

        // Shutdown ethereum_node via a message to ensure that the child process
        // is properly cleaned-up.
        let (eth_abort_resp_send, eth_abort_resp_recv) =
            tokio::sync::oneshot::channel::<()>();
        let abort_send_for_broadcaster = abort_send.clone();

        // Channel for signalling shut down to broadcaster
        let (bc_abort_send, bc_abort_recv) =
            tokio::sync::oneshot::channel::<()>();

        (
            Some((
                ethereum_node,
                eth_abort_send,
                eth_abort_resp_send,
                eth_abort_resp_recv,
            )),
            Some(ethereum_height),
            Some((
                tokio::spawn(async move {
                    // Construct a service for broadcasting protocol txs from
                    // the ledger
                    let mut broadcaster =
                        Broadcaster::new(&rpc_address, broadcaster_receiver);
                    // On panic or exit, the `Drop` of `AbortSender` will send
                    // abort message
                    let aborter = Aborter {
                        sender: abort_send_for_broadcaster,
                        who: "Broadcaster",
                    };
                    let res = broadcaster.run(bc_abort_recv).await;
                    tracing::info!("Broadcaster is no longer running.");

                    drop(aborter);
                    res
                }),
                bc_abort_send,
            )),
        )
    } else {
        (None, None, None)
    };

    // Channel for signalling shut down to Tendermint process
    let (tm_abort_send, tm_abort_recv) =
        tokio::sync::oneshot::channel::<tokio::sync::oneshot::Sender<()>>();

    // Start Tendermint node
    let abort_send_for_tm = abort_send.clone();
    let tendermint_node = tokio::spawn(async move {
        // On panic or exit, the `Drop` of `AbortSender` will send abort message
        let aborter = Aborter {
            sender: abort_send_for_tm,
            who: "Tendermint",
        };

        let res = tendermint_node::run(
            tendermint_dir,
            chain_id,
            genesis_time,
            ledger_address,
            tendermint_config,
            tm_abort_recv,
        )
        .map_err(Error::Tendermint)
        .await;
        tracing::info!("Tendermint node is no longer running.");

        drop(aborter);
        res
    });

    // Construct our ABCI application.
    let ledger_address = config.shell.ledger_address;
    let (shell, abci_service) = AbcippShim::new(
        config,
        wasm_dir,
        broadcaster_sender,
        eth_relayer_receiver,
        &db_cache,
        vp_wasm_compilation_cache,
        tx_wasm_compilation_cache,
        ethereum_height,
    );

    // Start the ABCI server
    let abci = tokio::spawn(async move {
        // On panic or exit, the `Drop` of `AbortSender` will send abort
        // message
        let aborter = Aborter {
            sender: abort_send,
            who: "ABCI",
        };

        let res = run_abci(abci_service, ledger_address).await;

        drop(aborter);
        res
    });

    // Run the shell in the main thread
    let thread_builder =
        std::thread::Builder::new().name("ledger-shell".into());
    let shell_handler = thread_builder
        .spawn(move || {
            tracing::info!("Anoma ledger node started.");
            shell.run()
        })
        .expect("Must be able to start a thread for the shell");

    // Wait for interrupt signal or abort message
    let aborted = wait_for_abort(abort_recv).await;

    // Abort the ABCI service task
    abci.abort();

    // Shutdown tendermint_node via a message to ensure that the child process
    // is properly cleaned-up.
    let (tm_abort_resp_send, tm_abort_resp_recv) =
        tokio::sync::oneshot::channel::<()>();
    // Ask to shutdown tendermint node cleanly. Ignore error, which can happen
    // if the tendermint_node task has already finished.
    if let Ok(()) = tm_abort_send.send(tm_abort_resp_send) {
        match tm_abort_resp_recv.await {
            Ok(()) => {}
            Err(err) => {
                tracing::error!(
                    "Failed to receive a response from tendermint: {}",
                    err
                );
            }
        }
    }

    let res = if let (
        Some((
            ethereum_node,
            eth_abort_send,
            eth_abort_resp_send,
            eth_abort_resp_recv,
        )),
        Some((broadcaster, bc_abort_send)),
    ) = (ethereum_node, broadcaster)
    {
        // Ask to shutdown tendermint node cleanly. Ignore error, which can
        // happen if the tendermint_node task has already finished.
        if let Ok(()) = eth_abort_send.send(eth_abort_resp_send) {
            match eth_abort_resp_recv.await {
                Ok(()) => {}
                Err(err) => {
                    tracing::error!(
                        "Failed to receive a response from Ethereum: {}",
                        err
                    );
                }
            }
        }
        // request the broadcaster shutdown
        let _ = bc_abort_send.send(());
        tokio::try_join!(tendermint_node, ethereum_node, abci, broadcaster)
    } else {
        // if we are not a validator, the broadcaster service and Ethereum
        // fullnode are not active. Thus, we fill in their return values
        // with ()
        tokio::try_join!(tendermint_node, abci)
            .map(|results| (results.0, Ok(()), results.1, ()))
    };

    match res {
        Ok((tendermint_res, eth_res, abci_res, _)) => {
            // we ignore errors on user-initiated shutdown
            if aborted {
                if let Err(err) = tendermint_res {
                    tracing::error!("Tendermint error: {}", err);
                }
                if let Err(err) = eth_res {
                    tracing::error!("Ethereum error: {}", err);
                }
                if let Err(err) = abci_res {
                    tracing::error!("ABCI error: {}", err);
                }
            }
        }
        Err(err) => {
            // Ignore cancellation errors
            if !err.is_cancelled() {
                tracing::error!("Ledger error: {}", err);
            }
        }
    }

    tracing::info!("Anoma ledger node has shut down.");

    if let Err(err) = shell_handler.join() {
        std::panic::resume_unwind(err)
    }
}

/// Runs the an asynchronous ABCI server with four sub-components for consensus,
/// mempool, snapshot, and info.
async fn run_abci(
    abci_service: AbciService,
    ledger_address: SocketAddr,
) -> shell::Result<()> {
    // Split it into components.
    let (consensus, mempool, snapshot, info) = split::service(abci_service, 5);

    // Hand those components to the ABCI server, but customize request behavior
    // for each category
    let server = Server::builder()
        .consensus(consensus)
        .snapshot(snapshot)
        .mempool(
            ServiceBuilder::new()
                .load_shed()
                .buffer(1024)
                .service(mempool),
        )
        .info(
            ServiceBuilder::new()
                .load_shed()
                .buffer(100)
                .rate_limit(50, std::time::Duration::from_secs(1))
                .service(info),
        )
        .finish()
        .unwrap();

    // Run the server with the ABCI service
    server
        .listen(ledger_address)
        .await
        .map_err(|err| Error::TowerServer(err.to_string()))
}

/// A panic-proof handle for aborting a future. Will abort during stack
/// unwinding and its drop method sends abort message with `who` inside it.
struct Aborter {
    sender: tokio::sync::mpsc::UnboundedSender<&'static str>,
    who: &'static str,
}

impl Drop for Aborter {
    fn drop(&mut self) {
        // Send abort message, ignore result
        let _ = self.sender.send(self.who);
    }
}

/// Function that blocks until either
///   1. User sends a shutdown signal
///   2. One of the child processes terminates, sending a message on `drop`
/// Returns a boolean to indicate which scenario occurred.
/// `true` means that the latter happened
#[cfg(unix)]
async fn wait_for_abort(
    mut abort_recv: tokio::sync::mpsc::UnboundedReceiver<&'static str>,
) -> bool {
    use tokio::signal::unix::{signal, SignalKind};
    let mut sigterm = signal(SignalKind::terminate()).unwrap();
    let mut sighup = signal(SignalKind::hangup()).unwrap();
    let mut sigpipe = signal(SignalKind::pipe()).unwrap();
    let _ = tokio::select! {
        signal = tokio::signal::ctrl_c() => {
            match signal {
                Ok(()) => tracing::info!("Received interrupt signal, exiting..."),
                Err(err) => tracing::error!("Failed to listen for CTRL+C signal: {}", err),
            }
        },
        signal = sigterm.recv() => {
            match signal {
                Some(()) => tracing::info!("Received termination signal, exiting..."),
                None => tracing::error!("Termination signal cannot be caught anymore, exiting..."),
            }
        },
        signal = sighup.recv() => {
            match signal {
                Some(()) => tracing::info!("Received hangup signal, exiting..."),
                None => tracing::error!("Hangup signal cannot be caught anymore, exiting..."),
            }
        },
        signal = sigpipe.recv() => {
            match signal {
                Some(()) => tracing::info!("Received pipe signal, exiting..."),
                None => tracing::error!("Pipe signal cannot be caught anymore, exiting..."),
            }
        },
        msg = abort_recv.recv() => {
            // When the msg is `None`, there are no more abort senders, so both
            // Tendermint and the shell must have already exited
            if let Some(who) = msg {
                 tracing::info!("{} has exited, shutting down...", who);
            }
            return true;
        }
    };
    false
}

/// Function that blocks until either
///   1. User sends a shutdown signal
///   2. One of the child processes terminates, sending a message on `drop`
/// Returns a boolean to indicate which scenario occurred.
/// `true` means that the latter happened
#[cfg(windows)]
async fn wait_for_abort(
    mut abort_recv: tokio::sync::mpsc::UnboundedReceiver<&'static str>,
) -> bool {
    let mut sigbreak = tokio::signal::windows::ctrl_break().unwrap();
    let _ = tokio::select! {
        signal = tokio::signal::ctrl_c() => {
            match signal {
                Ok(()) => tracing::info!("Received interrupt signal, exiting..."),
                Err(err) => tracing::error!("Failed to listen for CTRL+C signal: {}", err),
            }
        },
        signal = sigbreak.recv() => {
            match signal {
                Some(()) => tracing::info!("Received break signal, exiting..."),
                None => tracing::error!("Break signal cannot be caught anymore, exiting..."),
            }
        },
        msg = abort_recv.recv() => {
            // When the msg is `None`, there are no more abort senders, so both
            // Tendermint and the shell must have already exited
            if let Some(who) = msg {
                 tracing::info!("{} has exited, shutting down...", who);
            }
            return true;
        }
    };
    false
}

/// Function that blocks until either
///   1. User sends a shutdown signal
///   2. One of the child processes terminates, sending a message on `drop`
/// Returns a boolean to indicate which scenario occurred.
/// `true` means that the latter happened
#[cfg(not(any(unix, windows)))]
async fn wait_for_abort(
    mut abort_recv: tokio::sync::mpsc::UnboundedReceiver<&'static str>,
) -> bool {
    let _ = tokio::select! {
        signal = tokio::signal::ctrl_c() => {
            match signal {
                Ok(()) => tracing::info!("Received interrupt signal, exiting..."),
                Err(err) => tracing::error!("Failed to listen for CTRL+C signal: {}", err),
            }
        },
        msg = abort_recv.recv() => {
            // When the msg is `None`, there are no more abort senders, so both
            // Tendermint and the shell must have already exited
            if let Some(who) = msg {
                 tracing::info!("{} has exited, shutting down...", who);
            }
            return true;
        }
    };
    false
}
