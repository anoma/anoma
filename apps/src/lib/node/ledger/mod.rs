mod abortable;
mod broadcaster;
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
use std::str::FromStr;
use std::sync::Arc;
use std::thread;

use anoma::ledger::governance::storage as gov_storage;
use anoma::types::storage::Key;
use byte_unit::Byte;
use futures::future::TryFutureExt;
use once_cell::unsync::Lazy;
use sysinfo::{RefreshKind, System, SystemExt};
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::abci::CheckTxType;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::CheckTxType;
use tokio::task;
use tower::ServiceBuilder;
#[cfg(not(feature = "ABCI"))]
use tower_abci::{response, split, Server};
#[cfg(feature = "ABCI")]
use tower_abci_old::{response, split, Server};

use self::abortable::AbortableSpawner;
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
    fn load_proposals(&mut self) {
        let proposals_key = gov_storage::get_commiting_proposals_prefix(
            self.storage.last_epoch.0,
        );

        let (proposal_iter, _) = self.storage.iter_prefix(&proposals_key);
        for (key, _, _) in proposal_iter {
            let key =
                Key::from_str(key.as_str()).expect("Key should be parsable");
            if gov_storage::get_commit_proposal_epoch(&key).unwrap()
                != self.storage.last_epoch.0
            {
                // NOTE: `iter_prefix` iterate over the matching prefix. In this
                // case  a proposal with grace_epoch 110 will be
                // matched by prefixes  1, 11 and 110. Thus we
                // have to skip to the next iteration of
                //  the cycle for all the prefixes that don't actually match
                //  the desired epoch.
                continue;
            }

            let proposal_id = gov_storage::get_commit_proposal_id(&key);
            if let Some(id) = proposal_id {
                self.proposal_data.insert(id);
            }
        }
    }

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
            Request::DeliverTx(deliver_tx) => Ok(Response::DeliverTx(
                self.process_and_decode_proposal(deliver_tx),
            )),
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
                self.load_proposals();
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
/// ABCI server for talking to the tendermint node, and a broadcaster so that
/// the ledger may submit txs to the chain. All must be alive for correct
/// functioning.
async fn run_aux(config: config::Ledger, wasm_dir: PathBuf) {
    let setup_data = run_aux_setup(&config, &wasm_dir).await;

    // Create an `AbortableSpawner` for signalling shut down from the shell or
    // from Tendermint
    let mut spawner = AbortableSpawner::new();

    // Start Tendermint node
    let tendermint_node = start_tendermint(&mut spawner, &config);

    // Start ABCI server and broadcaster (the latter only if we are a validator
    // node)
    let (abci, broadcaster, shell_handler) = start_abci_broadcaster_shell(
        &mut spawner,
        wasm_dir,
        setup_data,
        config,
    );

    // Wait for interrupt signal or abort message
    let aborted = spawner.wait_for_abort().await.child_terminated();

    // Regain ownership of the ABCI `JoinHandle`
    //
    // TODO(tiago): this is only here because of a temporary hack.
    // the method we are using to cancel the ABCI server task is calling
    // `.abort()` on the `JoinHandle` of the returned tokio task. the handle
    // therefore needs to be stored in the `AbortableSpawner`, which requires
    // it to be an `Arc`, such that we may retain shared ownership of the
    // `JoinHandle`, after we abort the ABCI server.
    //
    // tl;dr: make it such that we can cancel the ABCI server without calling
    // `.abort()` on the Tokio `JoinHandle`
    //
    let abci = match Arc::try_unwrap(abci) {
        Ok(handle) => handle,
        _ => {
            // NOTE: this operation is infallible, since the only
            // other live instance of the `Arc` was consumed after the
            // cleanup job for the ABCI server ran
            unreachable!()
        }
    };

    let res = tokio::try_join!(tendermint_node, abci, broadcaster);

    match res {
        Ok((tendermint_res, abci_res, _)) => {
            // we ignore errors on user-initiated shutdown
            if aborted {
                if let Err(err) = tendermint_res {
                    tracing::error!("Tendermint error: {}", err);
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

    let res = task::block_in_place(move || shell_handler.join());

    if let Err(err) = res {
        std::panic::resume_unwind(err)
    }
}

/// A [`RunAuxSetup`] stores some variables used to start child
/// processes of the ledger.
struct RunAuxSetup {
    vp_wasm_compilation_cache: u64,
    tx_wasm_compilation_cache: u64,
    db_block_cache_size_bytes: u64,
}

/// Return some variables used to start child processes of the ledger.
async fn run_aux_setup(
    config: &config::Ledger,
    wasm_dir: &PathBuf,
) -> RunAuxSetup {
    // Prefetch needed wasm artifacts
    wasm_loader::pre_fetch_wasm(wasm_dir).await;

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

    // Find the RocksDB block cache size
    let db_block_cache_size_bytes = match config.shell.block_cache_bytes {
        Some(block_cache_bytes) => {
            tracing::info!("Block cache set from the configuration.");
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
        Byte::from_bytes(db_block_cache_size_bytes as u128)
            .get_appropriate_unit(true)
    );

    RunAuxSetup {
        vp_wasm_compilation_cache,
        tx_wasm_compilation_cache,
        db_block_cache_size_bytes,
    }
}

/// Launches two tasks into the asynchronous runtime:
///
///   1. An ABCI server.
///   2. A service for broadcasting transactions via an HTTP client.
///
/// Lastly, this function executes an ABCI shell on a new OS thread.
fn start_abci_broadcaster_shell(
    spawner: &mut AbortableSpawner,
    wasm_dir: PathBuf,
    setup_data: RunAuxSetup,
    config: config::Ledger,
) -> (
    Arc<task::JoinHandle<shell::Result<()>>>,
    task::JoinHandle<()>,
    thread::JoinHandle<()>,
) {
    let rpc_address = config.tendermint.rpc_address.to_string();
    let RunAuxSetup {
        vp_wasm_compilation_cache,
        tx_wasm_compilation_cache,
        db_block_cache_size_bytes,
    } = setup_data;

    // Channels for validators to send protocol txs to be broadcast to the
    // broadcaster service
    let (broadcaster_sender, broadcaster_receiver) =
        tokio::sync::mpsc::unbounded_channel();

    // Start broadcaster
    let broadcaster = if matches!(
        config.tendermint.tendermint_mode,
        TendermintMode::Validator
    ) {
        let (bc_abort_send, bc_abort_recv) =
            tokio::sync::oneshot::channel::<()>();

        spawner
            .spawn_abortable("Broadcaster", move |aborter| async move {
                // Construct a service for broadcasting protocol txs from the
                // ledger
                let mut broadcaster =
                    Broadcaster::new(&rpc_address, broadcaster_receiver);
                broadcaster.run(bc_abort_recv).await;
                tracing::info!("Broadcaster is no longer running.");

                drop(aborter);
            })
            .with_cleanup(async move {
                let _ = bc_abort_send.send(());
            })
    } else {
        // dummy async task, which will resolve instantly
        tokio::spawn(async { std::future::ready(()).await })
    };

    // Setup DB cache, it must outlive the DB instance that's in the shell
    let db_cache =
        rocksdb::Cache::new_lru_cache(db_block_cache_size_bytes as usize)
            .unwrap();

    // Construct our ABCI application.
    let ledger_address = config.shell.ledger_address;
    let (shell, abci_service) = AbcippShim::new(
        config,
        wasm_dir,
        broadcaster_sender,
        &db_cache,
        vp_wasm_compilation_cache,
        tx_wasm_compilation_cache,
    );

    // Start the ABCI server
    let abci = spawner
        .spawn_abortable("ABCI", move |aborter| async move {
            let res = run_abci(abci_service, ledger_address).await;

            drop(aborter);
            res
        })
        .with_join_handle_abort_cleanup();

    // Run the shell in the main thread
    let thread_builder = thread::Builder::new().name("ledger-shell".into());
    let shell_handler = thread_builder
        .spawn(move || {
            tracing::info!("Anoma ledger node started.");
            shell.run()
        })
        .expect("Must be able to start a thread for the shell");

    (abci, broadcaster, shell_handler)
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

/// Launches a new task managing a Tendermint process into the asynchronous
/// runtime, and returns its `JoinHandle`.
fn start_tendermint(
    spawner: &mut AbortableSpawner,
    config: &config::Ledger,
) -> task::JoinHandle<shell::Result<()>> {
    let tendermint_dir = config.tendermint_dir();
    let chain_id = config.chain_id.clone();
    let ledger_address = config.shell.ledger_address.to_string();
    let tendermint_config = config.tendermint.clone();
    let genesis_time = config
        .genesis_time
        .clone()
        .try_into()
        .expect("expected RFC3339 genesis_time");

    // Channel for signalling shut down to Tendermint process
    let (tm_abort_send, tm_abort_recv) =
        tokio::sync::oneshot::channel::<tokio::sync::oneshot::Sender<()>>();

    spawner
        .spawn_abortable("Tendermint", move |aborter| async move {
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
            if res.is_err() {
                tracing::error!("{:?}", &res);
            }
            res
        })
        .with_cleanup(async move {
            // Shutdown tendermint_node via a message to ensure that the child
            // process is properly cleaned-up.
            let (tm_abort_resp_send, tm_abort_resp_recv) =
                tokio::sync::oneshot::channel::<()>();
            // Ask to shutdown tendermint node cleanly. Ignore error, which can
            // happen if the tendermint_node task has already
            // finished.
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
        })
}
