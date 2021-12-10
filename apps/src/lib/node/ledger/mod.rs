mod broadcaster;
pub mod events;
pub mod protocol;
pub mod rpc;
mod shell;
mod shims;
pub mod storage;
pub mod tendermint_node;

use std::convert::TryInto;
use std::path::PathBuf;
use std::sync::mpsc::{channel, Receiver};

use futures::future::{AbortHandle, AbortRegistration, Abortable};
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::abci::CheckTxType;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::CheckTxType;
use tower::ServiceBuilder;
#[cfg(not(feature = "ABCI"))]
use tower_abci::{response, split, Server};
#[cfg(feature = "ABCI")]
use tower_abci_old::{response, split, Server};

use crate::config::TendermintMode;
use crate::node::ledger::broadcaster::Broadcaster;
use crate::node::ledger::shell::{Error, MempoolTxType, Shell};
use crate::node::ledger::shims::abcipp_shim::AbcippShim;
use crate::node::ledger::shims::abcipp_shim_types::shim::{Request, Response};
use crate::{config, wasm_loader};

/// A panic-proof handle for aborting a future. Will abort during
/// stack unwinding as its drop method calls abort.
struct Aborter {
    handle: AbortHandle,
}

impl Drop for Aborter {
    fn drop(&mut self) {
        self.handle.abort();
    }
}

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
            Request::ProcessProposal(block) => {
                #[cfg(not(feature = "ABCI"))]
                {
                    Ok(Response::ProcessProposal(self.process_proposal(block)))
                }
                #[cfg(feature = "ABCI")]
                {
                    Ok(Response::ProcessProposal(
                        self.process_and_decode_proposal(block),
                    ))
                }
            }
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

/// Resets the tendermint_node state and removes database files
pub fn reset(config: config::Ledger) -> Result<(), shell::Error> {
    shell::reset(config)
}

/// Runs the an asynchronous ABCI server with four sub-components for consensus,
/// mempool, snapshot, and info.
///
/// Runs until an abort handles sends a message to terminate the process
#[tokio::main]
async fn run_shell(
    config: config::Ledger,
    wasm_dir: PathBuf,
    abort_registration: AbortRegistration,
    failure_receiver: Receiver<()>,
) {
    // Construct our ABCI application.
    #[allow(clippy::clone_on_copy)]
    let rpc_address = config.tendermint.rpc_address.clone();
    #[allow(clippy::clone_on_copy)]
    let ledger_address = config.shell.ledger_address.clone();
    let mode = config.tendermint.tendermint_mode.clone();
    let (broadcaster_sender, broadcaster_receiver) =
        tokio::sync::mpsc::unbounded_channel();
    let service = AbcippShim::new(config, wasm_dir, broadcaster_sender);

    // Split it into components.
    let (consensus, mempool, snapshot, info) = split::service(service, 5);

    // Hand those components to the ABCI server, but customize request behavior
    // for each category
    let server = Server::builder()
        .consensus(consensus)
        .snapshot(snapshot)
        .mempool(
            ServiceBuilder::new()
                .load_shed()
                .buffer(10)
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

    // Run the server with the shell
    let abortable_shell =
        Abortable::new(server.listen(ledger_address), abort_registration);

    // Start up the service to broadcast protocol txs if we are in validator
    // mode
    if matches!(mode, TendermintMode::Validator) {
        let broadcaster = Broadcaster::new(rpc_address, broadcaster_receiver);
        // The shell will be aborted when Tendermint exits
        let _ = tokio::select!(
            _ = abortable_shell => {},
            result = broadcaster::run(broadcaster) => {
                if let Err(err) = result {
                    use std::io::Write;
                    let _ = std::io::stdout().lock().flush();
                    let _ = std::io::stderr().lock().flush();
                    tracing::error!("{}", err);
                    std::process::exit(1);
                }
            }
        );
    } else {
        let _ = abortable_shell.await;
    }

    // Check if a failure signal was sent
    if let Ok(()) = failure_receiver.try_recv() {
        // Exit with error status code
        use std::io::Write;
        let _ = std::io::stdout().lock().flush();
        let _ = std::io::stderr().lock().flush();
        std::process::exit(1)
    }
}

/// Runs two child processes: A tendermint node, a shell which contains an ABCI
/// server for talking to the tendermint node. Both should be alive for correct
/// functioning.
///
/// When the thread containing the tendermint node finishes its work (either by
/// panic or by a termination signal), will send an abort message to the shell.
///
/// When the shell process finishes, we check if it finished with a panic. If it
/// did we stop the tendermint node with a channel that acts as a kill switch.
pub fn run(config: config::Ledger, wasm_dir: PathBuf) {
    let tendermint_dir = config.tendermint_dir();
    let ledger_address = config.shell.ledger_address.to_string();
    let chain_id = config.chain_id.clone();
    let genesis_time = config
        .genesis_time
        .clone()
        .try_into()
        .expect("expected RFC3339 genesis_time");
    let tendermint_config = config.tendermint.clone();

    // For signalling shut down to the Tendermint node, sent from the
    // shell or from within the Tendermint process itself.
    // Send `true` for a graceful shutdown or `false` on a critical error.
    let (abort_sender, abort_receiver) = channel();
    let shell_abort_sender = abort_sender.clone();

    // For signalling shut down to the shell from Tendermint, which ensures that
    // drop is called on the database
    let (abort_handle, abort_registration) = AbortHandle::new_pair();

    // Prefetch needed wasm artifacts
    wasm_loader::pre_fetch_wasm(&wasm_dir);
    // Because we cannot attach any data to the `abort_handle`, we also need
    // another channel for signalling an error to the shell from Tendermint
    let (failure_sender, failure_receiver) = channel();

    // start Tendermint node
    let tendermint_handle = std::thread::spawn(move || {
        if let Err(err) = tendermint_node::run(
            tendermint_dir,
            chain_id,
            genesis_time,
            ledger_address,
            tendermint_config,
            abort_sender,
            abort_receiver,
        ) {
            tracing::error!("Tendermint node failed with {}", err);
            failure_sender.send(()).unwrap();
        }
        // Once tendermint node stops, ensure that we stop the shell.
        // Implemented in the drop method to be panic-proof
        Aborter {
            handle: abort_handle,
        };
    });

    // start the shell + ABCI server
    let shell_handle = std::thread::spawn(move || {
        run_shell(config, wasm_dir, abort_registration, failure_receiver);
    });

    tracing::info!("Anoma ledger node started.");

    match shell_handle.join() {
        Err(_) => {
            tracing::info!("Anoma shut down unexpectedly");
            // if the shell panicked, shut down the tendermint node
            let _ = shell_abort_sender.send(false);
        }
        _ => tracing::info!("Shutting down Anoma node"),
    }
    tendermint_handle
        .join()
        .expect("Tendermint node did not shut down properly");
}
