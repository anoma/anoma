mod events;
pub mod protocol;
pub mod rpc;
mod shell;
mod shims;
pub mod storage;
pub mod tendermint_node;

use std::convert::{TryFrom, TryInto};
use std::mem;
use std::sync::mpsc::{channel, Receiver};

use anoma::types::storage::BlockHash;
use futures::future::{AbortHandle, AbortRegistration, Abortable};
use tendermint_proto::abci::CheckTxType;
use tower::ServiceBuilder;
use tower_abci::{response, split, Server};

use crate::config;
use crate::node::ledger::shell::{Error, MempoolTxType, Shell};
use crate::node::ledger::shims::abcipp_shim::AbcippShim;
use crate::node::ledger::shims::abcipp_shim_types::shim::{Request, Response};

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
            Request::PrepareProposal(block) => {
                match (
                    BlockHash::try_from(&*block.hash),
                    block.header.expect("missing block's header").try_into(),
                ) {
                    (Ok(hash), Ok(header)) => {
                        let _ = self.prepare_proposal(
                            hash,
                            header,
                            block.byzantine_validators,
                        );
                    }
                    (Ok(_), Err(msg)) => {
                        tracing::error!("Unexpected block header {}", msg);
                    }
                    (err @ Err(_), _) => tracing::error!("{:#?}", err),
                };
                Ok(Response::PrepareProposal(Default::default()))
            }
            Request::VerifyHeader(_req) => {
                Ok(Response::VerifyHeader(self.verify_header(_req)))
            }
            Request::ProcessProposal(block) => {
                Ok(Response::ProcessProposal(self.process_proposal(block)))
            }
            Request::RevertProposal(_req) => {
                Ok(Response::RevertProposal(self.revert_proposal(_req)))
            }
            Request::ExtendVote(_req) => {
                Ok(Response::ExtendVote(self.extend_vote(_req)))
            }
            Request::VerifyVoteExtension(_req) => {
                Ok(Response::VerifyVoteExtension(Default::default()))
            }
            Request::FinalizeBlock(finalize) => {
                self.finalize_block(finalize).map(Response::FinalizeBlock)
            }
            Request::Commit(_) => Ok(Response::Commit(self.commit())),
            Request::Flush(_) => Ok(Response::Flush(Default::default())),
            Request::SetOption(_) => {
                Ok(Response::SetOption(Default::default()))
            }
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
    abort_registration: AbortRegistration,
    failure_receiver: Receiver<()>,
) {
    // Construct our ABCI application.
    let service = AbcippShim::new(
        config.base_dir,
        &config.db,
        config.chain_id,
        config.wasm_dir,
    );

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
    let abortable_shell = Abortable::new(
        server.listen(config.ledger_address),
        abort_registration,
    );
    // The shell will be aborted when Tendermint exits
    let _ = abortable_shell.await;

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
pub fn run(mut config: config::Ledger) {
    let home_dir = config.tendermint.clone();
    let ledger_address = config.ledger_address.to_string();
    let rpc_address = config.rpc_address.to_string();
    let p2p_address = config.p2p_address.to_string();
    let p2p_persistent_peers = mem::take(&mut config.p2p_persistent_peers);
    let chain_id = config.chain_id.clone();
    let genesis_time = config
        .genesis_time
        .clone()
        .try_into()
        .expect("expected RFC3339 genesis_time");
    let p2p_pex = config.p2p_pex;

    // For signalling shut down to the Tendermint node, sent from the
    // shell or from within the Tendermint process itself.
    // Send `true` for a graceful shutdown or `false` on a critical error.
    let (abort_sender, abort_receiver) = channel();
    let shell_abort_sender = abort_sender.clone();

    // For signalling shut down to the shell from Tendermint, which ensures that
    // drop is called on the database
    let (abort_handle, abort_registration) = AbortHandle::new_pair();

    // Because we cannot attach any data to the `abort_handle`, we also need
    // another channel for signalling an error to the shell from Tendermint
    let (failure_sender, failure_receiver) = channel();

    // start Tendermint node
    let tendermint_handle = std::thread::spawn(move || {
        if let Err(err) = tendermint_node::run(
            home_dir,
            chain_id,
            genesis_time,
            ledger_address,
            rpc_address,
            p2p_address,
            p2p_persistent_peers,
            p2p_pex,
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
        run_shell(config, abort_registration, failure_receiver);
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
