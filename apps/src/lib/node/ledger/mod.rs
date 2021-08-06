pub mod protocol;
pub mod rpc;
mod shell;
pub mod storage;
mod tendermint_node;

use std::convert::{TryFrom, TryInto};
use std::future::Future;
use std::pin::Pin;
use std::sync::mpsc::channel;
use std::task::{Context, Poll};

use anoma::types::storage::{BlockHash, BlockHeight};
use futures::future::{AbortHandle, AbortRegistration, Abortable, FutureExt};
use tendermint_proto::abci::CheckTxType;
use tower::{Service, ServiceBuilder};
use tower_abci::{response, split, BoxError, Request, Response, Server};

use crate::config;
use crate::config::genesis;
use crate::node::ledger::shell::{MempoolTxType, Shell};

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

impl Service<Request> for Shell {
    type Error = BoxError;
    type Future = Pin<
        Box<dyn Future<Output = Result<Response, BoxError>> + Send + 'static>,
    >;
    type Response = Response;

    fn poll_ready(
        &mut self,
        _cx: &mut Context<'_>,
    ) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Request) -> Self::Future {
        tracing::debug!(?req);
        let rsp = match req {
            Request::InitChain(init) => {
                match self.init_chain(init) {
                    Ok(mut resp) => {
                        // Set the initial validator set
                        let genesis = genesis::genesis();
                        let mut abci_validator =
                            tendermint_proto::abci::ValidatorUpdate::default();
                        let pub_key = tendermint_proto::crypto::PublicKey {
                            sum: Some(tendermint_proto::crypto::public_key::Sum::Ed25519(
                                genesis.validator.keypair.public.to_bytes().to_vec(),
                            )),
                        };
                        abci_validator.pub_key = Some(pub_key);
                        abci_validator.power = genesis
                            .validator
                            .voting_power
                            .try_into()
                            .expect("unexpected validator's voting power");
                        resp.validators.push(abci_validator);
                        Ok(Response::InitChain(resp))
                    }
                    Err(inner) => Err(inner),
                }
            }
            Request::Info(_) => Ok(Response::Info(self.last_state())),
            Request::Query(query) => Ok(Response::Query(self.query(query))),
            Request::BeginBlock(block) => {
                match (
                    BlockHash::try_from(&*block.hash),
                    block.header.expect("missing block's header").try_into(),
                ) {
                    (Ok(hash), Ok(header)) => {
                        let _ = self.begin_block(hash, header);
                    }
                    (Ok(_), Err(msg)) => {
                        tracing::error!("Unexpected block header {}", msg);
                    }
                    (err @ Err(_), _) => tracing::error!("{:#?}", err),
                };
                Ok(Response::BeginBlock(Default::default()))
            }
            Request::DeliverTx(deliver_tx) => {
                Ok(Response::DeliverTx(self.apply_tx(deliver_tx)))
            }
            Request::EndBlock(end) => match BlockHeight::try_from(end.height) {
                Ok(height) => Ok(Response::EndBlock(self.end_block(height))),
                Err(_) => {
                    tracing::error!("Unexpected block height {}", end.height);
                    Ok(Response::EndBlock(Default::default()))
                }
            },
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
        };
        tracing::debug!(?rsp);
        Box::pin(async move { rsp.map_err(|e| e.into()) }.boxed())
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
) {
    // Construct our ABCI application.
    let service = Shell::new(&config.db, config::DEFAULT_CHAIN_ID.to_owned());

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
    let future =
        Abortable::new(server.listen(config.address), abort_registration);
    let _ = future.await;
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
pub fn run(config: config::Ledger) {
    let home_dir = config.tendermint.clone();
    let socket_address = config.address.to_string();

    // used for shutting down Tendermint node in case the shell panics
    let (sender, receiver) = channel();
    let kill_switch = sender.clone();
    // used for shutting down the shell and making sure that drop is called
    // on the database
    let (abort_handle, abort_registration) = AbortHandle::new_pair();

    // start Tendermint node
    let tendermint_handle = std::thread::spawn(move || {
        if let Err(err) =
            tendermint_node::run(home_dir, &socket_address, sender, receiver)
        {
            tracing::error!(
                "Failed to start-up a Tendermint node with {}",
                err
            );
        }
        // Once tendermint node stops, ensure that we stop the shell.
        // Implemented in the drop method to be panic-proof
        Aborter {
            handle: abort_handle,
        };
    });

    // start the shell + ABCI server
    let shell_handle = std::thread::spawn(move || {
        run_shell(config, abort_registration);
    });

    tracing::info!("Anoma ledger node started.");

    match shell_handle.join() {
        Err(_) => {
            tracing::info!("Anoma shut down unexpectedly");
            // if the shell panicked, shut down the tendermint node
            let _ = kill_switch.send(true);
        }
        _ => tracing::info!("Shutting down Anoma node"),
    }
    tendermint_handle
        .join()
        .expect("Tendermint node did not shut down properly");
}
