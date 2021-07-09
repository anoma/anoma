pub mod protocol;
mod shell;
pub mod storage;

use std::convert::{TryFrom, TryInto};
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

use anoma_shared::types::storage::{BlockHash, BlockHeight};
use ed25519_dalek::PublicKey as Ed25519;
use futures::future::FutureExt;
use tendermint::abci::{request, response, Request, Response};
use tower::{Service, ServiceBuilder};
use tower_abci::{split, BoxError, Server};

use crate::node::ledger::shell::{MempoolTxType, Shell};
use crate::{config, genensis};

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
        // TODO: is this how we want to do this?
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Request) -> Self::Future {
        tracing::info!(?req);
        let rsp = match req {
            Request::InitChain(init) => {
                match self.init_chain(init) {
                    Ok(mut resp) => {
                        // Set the initial validator set
                        let genesis = genesis::genesis();
                        let pub_key = tendermint::PublicKey::Ed25519(
                            Ed25519::from_bytes(
                                &genesis.validator.keypair.public.to_bytes(),
                            )
                            .expect("Invalid public key"),
                        );
                        let power = genesis
                            .validator
                            .voting_power
                            .try_into()
                            .expect("unexpected validator's voting power");
                        let abci_validator =
                            tendermint::abci::types::ValidatorUpdate {
                                pub_key,
                                power,
                            };
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
                    BlockHeight::try_from(i64::from(block.header.height)),
                ) {
                    (Ok(hash), Ok(height)) => {
                        let _ = self.begin_block(hash, height);
                    }
                    (Ok(_), Err(_)) => {
                        tracing::error!(
                            "Unexpected block height {}",
                            block.header.height
                        );
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
            Request::Commit => Ok(Response::Commit(self.commit())),
            Request::Flush => Ok(Response::Flush),
            Request::Echo(msg) => Ok(Response::Echo(response::Echo {
                message: msg.message,
            })),
            Request::CheckTx(tx) => {
                let r#type = match tx.kind {
                    request::CheckTxKind::New => MempoolTxType::NewTransaction,
                    request::CheckTxKind::Recheck => {
                        MempoolTxType::RecheckTransaction
                    }
                };
                Ok(Response::CheckTx(self.mempool_validate(&*tx.tx, r#type)))
            }
            Request::ListSnapshots => {
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
        tracing::info!(?rsp);
        Box::pin(async move { rsp.map_err(|e| e.into()) }.boxed())
    }
}

pub fn reset(config: config::Ledger) -> Result<(), shell::Error> {
    shell::reset(config)
}

#[tokio::main]
pub async fn run(config: config::Ledger) {
    tracing_subscriber::fmt::init();

    // Construct our ABCI application.
    let service = Shell::new(&config.db, config::DEFAULT_CHAIN_ID.to_owned());

    // Split it into components.
    let (consensus, mempool, snapshot, info) = split::service(service, 1);

    // Hand those components to the ABCI server, but customize request behavior
    // for each category -- for instance, apply load-shedding only to mempool
    // and info requests, but not to consensus requests.
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

    // Run the ABCI server.
    server.listen(config.address).await.unwrap();
}
