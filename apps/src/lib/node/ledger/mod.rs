pub mod protocol;
mod shell;
pub mod storage;

use std::convert::{TryFrom, TryInto};
use std::future::Future;
use std::path::Path;
use std::pin::Pin;
use std::task::{Context, Poll};

use anoma_shared::types::storage::{BlockHash, BlockHeight};
use futures::future::FutureExt;
use tendermint::abci::{
    event::{Event, EventAttributeIndexExt},
    request, response, Request, Response,
};
use thiserror::Error;
use tower::{Service, ServiceBuilder};
use tower_abci::{split, BoxError, Server};

use crate::config;
use crate::genesis::{self, Validator};
use crate::node::ledger::shell::{MempoolTxType, Shell};

impl Service<Request> for Shell {
    type Response = Response;
    type Error = BoxError;
    type Future = Pin<Box<dyn Future<Output= Result<Response, BoxError>> + Send + 'static>>;

    fn poll_ready(&mut self, _cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
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
                    },
                    err @ Err(_) => err
                }

            },
            Request::Info(_) => Ok(Response::Info(self.last_state())),
            Request::Query(query) => self.query(query).map(Response::Query),
            Request::BeginBlock(block) => {
                match (
                    BlockHash::try_from(block.hash),
                    BlockHeight::try_from(block.header.height as i64).expect("Invalid block height")
                ) {
                    (Ok(hash), Ok(height)) => self.begin_block(hash, height),
                    (Ok(_), Err(_)) => {
                        tracing::error!(
                            "Unexpected block height {}",
                            req.header.unwrap().height
                        );
                    },
                    (err @ Err(_), _) => tracing::error!("{:#?}", err)
                };
                Ok(Response::BeginBlock(Default::default()))
            },
            Request::DeliverTx(deliver_tx) => Ok(Response::DeliverTx(self.apply_tx(deliver_tx))),
            Request::EndBlock(end) => {
                match BlockHeight::try_from(end.height) {
                    Ok(height) => Ok(Response::EndBlock(self.end_block(height))),
                    Err(_) => {
                        tracing::error!("Unexpected block height {}", end.height);
                        Ok(Response::EndBlock(Default::default()))
                    }
                }
            },
            Request::Commit => Ok(Response::Commit(self.commit())),
            Request::Flush => Ok(Response::Flush),
            Request::Echo(msg) => Ok(Response::Echo(response::Echo{message: msg.message})),
            Request::CheckTx(tx) => {
                let r#type = match tx.kind {
                    request::CheckTxKind::New => MempoolTxType::NewTransaction,
                    request::CheckTxKind::Recheck => MempoolTxType::RecheckTransaction,
                };
                Ok(Response::CheckTx(self.mempool_validate(tx.tx.into(), r#type)))
            }
            Request::ListSnapshots => Ok(Response::ListSnapshots(Default::default())),
            Request::OfferSnapshot(_) => Ok(Response::OfferSnapshot(Default::default())),
            Request::LoadSnapshotChunk(_) => Ok(Response::LoadSnapshotChunk(Default::default())),
            Request::ApplySnapshotChunk(_) => Ok(Response::ApplySnapshotChunk(Default::default())),
        };
        tracing::info!(?rsp);
        async move { rsp }.boxed()
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
    server
        .listen(config.address)
        .await
        .uwnrap()?

}
