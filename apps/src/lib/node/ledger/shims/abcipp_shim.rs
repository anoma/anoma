use std::convert::TryFrom;
use std::future::Future;
use std::path::Path;
use std::pin::Pin;
use std::task::{Context, Poll};

use anoma::types::storage::BlockHeight;
use futures::future::FutureExt;
use tower::Service;
use tower_abci::{BoxError, Request as Req, Response as Resp};

use super::super::Shell;
use super::abcipp_shim_types::shim::{
    request, Error, Request, Response, TxBytes,
};

/// The shim wraps the shell, which implements ABCI++
/// The shim makes a crude translation between the ABCI
/// interface currently used by tendermint and the shell's
/// interface
pub struct AbcippShim {
    service: Shell,
    block_txs: Vec<TxBytes>,
}

impl AbcippShim {
    pub fn new(db_path: impl AsRef<Path>, chain_id: String) -> Self {
        Self {
            service: Shell::new(db_path, chain_id),
            block_txs: vec![],
        }
    }
}

/// This is the actual tower service that we run for now.
/// It provides the translation between tendermints interface
/// and the interface of the shell service.
impl Service<Req> for AbcippShim {
    type Error = BoxError;
    type Future =
        Pin<Box<dyn Future<Output = Result<Resp, BoxError>> + Send + 'static>>;
    type Response = Resp;

    fn poll_ready(
        &mut self,
        _cx: &mut Context<'_>,
    ) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Req) -> Self::Future {
        let rsp = match req {
            Req::CheckTx(tx_request) => self
                .service
                .call(Request::ProcessProposal(tx_request.tx.into()))
                .map_err(Error::from)
                .and_then(|res| match res {
                    Response::ProcessProposal(resp) => {
                        Ok(Resp::CheckTx(resp.into()))
                    }
                    _ => Err(Error::ConvertResp(res)),
                }),
            Req::BeginBlock(block) => {
                // we simply forward BeginBlock request to the PrepareProposal
                // request
                self.service
                    .call(Request::PrepareProposal(block.into()))
                    .map_err(Error::from)
                    .and_then(|res| match res {
                        Response::PrepareProposal(resp) => {
                            Ok(Resp::BeginBlock(resp.into()))
                        }
                        _ => Err(Error::ConvertResp(res)),
                    })
            }
            Req::DeliverTx(deliver_tx) => {
                // We store all the transactions to be applied in
                // bulk at a later step
                self.block_txs.push(deliver_tx.tx);
                Ok(Resp::DeliverTx(Default::default()))
            }
            Req::EndBlock(end) => {
                BlockHeight::try_from(end.height).unwrap_or_else(|_| {
                    panic!("Unexpected block height {}", end.height)
                });
                let mut txs = vec![];
                std::mem::swap(&mut txs, &mut self.block_txs);

                self.service
                    .call(Request::FinalizeBlock(request::FinalizeBlock {
                        height: end.height,
                        txs,
                    }))
                    .map_err(Error::from)
                    .and_then(|res| match res {
                        Response::FinalizeBlock(resp) => {
                            let x = Resp::EndBlock(resp.into());
                            Ok(x)
                        }
                        _ => Err(Error::ConvertResp(res)),
                    })
            }
            _ => match Request::try_from(req.clone()) {
                Ok(request) => self
                    .service
                    .call(request)
                    .map(Resp::try_from)
                    .map_err(Error::Shell)
                    .and_then(|inner| inner),
                Err(err) => Err(err),
            },
        };
        Box::pin(async move { rsp.map_err(|e| e.into()) }.boxed())
    }
}
