use std::convert::{TryFrom, TryInto};
use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::task::{Context, Poll};

use anoma::types::storage::BlockHeight;
use futures::future::FutureExt;
use tokio::sync::mpsc::UnboundedSender;
use tower::Service;
#[cfg(not(feature = "ABCI"))]
use tower_abci::{BoxError, Request as Req, Response as Resp};
#[cfg(feature = "ABCI")]
use tower_abci_old::{BoxError, Request as Req, Response as Resp};

use super::super::Shell;
use super::abcipp_shim_types::shim::{request, Error, Request, Response};
use crate::config;
use crate::node::ledger::shims::abcipp_shim_types::shim::request::{
    BeginBlock, ProcessedTx,
};

/// The shim wraps the shell, which implements ABCI++
/// The shim makes a crude translation between the ABCI
/// interface currently used by tendermint and the shell's
/// interface
pub struct AbcippShim {
    service: Shell,
    begin_block_request: Option<BeginBlock>,
    block_txs: Vec<ProcessedTx>,
}

impl AbcippShim {
    pub fn new(
        config: config::Ledger,
        wasm_dir: PathBuf,
        broadcast_sender: UnboundedSender<Vec<u8>>,
    ) -> Self {
        Self {
            service: Shell::new(config, wasm_dir, broadcast_sender),
            begin_block_request: None,
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
            Req::BeginBlock(block) => {
                // we save this data to be forwarded to finalize later
                self.begin_block_request =
                    Some(block.try_into().unwrap_or_else(|_| {
                        panic!("Could not read begin block request");
                    }));
                Ok(Resp::BeginBlock(Default::default()))
            }
            Req::DeliverTx(deliver_tx) => {
                // We call [`process_proposal`] to report back the validity
                // of the tx to tendermint.
                // Invariant: The service call with `Request::ProcessProposal`
                // must always return `Response::ProcessProposal`
                self.service
                    .call(Request::ProcessProposal(
                        #[cfg(not(feature = "ABCI"))]
                        deliver_tx.tx.clone().into(),
                        #[cfg(feature = "ABCI")]
                        deliver_tx.tx.into(),
                    ))
                    .map_err(Error::from)
                    .and_then(|res| match res {
                        Response::ProcessProposal(resp) => {
                            self.block_txs.push(ProcessedTx {
                                #[cfg(not(feature = "ABCI"))]
                                tx: deliver_tx.tx,
                                #[cfg(feature = "ABCI")]
                                tx: resp.tx,
                                result: resp.result,
                            });
                            Ok(Resp::DeliverTx(Default::default()))
                        }
                        _ => unreachable!(),
                    })
            }
            Req::EndBlock(end) => {
                BlockHeight::try_from(end.height).unwrap_or_else(|_| {
                    panic!("Unexpected block height {}", end.height)
                });
                let mut txs = vec![];
                std::mem::swap(&mut txs, &mut self.block_txs);
                // If the wrapper txs were not properly submitted, reject all
                // txs
                let out_of_order = txs.iter().any(|tx| tx.result.code > 3u32);
                if out_of_order {
                    // The wrapper txs will need to be decrypted again
                    // and included in the proposed block after the current
                    self.service.reset_queue();
                }
                let begin_block_request =
                    self.begin_block_request.take().expect(
                        "Cannot process end block request without begin block \
                         request",
                    );
                self.service
                    .call(Request::FinalizeBlock(request::FinalizeBlock {
                        hash: begin_block_request.hash,
                        header: begin_block_request.header,
                        byzantine_validators: begin_block_request
                            .byzantine_validators,
                        txs,
                        reject_all_decrypted: out_of_order,
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
