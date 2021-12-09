use std::convert::{TryFrom, TryInto};
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::task::{Context, Poll};

use anoma::types::chain::ChainId;
use anoma::types::storage::BlockHeight;
use futures::future::FutureExt;
use tower::Service;
#[cfg(not(feature = "ABCI"))]
use tower_abci::{BoxError, Request as Req, Response as Resp};
#[cfg(feature = "ABCI")]
use tower_abci_old::{BoxError, Request as Req, Response as Resp};

use super::super::Shell;
use super::abcipp_shim_types::shim::{request, Error, Request, Response};
use crate::node::ledger::shims::abcipp_shim_types::shim::request::{
    BeginBlock, ProcessedTx,
};

/// The shim wraps the shell, which implements ABCI++.
/// The shim makes a crude translation between the ABCI interface currently used
/// by tendermint and the shell's interface.
#[derive(Debug)]
pub struct AbcippShim {
    service: Shell,
    begin_block_request: Option<BeginBlock>,
    block_txs: Vec<ProcessedTx>,
    shell_recv: tokio::sync::mpsc::Receiver<(
        Req,
        tokio::sync::oneshot::Sender<Result<Resp, BoxError>>,
    )>,
}

impl AbcippShim {
    /// Create a shell with a ABCI service that passes messages to and from the
    /// shell.
    pub fn new(
        base_dir: PathBuf,
        db_path: impl AsRef<Path>,
        chain_id: ChainId,
        wasm_dir: PathBuf,
        db_cache: &rocksdb::Cache,
        vp_wasm_compilation_cache: u64,
    ) -> (Self, AbciService) {
        let (shell_send, shell_recv) = tokio::sync::mpsc::channel(1024);
        (
            Self {
                service: Shell::new(
                    base_dir,
                    db_path,
                    chain_id,
                    wasm_dir,
                    Some(db_cache),
                    vp_wasm_compilation_cache,
                ),
                begin_block_request: None,
                block_txs: vec![],
                shell_recv,
            },
            AbciService { shell_send },
        )
    }

    /// Run the shell's blocking loop that receives messages from the
    /// [`AbciService`].
    pub fn run(mut self) {
        while let Some((req, resp_sender)) = self.shell_recv.blocking_recv() {
            let resp = match req {
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
                    // Invariant: The service call with
                    // `Request::ProcessProposal`
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
                    // If the wrapper txs were not properly submitted, reject
                    // all txs
                    let out_of_order =
                        txs.iter().any(|tx| tx.result.code > 3u32);
                    if out_of_order {
                        // The wrapper txs will need to be decrypted again
                        // and included in the proposed block after the current
                        self.service.reset_queue();
                    }
                    let begin_block_request =
                        self.begin_block_request.take().unwrap();
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
            let resp = resp.map_err(|e| e.into());
            if resp_sender.send(resp).is_err() {
                tracing::info!("ABCI response channel is closed")
            }
        }
    }
}

#[derive(Debug)]
pub struct AbciService {
    shell_send: tokio::sync::mpsc::Sender<(
        Req,
        tokio::sync::oneshot::Sender<Result<Resp, BoxError>>,
    )>,
}

/// The ABCI tower service implementation sends and receives messages to and
/// from the [`AbcippShim`] for requests from Tendermint.
impl Service<Req> for AbciService {
    type Error = BoxError;
    type Future =
        Pin<Box<dyn Future<Output = Result<Resp, BoxError>> + Send + 'static>>;
    type Response = Resp;

    fn poll_ready(
        &mut self,
        _cx: &mut Context<'_>,
    ) -> Poll<Result<(), Self::Error>> {
        // First, check if the shell is still alive
        if self.shell_send.is_closed() {
            return Poll::Ready(Err("The shell is closed".into()));
        }
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Req) -> Self::Future {
        let sender = self.shell_send.clone();
        Box::pin(
            async move {
                let (resp_send, recv) = tokio::sync::oneshot::channel();
                sender.send((req, resp_send)).await?;
                match recv.await {
                    Ok(resp) => resp,
                    Err(err) => {
                        tracing::info!("ABCI response channel didn't respond");
                        Err(err.into())
                    }
                }
            }
            .boxed(),
        )
    }
}
