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
use anoma::proto::Tx;
use crate::node::ledger::shell::Error::TxDecoding;

/// The shim wraps the shell, which implements ABCI++.
/// The shim makes a crude translation between the ABCI interface currently used
/// by tendermint and the shell's interface.
#[derive(Debug)]
pub struct AbcippShim {
    service: Shell,
    begin_block_request: Option<BeginBlock>,
    block_txs: Vec<ProcessedTx>,
    shell_recv: std::sync::mpsc::Receiver<(
        Req,
        tokio::sync::oneshot::Sender<Result<Resp, BoxError>>,
    )>,
}

impl AbcippShim {
    /// Create a shell with a ABCI service that passes messages to and from the
    /// shell.
    pub fn new(
        config: config::Ledger,
        wasm_dir: PathBuf,
        broadcast_sender: UnboundedSender<Vec<u8>>,
        db_cache: &rocksdb::Cache,
        vp_wasm_compilation_cache: u64,
        tx_wasm_compilation_cache: u64,
    ) -> (Self, AbciService) {
        // We can use an unbounded channel here, because tower-abci limits the
        // the number of requests that can come in
        let (shell_send, shell_recv) = std::sync::mpsc::channel();
        (
            Self {
                service: Shell::new(
                    config,
                    wasm_dir,
                    broadcast_sender,
                    Some(db_cache),
                    vp_wasm_compilation_cache,
                    tx_wasm_compilation_cache,
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
        while let Ok((req, resp_sender)) = self.shell_recv.recv() {
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
                                #[cfg(feature = "ABCI")]
                                let tx = Tx::try_from(resp.tx.as_ref());
                                #[cfg(not(feature = "ABCI"))]
                                let tx = Tx::try_from(deliver_tx.tx.as_ref());
                                self.block_txs.push(ProcessedTx {
                                    #[cfg(not(feature = "ABCI"))]
                                    tx: deliver_tx.tx,
                                    #[cfg(feature = "ABCI")]
                                    tx: resp.tx,
                                    result: resp.result,
                                });
                                Ok(Resp::DeliverTx(tx.map_err(TxDecoding)?.into()))
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
                        self.service.reset_tx_queue_iter();
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
    shell_send: std::sync::mpsc::Sender<(
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
        // Nothing to check as the sender's channel is unbounded
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Req) -> Self::Future {
        let (resp_send, recv) = tokio::sync::oneshot::channel();
        let result = self.shell_send.send((req, resp_send));
        Box::pin(
            async move {
                if let Err(err) = result {
                    // The shell has shut-down
                    return Err(err.into());
                }
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
