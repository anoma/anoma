use std::convert::TryFrom;
use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::task::{Context, Poll};

#[cfg(feature = "ABCI")]
use anoma::proto::Tx;
#[cfg(feature = "ABCI")]
use anoma::types::hash::Hash;
#[cfg(feature = "ABCI")]
use anoma::types::storage::BlockHash;
#[cfg(feature = "ABCI")]
use anoma::types::transaction::hash_tx;
use futures::future::FutureExt;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::abci::RequestBeginBlock;
use tokio::sync::mpsc::UnboundedSender;
use tower::Service;
#[cfg(not(feature = "ABCI"))]
use tower_abci::{BoxError, Request as Req, Response as Resp};
#[cfg(feature = "ABCI")]
use tower_abci_old::{BoxError, Request as Req, Response as Resp};

use super::super::Shell;
use super::abcipp_shim_types::shim::request::{FinalizeBlock, ProcessedTx};
#[cfg(not(feature = "ABCI"))]
use super::abcipp_shim_types::shim::response::TxResult;
use super::abcipp_shim_types::shim::{Error, Request, Response};
use crate::config;
#[cfg(feature = "ABCI")]
use crate::node::ledger::shell::Error::TxDecoding;

/// The shim wraps the shell, which implements ABCI++.
/// The shim makes a crude translation between the ABCI interface currently used
/// by tendermint and the shell's interface.
#[derive(Debug)]
pub struct AbcippShim {
    service: Shell,
    #[cfg(feature = "ABCI")]
    begin_block_request: Option<RequestBeginBlock>,
    processed_txs: Vec<ProcessedTx>,
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
                #[cfg(feature = "ABCI")]
                begin_block_request: None,
                processed_txs: vec![],
                shell_recv,
            },
            AbciService { shell_send },
        )
    }

    #[cfg(feature = "ABCI")]
    /// Get the hash of the txs in the block
    pub fn get_hash(&self) -> Hash {
        let bytes: Vec<u8> = self
            .processed_txs
            .iter()
            .flat_map(|processed| processed.tx.clone())
            .collect();
        hash_tx(bytes.as_slice())
    }

    /// Run the shell's blocking loop that receives messages from the
    /// [`AbciService`].
    pub fn run(mut self) {
        while let Ok((req, resp_sender)) = self.shell_recv.recv() {
            let resp = match req {
                #[cfg(not(feature = "ABCI"))]
                Req::ProcessProposal(proposal) => {
                    let txs = proposal.txs.clone();
                    self.service
                        .call(Request::ProcessProposal(proposal))
                        .map_err(Error::from)
                        .and_then(|res| match res {
                            Response::ProcessProposal(resp) => {
                                for (result, tx) in resp
                                    .tx_results
                                    .iter()
                                    .map(TxResult::from)
                                    .zip(txs.into_iter())
                                {
                                    self.processed_txs
                                        .push(ProcessedTx { tx, result });
                                }
                                Ok(Resp::ProcessProposal(resp))
                            }
                            _ => unreachable!(),
                        })
                }
                #[cfg(not(feature = "ABCI"))]
                Req::FinalizeBlock(block) => {
                    let mut txs = vec![];
                    std::mem::swap(&mut txs, &mut self.processed_txs);
                    let mut finalize_req: FinalizeBlock = block.into();
                    finalize_req.txs = txs;
                    self.service
                        .call(Request::FinalizeBlock(finalize_req))
                        .map_err(Error::from)
                        .and_then(|res| match res {
                            Response::FinalizeBlock(resp) => {
                                Ok(Resp::FinalizeBlock(resp.into()))
                            }
                            _ => Err(Error::ConvertResp(res)),
                        })
                }
                #[cfg(feature = "ABCI")]
                Req::BeginBlock(block) => {
                    // we save this data to be forwarded to finalize later
                    self.begin_block_request = Some(block);
                    Ok(Resp::BeginBlock(Default::default()))
                }
                #[cfg(feature = "ABCI")]
                Req::DeliverTx(deliver_tx) => {
                    // We call [`process_single_tx`] to report back the validity
                    // of the tx to tendermint.
                    self.service
                        .call(Request::DeliverTx(deliver_tx))
                        .map_err(Error::from)
                        .and_then(|res| match res {
                            Response::DeliverTx(resp) => {
                                // The transaction's data is used in the
                                // response to add transfer event via
                                // `impl From<Tx> for ResponseDeliverTx`
                                // conversion.
                                let tx = Tx::try_from(resp.tx.as_ref());
                                self.processed_txs.push(resp);
                                Ok(Resp::DeliverTx(
                                    tx.map_err(TxDecoding)?.into(),
                                ))
                            }
                            _ => unreachable!(),
                        })
                }
                #[cfg(feature = "ABCI")]
                Req::EndBlock(_) => {
                    let mut txs = vec![];
                    std::mem::swap(&mut txs, &mut self.processed_txs);
                    let mut end_block_request: FinalizeBlock =
                        self.begin_block_request.take().unwrap().into();
                    let hash = self.get_hash();
                    end_block_request.hash = BlockHash::from(hash.clone());
                    end_block_request.header.hash = hash;
                    end_block_request.txs = txs;
                    self.service
                        .call(Request::FinalizeBlock(end_block_request))
                        .map_err(Error::from)
                        .and_then(|res| match res {
                            Response::FinalizeBlock(resp) => {
                                Ok(Resp::EndBlock(resp.into()))
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
