#[cfg(not(feature = "ABCI"))]
use tower_abci::{Request, Response};
#[cfg(feature = "ABCI")]
use tower_abci_old::{Request, Response};

pub mod shim {
    use std::convert::TryFrom;

    #[cfg(not(feature = "ABCI"))]
    use tendermint_proto::abci::{
        RequestApplySnapshotChunk, RequestCheckTx, RequestCommit, RequestEcho,
        RequestExtendVote, RequestFinalizeBlock, RequestFlush, RequestInfo, RequestInitChain,
        RequestListSnapshots, RequestLoadSnapshotChunk, RequestOfferSnapshot,
        RequestPrepareProposal, RequestProcessProposal, RequestQuery, RequestVerifyVoteExtension,
        ResponseApplySnapshotChunk, ResponseCheckTx, ResponseCommit,
        ResponseEcho, ResponseExtendVote, ResponseFinalizeBlock, ResponseFlush, ResponseInfo,
        ResponseInitChain, ResponseListSnapshots, ResponseLoadSnapshotChunk,
        ResponseOfferSnapshot, ResponsePrepareProposal, ResponseProcessProposal,
        ResponseQuery, ResponseVerifyVoteExtension,
    };
    #[cfg(feature = "ABCI")]
    use tendermint_proto_abci::abci::{
        RequestApplySnapshotChunk, RequestCheckTx, RequestCommit, RequestDeliverTx,
        RequestEcho, RequestEndBlock, RequestFlush, RequestInfo, RequestInitChain, RequestListSnapshots,
        RequestLoadSnapshotChunk, RequestOfferSnapshot, RequestQuery,
        ResponseApplySnapshotChunk, ResponseCheckTx, ResponseCommit,
        ResponseDeliverTx, ResponseEcho, ResponseEndBlock, ResponseFlush, ResponseInfo,
        ResponseInitChain,ResponseListSnapshots, ResponseLoadSnapshotChunk,
        ResponseOfferSnapshot, ResponseQuery,
    };
    use thiserror::Error;

    use super::{Request as Req, Response as Resp};
    use crate::node::ledger::shell;

    pub type TxBytes = Vec<u8>;

    #[derive(Error, Debug)]
    #[allow(clippy::large_enum_variant)]
    pub enum Error {
        #[error("Error converting Request from ABCI to ABCI++: {0:?}")]
        ConvertReq(Req),
        #[error("Error converting Response from ABCI++ to ABCI: {0:?}")]
        ConvertResp(Response),
        #[error("{0:?}")]
        Shell(shell::Error),
    }

    /// Errors from the shell need to be propagated upward
    impl From<shell::Error> for Error {
        fn from(err: shell::Error) -> Self {
            Self::Shell(err)
        }
    }

    #[allow(clippy::large_enum_variant)]
    /// Our custom request types. It is the duty of the shim to change
    /// the request types coming from tower-abci to these before forwarding
    /// it to the shell
    ///
    /// Each request contains a custom payload type as well, which may
    /// be simply a unit struct
    pub enum Request {
        InitChain(RequestInitChain),
        Info(RequestInfo),
        Query(RequestQuery),
        #[cfg(not(feature = "ABCI"))]
        PrepareProposal(RequestPrepareProposal),
        #[allow(dead_code)]
        VerifyHeader(request::VerifyHeader),
        #[cfg(not(feature = "ABCI"))]
        ProcessProposal(RequestProcessProposal),
        #[cfg(feature = "ABCI")]
        DeliverTx(RequestDeliverTx),
        #[allow(dead_code)]
        #[cfg(not(feature = "ABCI"))]
        RevertProposal(request::RevertProposal),
        #[cfg(not(feature = "ABCI"))]
        ExtendVote(RequestExtendVote),
        #[cfg(not(feature = "ABCI"))]
        VerifyVoteExtension(RequestVerifyVoteExtension),
        #[cfg(not(feature = "ABCI"))]
        FinalizeBlock(RequestFinalizeBlock),
        #[cfg(feature = "ABCI")]
        EndBlock(RequestEndBlock),
        Commit(RequestCommit),
        Flush(RequestFlush),
        Echo(RequestEcho),
        CheckTx(RequestCheckTx),
        ListSnapshots(RequestListSnapshots),
        OfferSnapshot(RequestOfferSnapshot),
        LoadSnapshotChunk(RequestLoadSnapshotChunk),
        ApplySnapshotChunk(RequestApplySnapshotChunk),
    }

    /// Attempt to convert a tower-abci request to an internal one
    impl TryFrom<Req> for Request {
        type Error = Error;

        fn try_from(req: Req) -> Result<Request, Error> {
            match req {
                Req::InitChain(inner) => Ok(Request::InitChain(inner)),
                Req::Info(inner) => Ok(Request::Info(inner)),
                Req::Query(inner) => Ok(Request::Query(inner)),
                Req::Commit(inner) => Ok(Request::Commit(inner)),
                Req::Flush(inner) => Ok(Request::Flush(inner)),
                Req::Echo(inner) => Ok(Request::Echo(inner)),
                #[cfg(not(feature = "ABCI"))]
                Req::ExtendVote(inner) => Ok(Request::ExtendVote(inner)),
                #[cfg(not(feature = "ABCI"))]
                Req::VerifyVoteExtension(inner) => {
                    Ok(Request::VerifyVoteExtension(inner))
                }
                #[cfg(feature = "ABCI")]
                Req::EndBlock(end) => Ok(Request::EndBlock(end)),
                Req::CheckTx(inner) => Ok(Request::CheckTx(inner)),
                Req::ListSnapshots(inner) => Ok(Request::ListSnapshots(inner)),
                Req::OfferSnapshot(inner) => Ok(Request::OfferSnapshot(inner)),
                Req::LoadSnapshotChunk(inner) => {
                    Ok(Request::LoadSnapshotChunk(inner))
                }
                Req::ApplySnapshotChunk(inner) => {
                    Ok(Request::ApplySnapshotChunk(inner))
                }
                #[cfg(not(feature = "ABCI"))]
                Req::PrepareProposal(inner) => {
                    Ok(Request::PrepareProposal(inner))
                }
                #[cfg(not(feature = "ABCI"))]
                Req::ProcessProposal(inner) => {
                    Ok(Request::ProcessProposal(inner))
                }
                #[cfg(feature = "ABCI")]
                Req::DeliverTx(inner) => Ok(Request::DeliverTx(inner)),
                #[cfg(not(feature = "ABCI"))]
                Req::FinalizeBlock(inner) => {
                    Ok(Request::FinalizeBlock(inner))
                }
                _ => Err(Error::ConvertReq(req)),
            }
        }
    }

    /// Custom response types. These will be returned by the shell along with
    /// custom payload types (which may be unit structs). It is the duty of
    /// the shim to convert these to responses understandable to tower-abci
    #[derive(Debug)]
    pub enum Response {
        InitChain(ResponseInitChain),
        Info(ResponseInfo),
        Query(ResponseQuery),
        #[cfg(not(feature = "ABCI"))]
        PrepareProposal(ResponsePrepareProposal),
        VerifyHeader(response::VerifyHeader),
        #[cfg(not(feature = "ABCI"))]
        ProcessProposal(ResponseProcessProposal),
        #[cfg(feature = "ABCI")]
        DeliverTx(request::ProcessedTx),
        #[cfg(not(feature = "ABCI"))]
        RevertProposal(response::RevertProposal),
        #[cfg(not(feature = "ABCI"))]
        ExtendVote(ResponseExtendVote),
        #[cfg(not(feature = "ABCI"))]
        VerifyVoteExtension(ResponseVerifyVoteExtension),
        #[cfg(not(feature = "ABCI"))]
        FinalizeBlock(ResponseFinalizeBlock),
        #[cfg(feature = "ABCI")]
        EndBlock(ResponseEndBlock),
        Commit(ResponseCommit),
        Flush(ResponseFlush),
        Echo(ResponseEcho),
        CheckTx(ResponseCheckTx),
        ListSnapshots(ResponseListSnapshots),
        OfferSnapshot(ResponseOfferSnapshot),
        LoadSnapshotChunk(ResponseLoadSnapshotChunk),
        ApplySnapshotChunk(ResponseApplySnapshotChunk),
    }

    /// Attempt to convert response from shell to a tower-abci response type
    impl TryFrom<Response> for Resp {
        type Error = Error;

        fn try_from(resp: Response) -> Result<Resp, Error> {
            match resp {
                Response::InitChain(inner) => Ok(Resp::InitChain(inner)),
                Response::Info(inner) => Ok(Resp::Info(inner)),
                Response::Query(inner) => Ok(Resp::Query(inner)),
                Response::Commit(inner) => Ok(Resp::Commit(inner)),
                Response::Flush(inner) => Ok(Resp::Flush(inner)),
                Response::Echo(inner) => Ok(Resp::Echo(inner)),
                Response::CheckTx(inner) => Ok(Resp::CheckTx(inner)),
                Response::ListSnapshots(inner) => {
                    Ok(Resp::ListSnapshots(inner))
                }
                Response::OfferSnapshot(inner) => {
                    Ok(Resp::OfferSnapshot(inner))
                }
                Response::LoadSnapshotChunk(inner) => {
                    Ok(Resp::LoadSnapshotChunk(inner))
                }
                Response::ApplySnapshotChunk(inner) => {
                    Ok(Resp::ApplySnapshotChunk(inner))
                }
                #[cfg(not(feature = "ABCI"))]
                Response::PrepareProposal(inner) => {
                    Ok(Resp::PrepareProposal(inner))
                }
                #[cfg(not(feature = "ABCI"))]
                Response::ExtendVote(inner) => Ok(Resp::ExtendVote(inner)),
                #[cfg(not(feature = "ABCI"))]
                Response::VerifyVoteExtension(inner) => {
                    Ok(Resp::VerifyVoteExtension(inner))
                }
                #[cfg(not(feature = "ABCI"))]
                Response::ProcessProposal(inner) => {
                    Ok(Resp::ProcessProposal(inner))
                }
                #[cfg(not(feature = "ABCI"))]
                Response::FinalizeBlock(inner) => {
                    Ok(Resp::FinalizeBlock(inner))
                }
                _ => Err(Error::ConvertResp(resp)),
            }
        }
    }

    /// Custom types for request payloads
    pub mod request {
        use std::convert::{TryFrom, TryInto};

        use anoma::types::storage::BlockHash;
        #[cfg(not(feature = "ABCI"))]
        use tendermint::block::Header;
        #[cfg(not(feature = "ABCI"))]
        use tendermint_proto::abci::{Evidence, RequestBeginBlock};
        #[cfg(feature = "ABCI")]
        use tendermint_proto_abci::abci::{Evidence, RequestBeginBlock};
        #[cfg(feature = "ABCI")]
        use tendermint_stable::block::Header;
        use tendermint_proto::abci::RequestFinalizeBlock;

        pub struct VerifyHeader;

        #[derive(Clone)]
        pub struct ProcessProposal {
            pub tx: super::TxBytes,
        }

        impl From<super::TxBytes> for ProcessProposal {
            fn from(tx: super::TxBytes) -> Self {
                Self { tx }
            }
        }

        #[cfg(not(feature = "ABCI"))]
        pub struct RevertProposal;

        /// A Tx and the result of calling Process Proposal on it
        #[derive(Debug, Clone)]
        pub struct ProcessedTx {
            pub tx: super::TxBytes,
            pub result: super::response::TxResult,
        }

        #[derive(Debug)]
        pub struct BeginBlock {
            pub hash: BlockHash,
            pub header: Header,
            pub byzantine_validators: Vec<Evidence>,
        }

        impl TryFrom<RequestBeginBlock> for BeginBlock {
            type Error = super::Error;

            fn try_from(req: RequestBeginBlock) -> Result<Self, super::Error> {
                match (
                    BlockHash::try_from(&*req.hash),
                    req.header
                        .clone()
                        .expect("Missing block's header")
                        .try_into(),
                ) {
                    (Ok(hash), Ok(header)) => Ok(BeginBlock {
                        hash,
                        header,
                        byzantine_validators: req.byzantine_validators,
                    }),
                    (Ok(_), Err(msg)) => {
                        tracing::error!("Unexpected block header {}", msg);
                        Err(super::Error::ConvertReq(super::Req::BeginBlock(
                            req,
                        )))
                    }
                    (err @ Err(_), _) => {
                        tracing::error!("{:#?}", err);
                        Err(super::Error::ConvertReq(super::Req::BeginBlock(
                            req,
                        )))
                    }
                }
            }
        }

        pub struct FinalizeBlock {
            pub hash: BlockHash,
            pub header: Header,
            pub byzantine_validators: Vec<Evidence>,
            pub txs: Vec<ProcessedTx>,
            pub reject_all_decrypted: bool,
        }
        
        #[cfg(not(feature = "ABCI"))]
        fn finalize_block_req(req: RequestFinalizeBlock, txs: Vec<ProcessedTx>) -> FinalizeBlock {
            FinalizeBlock {
                hash: req.hash,
                header: Header {},
                byzantine_validators: vec![],
                txs,
                reject_all_decrypted: false
            }
        }
    }

    /// Custom types for response payloads
    pub mod response {
        use std::convert::TryFrom;
        #[cfg(not(feature = "ABCI"))]
        use tendermint_proto::abci::{Event, ValidatorUpdate};
        #[cfg(not(feature = "ABCI"))]
        use tendermint_proto::types::ConsensusParams;
        #[cfg(feature = "ABCI")]
        use tendermint_proto_abci::abci::ConsensusParams;
        #[cfg(feature = "ABCI")]
        use tendermint_proto_abci::abci::{Event, ValidatorUpdate};
        #[cfg(not(feature = "ABCI"))]
        use tower_abci::response;
        #[cfg(feature = "ABCI")]
        use tower_abci_old::response;

        #[cfg(feature = "ABCI")]
        use crate::node::ledger::shims::abcipp_shim_types::shim::TxBytes;
        use tendermint_proto::abci::ExecTxResult;

        #[derive(Debug, Default)]
        pub struct VerifyHeader;

        #[derive(Debug, Default, Clone)]
        pub struct TxResult {
            pub code: u32,
            pub info: String,
        }

        impl<T> From<T> for TxResult
        where
            T: std::error::Error,
        {
            fn from(err: T) -> Self {
                TxResult {
                    code: 1,
                    info: err.to_string(),
                }
            }
        }

        #[cfg(not(feature = "ABCI"))]
        impl From<TxResult> for ExecTxResult {
            fn from(TxResult{ code, info }: TxResult) -> Self {
                ExecTxResult {
                    code: code.into(),
                    info,
                    ..Default::default()
                }
            }
        }

        #[cfg(not(feature = "ABCI"))]
        impl From<&ExecTxResult> for TxResult {
            fn from(ExecTxResult{code, info, ..}: &ExecTxResult) -> Self {
                TxResult {
                    code: u32::try_from(code).unwrap(),
                    info: info.clone(),
                }
            }
        }

        #[derive(Debug, Default)]
        pub struct ProcessProposal {
            pub result: TxResult,
            #[cfg(feature = "ABCI")]
            pub tx: TxBytes,
        }

        #[cfg(not(feature = "ABCI"))]
        impl From<TxResult> for ProcessProposal {
            fn from(result: TxResult) -> Self {
                ProcessProposal { result }
            }
        }

        #[cfg(feature = "ABCI")]
        impl From<TxResult> for ProcessProposal {
            fn from(result: TxResult) -> Self {
                ProcessProposal {
                    result,
                    ..Default::default()
                }
            }
        }

        #[derive(Debug, Default)]
        pub struct RevertProposal;

        #[derive(Debug, Default)]
        pub struct FinalizeBlock {
            pub events: Vec<Event>,
            pub gas_used: u64,
            pub validator_updates: Vec<ValidatorUpdate>,
            pub consensus_param_updates: Option<ConsensusParams>,
        }

        impl From<FinalizeBlock> for response::EndBlock {
            fn from(resp: FinalizeBlock) -> Self {
                Self {
                    events: resp.events,
                    validator_updates: resp.validator_updates,
                    consensus_param_updates: resp.consensus_param_updates,
                }
            }
        }
    }
}
