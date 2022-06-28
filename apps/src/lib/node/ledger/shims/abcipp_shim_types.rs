#[cfg(not(feature = "ABCI"))]
use tower_abci::{Request, Response};
#[cfg(feature = "ABCI")]
use tower_abci_old::{Request, Response};

pub mod shim {
    use std::convert::TryFrom;

    #[cfg(not(feature = "ABCI"))]
    use tendermint_proto::abci::{
        RequestApplySnapshotChunk, RequestCheckTx, RequestCommit, RequestEcho,
        RequestExtendVote, RequestFlush, RequestInfo, RequestInitChain,
        RequestListSnapshots, RequestLoadSnapshotChunk, RequestOfferSnapshot,
        RequestPrepareProposal, RequestProcessProposal, RequestQuery,
        RequestVerifyVoteExtension, ResponseApplySnapshotChunk,
        ResponseCheckTx, ResponseCommit, ResponseEcho, ResponseExtendVote,
        ResponseFlush, ResponseInfo, ResponseInitChain, ResponseListSnapshots,
        ResponseLoadSnapshotChunk, ResponseOfferSnapshot,
        ResponsePrepareProposal, ResponseProcessProposal, ResponseQuery,
        ResponseVerifyVoteExtension,
    };
    #[cfg(feature = "ABCI")]
    use tendermint_proto_abci::abci::{
        RequestApplySnapshotChunk, RequestCheckTx, RequestCommit,
        RequestDeliverTx, RequestEcho, RequestFlush, RequestInfo,
        RequestInitChain, RequestListSnapshots, RequestLoadSnapshotChunk,
        RequestOfferSnapshot, RequestQuery, ResponseApplySnapshotChunk,
        ResponseCheckTx, ResponseCommit, ResponseEcho, ResponseEndBlock,
        ResponseFlush, ResponseInfo, ResponseInitChain, ResponseListSnapshots,
        ResponseLoadSnapshotChunk, ResponseOfferSnapshot, ResponseQuery,
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
        FinalizeBlock(request::FinalizeBlock),
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
        FinalizeBlock(response::FinalizeBlock),
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
                _ => Err(Error::ConvertResp(resp)),
            }
        }
    }

    /// Custom types for request payloads
    pub mod request {
        use std::convert::TryFrom;

        use anoma::types::hash::Hash;
        use anoma::types::storage::{BlockHash, Header};
        use anoma::types::time::DateTimeUtc;
        #[cfg(not(feature = "ABCI"))]
        use tendermint_proto::abci::{
            Misbehavior as Evidence, RequestFinalizeBlock,
        };
        #[cfg(feature = "ABCI")]
        use tendermint_proto_abci::abci::{Evidence, RequestBeginBlock};

        pub struct VerifyHeader;

        #[cfg(not(feature = "ABCI"))]
        pub struct RevertProposal;

        /// A Tx and the result of calling Process Proposal on it
        #[derive(Debug, Clone)]
        pub struct ProcessedTx {
            pub tx: super::TxBytes,
            pub result: super::response::TxResult,
        }

        pub struct FinalizeBlock {
            pub hash: BlockHash,
            pub header: Header,
            pub byzantine_validators: Vec<Evidence>,
            pub txs: Vec<ProcessedTx>,
        }

        #[cfg(not(feature = "ABCI"))]
        impl From<RequestFinalizeBlock> for FinalizeBlock {
            fn from(req: RequestFinalizeBlock) -> FinalizeBlock {
                FinalizeBlock {
                    hash: BlockHash::try_from(req.hash.as_slice()).unwrap(),
                    header: Header {
                        hash: Hash::try_from(req.hash.as_slice()).unwrap(),
                        time: DateTimeUtc::try_from(req.time.unwrap()).unwrap(),
                        next_validators_hash: Hash::try_from(
                            req.next_validators_hash.as_slice(),
                        )
                        .unwrap(),
                    },
                    byzantine_validators: req.byzantine_validators,
                    txs: vec![],
                }
            }
        }

        #[cfg(feature = "ABCI")]
        impl From<RequestBeginBlock> for FinalizeBlock {
            fn from(req: RequestBeginBlock) -> FinalizeBlock {
                let header = req.header.unwrap();
                FinalizeBlock {
                    hash: BlockHash::default(),
                    header: Header {
                        hash: Hash::default(),
                        time: DateTimeUtc::try_from(header.time.unwrap())
                            .unwrap(),
                        next_validators_hash: Hash::try_from(
                            header.next_validators_hash.as_slice(),
                        )
                        .unwrap(),
                    },
                    byzantine_validators: req.byzantine_validators,
                    txs: vec![],
                }
            }
        }
    }

    /// Custom types for response payloads
    pub mod response {
        #[cfg(not(feature = "ABCI"))]
        use tendermint_proto::abci::{
            Event as TmEvent, ExecTxResult, ResponseFinalizeBlock,
            ValidatorUpdate,
        };
        #[cfg(not(feature = "ABCI"))]
        use tendermint_proto::types::ConsensusParams;
        #[cfg(feature = "ABCI")]
        use tendermint_proto_abci::abci::ConsensusParams;
        #[cfg(feature = "ABCI")]
        use tendermint_proto_abci::abci::{Event as TmEvent, ValidatorUpdate};
        #[cfg(feature = "ABCI")]
        use tower_abci_old::response;

        use crate::node::ledger::events::Event;
        #[cfg(not(feature = "ABCI"))]
        use crate::node::ledger::events::EventLevel;

        #[derive(Debug, Default)]
        pub struct VerifyHeader;

        #[derive(Debug, Default, Clone)]
        pub struct TxResult {
            pub code: u32,
            pub info: String,
        }

        #[cfg(not(feature = "ABCI"))]
        impl From<TxResult> for ExecTxResult {
            fn from(TxResult { code, info }: TxResult) -> Self {
                ExecTxResult {
                    code,
                    info,
                    ..Default::default()
                }
            }
        }

        #[cfg(not(feature = "ABCI"))]
        impl From<&ExecTxResult> for TxResult {
            fn from(ExecTxResult { code, info, .. }: &ExecTxResult) -> Self {
                TxResult {
                    code: *code,
                    info: info.clone(),
                }
            }
        }

        #[derive(Debug, Default)]
        pub struct RevertProposal;

        #[derive(Debug, Default)]
        pub struct FinalizeBlock {
            pub events: Vec<Event>,
            pub validator_updates: Vec<ValidatorUpdate>,
            pub consensus_param_updates: Option<ConsensusParams>,
        }

        #[cfg(not(feature = "ABCI"))]
        impl From<FinalizeBlock> for ResponseFinalizeBlock {
            fn from(resp: FinalizeBlock) -> Self {
                ResponseFinalizeBlock {
                    tx_results: resp
                        .events
                        .iter()
                        .filter(|event| matches!(event.level, EventLevel::Tx))
                        .map(|event| ExecTxResult {
                            code: event
                                .get("code")
                                .map(|code| code.parse::<u32>().unwrap())
                                .unwrap_or_default(),
                            log: event
                                .get("log")
                                .map(|log| log.to_owned())
                                .unwrap_or_default(),
                            info: event
                                .get("info")
                                .map(|info| info.to_owned())
                                .unwrap_or_default(),
                            gas_used: event
                                .get("gas_used")
                                .map(|gas| gas.parse::<i64>().unwrap())
                                .unwrap_or_default(),
                            ..Default::default()
                        })
                        .collect(),
                    events: resp
                        .events
                        .into_iter()
                        .map(TmEvent::from)
                        .collect(),
                    consensus_param_updates: resp.consensus_param_updates,
                    validator_updates: resp.validator_updates,
                    ..Default::default()
                }
            }
        }

        #[cfg(feature = "ABCI")]
        impl From<FinalizeBlock> for response::EndBlock {
            fn from(resp: FinalizeBlock) -> Self {
                Self {
                    events: resp
                        .events
                        .into_iter()
                        .map(TmEvent::from)
                        .collect(),
                    validator_updates: resp.validator_updates,
                    consensus_param_updates: resp.consensus_param_updates,
                }
            }
        }
    }
}
