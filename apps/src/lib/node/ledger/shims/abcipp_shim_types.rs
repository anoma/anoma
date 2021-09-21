use tower_abci::{Request, Response};

pub mod shim {
    use std::convert::TryFrom;

    use tendermint_proto::abci::{
        RequestApplySnapshotChunk, RequestCheckTx, RequestCommit, RequestEcho,
        RequestFlush, RequestInfo, RequestInitChain, RequestListSnapshots,
        RequestLoadSnapshotChunk, RequestOfferSnapshot, RequestQuery,
        RequestSetOption, ResponseApplySnapshotChunk, ResponseCheckTx,
        ResponseCommit, ResponseEcho, ResponseFlush, ResponseInfo,
        ResponseInitChain, ResponseListSnapshots, ResponseLoadSnapshotChunk,
        ResponseOfferSnapshot, ResponseQuery, ResponseSetOption,
    };
    use thiserror::Error;

    use super::{Request as Req, Response as Resp};
    use crate::node::ledger::shell;
    pub type TxBytes = Vec<u8>;

    #[derive(Error, Debug)]
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
        PrepareProposal(request::PrepareProposal),
        #[allow(dead_code)]
        VerifyHeader(request::VerifyHeader),
        #[allow(dead_code)]
        ProcessProposal(request::ProcessProposal),
        #[allow(dead_code)]
        RevertProposal(request::RevertProposal),
        #[allow(dead_code)]
        ExtendVote(request::ExtendVote),
        #[allow(dead_code)]
        VerifyVoteExtension(request::VerifyVoteExtension),
        FinalizeBlock(request::FinalizeBlock),
        Commit(RequestCommit),
        Flush(RequestFlush),
        SetOption(RequestSetOption),
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
                Req::SetOption(inner) => Ok(Request::SetOption(inner)),
                Req::Echo(inner) => Ok(Request::Echo(inner)),
                Req::CheckTx(inner) => Ok(Request::CheckTx(inner)),
                Req::ListSnapshots(inner) => Ok(Request::ListSnapshots(inner)),
                Req::OfferSnapshot(inner) => Ok(Request::OfferSnapshot(inner)),
                Req::LoadSnapshotChunk(inner) => {
                    Ok(Request::LoadSnapshotChunk(inner))
                }
                Req::ApplySnapshotChunk(inner) => {
                    Ok(Request::ApplySnapshotChunk(inner))
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
        PrepareProposal(response::PrepareProposal),
        VerifyHeader(response::VerifyHeader),
        ProcessProposal(response::ProcessProposal),
        RevertProposal(response::RevertProposal),
        ExtendVote(response::ExtendVote),
        VerifyVoteExtension(response::VerifyVoteExtension),
        FinalizeBlock(response::FinalizeBlock),
        Commit(ResponseCommit),
        Flush(ResponseFlush),
        SetOption(ResponseSetOption),
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
                Response::SetOption(inner) => Ok(Resp::SetOption(inner)),
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
                _ => Err(Error::ConvertResp(resp)),
            }
        }
    }

    /// Custom types for request payloads
    pub mod request {
        use tendermint_proto::abci::{Evidence, RequestBeginBlock};
        use tendermint_proto::types::Header;

        pub struct PrepareProposal {
            pub hash: Vec<u8>,
            pub header: Option<Header>,
            pub byzantine_validators: Vec<Evidence>,
        }

        impl From<RequestBeginBlock> for PrepareProposal {
            fn from(block: RequestBeginBlock) -> Self {
                PrepareProposal {
                    hash: block.hash,
                    header: block.header,
                    byzantine_validators: block.byzantine_validators,
                }
            }
        }

        pub struct VerifyHeader;

        pub struct ProcessProposal {
            pub tx: super::TxBytes,
        }

        impl From<super::TxBytes> for ProcessProposal {
            fn from(tx: super::TxBytes) -> Self {
                Self { tx }
            }
        }

        pub struct RevertProposal;
        pub struct ExtendVote;
        pub struct VerifyVoteExtension;

        pub struct FinalizeBlock {
            pub height: i64,
            pub txs: Vec<super::TxBytes>,
        }
    }

    /// Custom types for response payloads
    pub mod response {
        use tendermint_proto::abci::{ConsensusParams, Event, ValidatorUpdate};
        use tower_abci::response;

        #[derive(Debug, Default)]
        pub struct PrepareProposal;

        impl From<PrepareProposal> for response::BeginBlock {
            fn from(_: PrepareProposal) -> Self {
                Default::default()
            }
        }

        #[derive(Debug, Default)]
        pub struct VerifyHeader;

        #[derive(Debug, Default)]
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

        #[derive(Debug, Default)]
        pub struct ProcessProposal {
            pub result: TxResult,
            pub tx: super::TxBytes,
        }

        impl From<ProcessProposal> for response::CheckTx {
            fn from(resp: ProcessProposal) -> Self {
                Self {
                    code: resp.result.code,
                    log: resp.result.info,
                    ..Default::default()
                }
            }
        }

        #[derive(Debug, Default)]
        pub struct RevertProposal;

        #[derive(Debug, Default)]
        pub struct ExtendVote;

        #[derive(Debug, Default)]
        pub struct VerifyVoteExtension;

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
