use std::net::SocketAddr;
use std::thread;

use tokio::sync::mpsc::{self, Sender};
use tokio::sync::oneshot;
use tonic::transport::Server;
use tonic::{Request as TonicRequest, Response as TonicResponse, Status};

use crate::config::RpcServer;
use crate::proto::services::rpc_service_server::{
    RpcService, RpcServiceServer,
};
use crate::proto::services::{rpc_message, RpcMessage, RpcResponse};

#[derive(Debug)]
struct Rpc {
    inject_message:
        mpsc::Sender<(rpc_message::Message, oneshot::Sender<RpcResponse>)>,
}

#[tonic::async_trait]
impl RpcService for Rpc {
    async fn send_message(
        &self,
        request: TonicRequest<RpcMessage>,
    ) -> Result<TonicResponse<RpcResponse>, Status> {
        if let RpcMessage { message: Some(msg) } = request.into_inner() {
            let (sender, receiver) = oneshot::channel();
            self.inject_message
                .send((msg, sender))
                .await
                .map_err(|err|
                         Status::cancelled(format!{"failed to send message to gossip app: {:?}",err})
                )?
                ;
            let response = receiver.await.map_err(|err|
                Status::data_loss(format!{"failed to receive response from gossip app: {:?}", err}))?;
            Ok(TonicResponse::new(response))
        } else {
            tracing::error!("Received empty rpc message, nothing can be done");
            Ok(TonicResponse::new(RpcResponse::default()))
        }
    }
}

#[tokio::main]
pub async fn rpc_server(
    addr: SocketAddr,
    inject_message: Sender<(
        rpc_message::Message,
        oneshot::Sender<RpcResponse>,
    )>,
) -> Result<(), tonic::transport::Error> {
    let rpc = Rpc { inject_message };
    let svc = RpcServiceServer::new(rpc);
    Server::builder().add_service(svc).serve(addr).await
}

/// Start a rpc server in it's own thread. The used address to listen is in the
/// `config` argument. All received event by the rpc are send to the channel
/// return by this function.
pub fn start_rpc_server(
    config: &RpcServer,
) -> mpsc::Receiver<(
    rpc_message::Message,
    tokio::sync::oneshot::Sender<RpcResponse>,
)> {
    let addr = config.address;
    let (sender, receiver) = mpsc::channel(100);
    thread::spawn(move || rpc_server(addr, sender).unwrap());
    receiver
}
