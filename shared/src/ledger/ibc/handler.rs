//! Functions to handle IBC modules

use std::str::FromStr;

use borsh::BorshSerialize;
#[cfg(not(feature = "ABCI"))]
use ibc::clients::ics07_tendermint::consensus_state::ConsensusState as TmConsensusState;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_consensus::{
    AnyConsensusState, ConsensusState,
};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_state::{AnyClientState, ClientState};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::client_type::ClientType;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::events::{
    Attributes as ClientAttributes, CreateClient, UpdateClient, UpgradeClient,
};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::header::{AnyHeader, Header};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::height::Height;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::connection::{
    ConnectionEnd, Counterparty as ConnCounterparty, State as ConnState,
};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::events::{
    Attributes as ConnectionAttributes, OpenAck as ConnOpenAck,
    OpenConfirm as ConnOpenConfirm, OpenInit as ConnOpenInit,
    OpenTry as ConnOpenTry,
};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::msgs::conn_open_ack::MsgConnectionOpenAck;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::msgs::conn_open_confirm::MsgConnectionOpenConfirm;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::msgs::conn_open_init::MsgConnectionOpenInit;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics03_connection::msgs::conn_open_try::MsgConnectionOpenTry;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, State as ChanState,
};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::events::{
    AcknowledgePacket, Attributes as ChannelAttributes,
    CloseConfirm as ChanCloseConfirm, CloseInit as ChanCloseInit,
    OpenAck as ChanOpenAck, OpenConfirm as ChanOpenConfirm,
    OpenInit as ChanOpenInit, OpenTry as ChanOpenTry, SendPacket,
    TimeoutPacket, WriteAcknowledgement,
};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_close_confirm::MsgChannelCloseConfirm;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_close_init::MsgChannelCloseInit;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_open_ack::MsgChannelOpenAck;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_open_confirm::MsgChannelOpenConfirm;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_open_init::MsgChannelOpenInit;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::chan_open_try::MsgChannelOpenTry;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::packet::{Packet, Sequence};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics23_commitment::commitment::CommitmentPrefix;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics24_host::error::ValidationError as Ics24Error;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortChannelId, PortId,
};
#[cfg(not(feature = "ABCI"))]
use ibc::events::IbcEvent;
#[cfg(not(feature = "ABCI"))]
use ibc::mock::client_state::{MockClientState, MockConsensusState};
#[cfg(feature = "ABCI")]
use ibc_abci::clients::ics07_tendermint::consensus_state::ConsensusState as TmConsensusState;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_consensus::{
    AnyConsensusState, ConsensusState,
};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_state::{AnyClientState, ClientState};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::client_type::ClientType;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::events::{
    Attributes as ClientAttributes, CreateClient, UpdateClient, UpgradeClient,
};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::header::{AnyHeader, Header};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::height::Height;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::connection::{
    ConnectionEnd, Counterparty as ConnCounterparty, State as ConnState,
};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::events::{
    Attributes as ConnectionAttributes, OpenAck as ConnOpenAck,
    OpenConfirm as ConnOpenConfirm, OpenInit as ConnOpenInit,
    OpenTry as ConnOpenTry,
};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::msgs::conn_open_ack::MsgConnectionOpenAck;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::msgs::conn_open_confirm::MsgConnectionOpenConfirm;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::msgs::conn_open_init::MsgConnectionOpenInit;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics03_connection::msgs::conn_open_try::MsgConnectionOpenTry;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, State as ChanState,
};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::events::{
    AcknowledgePacket, Attributes as ChannelAttributes,
    CloseConfirm as ChanCloseConfirm, CloseInit as ChanCloseInit,
    OpenAck as ChanOpenAck, OpenConfirm as ChanOpenConfirm,
    OpenInit as ChanOpenInit, OpenTry as ChanOpenTry, SendPacket,
    TimeoutPacket, WriteAcknowledgement,
};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_close_confirm::MsgChannelCloseConfirm;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_close_init::MsgChannelCloseInit;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_open_ack::MsgChannelOpenAck;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_open_confirm::MsgChannelOpenConfirm;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_open_init::MsgChannelOpenInit;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::chan_open_try::MsgChannelOpenTry;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::packet::{Packet, Sequence};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics23_commitment::commitment::CommitmentPrefix;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics24_host::error::ValidationError as Ics24Error;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortChannelId, PortId,
};
#[cfg(feature = "ABCI")]
use ibc_abci::events::IbcEvent;
#[cfg(feature = "ABCI")]
use ibc_abci::mock::client_state::{MockClientState, MockConsensusState};
use sha2::Digest;
use thiserror::Error;

use crate::types::address::{Address, InternalAddress};
use crate::types::storage::KeySeg;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Invalid client error: {0}")]
    ClientId(Ics24Error),
    #[error("Invalid port error: {0}")]
    PortId(Ics24Error),
    #[error("Updating a client error: {0}")]
    ClientUpdate(String),
}

/// for handling IBC modules
pub type Result<T> = std::result::Result<T, Error>;

/// Update a client with the given state and headers
pub fn update_client(
    client_state: AnyClientState,
    header: AnyHeader,
) -> Result<(AnyClientState, AnyConsensusState)> {
    match client_state {
        AnyClientState::Tendermint(cs) => match header {
            AnyHeader::Tendermint(h) => {
                let new_client_state = cs.with_header(h.clone()).wrap_any();
                let new_consensus_state = TmConsensusState::from(h).wrap_any();
                Ok((new_client_state, new_consensus_state))
            }
            _ => Err(Error::ClientUpdate(
                "The header type is mismatched".to_owned(),
            )),
        },
        AnyClientState::Mock(_) => match header {
            AnyHeader::Mock(h) => Ok((
                MockClientState(h).wrap_any(),
                MockConsensusState::new(h).wrap_any(),
            )),
            _ => Err(Error::ClientUpdate(
                "The header type is mismatched".to_owned(),
            )),
        },
    }
}

/// Returns a new client ID
pub fn client_id(client_type: ClientType, counter: u64) -> Result<ClientId> {
    ClientId::new(client_type, counter).map_err(Error::ClientId)
}

/// Returns a new connection ID
pub fn connection_id(counter: u64) -> ConnectionId {
    ConnectionId::new(counter)
}

/// Make a connection end from the init message
pub fn init_connection(msg: &MsgConnectionOpenInit) -> ConnectionEnd {
    ConnectionEnd::new(
        ConnState::Init,
        msg.client_id.clone(),
        msg.counterparty.clone(),
        vec![msg.version.clone()],
        msg.delay_period,
    )
}

/// Make a connection end from the try message
pub fn try_connection(msg: &MsgConnectionOpenTry) -> ConnectionEnd {
    ConnectionEnd::new(
        ConnState::TryOpen,
        msg.client_id.clone(),
        msg.counterparty.clone(),
        msg.counterparty_versions.clone(),
        msg.delay_period,
    )
}

/// Open the connection
pub fn open_connection(conn: &mut ConnectionEnd) {
    conn.set_state(ConnState::Open);
}

/// Returns a new channel ID
pub fn channel_id(counter: u64) -> ChannelId {
    ChannelId::new(counter)
}

/// Open the channel
pub fn open_channel(channel: &mut ChannelEnd) {
    channel.set_state(ChanState::Open);
}

/// Close the channel
pub fn close_channel(channel: &mut ChannelEnd) {
    channel.set_state(ChanState::Closed);
}

/// Returns a port ID
pub fn port_id(id: &str) -> Result<PortId> {
    PortId::from_str(id).map_err(Error::PortId)
}

/// Returns a pair of port ID and channel ID
pub fn port_channel_id(
    port_id: PortId,
    channel_id: ChannelId,
) -> PortChannelId {
    PortChannelId {
        port_id,
        channel_id,
    }
}

/// Returns a sequence
pub fn sequence(index: u64) -> Sequence {
    Sequence::from(index)
}

/// Returns a commitment from the given packet
pub fn commitment(packet: &Packet) -> String {
    let input = format!(
        "{:?},{:?},{:?}",
        packet.timeout_timestamp, packet.timeout_height, packet.data,
    );
    let r = sha2::Sha256::digest(input.as_bytes());
    format!("{:x}", r)
}

/// Returns a counterparty of a connection
pub fn connection_counterparty(
    client_id: ClientId,
    conn_id: ConnectionId,
) -> ConnCounterparty {
    ConnCounterparty::new(client_id, Some(conn_id), commitment_prefix())
}

/// Returns a counterparty of a channel
pub fn channel_counterparty(
    port_id: PortId,
    channel_id: ChannelId,
) -> ChanCounterparty {
    ChanCounterparty::new(port_id, Some(channel_id))
}

fn commitment_prefix() -> CommitmentPrefix {
    let addr = Address::Internal(InternalAddress::Ibc);
    let bytes = addr
        .raw()
        .try_to_vec()
        .expect("Encoding an address string shouldn't fail");
    CommitmentPrefix::from(bytes)
}

/// Makes CreateClient event
pub fn make_create_client_event(
    client_id: &ClientId,
    msg: &MsgCreateAnyClient,
) -> IbcEvent {
    let attributes = ClientAttributes {
        client_id: client_id.clone(),
        client_type: msg.client_state.client_type(),
        consensus_height: msg.client_state.latest_height(),
        ..Default::default()
    };
    IbcEvent::CreateClient(CreateClient::from(attributes))
}

/// Makes UpdateClient event
pub fn make_update_client_event(
    client_id: &ClientId,
    msg: &MsgUpdateAnyClient,
) -> IbcEvent {
    let attributes = ClientAttributes {
        client_id: client_id.clone(),
        client_type: msg.header.client_type(),
        consensus_height: msg.header.height(),
        ..Default::default()
    };
    IbcEvent::UpdateClient(UpdateClient::from(attributes))
}

/// Makes UpgradeClient event
pub fn make_upgrade_client_event(
    client_id: &ClientId,
    msg: &MsgUpgradeAnyClient,
) -> IbcEvent {
    let attributes = ClientAttributes {
        client_id: client_id.clone(),
        client_type: msg.client_state.client_type(),
        consensus_height: msg.client_state.latest_height(),
        ..Default::default()
    };
    IbcEvent::UpgradeClient(UpgradeClient::from(attributes))
}

/// Makes OpenInitConnection event
pub fn make_open_init_connection_event(
    conn_id: &ConnectionId,
    msg: &MsgConnectionOpenInit,
) -> IbcEvent {
    let attributes = ConnectionAttributes {
        connection_id: Some(conn_id.clone()),
        client_id: msg.client_id.clone(),
        counterparty_connection_id: msg.counterparty.connection_id().cloned(),
        counterparty_client_id: msg.counterparty.client_id().clone(),
        ..Default::default()
    };
    IbcEvent::OpenInitConnection(ConnOpenInit::from(attributes))
}

/// Makes OpenTryConnection event
pub fn make_open_try_connection_event(
    conn_id: &ConnectionId,
    msg: &MsgConnectionOpenTry,
) -> IbcEvent {
    let attributes = ConnectionAttributes {
        connection_id: Some(conn_id.clone()),
        client_id: msg.client_id.clone(),
        counterparty_connection_id: msg.counterparty.connection_id().cloned(),
        counterparty_client_id: msg.counterparty.client_id().clone(),
        ..Default::default()
    };
    IbcEvent::OpenTryConnection(ConnOpenTry::from(attributes))
}

/// Makes OpenAckConnection event
pub fn make_open_ack_connection_event(msg: &MsgConnectionOpenAck) -> IbcEvent {
    let attributes = ConnectionAttributes {
        connection_id: Some(msg.connection_id.clone()),
        counterparty_connection_id: Some(
            msg.counterparty_connection_id.clone(),
        ),
        ..Default::default()
    };
    IbcEvent::OpenAckConnection(ConnOpenAck::from(attributes))
}

/// Makes OpenConfirmConnection event
pub fn make_open_confirm_connection_event(
    msg: &MsgConnectionOpenConfirm,
) -> IbcEvent {
    let attributes = ConnectionAttributes {
        connection_id: Some(msg.connection_id.clone()),
        ..Default::default()
    };
    IbcEvent::OpenConfirmConnection(ConnOpenConfirm::from(attributes))
}

/// Makes OpenInitChannel event
pub fn make_open_init_channel_event(
    channel_id: &ChannelId,
    msg: &MsgChannelOpenInit,
) -> IbcEvent {
    let connection_id = match msg.channel.connection_hops().get(0) {
        Some(c) => c.clone(),
        None => ConnectionId::default(),
    };
    let attributes = ChannelAttributes {
        port_id: msg.port_id.clone(),
        channel_id: Some(channel_id.clone()),
        connection_id,
        counterparty_port_id: msg.channel.counterparty().port_id().clone(),
        counterparty_channel_id: msg
            .channel
            .counterparty()
            .channel_id()
            .cloned(),
        ..Default::default()
    };
    IbcEvent::OpenInitChannel(ChanOpenInit::from(attributes))
}

/// Makes OpenTryChannel event
pub fn make_open_try_channel_event(
    channel_id: &ChannelId,
    msg: &MsgChannelOpenTry,
) -> IbcEvent {
    let connection_id = match msg.channel.connection_hops().get(0) {
        Some(c) => c.clone(),
        None => ConnectionId::default(),
    };
    let attributes = ChannelAttributes {
        port_id: msg.port_id.clone(),
        channel_id: Some(channel_id.clone()),
        connection_id,
        counterparty_port_id: msg.channel.counterparty().port_id().clone(),
        counterparty_channel_id: msg
            .channel
            .counterparty()
            .channel_id()
            .cloned(),
        ..Default::default()
    };
    IbcEvent::OpenTryChannel(ChanOpenTry::from(attributes))
}

/// Makes OpenAckChannel event
pub fn make_open_ack_channel_event(msg: &MsgChannelOpenAck) -> IbcEvent {
    let attributes = ChannelAttributes {
        port_id: msg.port_id.clone(),
        channel_id: Some(msg.channel_id.clone()),
        counterparty_channel_id: Some(msg.counterparty_channel_id.clone()),
        ..Default::default()
    };
    IbcEvent::OpenAckChannel(ChanOpenAck::from(attributes))
}

/// Makes OpenConfirmChannel event
pub fn make_open_confirm_channel_event(
    msg: &MsgChannelOpenConfirm,
) -> IbcEvent {
    let attributes = ChannelAttributes {
        port_id: msg.port_id.clone(),
        channel_id: Some(msg.channel_id.clone()),
        ..Default::default()
    };
    IbcEvent::OpenConfirmChannel(ChanOpenConfirm::from(attributes))
}

/// Makes CloseInitChannel event
pub fn make_close_init_channel_event(msg: &MsgChannelCloseInit) -> IbcEvent {
    let attributes = ChannelAttributes {
        port_id: msg.port_id.clone(),
        channel_id: Some(msg.channel_id.clone()),
        ..Default::default()
    };
    IbcEvent::CloseInitChannel(ChanCloseInit::from(attributes))
}

/// Makes CloseConfirmChannel event
pub fn make_close_confirm_channel_event(
    msg: &MsgChannelCloseConfirm,
) -> IbcEvent {
    let attributes = ChannelAttributes {
        port_id: msg.port_id.clone(),
        channel_id: Some(msg.channel_id.clone()),
        ..Default::default()
    };
    IbcEvent::CloseConfirmChannel(ChanCloseConfirm::from(attributes))
}

/// Makes SendPacket event
pub fn make_send_packet_event(packet: Packet) -> IbcEvent {
    IbcEvent::SendPacket(SendPacket {
        height: packet.timeout_height,
        packet,
    })
}

/// Makes WriteAcknowledgement event
pub fn make_write_ack_event(packet: Packet, ack: Vec<u8>) -> IbcEvent {
    IbcEvent::WriteAcknowledgement(WriteAcknowledgement {
        // this height is not used
        height: Height::default(),
        packet,
        ack,
    })
}

/// Makes AcknowledgePacket event
pub fn make_ack_event(packet: Packet) -> IbcEvent {
    IbcEvent::AcknowledgePacket(AcknowledgePacket {
        // this height is not used
        height: Height::default(),
        packet,
    })
}

/// Makes TimeoutPacket event
pub fn make_timeout_event(packet: Packet) -> IbcEvent {
    IbcEvent::TimeoutPacket(TimeoutPacket {
        // this height is not used
        height: Height::default(),
        packet,
    })
}
