//! Functions to handle IBC modules

use std::str::FromStr;

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
use ibc::core::ics02_client::msgs::ClientMsg;
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
use ibc::core::ics03_connection::msgs::ConnectionMsg;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order, State as ChanState,
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
use ibc::core::ics04_channel::msgs::acknowledgement::MsgAcknowledgement;
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
use ibc::core::ics04_channel::msgs::recv_packet::MsgRecvPacket;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::timeout::MsgTimeout;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::timeout_on_close::MsgTimeoutOnClose;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics04_channel::msgs::{ChannelMsg, PacketMsg};
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
use ibc::core::ics26_routing::msgs::Ics26Envelope;
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
use ibc_abci::core::ics02_client::msgs::ClientMsg;
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
use ibc_abci::core::ics03_connection::msgs::ConnectionMsg;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order, State as ChanState,
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
use ibc_abci::core::ics04_channel::msgs::acknowledgement::MsgAcknowledgement;
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
use ibc_abci::core::ics04_channel::msgs::recv_packet::MsgRecvPacket;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::timeout::MsgTimeout;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::timeout_on_close::MsgTimeoutOnClose;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics04_channel::msgs::{ChannelMsg, PacketMsg};
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
use ibc_abci::core::ics26_routing::msgs::Ics26Envelope;
#[cfg(feature = "ABCI")]
use ibc_abci::events::IbcEvent;
#[cfg(feature = "ABCI")]
use ibc_abci::mock::client_state::{MockClientState, MockConsensusState};
use sha2::Digest;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::Error as ProtoError;
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::Protobuf;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::Error as ProtoError;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::Protobuf;
use thiserror::Error;

use crate::ledger::ibc::storage;
use crate::types::ibc::data::{
    Error as IbcDataError, IbcMessage, PacketAck, PacketReceipt, PacketSendData,
};
use crate::types::storage::Key;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Invalid client error: {0}")]
    ClientId(Ics24Error),
    #[error("Invalid port error: {0}")]
    PortId(Ics24Error),
    #[error("Updating a client error: {0}")]
    ClientUpdate(String),
    #[error("IBC data error: {0}")]
    IbcData(IbcDataError),
    #[error("Decoding IBC data error: {0}")]
    Decoding(ProtoError),
    #[error("Client error: {0}")]
    Client(String),
    #[error("Connection error: {0}")]
    Connection(String),
    #[error("Channel error: {0}")]
    Channel(String),
    #[error("Counter error: {0}")]
    Counter(String),
    #[error("Sequence error: {0}")]
    Sequence(String),
}

/// for handling IBC modules
pub type Result<T> = std::result::Result<T, Error>;

/// IBC trait to be implemented in integration that can read and write
pub trait IbcActions {
    /// Read IBC-related data
    fn read_ibc_data(&self, key: &Key) -> Option<Vec<u8>>;

    /// Write IBC-related data
    fn write_ibc_data(&self, key: &Key, data: impl AsRef<[u8]>);

    /// Delete IBC-related data
    fn delete_ibc_data(&self, key: &Key);

    /// Emit an IBC event
    fn emit_ibc_event(&self, event: IbcEvent);

    /// dispatch according to ICS26 routing
    fn dispatch(&self, tx_data: &[u8]) -> Result<()> {
        let ibc_msg = IbcMessage::decode(tx_data).map_err(Error::IbcData)?;
        match &ibc_msg.0 {
            Ics26Envelope::Ics2Msg(ics02_msg) => match ics02_msg {
                ClientMsg::CreateClient(msg) => self.create_client(msg),
                ClientMsg::UpdateClient(msg) => self.update_client(msg),
                ClientMsg::Misbehaviour(_msg) => todo!(),
                ClientMsg::UpgradeClient(msg) => self.upgrade_client(msg),
            },
            Ics26Envelope::Ics3Msg(ics03_msg) => match ics03_msg {
                ConnectionMsg::ConnectionOpenInit(msg) => {
                    self.init_connection(msg)
                }
                ConnectionMsg::ConnectionOpenTry(msg) => {
                    self.try_connection(msg)
                }
                ConnectionMsg::ConnectionOpenAck(msg) => {
                    self.ack_connection(msg)
                }
                ConnectionMsg::ConnectionOpenConfirm(msg) => {
                    self.confirm_connection(msg)
                }
            },
            Ics26Envelope::Ics4ChannelMsg(ics04_msg) => match ics04_msg {
                ChannelMsg::ChannelOpenInit(msg) => self.init_channel(msg),
                ChannelMsg::ChannelOpenTry(msg) => self.try_channel(msg),
                ChannelMsg::ChannelOpenAck(msg) => self.ack_channel(msg),
                ChannelMsg::ChannelOpenConfirm(msg) => {
                    self.confirm_channel(msg)
                }
                ChannelMsg::ChannelCloseInit(msg) => {
                    self.close_init_channel(msg)
                }
                ChannelMsg::ChannelCloseConfirm(msg) => {
                    self.close_confirm_channel(msg)
                }
            },
            Ics26Envelope::Ics4PacketMsg(ics04_msg) => match ics04_msg {
                PacketMsg::AckPacket(msg) => self.acknowledge_packet(msg),
                PacketMsg::RecvPacket(msg) => self.receive_packet(msg),
                PacketMsg::ToPacket(msg) => self.timeout_packet(msg),
                PacketMsg::ToClosePacket(msg) => {
                    self.timeout_on_close_packet(msg)
                }
            },
            Ics26Envelope::Ics20Msg(_ics20_msg) => todo!(),
        }
    }

    /// Create a new client
    fn create_client(&self, msg: &MsgCreateAnyClient) -> Result<()> {
        let counter_key = storage::client_counter_key();
        let counter = self.get_and_inc_counter(&counter_key)?;
        let client_type = msg.client_state.client_type();
        let client_id = client_id(client_type, counter)?;
        // client type
        let client_type_key = storage::client_type_key(&client_id);
        self.write_ibc_data(&client_type_key, client_type.as_str().as_bytes());
        // client state
        let client_state_key = storage::client_state_key(&client_id);
        self.write_ibc_data(
            &client_state_key,
            msg.client_state
                .encode_vec()
                .expect("encoding shouldn't fail"),
        );
        // consensus state
        let height = msg.client_state.latest_height();
        let consensus_state_key =
            storage::consensus_state_key(&client_id, height);
        self.write_ibc_data(
            &consensus_state_key,
            msg.consensus_state
                .encode_vec()
                .expect("encoding shouldn't fail"),
        );

        let event = make_create_client_event(&client_id, msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Update a client
    fn update_client(&self, msg: &MsgUpdateAnyClient) -> Result<()> {
        // get and update the client
        let client_id = msg.client_id.clone();
        let client_state_key = storage::client_state_key(&client_id);
        let value = self.read_ibc_data(&client_state_key).ok_or_else(|| {
            Error::Client(format!("The client doesn't exist: ID {}", client_id))
        })?;
        let client_state =
            AnyClientState::decode_vec(&value).map_err(Error::Decoding)?;
        let (new_client_state, new_consensus_state) =
            update_client(client_state, msg.header.clone())?;

        let height = new_client_state.latest_height();
        self.write_ibc_data(
            &client_state_key,
            new_client_state
                .encode_vec()
                .expect("encoding shouldn't fail"),
        );
        let consensus_state_key =
            storage::consensus_state_key(&client_id, height);
        self.write_ibc_data(
            &consensus_state_key,
            new_consensus_state
                .encode_vec()
                .expect("encoding shouldn't fail"),
        );

        let event = make_update_client_event(&client_id, msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Upgrade a client
    fn upgrade_client(&self, msg: &MsgUpgradeAnyClient) -> Result<()> {
        let client_state_key = storage::client_state_key(&msg.client_id);
        let height = msg.client_state.latest_height();
        let consensus_state_key =
            storage::consensus_state_key(&msg.client_id, height);
        self.write_ibc_data(
            &client_state_key,
            msg.client_state
                .encode_vec()
                .expect("encoding shouldn't fail"),
        );
        self.write_ibc_data(
            &consensus_state_key,
            msg.consensus_state
                .encode_vec()
                .expect("encoding shouldn't fail"),
        );

        let event = make_upgrade_client_event(&msg.client_id, msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Initialize a connection for ConnectionOpenInit
    fn init_connection(&self, msg: &MsgConnectionOpenInit) -> Result<()> {
        let counter_key = storage::connection_counter_key();
        let counter = self.get_and_inc_counter(&counter_key)?;
        // new connection
        let conn_id = connection_id(counter);
        let conn_key = storage::connection_key(&conn_id);
        let connection = init_connection(msg);
        self.write_ibc_data(
            &conn_key,
            connection.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_init_connection_event(&conn_id, msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Initialize a connection for ConnectionOpenTry
    fn try_connection(&self, msg: &MsgConnectionOpenTry) -> Result<()> {
        let counter_key = storage::connection_counter_key();
        let counter = self.get_and_inc_counter(&counter_key)?;
        // new connection
        let conn_id = connection_id(counter);
        let conn_key = storage::connection_key(&conn_id);
        let connection = try_connection(msg);
        self.write_ibc_data(
            &conn_key,
            connection.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_try_connection_event(&conn_id, msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Open the connection for ConnectionOpenAck
    fn ack_connection(&self, msg: &MsgConnectionOpenAck) -> Result<()> {
        let conn_key = storage::connection_key(&msg.connection_id);
        let value = self.read_ibc_data(&conn_key).ok_or_else(|| {
            Error::Connection(format!(
                "The connection doesn't exist: ID {}",
                msg.connection_id
            ))
        })?;
        let mut connection =
            ConnectionEnd::decode_vec(&value).map_err(Error::Decoding)?;
        open_connection(&mut connection);
        self.write_ibc_data(
            &conn_key,
            connection.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_ack_connection_event(msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Open the connection for ConnectionOpenConfirm
    fn confirm_connection(&self, msg: &MsgConnectionOpenConfirm) -> Result<()> {
        let conn_key = storage::connection_key(&msg.connection_id);
        let value = self.read_ibc_data(&conn_key).ok_or_else(|| {
            Error::Connection(format!(
                "The connection doesn't exist: ID {}",
                msg.connection_id
            ))
        })?;
        let mut connection =
            ConnectionEnd::decode_vec(&value).map_err(Error::Decoding)?;
        open_connection(&mut connection);
        self.write_ibc_data(
            &conn_key,
            connection.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_confirm_connection_event(msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Initialize a channel for ChannelOpenInit
    fn init_channel(&self, msg: &MsgChannelOpenInit) -> Result<()> {
        self.bind_port(&msg.port_id)?;
        let counter_key = storage::channel_counter_key();
        let counter = self.get_and_inc_counter(&counter_key)?;
        let channel_id = channel_id(counter);
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), channel_id.clone());
        let channel_key = storage::channel_key(&port_channel_id);
        self.write_ibc_data(
            &channel_key,
            msg.channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_init_channel_event(&channel_id, msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Initialize a channel for ChannelOpenTry
    fn try_channel(&self, msg: &MsgChannelOpenTry) -> Result<()> {
        self.bind_port(&msg.port_id)?;
        let counter_key = storage::channel_counter_key();
        let counter = self.get_and_inc_counter(&counter_key)?;
        let channel_id = channel_id(counter);
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), channel_id.clone());
        let channel_key = storage::channel_key(&port_channel_id);
        self.write_ibc_data(
            &channel_key,
            msg.channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_try_channel_event(&channel_id, msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Open the channel for ChannelOpenAck
    fn ack_channel(&self, msg: &MsgChannelOpenAck) -> Result<()> {
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), msg.channel_id.clone());
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel doesn't exist: Port/Channel {}",
                port_channel_id
            ))
        })?;
        let mut channel =
            ChannelEnd::decode_vec(&value).map_err(Error::Decoding)?;
        open_channel(&mut channel);
        self.write_ibc_data(
            &channel_key,
            channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_ack_channel_event(msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Open the channel for ChannelOpenConfirm
    fn confirm_channel(&self, msg: &MsgChannelOpenConfirm) -> Result<()> {
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), msg.channel_id.clone());
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel doesn't exist: Port/Channel {}",
                port_channel_id
            ))
        })?;
        let mut channel =
            ChannelEnd::decode_vec(&value).map_err(Error::Decoding)?;
        open_channel(&mut channel);
        self.write_ibc_data(
            &channel_key,
            channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_confirm_channel_event(msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Close the channel for ChannelCloseInit
    fn close_init_channel(&self, msg: &MsgChannelCloseInit) -> Result<()> {
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), msg.channel_id.clone());
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel doesn't exist: Port/Channel {}",
                port_channel_id
            ))
        })?;
        let mut channel =
            ChannelEnd::decode_vec(&value).map_err(Error::Decoding)?;
        close_channel(&mut channel);
        self.write_ibc_data(
            &channel_key,
            channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_close_init_channel_event(msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Close the channel for ChannelCloseConfirm
    fn close_confirm_channel(
        &self,
        msg: &MsgChannelCloseConfirm,
    ) -> Result<()> {
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), msg.channel_id.clone());
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel doesn't exist: Port/Channel {}",
                port_channel_id
            ))
        })?;
        let mut channel =
            ChannelEnd::decode_vec(&value).map_err(Error::Decoding)?;
        close_channel(&mut channel);
        self.write_ibc_data(
            &channel_key,
            channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_close_confirm_channel_event(msg);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Send a packet
    fn send_packet(&self, data: &PacketSendData) -> Result<()> {
        // get and increment the next sequence send
        let port_channel_id =
            port_channel_id(data.source_port_id(), data.source_channel_id());
        let seq_key = storage::next_sequence_send_key(&port_channel_id);
        let sequence = self.get_and_inc_sequence(&seq_key)?;

        // store the commitment of the packet
        let packet = data.packet(sequence);
        let commitment_key = storage::commitment_key(
            &data.source_port_id(),
            &data.source_channel_id(),
            packet.sequence,
        );
        let commitment = commitment(&packet);
        self.write_ibc_data(&commitment_key, commitment.as_bytes());

        let event = make_send_packet_event(packet);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Receive a packet
    fn receive_packet(&self, msg: &MsgRecvPacket) -> Result<()> {
        // store the receipt
        let receipt_key = storage::receipt_key(
            &msg.packet.destination_port,
            &msg.packet.destination_channel,
            msg.packet.sequence,
        );
        self.write_ibc_data(&receipt_key, PacketReceipt::new().as_bytes());

        // store the ack
        let ack_key = storage::ack_key(
            &msg.packet.destination_port,
            &msg.packet.destination_channel,
            msg.packet.sequence,
        );
        let ack = PacketAck::new().encode_to_vec();
        self.write_ibc_data(&ack_key, ack.clone());

        // increment the next sequence receive
        let port_channel_id = port_channel_id(
            msg.packet.destination_port.clone(),
            msg.packet.destination_channel.clone(),
        );
        let seq_key = storage::next_sequence_recv_key(&port_channel_id);
        self.get_and_inc_sequence(&seq_key)?;

        let event = make_write_ack_event(msg.packet.clone(), ack);
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Receive a acknowledgement
    fn acknowledge_packet(&self, msg: &MsgAcknowledgement) -> Result<()> {
        let commitment_key = storage::commitment_key(
            &msg.packet.source_port,
            &msg.packet.source_channel,
            msg.packet.sequence,
        );
        self.delete_ibc_data(&commitment_key);

        let event = make_ack_event(msg.packet.clone());
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Receive a timeout
    fn timeout_packet(&self, msg: &MsgTimeout) -> Result<()> {
        // delete the commitment of the packet
        let commitment_key = storage::commitment_key(
            &msg.packet.source_port,
            &msg.packet.source_channel,
            msg.packet.sequence,
        );
        self.delete_ibc_data(&commitment_key);

        // close the channel
        let port_channel_id = port_channel_id(
            msg.packet.source_port.clone(),
            msg.packet.source_channel.clone(),
        );
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel doesn't exist: Port/Channel {}",
                port_channel_id
            ))
        })?;
        let mut channel =
            ChannelEnd::decode_vec(&value).map_err(Error::Decoding)?;
        if channel.order_matches(&Order::Ordered) {
            close_channel(&mut channel);
            self.write_ibc_data(
                &channel_key,
                channel.encode_vec().expect("encoding shouldn't fail"),
            );
        }

        let event = make_timeout_event(msg.packet.clone());
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Receive a timeout for TimeoutOnClose
    fn timeout_on_close_packet(&self, msg: &MsgTimeoutOnClose) -> Result<()> {
        // delete the commitment of the packet
        let commitment_key = storage::commitment_key(
            &msg.packet.source_port,
            &msg.packet.source_channel,
            msg.packet.sequence,
        );
        self.delete_ibc_data(&commitment_key);

        // close the channel
        let port_channel_id = port_channel_id(
            msg.packet.source_port.clone(),
            msg.packet.source_channel.clone(),
        );
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel doesn't exist: Port/Channel {}",
                port_channel_id
            ))
        })?;
        let mut channel =
            ChannelEnd::decode_vec(&value).map_err(Error::Decoding)?;
        if channel.order_matches(&Order::Ordered) {
            close_channel(&mut channel);
            self.write_ibc_data(
                &channel_key,
                channel.encode_vec().expect("encoding shouldn't fail"),
            );
        }

        Ok(())
    }

    /// Get and increment the counter
    fn get_and_inc_counter(&self, key: &Key) -> Result<u64> {
        let value = self.read_ibc_data(key).ok_or_else(|| {
            Error::Counter(format!("The counter doesn't exist: {}", key))
        })?;
        let value: [u8; 8] = value.try_into().map_err(|_| {
            Error::Counter(format!("The counter value wasn't u64: Key {}", key))
        })?;
        let counter = u64::from_be_bytes(value);
        self.write_ibc_data(key, (counter + 1).to_be_bytes());
        Ok(counter)
    }

    /// Get and increment the sequence
    fn get_and_inc_sequence(&self, key: &Key) -> Result<Sequence> {
        if let Some(v) = self.read_ibc_data(key) {
            let index: [u8; 8] = v.try_into().map_err(|_| {
                Error::Sequence(format!(
                    "The sequence index wasn't u64: Key {}",
                    key
                ))
            })?;
            let index: u64 = u64::from_be_bytes(index);
            self.write_ibc_data(key, (index + 1).to_be_bytes());
            Ok(index.into())
        } else {
            // when the sequence has never been used, returns the initial value
            Ok(1.into())
        }
    }

    /// Bind a new port
    fn bind_port(&self, port_id: &PortId) -> Result<()> {
        let index_key = storage::capability_index_key();
        let cap_index = self.get_and_inc_counter(&index_key)?;
        let port_key = storage::port_key(port_id);
        self.write_ibc_data(&port_key, cap_index.to_be_bytes());
        let cap_key = storage::capability_key(cap_index);
        self.write_ibc_data(&cap_key, port_id.as_bytes());
        Ok(())
    }
}

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

/// Returns Anoma commitment prefix
pub fn commitment_prefix() -> CommitmentPrefix {
    let bytes = "ibc".as_bytes().to_vec();
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
