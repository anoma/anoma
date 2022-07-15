//! Functions to handle IBC modules

use std::str::FromStr;

use sha2::Digest;
use thiserror::Error;

use crate::ibc::applications::ics20_fungible_token_transfer::msgs::transfer::MsgTransfer;
use crate::ibc::clients::ics07_tendermint::consensus_state::ConsensusState as TmConsensusState;
use crate::ibc::core::ics02_client::client_consensus::{
    AnyConsensusState, ConsensusState,
};
use crate::ibc::core::ics02_client::client_state::{
    AnyClientState, ClientState,
};
use crate::ibc::core::ics02_client::client_type::ClientType;
use crate::ibc::core::ics02_client::events::{
    Attributes as ClientAttributes, CreateClient, UpdateClient, UpgradeClient,
};
use crate::ibc::core::ics02_client::header::{AnyHeader, Header};
use crate::ibc::core::ics02_client::height::Height;
use crate::ibc::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
use crate::ibc::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
use crate::ibc::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
use crate::ibc::core::ics02_client::msgs::ClientMsg;
use crate::ibc::core::ics03_connection::connection::{
    ConnectionEnd, Counterparty as ConnCounterparty, State as ConnState,
};
use crate::ibc::core::ics03_connection::events::{
    Attributes as ConnectionAttributes, OpenAck as ConnOpenAck,
    OpenConfirm as ConnOpenConfirm, OpenInit as ConnOpenInit,
    OpenTry as ConnOpenTry,
};
use crate::ibc::core::ics03_connection::msgs::conn_open_ack::MsgConnectionOpenAck;
use crate::ibc::core::ics03_connection::msgs::conn_open_confirm::MsgConnectionOpenConfirm;
use crate::ibc::core::ics03_connection::msgs::conn_open_init::MsgConnectionOpenInit;
use crate::ibc::core::ics03_connection::msgs::conn_open_try::MsgConnectionOpenTry;
use crate::ibc::core::ics03_connection::msgs::ConnectionMsg;
use crate::ibc::core::ics03_connection::version::Version as ConnVersion;
use crate::ibc::core::ics04_channel::channel::{
    ChannelEnd, Counterparty as ChanCounterparty, Order, State as ChanState,
};
use crate::ibc::core::ics04_channel::commitment::PacketCommitment;
use crate::ibc::core::ics04_channel::events::{
    AcknowledgePacket, CloseConfirm as ChanCloseConfirm,
    CloseInit as ChanCloseInit, OpenAck as ChanOpenAck,
    OpenConfirm as ChanOpenConfirm, OpenInit as ChanOpenInit,
    OpenTry as ChanOpenTry, SendPacket, TimeoutPacket, WriteAcknowledgement,
};
use crate::ibc::core::ics04_channel::msgs::acknowledgement::MsgAcknowledgement;
use crate::ibc::core::ics04_channel::msgs::chan_close_confirm::MsgChannelCloseConfirm;
use crate::ibc::core::ics04_channel::msgs::chan_close_init::MsgChannelCloseInit;
use crate::ibc::core::ics04_channel::msgs::chan_open_ack::MsgChannelOpenAck;
use crate::ibc::core::ics04_channel::msgs::chan_open_confirm::MsgChannelOpenConfirm;
use crate::ibc::core::ics04_channel::msgs::chan_open_init::MsgChannelOpenInit;
use crate::ibc::core::ics04_channel::msgs::chan_open_try::MsgChannelOpenTry;
use crate::ibc::core::ics04_channel::msgs::recv_packet::MsgRecvPacket;
use crate::ibc::core::ics04_channel::msgs::timeout::MsgTimeout;
use crate::ibc::core::ics04_channel::msgs::timeout_on_close::MsgTimeoutOnClose;
use crate::ibc::core::ics04_channel::msgs::{ChannelMsg, PacketMsg};
use crate::ibc::core::ics04_channel::packet::{Packet, Sequence};
use crate::ibc::core::ics23_commitment::commitment::CommitmentPrefix;
use crate::ibc::core::ics24_host::error::ValidationError as Ics24Error;
use crate::ibc::core::ics24_host::identifier::{
    ChannelId, ClientId, ConnectionId, PortChannelId, PortId,
};
use crate::ibc::core::ics26_routing::msgs::Ics26Envelope;
use crate::ibc::events::IbcEvent;
#[cfg(any(feature = "ibc-mocks-abci", feature = "ibc-mocks"))]
use crate::ibc::mock::client_state::{MockClientState, MockConsensusState};
use crate::ibc::timestamp::Timestamp;
use crate::ledger::ibc::storage;
use crate::tendermint::Time;
use crate::tendermint_proto::{Error as ProtoError, Protobuf};
use crate::types::address::{Address, InternalAddress};
use crate::types::ibc::data::{
    Error as IbcDataError, FungibleTokenPacketData, IbcMessage, PacketAck,
    PacketReceipt,
};
use crate::types::ibc::IbcEvent as AnomaIbcEvent;
use crate::types::storage::{BlockHeight, Key};
use crate::types::time::Rfc3339String;
use crate::types::token::{self, Amount};

const COMMITMENT_PREFIX: &[u8] = b"ibc";

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
    #[error("Time error: {0}")]
    Time(String),
    #[error("Invalid transfer message: {0}")]
    TransferMessage(token::TransferError),
    #[error("Sending a token error: {0}")]
    SendingToken(String),
    #[error("Receiving a token error: {0}")]
    ReceivingToken(String),
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
    fn emit_ibc_event(&self, event: AnomaIbcEvent);

    /// Transfer token
    fn transfer_token(
        &self,
        src: &Address,
        dest: &Address,
        token: &Address,
        amount: Amount,
    );

    /// Get the current height of this chain
    fn get_height(&self) -> BlockHeight;

    /// Get the current time of the tendermint header of this chain
    fn get_header_time(&self) -> Rfc3339String;

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
            Ics26Envelope::Ics20Msg(msg) => self.send_token(msg),
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

        self.set_client_update_time(&client_id)?;

        let event = make_create_client_event(&client_id, msg)
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Update a client
    fn update_client(&self, msg: &MsgUpdateAnyClient) -> Result<()> {
        // get and update the client
        let client_id = msg.client_id.clone();
        let client_state_key = storage::client_state_key(&client_id);
        let value = self.read_ibc_data(&client_state_key).ok_or_else(|| {
            Error::Client(format!(
                "The client to be updated doesn't exist: ID {}",
                client_id
            ))
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

        self.set_client_update_time(&client_id)?;

        let event = make_update_client_event(&client_id, msg)
            .try_into()
            .unwrap();
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

        self.set_client_update_time(&msg.client_id)?;

        let event = make_upgrade_client_event(&msg.client_id, msg)
            .try_into()
            .unwrap();
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

        let event = make_open_init_connection_event(&conn_id, msg)
            .try_into()
            .unwrap();
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

        let event = make_open_try_connection_event(&conn_id, msg)
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Open the connection for ConnectionOpenAck
    fn ack_connection(&self, msg: &MsgConnectionOpenAck) -> Result<()> {
        let conn_key = storage::connection_key(&msg.connection_id);
        let value = self.read_ibc_data(&conn_key).ok_or_else(|| {
            Error::Connection(format!(
                "The connection to be opened doesn't exist: ID {}",
                msg.connection_id
            ))
        })?;
        let mut connection =
            ConnectionEnd::decode_vec(&value).map_err(Error::Decoding)?;
        open_connection(&mut connection);
        let mut counterparty = connection.counterparty().clone();
        counterparty.connection_id =
            Some(msg.counterparty_connection_id.clone());
        connection.set_counterparty(counterparty);
        self.write_ibc_data(
            &conn_key,
            connection.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_ack_connection_event(msg).try_into().unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Open the connection for ConnectionOpenConfirm
    fn confirm_connection(&self, msg: &MsgConnectionOpenConfirm) -> Result<()> {
        let conn_key = storage::connection_key(&msg.connection_id);
        let value = self.read_ibc_data(&conn_key).ok_or_else(|| {
            Error::Connection(format!(
                "The connection to be opend doesn't exist: ID {}",
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

        let event = make_open_confirm_connection_event(msg).try_into().unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Initialize a channel for ChannelOpenInit
    fn init_channel(&self, msg: &MsgChannelOpenInit) -> Result<()> {
        self.bind_port(&msg.port_id)?;
        let counter_key = storage::channel_counter_key();
        let counter = self.get_and_inc_counter(&counter_key)?;
        let channel_id = channel_id(counter);
        let port_channel_id = port_channel_id(msg.port_id.clone(), channel_id);
        let channel_key = storage::channel_key(&port_channel_id);
        self.write_ibc_data(
            &channel_key,
            msg.channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_init_channel_event(&channel_id, msg)
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Initialize a channel for ChannelOpenTry
    fn try_channel(&self, msg: &MsgChannelOpenTry) -> Result<()> {
        self.bind_port(&msg.port_id)?;
        let counter_key = storage::channel_counter_key();
        let counter = self.get_and_inc_counter(&counter_key)?;
        let channel_id = channel_id(counter);
        let port_channel_id = port_channel_id(msg.port_id.clone(), channel_id);
        let channel_key = storage::channel_key(&port_channel_id);
        self.write_ibc_data(
            &channel_key,
            msg.channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_try_channel_event(&channel_id, msg)
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Open the channel for ChannelOpenAck
    fn ack_channel(&self, msg: &MsgChannelOpenAck) -> Result<()> {
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), msg.channel_id);
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel to be opened doesn't exist: Port/Channel {}",
                port_channel_id
            ))
        })?;
        let mut channel =
            ChannelEnd::decode_vec(&value).map_err(Error::Decoding)?;
        channel.set_counterparty_channel_id(msg.counterparty_channel_id);
        open_channel(&mut channel);
        self.write_ibc_data(
            &channel_key,
            channel.encode_vec().expect("encoding shouldn't fail"),
        );

        let event = make_open_ack_channel_event(msg, &channel)?
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Open the channel for ChannelOpenConfirm
    fn confirm_channel(&self, msg: &MsgChannelOpenConfirm) -> Result<()> {
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), msg.channel_id);
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel to be opened doesn't exist: Port/Channel {}",
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

        let event = make_open_confirm_channel_event(msg, &channel)?
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Close the channel for ChannelCloseInit
    fn close_init_channel(&self, msg: &MsgChannelCloseInit) -> Result<()> {
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), msg.channel_id);
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel to be closed doesn't exist: Port/Channel {}",
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

        let event = make_close_init_channel_event(msg, &channel)?
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Close the channel for ChannelCloseConfirm
    fn close_confirm_channel(
        &self,
        msg: &MsgChannelCloseConfirm,
    ) -> Result<()> {
        let port_channel_id =
            port_channel_id(msg.port_id.clone(), msg.channel_id);
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel to be closed doesn't exist: Port/Channel {}",
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

        let event = make_close_confirm_channel_event(msg, &channel)?
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Send a packet
    fn send_packet(
        &self,
        port_channel_id: PortChannelId,
        data: Vec<u8>,
        timeout_height: Height,
        timeout_timestamp: Timestamp,
    ) -> Result<()> {
        // get and increment the next sequence send
        let seq_key = storage::next_sequence_send_key(&port_channel_id);
        let sequence = self.get_and_inc_sequence(&seq_key)?;

        // get the channel for the destination info.
        let channel_key = storage::channel_key(&port_channel_id);
        let channel = self
            .read_ibc_data(&channel_key)
            .expect("cannot get the channel to be closed");
        let channel =
            ChannelEnd::decode_vec(&channel).expect("cannot get the channel");
        let counterparty = channel.counterparty();

        // make a packet
        let packet = Packet {
            sequence,
            source_port: port_channel_id.port_id.clone(),
            source_channel: port_channel_id.channel_id,
            destination_port: counterparty.port_id.clone(),
            destination_channel: *counterparty
                .channel_id()
                .expect("the counterparty channel should exist"),
            data,
            timeout_height,
            timeout_timestamp,
        };
        // store the commitment of the packet
        let commitment_key = storage::commitment_key(
            &port_channel_id.port_id,
            &port_channel_id.channel_id,
            packet.sequence,
        );
        let commitment = commitment(&packet);
        self.write_ibc_data(&commitment_key, commitment.into_vec());

        let event = make_send_packet_event(packet).try_into().unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Receive a packet
    fn receive_packet(&self, msg: &MsgRecvPacket) -> Result<()> {
        // TODO for other applications
        // check the packet data
        let packet_ack =
            if let Ok(data) = serde_json::from_slice(&msg.packet.data) {
                match self.receive_token(&msg.packet, &data) {
                    Ok(_) => PacketAck::result_success(),
                    Err(e) => PacketAck::result_error(e.to_string()),
                }
            } else {
                PacketAck::result_error("unknown packet data".to_string())
            };

        // store the receipt
        let receipt_key = storage::receipt_key(
            &msg.packet.destination_port,
            &msg.packet.destination_channel,
            msg.packet.sequence,
        );
        self.write_ibc_data(&receipt_key, PacketReceipt::default().as_bytes());

        // store the ack
        let ack_key = storage::ack_key(
            &msg.packet.destination_port,
            &msg.packet.destination_channel,
            msg.packet.sequence,
        );
        let ack = packet_ack.encode_to_vec();
        let ack_commitment = sha2::Sha256::digest(&ack).to_vec();
        self.write_ibc_data(&ack_key, ack_commitment);

        // increment the next sequence receive
        let port_channel_id = port_channel_id(
            msg.packet.destination_port.clone(),
            msg.packet.destination_channel,
        );
        let seq_key = storage::next_sequence_recv_key(&port_channel_id);
        self.get_and_inc_sequence(&seq_key)?;

        let event = make_write_ack_event(msg.packet.clone(), ack)
            .try_into()
            .unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Receive a acknowledgement
    fn acknowledge_packet(&self, msg: &MsgAcknowledgement) -> Result<()> {
        let ack = PacketAck::try_from(msg.acknowledgement.clone())
            .map_err(Error::IbcData)?;
        if !ack.is_success() {
            // TODO for other applications
            if let Ok(data) = serde_json::from_slice(&msg.packet.data) {
                self.refund_token(&msg.packet, &data)?;
            }
        }

        let commitment_key = storage::commitment_key(
            &msg.packet.source_port,
            &msg.packet.source_channel,
            msg.packet.sequence,
        );
        self.delete_ibc_data(&commitment_key);

        // get and increment the next sequence ack
        let port_channel_id = port_channel_id(
            msg.packet.source_port.clone(),
            msg.packet.source_channel,
        );
        let seq_key = storage::next_sequence_ack_key(&port_channel_id);
        self.get_and_inc_sequence(&seq_key)?;

        let event = make_ack_event(msg.packet.clone()).try_into().unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Receive a timeout
    fn timeout_packet(&self, msg: &MsgTimeout) -> Result<()> {
        // TODO for other applications
        // check the packet data
        if let Ok(data) = serde_json::from_slice(&msg.packet.data) {
            self.refund_token(&msg.packet, &data)?;
        }

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
            msg.packet.source_channel,
        );
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel to be closed doesn't exist: Port/Channel {}",
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

        let event = make_timeout_event(msg.packet.clone()).try_into().unwrap();
        self.emit_ibc_event(event);

        Ok(())
    }

    /// Receive a timeout for TimeoutOnClose
    fn timeout_on_close_packet(&self, msg: &MsgTimeoutOnClose) -> Result<()> {
        // TODO for other applications
        // check the packet data
        if let Ok(data) = serde_json::from_slice(&msg.packet.data) {
            self.refund_token(&msg.packet, &data)?;
        }

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
            msg.packet.source_channel,
        );
        let channel_key = storage::channel_key(&port_channel_id);
        let value = self.read_ibc_data(&channel_key).ok_or_else(|| {
            Error::Channel(format!(
                "The channel to be closed doesn't exist: Port/Channel {}",
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

    /// Set the timestamp and the height for the client update
    fn set_client_update_time(&self, client_id: &ClientId) -> Result<()> {
        let time = Time::parse_from_rfc3339(&self.get_header_time().0)
            .map_err(|e| {
                Error::Time(format!("The time of the header is invalid: {}", e))
            })?;
        let key = storage::client_update_timestamp_key(client_id);
        self.write_ibc_data(
            &key,
            time.encode_vec().expect("encoding shouldn't fail"),
        );

        // the revision number is always 0
        let height = Height::new(0, self.get_height().0);
        let height_key = storage::client_update_height_key(client_id);
        // write the current height as u64
        self.write_ibc_data(
            &height_key,
            height.encode_vec().expect("Encoding shouldn't fail"),
        );

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
        let index = match self.read_ibc_data(key) {
            Some(v) => {
                let index: [u8; 8] = v.try_into().map_err(|_| {
                    Error::Sequence(format!(
                        "The sequence index wasn't u64: Key {}",
                        key
                    ))
                })?;
                u64::from_be_bytes(index)
            }
            // when the sequence has never been used, returns the initial value
            None => 1,
        };
        self.write_ibc_data(key, (index + 1).to_be_bytes());
        Ok(index.into())
    }

    /// Bind a new port
    fn bind_port(&self, port_id: &PortId) -> Result<()> {
        let port_key = storage::port_key(port_id);
        match self.read_ibc_data(&port_key) {
            Some(_) => {}
            None => {
                // create a new capability and claim it
                let index_key = storage::capability_index_key();
                let cap_index = self.get_and_inc_counter(&index_key)?;
                self.write_ibc_data(&port_key, cap_index.to_be_bytes());
                let cap_key = storage::capability_key(cap_index);
                self.write_ibc_data(&cap_key, port_id.as_bytes());
            }
        }
        Ok(())
    }

    /// Send the specified token by escrowing or burning
    fn send_token(&self, msg: &MsgTransfer) -> Result<()> {
        let data = FungibleTokenPacketData::from(msg.clone());
        let source = Address::decode(data.sender.clone()).map_err(|e| {
            Error::SendingToken(format!(
                "Invalid sender address: sender {}, error {}",
                data.sender, e
            ))
        })?;
        let token_str = data.denom.split('/').last().ok_or_else(|| {
            Error::SendingToken(format!(
                "No token was specified: {}",
                data.denom
            ))
        })?;
        let token = Address::decode(token_str).map_err(|e| {
            Error::SendingToken(format!(
                "Invalid token address: token {}, error {}",
                token_str, e
            ))
        })?;
        let amount = Amount::from_str(&data.amount).map_err(|e| {
            Error::SendingToken(format!(
                "Invalid amount: amount {}, error {}",
                data.amount, e
            ))
        })?;

        // check the denom field
        let prefix = format!(
            "{}/{}/",
            msg.source_port.clone(),
            msg.source_channel.clone()
        );
        if data.denom.starts_with(&prefix) {
            // sink zone
            let burn = Address::Internal(InternalAddress::IbcBurn);
            self.transfer_token(&source, &burn, &token, amount);
        } else {
            // source zone
            let escrow =
                Address::Internal(InternalAddress::ibc_escrow_address(
                    msg.source_port.to_string(),
                    msg.source_channel.to_string(),
                ));
            self.transfer_token(&source, &escrow, &token, amount);
        }

        // send a packet
        let port_channel_id =
            port_channel_id(msg.source_port.clone(), msg.source_channel);
        let packet_data = serde_json::to_vec(&data)
            .expect("encoding the packet data shouldn't fail");
        self.send_packet(
            port_channel_id,
            packet_data,
            msg.timeout_height,
            msg.timeout_timestamp,
        )
    }

    /// Receive the specified token by unescrowing or minting
    fn receive_token(
        &self,
        packet: &Packet,
        data: &FungibleTokenPacketData,
    ) -> Result<()> {
        let dest = Address::decode(data.receiver.clone()).map_err(|e| {
            Error::ReceivingToken(format!(
                "Invalid receiver address: receiver {}, error {}",
                data.receiver, e
            ))
        })?;
        let token_str = data.denom.split('/').last().ok_or_else(|| {
            Error::ReceivingToken(format!(
                "No token was specified: {}",
                data.denom
            ))
        })?;
        let token = Address::decode(token_str).map_err(|e| {
            Error::ReceivingToken(format!(
                "Invalid token address: token {}, error {}",
                token_str, e
            ))
        })?;
        let amount = Amount::from_str(&data.amount).map_err(|e| {
            Error::ReceivingToken(format!(
                "Invalid amount: amount {}, error {}",
                data.amount, e
            ))
        })?;

        let prefix = format!(
            "{}/{}/",
            packet.source_port.clone(),
            packet.source_channel.clone()
        );
        if data.denom.starts_with(&prefix) {
            // unescrow the token because this chain is the source
            let escrow =
                Address::Internal(InternalAddress::ibc_escrow_address(
                    packet.destination_port.to_string(),
                    packet.destination_channel.to_string(),
                ));
            self.transfer_token(&escrow, &dest, &token, amount);
        } else {
            // mint the token because the sender chain is the source
            let mint = Address::Internal(InternalAddress::IbcMint);
            self.transfer_token(&mint, &dest, &token, amount);
        }
        Ok(())
    }

    /// Refund the specified token by unescrowing or minting
    fn refund_token(
        &self,
        packet: &Packet,
        data: &FungibleTokenPacketData,
    ) -> Result<()> {
        let dest = Address::decode(data.sender.clone()).map_err(|e| {
            Error::ReceivingToken(format!(
                "Invalid sender address: sender {}, error {}",
                data.sender, e
            ))
        })?;
        let token_str = data.denom.split('/').last().ok_or_else(|| {
            Error::ReceivingToken(format!(
                "No token was specified: {}",
                data.denom
            ))
        })?;
        let token = Address::decode(token_str).map_err(|e| {
            Error::ReceivingToken(format!(
                "Invalid token address: token {}, error {}",
                token_str, e
            ))
        })?;
        let amount = Amount::from_str(&data.amount).map_err(|e| {
            Error::ReceivingToken(format!(
                "Invalid amount: amount {}, error {}",
                data.amount, e
            ))
        })?;

        let prefix = format!(
            "{}/{}/",
            packet.source_port.clone(),
            packet.source_channel.clone()
        );
        if data.denom.starts_with(&prefix) {
            // mint the token because the sender chain is the sink zone
            let mint = Address::Internal(InternalAddress::IbcMint);
            self.transfer_token(&mint, &dest, &token, amount);
        } else {
            // unescrow the token because the sender chain is the source zone
            let escrow =
                Address::Internal(InternalAddress::ibc_escrow_address(
                    packet.source_port.to_string(),
                    packet.source_channel.to_string(),
                ));
            self.transfer_token(&escrow, &dest, &token, amount);
        }
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
            #[cfg(any(feature = "ibc-mocks-abci", feature = "ibc-mocks"))]
            _ => Err(Error::ClientUpdate(
                "The header type is mismatched".to_owned(),
            )),
        },
        #[cfg(any(feature = "ibc-mocks-abci", feature = "ibc-mocks"))]
        AnyClientState::Mock(_) => match header {
            AnyHeader::Mock(h) => Ok((
                MockClientState::new(h).wrap_any(),
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
        msg.version
            .clone()
            .map_or_else(|| vec![ConnVersion::default()], |v| vec![v]),
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

/// Make a packet from MsgTransfer
pub fn packet_from_message(
    msg: &MsgTransfer,
    sequence: Sequence,
    counterparty: &ChanCounterparty,
) -> Packet {
    Packet {
        sequence,
        source_port: msg.source_port.clone(),
        source_channel: msg.source_channel,
        destination_port: counterparty.port_id.clone(),
        destination_channel: *counterparty
            .channel_id()
            .expect("the counterparty channel should exist"),
        data: serde_json::to_vec(&FungibleTokenPacketData::from(msg.clone()))
            .expect("encoding the packet data shouldn't fail"),
        timeout_height: msg.timeout_height,
        timeout_timestamp: msg.timeout_timestamp,
    }
}

/// Returns a commitment from the given packet
pub fn commitment(packet: &Packet) -> PacketCommitment {
    let mut input = packet
        .timeout_timestamp
        .nanoseconds()
        .to_be_bytes()
        .to_vec();
    let revision_number = packet.timeout_height.revision_number.to_be_bytes();
    input.append(&mut revision_number.to_vec());
    let revision_height = packet.timeout_height.revision_height.to_be_bytes();
    input.append(&mut revision_height.to_vec());
    let data = sha2::Sha256::digest(&packet.data);
    input.append(&mut data.to_vec());
    sha2::Sha256::digest(&input).to_vec().into()
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
    CommitmentPrefix::try_from(COMMITMENT_PREFIX.to_vec())
        .expect("the conversion shouldn't fail")
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
    ConnOpenInit::from(attributes).into()
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
    ConnOpenTry::from(attributes).into()
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
    ConnOpenAck::from(attributes).into()
}

/// Makes OpenConfirmConnection event
pub fn make_open_confirm_connection_event(
    msg: &MsgConnectionOpenConfirm,
) -> IbcEvent {
    let attributes = ConnectionAttributes {
        connection_id: Some(msg.connection_id.clone()),
        ..Default::default()
    };
    ConnOpenConfirm::from(attributes).into()
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
    let attributes = ChanOpenInit {
        height: Height::default(),
        port_id: msg.port_id.clone(),
        channel_id: Some(*channel_id),
        connection_id,
        counterparty_port_id: msg.channel.counterparty().port_id().clone(),
        counterparty_channel_id: msg
            .channel
            .counterparty()
            .channel_id()
            .cloned(),
    };
    attributes.into()
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
    let attributes = ChanOpenTry {
        height: Height::default(),
        port_id: msg.port_id.clone(),
        channel_id: Some(*channel_id),
        connection_id,
        counterparty_port_id: msg.channel.counterparty().port_id().clone(),
        counterparty_channel_id: msg
            .channel
            .counterparty()
            .channel_id()
            .cloned(),
    };
    attributes.into()
}

/// Makes OpenAckChannel event
pub fn make_open_ack_channel_event(
    msg: &MsgChannelOpenAck,
    channel: &ChannelEnd,
) -> Result<IbcEvent> {
    let conn_id = get_connection_id_from_channel(channel)?;
    let counterparty = channel.counterparty();
    let attributes = ChanOpenAck {
        height: Height::default(),
        port_id: msg.port_id.clone(),
        channel_id: Some(msg.channel_id),
        counterparty_channel_id: Some(msg.counterparty_channel_id),
        connection_id: conn_id.clone(),
        counterparty_port_id: counterparty.port_id().clone(),
    };
    Ok(attributes.into())
}

/// Makes OpenConfirmChannel event
pub fn make_open_confirm_channel_event(
    msg: &MsgChannelOpenConfirm,
    channel: &ChannelEnd,
) -> Result<IbcEvent> {
    let conn_id = get_connection_id_from_channel(channel)?;
    let counterparty = channel.counterparty();
    let attributes = ChanOpenConfirm {
        height: Height::default(),
        port_id: msg.port_id.clone(),
        channel_id: Some(msg.channel_id),
        connection_id: conn_id.clone(),
        counterparty_port_id: counterparty.port_id().clone(),
        counterparty_channel_id: counterparty.channel_id().cloned(),
    };
    Ok(attributes.into())
}

/// Makes CloseInitChannel event
pub fn make_close_init_channel_event(
    msg: &MsgChannelCloseInit,
    channel: &ChannelEnd,
) -> Result<IbcEvent> {
    let conn_id = get_connection_id_from_channel(channel)?;
    let counterparty = channel.counterparty();
    let attributes = ChanCloseInit {
        height: Height::default(),
        port_id: msg.port_id.clone(),
        channel_id: msg.channel_id,
        connection_id: conn_id.clone(),
        counterparty_port_id: counterparty.port_id().clone(),
        counterparty_channel_id: counterparty.channel_id().cloned(),
    };
    Ok(attributes.into())
}

/// Makes CloseConfirmChannel event
pub fn make_close_confirm_channel_event(
    msg: &MsgChannelCloseConfirm,
    channel: &ChannelEnd,
) -> Result<IbcEvent> {
    let conn_id = get_connection_id_from_channel(channel)?;
    let counterparty = channel.counterparty();
    let attributes = ChanCloseConfirm {
        height: Height::default(),
        port_id: msg.port_id.clone(),
        channel_id: Some(msg.channel_id),
        connection_id: conn_id.clone(),
        counterparty_port_id: counterparty.port_id.clone(),
        counterparty_channel_id: counterparty.channel_id().cloned(),
    };
    Ok(attributes.into())
}

fn get_connection_id_from_channel(
    channel: &ChannelEnd,
) -> Result<&ConnectionId> {
    channel.connection_hops().get(0).ok_or_else(|| {
        Error::Channel("No connection for the channel".to_owned())
    })
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
