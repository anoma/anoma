//! IBC-related data definitions.
use std::convert::TryFrom;

use borsh::{BorshDeserialize, BorshSerialize};
#[cfg(not(feature = "ABCI"))]
use ibc::applications::ics20_fungible_token_transfer::msgs::transfer::MsgTransfer;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::height::Height;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::misbehavior::MsgSubmitAnyMisbehaviour;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics02_client::msgs::ClientMsg;
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
use ibc::core::ics24_host::identifier::{ChannelId, PortId};
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics26_routing::error::Error as Ics26Error;
#[cfg(not(feature = "ABCI"))]
use ibc::core::ics26_routing::msgs::Ics26Envelope;
#[cfg(not(feature = "ABCI"))]
use ibc::downcast;
#[cfg(not(feature = "ABCI"))]
use ibc::timestamp::Timestamp;
#[cfg(feature = "ABCI")]
use ibc_abci::applications::ics20_fungible_token_transfer::msgs::transfer::MsgTransfer;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::height::Height;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::create_client::MsgCreateAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::misbehavior::MsgSubmitAnyMisbehaviour;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::update_client::MsgUpdateAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::upgrade_client::MsgUpgradeAnyClient;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics02_client::msgs::ClientMsg;
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
use ibc_abci::core::ics24_host::identifier::{ChannelId, PortId};
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics26_routing::error::Error as Ics26Error;
#[cfg(feature = "ABCI")]
use ibc_abci::core::ics26_routing::msgs::Ics26Envelope;
#[cfg(feature = "ABCI")]
use ibc_abci::downcast;
#[cfg(feature = "ABCI")]
use ibc_abci::timestamp::Timestamp;
use prost::Message;
use prost_types::Any;
use thiserror::Error;

use crate::types::time::DateTimeUtc;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Decoding IBC data error: {0}")]
    DecodingData(prost::DecodeError),
    #[error("Decoding message error: {0}")]
    DecodingMessage(Ics26Error),
    #[error("Downcast error: {0}")]
    Downcast(String),
}

/// Decode result for IBC data
pub type Result<T> = std::result::Result<T, Error>;

/// Data for sending a packet
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize)]
pub struct PacketSendData {
    /// The source port
    pub source_port: PortId,
    /// The source channel
    pub source_channel: ChannelId,
    /// The destination port
    pub destination_port: PortId,
    /// The destination channel
    pub destination_channel: ChannelId,
    /// The data of packet
    pub packet_data: Vec<u8>,
    /// The timeout height
    pub timeout_height: Height,
    /// The timeout timestamp
    pub timeout_timestamp: Option<DateTimeUtc>,
}

impl PacketSendData {
    /// Create data for sending a packet
    pub fn new(
        source_port: PortId,
        source_channel: ChannelId,
        destination_port: PortId,
        destination_channel: ChannelId,
        packet_data: Vec<u8>,
        timeout_height: Height,
        timeout_timestamp: Timestamp,
    ) -> Self {
        let timeout_timestamp =
            timeout_timestamp.as_datetime().map(DateTimeUtc);
        Self {
            source_port,
            source_channel,
            destination_port,
            destination_channel,
            packet_data,
            timeout_height,
            timeout_timestamp,
        }
    }

    /// Returns a packet
    pub fn packet(&self, sequence: Sequence) -> Packet {
        let timeout_timestamp = match self.timeout_timestamp {
            Some(timestamp) => Timestamp::from_datetime(timestamp.0),
            None => Timestamp::none(),
        };
        Packet {
            sequence,
            source_port: self.source_port.clone(),
            source_channel: self.source_channel.clone(),
            destination_port: self.destination_port.clone(),
            destination_channel: self.destination_channel.clone(),
            data: self.packet_data.clone(),
            timeout_height: self.timeout_height,
            timeout_timestamp,
        }
    }
}

/// IBC Message
#[derive(Debug, Clone)]
pub struct IbcMessage(pub Ics26Envelope);

impl TryFrom<Any> for IbcMessage {
    type Error = Error;

    fn try_from(message: Any) -> Result<Self> {
        let envelope =
            Ics26Envelope::try_from(message).map_err(Error::DecodingMessage)?;
        Ok(Self(envelope))
    }
}

impl IbcMessage {
    /// Decode an IBC message from transaction data
    pub fn decode(tx_data: &[u8]) -> Result<Self> {
        let msg = Any::decode(tx_data).map_err(Error::DecodingData)?;
        msg.try_into()
    }

    /// Get the IBC message of CreateClient
    pub fn msg_create_any_client(self) -> Result<MsgCreateAnyClient> {
        let ics02_msg = self.ics02_msg()?;
        downcast!(ics02_msg => ClientMsg::CreateClient).ok_or_else(|| {
            Error::Downcast(
                "The message is not a CreateClient message".to_string(),
            )
        })
    }

    /// Get the IBC message of UpdateClient
    pub fn msg_update_any_client(self) -> Result<MsgUpdateAnyClient> {
        let ics02_msg = self.ics02_msg()?;
        downcast!(ics02_msg => ClientMsg::UpdateClient).ok_or_else(|| {
            Error::Downcast(
                "The message is not a UpdateClient message".to_string(),
            )
        })
    }

    /// Get the IBC message of Misbehaviour
    pub fn msg_submit_any_misbehaviour(
        self,
    ) -> Result<MsgSubmitAnyMisbehaviour> {
        let ics02_msg = self.ics02_msg()?;
        downcast!(ics02_msg => ClientMsg::Misbehaviour).ok_or_else(|| {
            Error::Downcast(
                "The message is not a Misbehaviour message".to_string(),
            )
        })
    }

    /// Get the IBC message of UpgradeClient
    pub fn msg_upgrade_any_client(self) -> Result<MsgUpgradeAnyClient> {
        let ics02_msg = self.ics02_msg()?;
        downcast!(ics02_msg => ClientMsg::UpgradeClient).ok_or_else(|| {
            Error::Downcast(
                "The message is not a UpgradeClient message".to_string(),
            )
        })
    }

    /// Get the IBC message of ConnectionOpenInit
    pub fn msg_connection_open_init(self) -> Result<MsgConnectionOpenInit> {
        let ics03_msg = self.ics03_msg()?;
        downcast!(ics03_msg => ConnectionMsg::ConnectionOpenInit).ok_or_else(
            || {
                Error::Downcast(
                    "The message is not a ConnectionOpenInit message"
                        .to_string(),
                )
            },
        )
    }

    /// Get the IBC message of ConnectionOpenTry
    pub fn msg_connection_open_try(self) -> Result<Box<MsgConnectionOpenTry>> {
        let ics03_msg = self.ics03_msg()?;
        downcast!(ics03_msg => ConnectionMsg::ConnectionOpenTry).ok_or_else(
            || {
                Error::Downcast(
                    "The message is not a ConnectionOpenTry message"
                        .to_string(),
                )
            },
        )
    }

    /// Get the IBC message of ConnectionOpenAck
    pub fn msg_connection_open_ack(self) -> Result<Box<MsgConnectionOpenAck>> {
        let ics03_msg = self.ics03_msg()?;
        downcast!(ics03_msg => ConnectionMsg::ConnectionOpenAck).ok_or_else(
            || {
                Error::Downcast(
                    "The message is not a ConnectionOpenAck message"
                        .to_string(),
                )
            },
        )
    }

    /// Get the IBC message of ConnectionOpenConfirm
    pub fn msg_connection_open_confirm(
        self,
    ) -> Result<MsgConnectionOpenConfirm> {
        let ics03_msg = self.ics03_msg()?;
        downcast!(ics03_msg => ConnectionMsg::ConnectionOpenConfirm).ok_or_else(
            || {
                Error::Downcast(
                    "The message is not a ConnectionOpenAck message"
                        .to_string(),
                )
            },
        )
    }

    /// Get the IBC message of ChannelOpenInit
    pub fn msg_channel_open_init(self) -> Result<MsgChannelOpenInit> {
        let ics04_msg = self.ics04_channel_msg()?;
        downcast!(ics04_msg => ChannelMsg::ChannelOpenInit).ok_or_else(|| {
            Error::Downcast(
                "The message is not a ChannelOpenInit message".to_string(),
            )
        })
    }

    /// Get the IBC message of ChannelOpenTry
    pub fn msg_channel_open_try(self) -> Result<MsgChannelOpenTry> {
        let ics04_msg = self.ics04_channel_msg()?;
        downcast!(ics04_msg => ChannelMsg::ChannelOpenTry).ok_or_else(|| {
            Error::Downcast(
                "The message is not a ChannelOpenTry message".to_string(),
            )
        })
    }

    /// Get the IBC message of ChannelOpenAck
    pub fn msg_channel_open_ack(self) -> Result<MsgChannelOpenAck> {
        let ics04_msg = self.ics04_channel_msg()?;
        downcast!(ics04_msg => ChannelMsg::ChannelOpenAck).ok_or_else(|| {
            Error::Downcast(
                "The message is not a ChannelOpenAck message".to_string(),
            )
        })
    }

    /// Get the IBC message of ChannelOpenConfirm
    pub fn msg_channel_open_confirm(self) -> Result<MsgChannelOpenConfirm> {
        let ics04_msg = self.ics04_channel_msg()?;
        downcast!(ics04_msg => ChannelMsg::ChannelOpenConfirm).ok_or_else(
            || {
                Error::Downcast(
                    "The message is not a ChannelOpenConfirm message"
                        .to_string(),
                )
            },
        )
    }

    /// Get the IBC message of ChannelCloseInit
    pub fn msg_channel_close_init(self) -> Result<MsgChannelCloseInit> {
        let ics04_msg = self.ics04_channel_msg()?;
        downcast!(ics04_msg => ChannelMsg::ChannelCloseInit).ok_or_else(|| {
            Error::Downcast(
                "The message is not a ChannelCloseInit message".to_string(),
            )
        })
    }

    /// Get the IBC message of ChannelCloseConfirm
    pub fn msg_channel_close_confirm(self) -> Result<MsgChannelCloseConfirm> {
        let ics04_msg = self.ics04_channel_msg()?;
        downcast!(ics04_msg => ChannelMsg::ChannelCloseConfirm).ok_or_else(
            || {
                Error::Downcast(
                    "The message is not a ChannelCloseInit message".to_string(),
                )
            },
        )
    }

    /// Get the IBC message of RecvPacket
    pub fn msg_recv_packet(self) -> Result<MsgRecvPacket> {
        let ics04_msg = self.ics04_packet_msg()?;
        downcast!(ics04_msg => PacketMsg::RecvPacket).ok_or_else(|| {
            Error::Downcast(
                "The message is not a RecvPacket message".to_string(),
            )
        })
    }

    /// Get the IBC message of Acknowledgement
    pub fn msg_acknowledgement(self) -> Result<MsgAcknowledgement> {
        let ics04_msg = self.ics04_packet_msg()?;
        downcast!(ics04_msg => PacketMsg::AckPacket).ok_or_else(|| {
            Error::Downcast(
                "The message is not an Acknowledgement message".to_string(),
            )
        })
    }

    /// Get the IBC message of TimeoutPacket
    pub fn msg_timeout(self) -> Result<MsgTimeout> {
        let ics04_msg = self.ics04_packet_msg()?;
        downcast!(ics04_msg => PacketMsg::ToPacket).ok_or_else(|| {
            Error::Downcast(
                "The message is not a TimeoutPacket message".to_string(),
            )
        })
    }

    /// Get the IBC message of TimeoutPacketOnClose
    pub fn msg_timeout_on_close(self) -> Result<MsgTimeoutOnClose> {
        let ics04_msg = self.ics04_packet_msg()?;
        downcast!(ics04_msg => PacketMsg::ToClosePacket).ok_or_else(|| {
            Error::Downcast(
                "The message is not a TimeoutPacketOnClose message".to_string(),
            )
        })
    }

    /// Get the IBC message of ICS20
    pub fn msg_transfer(self) -> Result<MsgTransfer> {
        downcast!(self.0 => Ics26Envelope::Ics20Msg).ok_or_else(|| {
            Error::Downcast("The message is not an ICS20 message".to_string())
        })
    }

    fn ics02_msg(self) -> Result<ClientMsg> {
        downcast!(self.0 => Ics26Envelope::Ics2Msg).ok_or_else(|| {
            Error::Downcast("The message is not an ICS02 message".to_string())
        })
    }

    fn ics03_msg(self) -> Result<ConnectionMsg> {
        downcast!(self.0 => Ics26Envelope::Ics3Msg).ok_or_else(|| {
            Error::Downcast("The message is not an ICS03 message".to_string())
        })
    }

    fn ics04_channel_msg(self) -> Result<ChannelMsg> {
        downcast!(self.0 => Ics26Envelope::Ics4ChannelMsg).ok_or_else(|| {
            Error::Downcast(
                "The message is not an ICS04 channel message".to_string(),
            )
        })
    }

    fn ics04_packet_msg(self) -> Result<PacketMsg> {
        downcast!(self.0 => Ics26Envelope::Ics4PacketMsg).ok_or_else(|| {
            Error::Downcast(
                "The message is not an ICS04 packet message".to_string(),
            )
        })
    }
}
