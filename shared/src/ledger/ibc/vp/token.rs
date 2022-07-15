//! IBC token transfer validation as a native validity predicate

use std::collections::{BTreeSet, HashSet};
use std::str::FromStr;

use borsh::BorshDeserialize;
use thiserror::Error;

use crate::ibc::applications::ics20_fungible_token_transfer::msgs::transfer::MsgTransfer;
use crate::ibc::core::ics04_channel::msgs::PacketMsg;
use crate::ibc::core::ics04_channel::packet::Packet;
use crate::ibc::core::ics26_routing::msgs::Ics26Envelope;
use crate::ledger::native_vp::{self, Ctx, NativeVp};
use crate::ledger::storage::{self as ledger_storage, StorageHasher};
use crate::proto::SignedTxData;
use crate::types::address::{Address, Error as AddressError, InternalAddress};
use crate::types::ibc::data::{
    Error as IbcDataError, FungibleTokenPacketData, IbcMessage,
};
use crate::types::storage::Key;
use crate::types::token::{
    self, is_non_owner_balance_key, Amount, AmountParseError,
};
use crate::vm::WasmCacheAccess;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("Native VP error: {0}")]
    NativeVpError(native_vp::Error),
    #[error("IBC message error: {0}")]
    IbcMessage(IbcDataError),
    #[error("Invalid message error")]
    InvalidMessage,
    #[error("Invalid address error")]
    Address(AddressError),
    #[error("Token error")]
    NoToken,
    #[error("Parsing amount error")]
    Amount(AmountParseError),
    #[error("Decoding error")]
    Decoding(std::io::Error),
    #[error("Decoding PacketData error")]
    DecodingPacketData(serde_json::Error),
    #[error("Invalid token transfer error")]
    TokenTransfer(String),
    #[error("IBC message is required as transaction data")]
    NoTxData,
}

/// Result for IBC token VP
pub type Result<T> = std::result::Result<T, Error>;

/// IBC token native VP for IBC token transfer
pub struct IbcToken<'a, DB, H, CA>
where
    DB: ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    /// Context to interact with the host structures.
    pub ctx: Ctx<'a, DB, H, CA>,
}

impl<'a, DB, H, CA> NativeVp for IbcToken<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    type Error = Error;

    const ADDR: InternalAddress = InternalAddress::IbcBurn;

    fn validate_tx(
        &self,
        tx_data: &[u8],
        keys_changed: &BTreeSet<Key>,
        _verifiers: &BTreeSet<Address>,
    ) -> Result<bool> {
        let signed =
            SignedTxData::try_from_slice(tx_data).map_err(Error::Decoding)?;
        let tx_data = &signed.data.ok_or(Error::NoTxData)?;

        // Check the non-onwer balance updates
        let keys_changed: HashSet<Key> = keys_changed
            .iter()
            .filter(|k| is_non_owner_balance_key(k).is_some())
            .cloned()
            .collect();
        if keys_changed.len() != 1 {
            // a transaction can update at most 1 non-owner balance for now
            return Err(Error::TokenTransfer(
                "Invalid transfer for multiple non-owner balances".to_owned(),
            ));
        }

        // Check the message
        let ibc_msg = IbcMessage::decode(tx_data).map_err(Error::IbcMessage)?;
        match &ibc_msg.0 {
            Ics26Envelope::Ics20Msg(msg) => self.validate_sending_token(msg),
            Ics26Envelope::Ics4PacketMsg(PacketMsg::RecvPacket(msg)) => {
                self.validate_receiving_token(&msg.packet)
            }
            Ics26Envelope::Ics4PacketMsg(PacketMsg::AckPacket(msg)) => {
                self.validate_refunding_token(&msg.packet)
            }
            Ics26Envelope::Ics4PacketMsg(PacketMsg::ToPacket(msg)) => {
                self.validate_refunding_token(&msg.packet)
            }
            Ics26Envelope::Ics4PacketMsg(PacketMsg::ToClosePacket(msg)) => {
                self.validate_refunding_token(&msg.packet)
            }
            _ => Err(Error::InvalidMessage),
        }
    }
}

impl<'a, DB, H, CA> IbcToken<'a, DB, H, CA>
where
    DB: 'static + ledger_storage::DB + for<'iter> ledger_storage::DBIter<'iter>,
    H: 'static + StorageHasher,
    CA: 'static + WasmCacheAccess,
{
    fn validate_sending_token(&self, msg: &MsgTransfer) -> Result<bool> {
        let data = FungibleTokenPacketData::from(msg.clone());
        let token_str = data.denom.split('/').last().ok_or(Error::NoToken)?;
        let token = Address::decode(token_str).map_err(Error::Address)?;
        let amount = Amount::from_str(&data.amount).map_err(Error::Amount)?;

        // check the denom field
        let prefix = format!(
            "{}/{}/",
            msg.source_port.clone(),
            msg.source_channel.clone()
        );
        let change = if data.denom.starts_with(&prefix) {
            // sink zone
            let target = Address::Internal(InternalAddress::IbcBurn);
            let target_key = token::balance_key(&token, &target);
            let post =
                try_decode_token_amount(self.ctx.read_temp(&target_key)?)?
                    .unwrap_or_default();
            // the previous balance of the burn address should be zero
            post.change()
        } else {
            // source zone
            let target =
                Address::Internal(InternalAddress::ibc_escrow_address(
                    msg.source_port.to_string(),
                    msg.source_channel.to_string(),
                ));
            let target_key = token::balance_key(&token, &target);
            let pre = try_decode_token_amount(self.ctx.read_pre(&target_key)?)?
                .unwrap_or_default();
            let post =
                try_decode_token_amount(self.ctx.read_post(&target_key)?)?
                    .unwrap_or_default();
            post.change() - pre.change()
        };

        if change == amount.change() {
            Ok(true)
        } else {
            Err(Error::TokenTransfer(format!(
                "Sending the token is invalid: {}",
                data
            )))
        }
    }

    fn validate_receiving_token(&self, packet: &Packet) -> Result<bool> {
        let data: FungibleTokenPacketData =
            serde_json::from_slice(&packet.data)
                .map_err(Error::DecodingPacketData)?;
        let token_str = data.denom.split('/').last().ok_or(Error::NoToken)?;
        let token = Address::decode(token_str).map_err(Error::Address)?;
        let amount = Amount::from_str(&data.amount).map_err(Error::Amount)?;

        let prefix = format!(
            "{}/{}/",
            packet.source_port.clone(),
            packet.source_channel.clone()
        );
        let change = if data.denom.starts_with(&prefix) {
            // this chain is the source
            let source =
                Address::Internal(InternalAddress::ibc_escrow_address(
                    packet.destination_port.to_string(),
                    packet.destination_channel.to_string(),
                ));
            let source_key = token::balance_key(&token, &source);
            let pre = try_decode_token_amount(self.ctx.read_pre(&source_key)?)?
                .unwrap_or_default();
            let post =
                try_decode_token_amount(self.ctx.read_post(&source_key)?)?
                    .unwrap_or_default();
            pre.change() - post.change()
        } else {
            // the sender is the source
            let source = Address::Internal(InternalAddress::IbcMint);
            let source_key = token::balance_key(&token, &source);
            let post =
                try_decode_token_amount(self.ctx.read_temp(&source_key)?)?
                    .unwrap_or_default();
            // the previous balance of the mint address should be the maximum
            Amount::max().change() - post.change()
        };

        if change == amount.change() {
            Ok(true)
        } else {
            Err(Error::TokenTransfer(format!(
                "Receivinging the token is invalid: {}",
                data
            )))
        }
    }

    fn validate_refunding_token(&self, packet: &Packet) -> Result<bool> {
        let data: FungibleTokenPacketData =
            serde_json::from_slice(&packet.data)
                .map_err(Error::DecodingPacketData)?;
        let token_str = data.denom.split('/').last().ok_or(Error::NoToken)?;
        let token = Address::decode(token_str).map_err(Error::Address)?;
        let amount = Amount::from_str(&data.amount).map_err(Error::Amount)?;

        // check the denom field
        let prefix = format!(
            "{}/{}/",
            packet.source_port.clone(),
            packet.source_channel.clone()
        );
        let change = if data.denom.starts_with(&prefix) {
            // sink zone: mint the token for the refund
            let source = Address::Internal(InternalAddress::IbcMint);
            let source_key = token::balance_key(&token, &source);
            let post =
                try_decode_token_amount(self.ctx.read_temp(&source_key)?)?
                    .unwrap_or_default();
            // the previous balance of the mint address should be the maximum
            Amount::max().change() - post.change()
        } else {
            // source zone: unescrow the token for the refund
            let source =
                Address::Internal(InternalAddress::ibc_escrow_address(
                    packet.source_port.to_string(),
                    packet.source_channel.to_string(),
                ));
            let source_key = token::balance_key(&token, &source);
            let pre = try_decode_token_amount(self.ctx.read_pre(&source_key)?)?
                .unwrap_or_default();
            let post =
                try_decode_token_amount(self.ctx.read_post(&source_key)?)?
                    .unwrap_or_default();
            pre.change() - post.change()
        };

        if change == amount.change() {
            Ok(true)
        } else {
            Err(Error::TokenTransfer(format!(
                "Refunding the token is invalid: {}",
                data,
            )))
        }
    }
}

impl From<native_vp::Error> for Error {
    fn from(err: native_vp::Error) -> Self {
        Self::NativeVpError(err)
    }
}

fn try_decode_token_amount(
    bytes: Option<Vec<u8>>,
) -> Result<Option<token::Amount>> {
    if let Some(bytes) = bytes {
        let tokens = Amount::try_from_slice(&bytes).map_err(Error::Decoding)?;
        return Ok(Some(tokens));
    }
    Ok(None)
}
