//! Bridge from Ethereum

use crate::types::address::{Address, InternalAddress};

/// Internal address for the Ethereum bridge VP
pub const ADDRESS: Address = Address::Internal(InternalAddress::EthBridge);
