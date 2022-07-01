// TODO: temporarily allow dead code in this file while we are still in
// eth-bridge-integration branch - the purpose is to share symbols in WIP
// branches
#![allow(dead_code)]
//! Types to do with interfacing with the Ethereum blockchain
use std::fmt::Debug;

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
use eyre::{eyre, Result};
use num_rational::Ratio;

use crate::proto::MultiSigned;
use crate::types::address::Address;
use crate::types::hash::Hash;
use crate::types::token::Amount;

/// Each event has a nonce set by the Ethereum smart contract that emitted it.
#[derive(
    Debug, PartialEq, Eq, Clone, BorshSerialize, BorshDeserialize, BorshSchema,
)]
pub struct Nonce(u64);

/// An Ethereum event to be processed by the Anoma ledger
#[derive(
    Debug, PartialEq, Eq, Clone, BorshSerialize, BorshDeserialize, BorshSchema,
)]
pub enum RawEvent {
    /// Event transferring batches of Ethereum assets from Ethereum to wrapped
    /// assets on Anoma
    TransfersToNamada(Vec<TransferToNamada>),
}

/// An Ethereum event emitted by an Ethereum bridge smart contract.
#[derive(
    Debug, PartialEq, Eq, Clone, BorshSerialize, BorshDeserialize, BorshSchema,
)]
pub struct EthereumEvent {
    /// The Ethereum event.
    pub event: RawEvent,
    /// All events must be emitted with a nonce so that otherwise identical
    /// events will be unique.
    pub nonce: Nonce,
}

impl EthereumEvent {
    /// SHA256 of the Borsh serialization of the [`EthereumEvent`].
    fn hash(&self) -> Result<Hash, std::io::Error> {
        let bytes = self.try_to_vec()?;
        Ok(Hash::sha256(&bytes))
    }
}

/// Representation of address on Ethereum
#[derive(
    Clone, PartialEq, Eq, Debug, BorshSerialize, BorshDeserialize, BorshSchema,
)]
pub struct EthAddress(pub [u8; 20]);

/// An event transferring some kind of value from Ethereum to Anoma
#[derive(
    Debug, PartialEq, Eq, Clone, BorshSerialize, BorshDeserialize, BorshSchema,
)]
pub struct TransferToNamada {
    /// Quantity of ether in the transfer
    pub amount: Amount,
    /// Address on Ethereum of the asset
    pub asset: EthereumAsset,
    /// The Namada address receiving wrapped assets on Anoma
    pub receiver: Address,
}

/// Represents Ethereum assets on the Ethereum blockchain
#[derive(
    Debug, PartialEq, Eq, Clone, BorshSerialize, BorshDeserialize, BorshSchema,
)]
pub enum EthereumAsset {
    /// An ERC20 token and the address of its contract
    ERC20(EthAddress),
}

/// A fraction of the total voting power. This should always be a reduced
/// fraction that is between zero and one inclusive.
#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct FractionalVotingPower(Ratio<u64>);

impl FractionalVotingPower {
    /// Create a new FractionalVotingPower. It must be between zero and one
    /// inclusive.
    pub fn new(numer: u64, denom: u64) -> Result<Self> {
        if denom == 0 {
            return Err(eyre!("denominator can't be zero"));
        }
        let ratio: Ratio<u64> = (numer, denom).into();
        if ratio > 1.into() {
            return Err(eyre!(
                "fractional voting power cannot be greater than one"
            ));
        }
        Ok(Self(ratio))
    }
}

impl From<&FractionalVotingPower> for (u64, u64) {
    fn from(ratio: &FractionalVotingPower) -> Self {
        (ratio.0.numer().to_owned(), ratio.0.denom().to_owned())
    }
}

impl BorshSerialize for FractionalVotingPower {
    fn serialize<W: ark_serialize::Write>(
        &self,
        writer: &mut W,
    ) -> std::io::Result<()> {
        let (numer, denom): (u64, u64) =
            TryFrom::<&FractionalVotingPower>::try_from(self).map_err(
                |err| {
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!(
                            "Could not serialize {:?} to Borsh: {:?}",
                            self, err
                        ),
                    )
                },
            )?;
        (numer, denom).serialize(writer)
    }
}

impl BorshDeserialize for FractionalVotingPower {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        let (numer, denom): (u64, u64) = BorshDeserialize::deserialize(buf)?;
        Ok(FractionalVotingPower(Ratio::<u64>::new(numer, denom)))
    }
}

impl BorshSchema for FractionalVotingPower {
    fn add_definitions_recursively(
        definitions: &mut std::collections::HashMap<
            borsh::schema::Declaration,
            borsh::schema::Definition,
        >,
    ) {
        let fields =
            borsh::schema::Fields::UnnamedFields(borsh::maybestd::vec![
                u64::declaration(),
                u64::declaration()
            ]);
        let definition = borsh::schema::Definition::Struct { fields };
        Self::add_definition(Self::declaration(), definition, definitions);
    }

    fn declaration() -> borsh::schema::Declaration {
        "FractionalVotingPower".into()
    }
}

/// This is created by the block proposer based on the Ethereum events included
/// in the vote extensions of the previous Tendermint round
#[derive(Debug, Clone, BorshSerialize, BorshDeserialize, BorshSchema)]
pub struct MultiSignedEthEvent {
    /// Address and voting power of the signing validators
    pub signers: Vec<(Address, FractionalVotingPower)>,
    /// Events as signed by validators
    pub event: MultiSigned<EthereumEvent>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ethereum_event_hash() {
        let nonce = Nonce(123);
        let event = RawEvent::TransfersToNamada(vec![]);
        let event = EthereumEvent { nonce, event };

        let hash = event.hash().unwrap();

        assert_eq!(
            hash,
            Hash([
                94, 227, 170, 45, 164, 208, 161, 180, 203, 148, 96, 173, 90,
                30, 102, 44, 30, 187, 124, 90, 117, 204, 19, 188, 7, 104, 19,
                46, 13, 62, 203, 243
            ])
        );
    }

    #[test]
    fn test_fractional_voting_power_ord_eq() {
        // though this test is ultimately just exercising the underlying library
        // we use for fractions, we want to make sure operators work as
        // expected with our FractionalVotingPower type itself
        assert!(
            FractionalVotingPower::new(2, 3).unwrap()
                > FractionalVotingPower::new(1, 4).unwrap()
        );
        assert!(
            FractionalVotingPower::new(1, 3).unwrap()
                > FractionalVotingPower::new(1, 4).unwrap()
        );
        assert!(
            FractionalVotingPower::new(1, 3).unwrap()
                == FractionalVotingPower::new(2, 6).unwrap()
        );
    }

    #[test]
    fn test_fractional_voting_power_valid_fractions() {
        assert!(FractionalVotingPower::new(0, 0).is_err());
        assert!(FractionalVotingPower::new(1, 0).is_err());
        assert!(FractionalVotingPower::new(0, 1).is_ok());
        assert!(FractionalVotingPower::new(1, 1).is_ok());
        assert!(FractionalVotingPower::new(1, 2).is_ok());
        assert!(FractionalVotingPower::new(3, 2).is_err());
    }
}
