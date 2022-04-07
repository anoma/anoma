//! Types needed to work with Ethereum headers
//! Includes tooling for transforming external types
//! to Anoma native types and back.

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
use thiserror::Error;

use crate::proto::{MultiSigned, Signed};
use crate::types::address::Address;
use crate::types::hash::Hash;
use crate::types::key::*;
use crate::types::transaction::hash_tx;

/// Tooling for converting from `ethereum_type` crate
/// types to Anoma native types. Unfortunately not
/// wasm compatible, thus requiring a feature flag.
#[cfg(feature = "ethereum-headers")]
pub mod eth_header_types {
    use std::convert::TryFrom;

    use ethereum_types::{H256, H64, U256};
    use web3::types::BlockHeader;

    use super::*;

    impl From<U256> for Difficulty {
        fn from(difficulty: U256) -> Self {
            Self(difficulty.0)
        }
    }

    impl From<web3::types::U256> for Difficulty {
        fn from(difficulty: web3::types::U256) -> Self {
            Self(difficulty.0)
        }
    }

    impl From<u64> for Difficulty {
        fn from(difficulty: u64) -> Self {
            Self::from(U256::from(difficulty))
        }
    }

    impl From<Difficulty> for U256 {
        fn from(difficulty: Difficulty) -> Self {
            Self(difficulty.0)
        }
    }

    impl Default for Nonce {
        fn default() -> Self {
            Self::from(H64::zero())
        }
    }

    impl From<H64> for Nonce {
        fn from(nonce: H64) -> Self {
            Nonce(nonce.0)
        }
    }

    impl From<Nonce> for H64 {
        fn from(nonce: Nonce) -> Self {
            Self(nonce.0)
        }
    }

    impl From<web3::types::H64> for Nonce {
        fn from(nonce: web3::types::H64) -> Self {
            Nonce(nonce.0)
        }
    }

    impl From<Hash> for H256 {
        fn from(hash: Hash) -> Self {
            H256(hash.0)
        }
    }

    impl From<H256> for Hash {
        fn from(hash: H256) -> Self {
            Hash(hash.0)
        }
    }

    impl From<web3::types::H256> for Hash {
        fn from(hash: web3::types::H256) -> Self {
            Hash(hash.0)
        }
    }

    impl TryFrom<BlockHeader> for EthereumHeader {
        type Error = Error;

        fn try_from(header: BlockHeader) -> Result<Self> {
            Ok(EthereumHeader {
                hash: header
                    .hash
                    .map(Hash::from)
                    .ok_or(Error::InvalidHeader)?,
                parent_hash: Hash(header.parent_hash.0),
                nonce: header
                    .nonce
                    .map(Nonce::from)
                    .ok_or(Error::InvalidHeader)?,
                mix_hash: header
                    .mix_hash
                    .map(Hash::from)
                    .ok_or(Error::InvalidHeader)?,
                number: header
                    .number
                    .map(|num| num.as_u64())
                    .ok_or(Error::InvalidHeader)?,
                difficulty: header.difficulty.into(),
                state_root: header.state_root.into(),
                transactions_root: header.transactions_root.into(),
            })
        }
    }
}

use std::hash::Hasher;

#[cfg(feature = "ethereum-headers")]
pub use eth_header_types::*;

use crate::tendermint::consensus::state::Ordering;

/// Errors in transforming types related to Ethereum headers
#[derive(Error, Debug)]
pub enum Error {
    /// Error for an invalid Ethereum header.
    #[error("Encountered an invalid Ethereum header")]
    InvalidHeader,
    /// Error when trying to combine different headers into a
    /// multi-signed header struct
    #[error("Could not combine Signed headers due to incompatibility")]
    IncompatibleHeaders,
}

type Result<T> = std::result::Result<T, Error>;

/// The difficulty of an Ethereum block
#[derive(
    Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema, PartialEq,
)]
pub struct Difficulty([u64; 4]);

/// The nonce found by mining an Ethereum block
#[derive(
    Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema, PartialEq,
)]
pub struct Nonce([u8; 8]);

#[derive(
    Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema, PartialEq,
)]
/// Pared down information from an Ethereum block header.
/// Should only represent headers of mined blocks.
pub struct EthereumHeader {
    /// Hash of the block
    pub hash: Hash,
    /// Hash of the parent
    pub parent_hash: Hash,
    /// Block number
    pub number: u64,
    /// Difficulty of the block
    pub difficulty: Difficulty,
    /// nonce
    pub nonce: Nonce,
    /// mix hash
    pub mix_hash: Hash,
    /// State root hash
    pub state_root: Hash,
    /// Transactions root hash
    pub transactions_root: Hash,
}

impl EthereumHeader {
    /// Create a signature for the Ethereum header and return
    /// the signed data.
    pub fn sign(
        self,
        voting_power: u64,
        address: Address,
        height: u64,
        signing_key: &common::SecretKey,
    ) -> SignedEthereumHeader {
        SignedEthereumHeader {
            voting_power,
            address,
            signed_header: Signed::new(signing_key, (self, height)),
        }
    }
}

#[derive(Clone, Debug, BorshSerialize, BorshDeserialize)]
/// Struct that represents the voting power
/// of a validator at given block height
pub struct EpochPower {
    /// Address of a validator
    pub validator: Address,
    /// voting power of validator at block `block_height`
    pub voting_power: u64,
    /// The height of the block at which the validator has this voting power
    pub block_height: u64,
}

impl PartialOrd for EpochPower {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.validator.partial_cmp(&other.validator)
    }
}

impl PartialEq for EpochPower {
    fn eq(&self, other: &Self) -> bool {
        self.validator.eq(&other.validator)
    }
}

impl Eq for EpochPower {}

impl core::hash::Hash for EpochPower {
    fn hash<H: Hasher>(&self, state: &mut H) {
        <str as core::hash::Hash>::hash(self.validator.encode().as_str(), state)
    }
}

/// A uniform interface for signed and multi-signed ethereum headers
pub trait SignedHeader {
    /// Get a reference to the signed header
    fn get_header(&self) -> &EthereumHeader;
    /// Get the voting power of validators at the given block
    /// height who have signed this header.
    fn get_voting_powers(&self) -> Vec<EpochPower>;
    /// Get the address of the validators who have signed this data
    fn get_addresses(&self) -> Vec<&Address>;
    /// Get the height of the block that this data was created
    /// as part of a vote extension
    fn get_height(&self) -> u64;
    /// Check that validity of the signatures
    fn verify_signatures(
        &self,
        public_keys: &[common::PublicKey],
    ) -> std::result::Result<(), VerifySigError>;
    /// Get the hash of the inner signed Ethereum header
    fn hash(&self) -> Hash;
}

#[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
/// A verifiable signed instance of the EthereumHeader.
pub struct SignedEthereumHeader {
    /// Voting power of the signing validator
    pub voting_power: u64,
    /// Address of the signing validator
    pub address: Address,
    /// A signed Ethereum header and the block height
    /// that this header appeared in a vote extension.
    /// This guards against replays.
    pub signed_header: Signed<(EthereumHeader, u64)>,
}

impl SignedHeader for SignedEthereumHeader {
    fn get_header(&self) -> &EthereumHeader {
        let Signed {
            data: (header, _), ..
        } = &self.signed_header;
        header
    }

    fn get_voting_powers(&self) -> Vec<EpochPower> {
        vec![EpochPower {
            validator: self.address.clone(),
            voting_power: self.voting_power,
            block_height: self.signed_header.data.1,
        }]
    }

    fn get_addresses(&self) -> Vec<&Address> {
        vec![&self.address]
    }

    fn get_height(&self) -> u64 {
        let Signed {
            data: (_, height), ..
        } = self.signed_header;
        height
    }

    fn verify_signatures(
        &self,
        public_keys: &[common::PublicKey],
    ) -> std::result::Result<(), VerifySigError> {
        self.signed_header.verify(&public_keys[0])
    }

    fn hash(&self) -> Hash {
        let Signed { data, .. } = &self.signed_header;
        hash_tx(&data.try_to_vec().unwrap())
    }
}

#[derive(Clone, Debug, BorshSerialize, BorshDeserialize, BorshSchema)]
/// An EthereumHeader with multiple validator signatures.
pub struct MultiSignedEthHeader {
    /// Address and voting power of the signing validator
    pub signers: Vec<(Address, u64)>,
    /// A multi-signed Ethereum header and the block height
    /// that these headers appeared in a vote extension.
    /// This guards against replays.
    pub signed_header: MultiSigned<(EthereumHeader, u64)>,
}

impl From<SignedEthereumHeader> for MultiSignedEthHeader {
    fn from(
        SignedEthereumHeader {
            voting_power,
            address,
            signed_header,
        }: SignedEthereumHeader,
    ) -> Self {
        Self {
            signers: vec![(address, voting_power)],
            signed_header: signed_header.into(),
        }
    }
}

impl MultiSignedEthHeader {
    /// Add a new signature for the same (block header, block height)
    /// to this instance.
    pub fn add(&mut self, other: SignedEthereumHeader) -> Result<()> {
        if self.hash() == other.hash()
            && self.get_height() == other.get_height()
        {
            self.signers.push((other.address, other.voting_power));
            self.signed_header.sigs.push(other.signed_header.sig);
            Ok(())
        } else {
            Err(Error::IncompatibleHeaders)
        }
    }
}

impl SignedHeader for MultiSignedEthHeader {
    fn get_header(&self) -> &EthereumHeader {
        &self.signed_header.data.0
    }

    fn get_voting_powers(&self) -> Vec<EpochPower> {
        let height = self.signed_header.data.1;
        self.signers
            .iter()
            .map(|(addr, power)| EpochPower {
                validator: addr.clone(),
                voting_power: *power,
                block_height: height,
            })
            .collect()
    }

    fn get_addresses(&self) -> Vec<&Address> {
        self.signers.iter().map(|(addr, _)| addr).collect()
    }

    fn get_height(&self) -> u64 {
        let MultiSigned {
            data: (_, height), ..
        } = self.signed_header;
        height
    }

    fn verify_signatures(
        &self,
        public_keys: &[common::PublicKey],
    ) -> std::result::Result<(), VerifySigError> {
        self.signed_header.verify(public_keys)
    }

    fn hash(&self) -> Hash {
        let MultiSigned { data, .. } = &self.signed_header;
        hash_tx(&data.try_to_vec().unwrap())
    }
}

#[cfg(test)]
mod test_eth_headers {
    use super::*;
    use crate::types::address::EstablishedAddressGen;

    /// This is not a meaningful default. It is
    /// just for testing
    impl Default for EthereumHeader {
        fn default() -> Self {
            EthereumHeader {
                hash: Hash([0; 32]),
                parent_hash: Hash([0; 32]),
                number: 0u64,
                difficulty: 0.into(),
                mix_hash: Hash([0; 32]),
                nonce: Default::default(),
                state_root: Hash([0; 32]),
                transactions_root: Hash([0; 32]),
            }
        }
    }

    /// Generate a random public/private keypair
    fn gen_keypair() -> common::SecretKey {
        use rand::prelude::ThreadRng;
        use rand::thread_rng;

        let mut rng: ThreadRng = thread_rng();
        ed25519::SigScheme::generate(&mut rng).try_to_sk().unwrap()
    }

    /// Generate a random address
    fn gen_address() -> Address {
        let mut gen = EstablishedAddressGen::new("Random McRandomFace");
        let random_bytes =
            (0..32).map(|_| rand::random::<u8>()).collect::<Vec<u8>>();
        gen.generate_address(&mut random_bytes.as_slice())
    }

    /// Test that we can add signatures to a multi-signed
    /// Ethereum header
    #[test]
    fn test_add_for_multi_signed_header() {
        let address_1 = gen_address();
        let address_2 = gen_address();
        let signing_key_1 = gen_keypair();
        let signing_key_2 = gen_keypair();
        let voting_power_1 = 100;
        let voting_power_2 = 200;

        let mut header_1: MultiSignedEthHeader = EthereumHeader::default()
            .sign(voting_power_1, address_1.clone(), 2, &signing_key_1)
            .into();

        let header_2 = EthereumHeader::default().sign(
            voting_power_2,
            address_2.clone(),
            2,
            &signing_key_2,
        );

        header_1.add(header_2).expect("Test failed");
        assert_eq!(
            header_1.signers,
            vec![
                (address_1.clone(), voting_power_1),
                (address_2.clone(), voting_power_2)
            ]
        );
        assert_eq!(header_1.signed_header.data, (EthereumHeader::default(), 2));
        assert!(
            header_1
                .verify_signatures(&[
                    signing_key_1.ref_to(),
                    signing_key_2.ref_to()
                ])
                .is_ok()
        );

        let expected = vec![
            EpochPower {
                validator: address_1.clone(),
                voting_power: voting_power_1,
                block_height: 2,
            },
            EpochPower {
                validator: address_2.clone(),
                voting_power: voting_power_2,
                block_height: 2,
            },
        ];

        assert_eq!(header_1.get_voting_powers(), expected);
        assert_eq!(header_1.get_addresses(), vec![&address_1, &address_2]);
        assert_eq!(header_1.get_height(), 2);
        assert_eq!(header_1.get_header(), &EthereumHeader::default());
    }

    /// Test that signature verification requires
    /// signatures to be given in correct order
    #[test]
    fn test_multisigned_sig_order_matters() {
        let address_1 = gen_address();
        let address_2 = gen_address();
        let signing_key_1 = gen_keypair();
        let signing_key_2 = gen_keypair();
        let voting_power_1 = 100;
        let voting_power_2 = 200;

        let mut header_1: MultiSignedEthHeader = EthereumHeader::default()
            .sign(voting_power_1, address_1, 2, &signing_key_1)
            .into();

        let header_2 = EthereumHeader::default().sign(
            voting_power_2,
            address_2,
            2,
            &signing_key_2,
        );

        header_1.add(header_2).expect("Test failed");
        assert!(
            header_1
                .verify_signatures(&[
                    signing_key_2.ref_to(),
                    signing_key_1.ref_to()
                ])
                .is_err()
        );
    }

    /// Test that two distinct ethereum headers cannot be
    /// added in a `MultiSignedEthHeader`
    #[test]
    fn test_cant_add_different_headers() {
        let address_1 = gen_address();
        let address_2 = gen_address();
        let signing_key_1 = gen_keypair();
        let signing_key_2 = gen_keypair();
        let voting_power_1 = 100;
        let voting_power_2 = 200;

        let mut header_1: MultiSignedEthHeader = EthereumHeader::default()
            .sign(voting_power_1, address_1, 2, &signing_key_1)
            .into();

        let header_2 = EthereumHeader {
            difficulty: 1.into(),
            ..Default::default()
        };
        let header_2 =
            header_2.sign(voting_power_2, address_2, 2, &signing_key_2);

        assert_ne!(header_1.hash(), header_2.hash());
        assert!(header_1.add(header_2).is_err());
    }

    /// Test that the same ethereum header cannot be
    /// added in a `MultiSignedEthHeader` if the heights
    /// differ
    #[test]
    fn test_cant_add_different_heights() {
        let address_1 = gen_address();
        let address_2 = gen_address();
        let signing_key_1 = gen_keypair();
        let signing_key_2 = gen_keypair();
        let voting_power_1 = 100;
        let voting_power_2 = 200;

        let mut header_1: MultiSignedEthHeader = EthereumHeader::default()
            .sign(voting_power_1, address_1, 2, &signing_key_1)
            .into();

        let header_2 = EthereumHeader::default().sign(
            voting_power_2,
            address_2,
            3,
            &signing_key_2,
        );

        assert_ne!(header_1.hash(), header_2.hash());
        assert!(header_1.add(header_2).is_err());
    }

    /// Test that two instance of the `EpochPower` struct
    /// with the same `validator` field hash to the same
    /// value
    #[test]
    fn test_epoch_power_hash() {
        use std::collections::HashSet;
        let mut powers = HashSet::new();
        let address = gen_address();
        let power_1 = EpochPower {
            validator: address.clone(),
            voting_power: 100,
            block_height: 2,
        };
        powers.insert(power_1.clone());
        let power_2 = EpochPower {
            validator: address,
            voting_power: 200,
            block_height: 11,
        };
        assert!(!powers.insert(power_2));
        assert_eq!(powers, HashSet::from([power_1]));
    }
}
