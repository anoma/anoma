//! Default addresses and keys.

use anoma::ledger::{governance, pos};
use anoma::types::address::Address;
use anoma::types::key::*;
#[cfg(feature = "dev")]
pub use dev::{
    addresses, albert_address, albert_keypair, bertha_address, bertha_keypair,
    christel_address, christel_keypair, daewon_address, daewon_keypair, keys,
    matchmaker_address, matchmaker_keypair, validator_address,
    validator_keypair, validator_keys,
};

use crate::config::genesis::genesis_config::GenesisConfig;
use crate::wallet::alias::Alias;

/// The default addresses with their aliases.
pub fn addresses_from_genesis(genesis: GenesisConfig) -> Vec<(Alias, Address)> {
    // Internal addresses
    let mut addresses: Vec<(Alias, Address)> = vec![
        ("pos".into(), pos::ADDRESS),
        ("pos_slash_pool".into(), pos::SLASH_POOL_ADDRESS),
        ("governance".into(), governance::vp::ADDRESS),
    ];
    // Genesis validators
    let validator_addresses =
        genesis.validator.into_iter().map(|(alias, validator)| {
            // The address must be set in the genesis config file
            (
                alias.into(),
                Address::decode(validator.address.unwrap()).unwrap(),
            )
        });
    addresses.extend(validator_addresses);
    // Genesis tokens
    if let Some(accounts) = genesis.token {
        let token_addresses = accounts.into_iter().map(|(alias, token)| {
            // The address must be set in the genesis config file
            (
                alias.into(),
                Address::decode(token.address.unwrap()).unwrap(),
            )
        });
        addresses.extend(token_addresses);
    }
    // Genesis established accounts
    if let Some(accounts) = genesis.established {
        let est_addresses = accounts.into_iter().map(|(alias, established)| {
            // The address must be set in the genesis config file
            (
                alias.into(),
                Address::decode(established.address.unwrap()).unwrap(),
            )
        });
        addresses.extend(est_addresses);
    }
    // Genesis implicit accounts
    if let Some(accounts) = genesis.implicit {
        let imp_addresses =
            accounts.into_iter().filter_map(|(alias, implicit)| {
                // The public key may not be revealed, only add it if it is
                implicit.public_key.map(|pk| {
                    let pk: common::PublicKey = pk.to_public_key().unwrap();
                    let addr: Address = (&pk).into();
                    (alias.into(), addr)
                })
            });
        addresses.extend(imp_addresses);
    }
    addresses
}

#[cfg(feature = "dev")]
mod dev {
    use anoma::ledger::{governance, pos};
    use anoma::types::address::{self, Address};
    use anoma::types::key::dkg_session_keys::DkgKeypair;
    use anoma::types::key::*;
    use borsh::BorshDeserialize;

    use crate::wallet::alias::Alias;

    /// Generate a new protocol signing keypair and DKG session keypair
    pub fn validator_keys() -> (common::SecretKey, DkgKeypair) {
        let bytes: [u8; 33] = [
            0, 200, 107, 23, 252, 78, 80, 8, 164, 142, 3, 194, 33, 12, 250,
            169, 211, 127, 47, 13, 194, 54, 199, 81, 102, 246, 189, 119, 144,
            25, 27, 113, 222,
        ];
        let dkg_bytes = [
            32, 0, 0, 0, 210, 193, 55, 24, 92, 233, 23, 2, 73, 204, 221, 107,
            110, 222, 192, 136, 54, 24, 108, 236, 137, 27, 121, 142, 142, 7,
            193, 248, 155, 56, 51, 21,
        ];

        (
            BorshDeserialize::deserialize(&mut bytes.as_ref()).unwrap(),
            BorshDeserialize::deserialize(&mut dkg_bytes.as_ref()).unwrap(),
        )
    }

    /// The default keys with their aliases.
    pub fn keys() -> Vec<(Alias, common::SecretKey)> {
        vec![
            ("albert".into(), albert_keypair()),
            ("bertha".into(), bertha_keypair()),
            ("christel".into(), christel_keypair()),
            ("daewon".into(), daewon_keypair()),
            ("matchmaker".into(), matchmaker_keypair()),
            ("validator".into(), validator_keypair()),
        ]
    }

    /// The default addresses with their aliases.
    pub fn addresses() -> Vec<(Alias, Address)> {
        let mut addresses: Vec<(Alias, Address)> = vec![
            ("pos".into(), pos::ADDRESS),
            ("pos_slash_pool".into(), pos::SLASH_POOL_ADDRESS),
            ("governance".into(), governance::vp::ADDRESS),
            ("matchmaker".into(), matchmaker_address()),
            ("validator".into(), validator_address()),
            ("albert".into(), albert_address()),
            ("bertha".into(), bertha_address()),
            ("christel".into(), christel_address()),
            ("daewon".into(), daewon_address()),
        ];
        let token_addresses = address::tokens()
            .into_iter()
            .map(|(addr, alias)| (alias.into(), addr));
        addresses.extend(token_addresses);
        addresses
    }

    /// An established user address for testing & development
    pub fn albert_address() -> Address {
        Address::decode("atest1v4ehgw368ycryv2z8qcnxv3cxgmrgvjpxs6yg333gym5vv2zxepnj334g4rryvj9xucrgve4x3xvr4").expect("The token address decoding shouldn't fail")
    }

    /// An established user address for testing & development
    pub fn bertha_address() -> Address {
        Address::decode("atest1v4ehgw36xvcyyvejgvenxs34g3zygv3jxqunjd6rxyeyys3sxy6rwvfkx4qnj33hg9qnvse4lsfctw").expect("The token address decoding shouldn't fail")
    }

    /// An established user address for testing & development
    pub fn christel_address() -> Address {
        Address::decode("atest1v4ehgw36x3qng3jzggu5yvpsxgcngv2xgguy2dpkgvu5x33kx3pr2w2zgep5xwfkxscrxs2pj8075p").expect("The token address decoding shouldn't fail")
    }

    /// An implicit user address for testing & development
    pub fn daewon_address() -> Address {
        // "atest1d9khqw36xprrzdpk89rrws69g4z5vd6pgv65gvjrgeqnv3pcg4zns335xymry335gcerqs3etd0xfa"
        (&daewon_keypair().ref_to()).into()
    }

    /// An established validator address for testing & development
    pub fn validator_address() -> Address {
        Address::decode("atest1v4ehgw36ggcnsdee8qerswph8y6ry3p5xgunvve3xaqngd3kxc6nqwz9gseyydzzg5unys3ht2n48q").expect("The token address decoding shouldn't fail")
    }

    /// An established matchmaker address for testing & development
    pub fn matchmaker_address() -> Address {
        Address::decode("atest1v4ehgw36x5mnswphx565gv2yxdprzvf5gdp523jpxy6rvv6zxaznzsejxeznzseh8pp5ywz93xwala").expect("The address decoding shouldn't fail")
    }

    pub fn albert_keypair() -> common::SecretKey {
        // generated from
        // [`anoma::types::key::ed25519::gen_keypair`]
        let bytes = [
            115, 191, 32, 247, 18, 101, 5, 106, 26, 203, 48, 145, 39, 41, 41,
            196, 252, 190, 245, 222, 96, 209, 34, 36, 40, 214, 169, 156, 235,
            78, 188, 33,
        ];
        let ed_sk = ed25519::SecretKey::try_from_slice(&bytes).unwrap();
        ed_sk.try_to_sk().unwrap()
    }

    pub fn bertha_keypair() -> common::SecretKey {
        // generated from
        // [`anoma::types::key::ed25519::gen_keypair`]
        let bytes = [
            240, 3, 224, 69, 201, 148, 60, 53, 112, 79, 80, 107, 101, 127, 186,
            6, 176, 162, 113, 224, 62, 8, 183, 187, 124, 234, 244, 251, 92, 36,
            119, 243,
        ];
        let ed_sk = ed25519::SecretKey::try_from_slice(&bytes).unwrap();
        ed_sk.try_to_sk().unwrap()
    }

    pub fn christel_keypair() -> common::SecretKey {
        // generated from
        // [`anoma::types::key::ed25519::gen_keypair`]
        let bytes = [
            65, 198, 96, 145, 237, 227, 84, 182, 107, 55, 209, 235, 115, 105,
            71, 190, 234, 137, 176, 188, 181, 174, 183, 49, 131, 230, 46, 39,
            70, 20, 130, 253,
        ];
        let ed_sk = ed25519::SecretKey::try_from_slice(&bytes).unwrap();
        ed_sk.try_to_sk().unwrap()
    }

    pub fn daewon_keypair() -> common::SecretKey {
        // generated from
        // [`anoma::types::key::ed25519::gen_keypair`]
        let bytes = [
            235, 250, 15, 1, 145, 250, 172, 218, 247, 27, 63, 212, 60, 47, 164,
            57, 187, 156, 182, 144, 107, 174, 38, 81, 37, 40, 19, 142, 68, 135,
            57, 50,
        ];
        let ed_sk = ed25519::SecretKey::try_from_slice(&bytes).unwrap();
        ed_sk.try_to_sk().unwrap()
    }

    pub fn validator_keypair() -> common::SecretKey {
        // generated from
        // [`anoma::types::key::ed25519::gen_keypair`]
        let bytes = [
            80, 110, 166, 33, 135, 254, 34, 138, 253, 44, 214, 71, 50, 230, 39,
            246, 124, 201, 68, 138, 194, 251, 192, 36, 55, 160, 211, 68, 65,
            189, 121, 217,
        ];
        let ed_sk = ed25519::SecretKey::try_from_slice(&bytes).unwrap();
        ed_sk.try_to_sk().unwrap()
    }

    pub fn matchmaker_keypair() -> common::SecretKey {
        // generated from
        // [`anoma::types::key::ed25519::gen_keypair`]
        let bytes = [
            91, 67, 244, 37, 241, 33, 157, 218, 37, 172, 191, 122, 75, 2, 44,
            219, 28, 123, 44, 34, 9, 240, 244, 49, 112, 192, 180, 98, 142, 160,
            182, 14,
        ];
        let ed_sk = ed25519::SecretKey::try_from_slice(&bytes).unwrap();
        ed_sk.try_to_sk().unwrap()
    }
}
