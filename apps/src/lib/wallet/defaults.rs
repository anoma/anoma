//! Default addresses and keys.

use anoma::ledger::pos;
use anoma::types::address::{self, Address};
use anoma::types::key::ed25519::Keypair;

use super::store::Alias;

/// The default keys with their aliases.
#[cfg(feature = "dev")]
pub fn keys() -> Vec<(Alias, Keypair)> {
    vec![
        ("Albert".into(), albert_keypair()),
        ("Bertha".into(), bertha_keypair()),
        ("Christel".into(), christel_keypair()),
        ("Daewon".into(), daewon_keypair()),
        ("matchmaker".into(), matchmaker_keypair()),
        ("validator".into(), validator_keypair()),
    ]
}
#[cfg(not(feature = "dev"))]
pub fn keys() -> Vec<(Alias, Keypair)> {
    vec![]
}

/// The default addresses with their aliases.
#[cfg(feature = "dev")]
pub fn addresses() -> Vec<(Alias, Address)> {
    let mut addresses: Vec<(Alias, Address)> = vec![
        ("matchmaker".into(), matchmaker_address()),
        ("validator".into(), validator_address()),
        ("PoS".into(), pos::ADDRESS),
        ("PosSlashPool".into(), pos::SLASH_POOL_ADDRESS),
    ];
    #[cfg(feature = "dev")]
    {
        addresses.extend([
            ("Albert".into(), albert_address()),
            ("Bertha".into(), bertha_address()),
            ("Christel".into(), christel_address()),
            ("Daewon".into(), daewon_address()),
        ]);
    }
    let token_addresses = address::tokens()
        .into_iter()
        .map(|(addr, alias)| (alias.to_owned(), addr));
    addresses.extend(token_addresses);
    addresses
}
#[cfg(not(feature = "dev"))]
pub fn addresses() -> Vec<(Alias, Address)> {
    let mut addresses: Vec<(Alias, Address)> = vec![
        ("PoS".into(), pos::ADDRESS),
        ("PosSlashPool".into(), pos::SLASH_POOL_ADDRESS),
    ];
    let token_addresses = address::tokens()
        .into_iter()
        .map(|(addr, alias)| (alias.to_owned(), addr));
    addresses.extend(token_addresses);
    addresses
}

/// An established user address for testing & development
#[cfg(feature = "dev")]
pub fn albert_address() -> Address {
    Address::decode("atest1v4ehgw368ycryv2z8qcnxv3cxgmrgvjpxs6yg333gym5vv2zxepnj334g4rryvj9xucrgve4x3xvr4").expect("The token address decoding shouldn't fail")
}

/// An established user address for testing & development
#[cfg(feature = "dev")]
pub fn bertha_address() -> Address {
    Address::decode("atest1v4ehgw36xvcyyvejgvenxs34g3zygv3jxqunjd6rxyeyys3sxy6rwvfkx4qnj33hg9qnvse4lsfctw").expect("The token address decoding shouldn't fail")
}

/// An established user address for testing & development
#[cfg(feature = "dev")]
pub fn christel_address() -> Address {
    Address::decode("atest1v4ehgw36x3qng3jzggu5yvpsxgcngv2xgguy2dpkgvu5x33kx3pr2w2zgep5xwfkxscrxs2pj8075p").expect("The token address decoding shouldn't fail")
}

/// An implicit user address for testing & development
#[cfg(feature = "dev")]
pub fn daewon_address() -> Address {
    // "atest1d9khqw36xprrzdpk89rrws69g4z5vd6pgv65gvjrgeqnv3pcg4zns335xymry335gcerqs3etd0xfa"
    (&daewon_keypair().public).into()
}

/// An established validator address for testing & development
#[cfg(feature = "dev")]
pub fn validator_address() -> Address {
    Address::decode("atest1v4ehgw36ggcnsdee8qerswph8y6ry3p5xgunvve3xaqngd3kxc6nqwz9gseyydzzg5unys3ht2n48q").expect("The token address decoding shouldn't fail")
}

/// An established matchmaker address for testing & development
#[cfg(feature = "dev")]
pub fn matchmaker_address() -> Address {
    Address::decode("atest1v4ehgw36x5mnswphx565gv2yxdprzvf5gdp523jpxy6rvv6zxaznzsejxeznzseh8pp5ywz93xwala").expect("The address decoding shouldn't fail")
}

#[cfg(feature = "dev")]
pub fn albert_keypair() -> Keypair {
    // generated from
    // [`anoma::types::key::ed25519::gen_keypair`]
    let bytes = [
        115, 191, 32, 247, 18, 101, 5, 106, 26, 203, 48, 145, 39, 41, 41, 196,
        252, 190, 245, 222, 96, 209, 34, 36, 40, 214, 169, 156, 235, 78, 188,
        33, 165, 114, 129, 225, 221, 159, 211, 158, 195, 232, 161, 98, 161,
        100, 60, 167, 200, 54, 192, 242, 218, 227, 190, 241, 65, 42, 58, 97,
        162, 253, 225, 167,
    ];
    Keypair::from_bytes(&bytes).unwrap()
}

#[cfg(feature = "dev")]
pub fn bertha_keypair() -> Keypair {
    // generated from
    // [`anoma::types::key::ed25519::gen_keypair`]
    let bytes = [
        240, 3, 224, 69, 201, 148, 60, 53, 112, 79, 80, 107, 101, 127, 186, 6,
        176, 162, 113, 224, 62, 8, 183, 187, 124, 234, 244, 251, 92, 36, 119,
        243, 87, 37, 18, 169, 91, 25, 13, 97, 91, 25, 135, 247, 7, 37, 114,
        166, 73, 81, 173, 80, 244, 249, 126, 249, 219, 184, 53, 69, 196, 106,
        230, 0,
    ];
    Keypair::from_bytes(&bytes).unwrap()
}

#[cfg(feature = "dev")]
pub fn christel_keypair() -> Keypair {
    // generated from
    // [`anoma::types::key::ed25519::gen_keypair`]
    let bytes = [
        65, 198, 96, 145, 237, 227, 84, 182, 107, 55, 209, 235, 115, 105, 71,
        190, 234, 137, 176, 188, 181, 174, 183, 49, 131, 230, 46, 39, 70, 20,
        130, 253, 208, 111, 141, 79, 137, 127, 50, 154, 80, 253, 35, 186, 93,
        37, 3, 187, 226, 47, 171, 47, 20, 213, 246, 37, 224, 122, 101, 246, 23,
        235, 39, 120,
    ];
    Keypair::from_bytes(&bytes).unwrap()
}

#[cfg(feature = "dev")]
pub fn daewon_keypair() -> Keypair {
    // generated from
    // [`anoma::types::key::ed25519::gen_keypair`]
    let bytes = [
        235, 250, 15, 1, 145, 250, 172, 218, 247, 27, 63, 212, 60, 47, 164, 57,
        187, 156, 182, 144, 107, 174, 38, 81, 37, 40, 19, 142, 68, 135, 57, 50,
        43, 91, 143, 218, 102, 251, 111, 196, 239, 13, 134, 248, 75, 33, 242,
        80, 3, 64, 119, 239, 252, 69, 159, 194, 64, 58, 119, 163, 90, 169, 94,
        63,
    ];
    Keypair::from_bytes(&bytes).unwrap()
}

#[cfg(feature = "dev")]
pub fn validator_keypair() -> Keypair {
    // generated from
    // [`anoma::types::key::ed25519::gen_keypair`]
    let bytes = [
        80, 110, 166, 33, 135, 254, 34, 138, 253, 44, 214, 71, 50, 230, 39,
        246, 124, 201, 68, 138, 194, 251, 192, 36, 55, 160, 211, 68, 65, 189,
        121, 217, 94, 112, 76, 78, 70, 38, 94, 28, 204, 135, 80, 81, 73, 247,
        155, 157, 46, 65, 77, 1, 164, 227, 128, 109, 252, 101, 240, 167, 57, 1,
        193, 208,
    ];
    Keypair::from_bytes(&bytes).unwrap()
}

#[cfg(feature = "dev")]
pub fn matchmaker_keypair() -> Keypair {
    // generated from
    // [`anoma::types::key::ed25519::gen_keypair`]
    let bytes = [
        91, 67, 244, 37, 241, 33, 157, 218, 37, 172, 191, 122, 75, 2, 44, 219,
        28, 123, 44, 34, 9, 240, 244, 49, 112, 192, 180, 98, 142, 160, 182, 14,
        244, 254, 3, 176, 211, 19, 15, 7, 126, 77, 81, 204, 119, 72, 186, 172,
        153, 135, 80, 71, 107, 239, 153, 74, 10, 115, 172, 78, 125, 24, 49,
        104,
    ];
    Keypair::from_bytes(&bytes).unwrap()
}
