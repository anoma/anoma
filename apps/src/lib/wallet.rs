//! Temporary helper until we have a proper wallet.

use anoma::types::key::ed25519::{Keypair, PublicKey};

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

pub fn alberto_keypair() -> Keypair {
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

pub fn alberto_pk() -> PublicKey {
    PublicKey::from(alberto_keypair().public)
}

pub fn bertha_pk() -> PublicKey {
    PublicKey::from(bertha_keypair().public)
}

pub fn christel_pk() -> PublicKey {
    PublicKey::from(christel_keypair().public)
}

pub fn matchmaker_pk() -> PublicKey {
    PublicKey::from(matchmaker_keypair().public)
}

pub fn key_of(name: impl AsRef<str>) -> Keypair {
    match name.as_ref() {
        "a1qq5qqqqqg4znssfsgcurjsfhgfpy2vjyxy6yg3z98pp5zvp5xgersvfjxvcnx3f4xycrzdfkak0xhx" => alberto_keypair(),
        "a1qq5qqqqqxv6yydz9xc6ry33589q5x33eggcnjs2xx9znydj9xuens3phxppnwvzpg4rrqdpswve4n9" => bertha_keypair(),
        "a1qq5qqqqqxsuygd2x8pq5yw2ygdryxs6xgsmrsdzx8pryxv34gfrrssfjgccyg3zpxezrqd2y2s3g5s" => christel_keypair(),
        "a1qq5qqqqqxu6rvdzpxymnqwfkxfznvsjxggunyd3jg5erg3p3geqnvv35gep5yvzxx5m5x3fsfje8td" => matchmaker_keypair(),
        "a1qq5qqqqqgfqnsd6pxse5zdj9g5crzsf5x4zyzv6yxerr2d2rxpryzwp5g5m5zvfjxv6ygsekjmraj0" => validator_keypair(),
        other => {
            panic!("Dont' have keys for: {}", other)
        }
    }
}
