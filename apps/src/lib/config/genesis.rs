//! The parameters used for the chain's genesis

use anoma::ledger::parameters::{EpochDuration, Parameters};
#[cfg(feature = "dev")]
use ed25519_dalek::Keypair;
#[cfg(not(feature = "dev"))]
use ed25519_dalek::PublicKey;
use rand::prelude::ThreadRng;
use rand::thread_rng;
use sha2::{Digest, Sha256};

#[derive(Debug)]
pub struct Genesis {
    #[cfg(not(feature = "dev"))]
    pub validators: Vec<Validator>,
    #[cfg(feature = "dev")]
    pub validator: Validator,
    pub parameters: Parameters,
}

#[cfg(not(feature = "dev"))]
#[derive(Debug)]
pub struct Validator {
    pub address: String,
    pub pk: PublicKey,
    pub voting_power: u64,
}

#[cfg(feature = "dev")]
#[derive(Debug)]
pub struct Validator {
    pub address: String,
    pub keypair: Keypair,
    pub voting_power: u64,
}

#[cfg(feature = "dev")]
pub fn genesis() -> Genesis {
    // NOTE When the validator's key changes, tendermint must be reset with
    // `anoma reset` command. To get fresh key bytes, generate a new
    // validator and print its keypair with:
    // ```
    // let validator = Validator::new();
    // println!(
    //     "keypair {:?}, address {}",
    //     validator.keypair.to_bytes(),
    //     validator.address
    // );
    // ```
    let keypair = Keypair::from_bytes(&[
        // SecretKey bytes
        80, 110, 166, 33, 135, 254, 34, 138, 253, 44, 214, 71, 50, 230, 39, 246,
        124, 201, 68, 138, 194, 251, 192, 36, 55, 160, 211, 68, 65, 189, 121,
        217, // PublicKey bytes
        94, 112, 76, 78, 70, 38, 94, 28, 204, 135, 80, 81, 73, 247, 155, 157,
        46, 65, 77, 1, 164, 227, 128, 109, 252, 101, 240, 167, 57, 1, 193, 208,
    ])
    .unwrap();
    let address = "E62578B4AA08AB8EB12A46DC2F05EAE4622542A7".to_owned();
    let validator = Validator {
        address,
        keypair,
        voting_power: 10,
    };
    let parameters = Parameters {
        epoch_duration: EpochDuration {
            min_num_of_blocks: 10,
            min_duration: anoma::types::time::Duration::minutes(1).into(),
        },
    };
    Genesis {
        validator,
        parameters,
    }
}

impl Validator {
    // Generates a new validator
    #[allow(dead_code)]
    fn new() -> Self {
        let mut rng: ThreadRng = thread_rng();
        let keypair = Keypair::generate(&mut rng);
        let mut hasher = Sha256::new();
        hasher.update(keypair.public.to_bytes());
        // hex of the first 40 chars of the hash
        let address = format!("{:.40X}", hasher.finalize());
        Validator {
            address,
            keypair,
            voting_power: 10,
        }
    }
}
