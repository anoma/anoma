//! The parameters used for the chain's genesis

use anoma::ledger::parameters::{EpochDuration, Parameters};
use anoma::ledger::pos::{GenesisValidator, PosParams};
use anoma::types::address::Address;
#[cfg(feature = "dev")]
use anoma::types::key::ed25519::Keypair;
use anoma::types::key::ed25519::PublicKey;
use anoma::types::token;

#[derive(Debug)]
pub struct Genesis {
    #[cfg(not(feature = "dev"))]
    pub validators: Vec<Validator>,
    #[cfg(feature = "dev")]
    pub validator: Validator,
    /// The consensus key will be written into Tendermint node's
    /// `priv_validator_key.json`
    #[cfg(feature = "dev")]
    pub validator_consensus_key: Keypair,
    pub parameters: Parameters,
    pub pos_params: PosParams,
}

#[derive(Clone, Debug)]
/// Genesis validator definition
pub struct Validator {
    /// Data that is used for PoS system initialization
    pub pos_data: GenesisValidator,
    /// Public key associated with the validator account. The default validator
    /// VP will check authorization of transactions from this account against
    /// this key on a transaction signature.
    /// Note that this is distinct from consensus key used in the PoS system.
    pub account_key: PublicKey,
    /// These tokens are no staked and hence do not contribute to the
    /// validator's voting power
    pub non_staked_balance: token::Amount,
}

#[cfg(feature = "dev")]
pub fn genesis() -> Genesis {
    use anoma::types::address;

    use crate::wallet;

    // NOTE When the validator's key changes, tendermint must be reset with
    // `anoma reset` command. To generate a new validator, use the
    // `tests::gen_genesis_validator` below.
    let consensus_keypair = wallet::defaults::validator_keypair();
    let account_keypair = wallet::defaults::validator_keypair();
    let staking_reward_keypair = Keypair::from_bytes(&[
        61, 198, 87, 204, 44, 94, 234, 228, 217, 72, 245, 27, 40, 2, 151, 174,
        24, 247, 69, 6, 9, 30, 44, 16, 88, 238, 77, 162, 243, 125, 240, 206,
        111, 92, 66, 23, 105, 211, 33, 236, 5, 208, 17, 88, 177, 112, 100, 154,
        1, 132, 143, 67, 162, 121, 136, 247, 20, 67, 4, 27, 226, 63, 47, 57,
    ])
    .unwrap();
    let address = address::validator();
    let staking_reward_address = Address::decode("a1qq5qqqqqxaz5vven8yu5gdpng9zrys6ygvurwv3sgsmrvd6xgdzrys6yg4pnwd6z89rrqv2xvjcy9t").unwrap();
    let validator = Validator {
        pos_data: GenesisValidator {
            address,
            staking_reward_address,
            tokens: token::Amount::whole(200_000),
            consensus_key: consensus_keypair.public.clone(),
            staking_reward_key: staking_reward_keypair.public,
        },
        account_key: account_keypair.public,
        non_staked_balance: token::Amount::whole(100_000),
    };
    let parameters = Parameters {
        epoch_duration: EpochDuration {
            min_num_of_blocks: 10,
            min_duration: anoma::types::time::Duration::minutes(1).into(),
        },
    };
    Genesis {
        validator,
        validator_consensus_key: consensus_keypair,
        parameters,
        pos_params: PosParams::default(),
    }
}

#[cfg(test)]
pub mod tests {
    use anoma::types::address::testing::gen_established_address;
    use anoma::types::key::ed25519::Keypair;
    use rand::prelude::ThreadRng;
    use rand::thread_rng;

    /// Run `cargo test gen_genesis_validator -- --nocapture` to generate a
    /// new genesis validator address, staking reward address and keypair.
    #[test]
    fn gen_genesis_validator() {
        let address = gen_established_address();
        let staking_reward_address = gen_established_address();
        let mut rng: ThreadRng = thread_rng();
        let keypair = Keypair::generate(&mut rng);
        let staking_reward_keypair = Keypair::generate(&mut rng);
        println!("address: {}", address);
        println!("staking_reward_address: {}", staking_reward_address);
        println!("keypair: {:?}", keypair.to_bytes());
        println!(
            "staking_reward_keypair: {:?}",
            staking_reward_keypair.to_bytes()
        );
    }
}
