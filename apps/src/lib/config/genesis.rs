//! The parameters used for the chain's genesis

use std::collections::HashMap;

use anoma::ledger::parameters::Parameters;
use anoma::ledger::pos::{GenesisValidator, PosParams};
use anoma::types::address::Address;
#[cfg(feature = "dev")]
use anoma::types::key::ed25519::Keypair;
use anoma::types::key::ed25519::PublicKey;
use anoma::types::{storage, token};

#[derive(Debug)]
pub struct Genesis {
    pub validators: Vec<Validator>,
    pub token_accounts: Vec<TokenAccount>,
    pub established_accounts: Vec<EstablishedAccount>,
    pub implicit_accounts: Vec<ImplicitAccount>,
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
    /// Validity predicate code WASM
    pub vp_code_path: String,
}

#[derive(Clone, Debug)]
pub struct EstablishedAccount {
    /// Address
    pub address: Address,
    /// Validity predicate code WASM
    pub vp_code_path: String,
    /// A public key to be stored in the account's storage, if any
    pub public_key: Option<PublicKey>,
    /// Account's sub-space storage. The values must be borsh encoded bytes.
    pub storage: HashMap<storage::Key, Vec<u8>>,
}

#[derive(Clone, Debug)]
pub struct TokenAccount {
    /// Address
    pub address: Address,
    /// Validity predicate code WASM
    pub vp_code_path: String,
    /// Accounts' balances of this token
    pub balances: HashMap<Address, token::Amount>,
}

#[derive(Clone, Debug)]
pub struct ImplicitAccount {
    /// A public key from which the implicit account is derived. This will be
    /// stored on chain for the account.
    pub public_key: PublicKey,
}

#[cfg(feature = "dev")]
pub fn genesis() -> Genesis {
    use std::iter::FromIterator;

    use anoma::ledger::parameters::EpochDuration;
    use anoma::types::address;

    use crate::wallet;

    let vp_token_path = "vp_token.wasm";
    let vp_user_path = "vp_user.wasm";

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
    let address = wallet::defaults::validator_address();
    let staking_reward_address = Address::decode("atest1v4ehgw36xcersvee8qerxd35x9prsw2xg5erxv6pxfpygd2x89z5xsf5xvmnysejgv6rwd2rnj2avt").unwrap();
    let validator = Validator {
        pos_data: GenesisValidator {
            address,
            staking_reward_address,
            tokens: token::Amount::whole(200_000),
            consensus_key: consensus_keypair.public,
            staking_reward_key: staking_reward_keypair.public,
        },
        account_key: account_keypair.public,
        non_staked_balance: token::Amount::whole(100_000),
        // TODO replace with https://github.com/anoma/anoma/issues/25)
        vp_code_path: vp_user_path.into(),
    };
    let parameters = Parameters {
        epoch_duration: EpochDuration {
            min_num_of_blocks: 10,
            min_duration: anoma::types::time::Duration::minutes(1).into(),
        },
    };
    let albert = EstablishedAccount {
        address: wallet::defaults::albert_address(),
        vp_code_path: vp_user_path.into(),
        public_key: Some(wallet::defaults::albert_keypair().public),
        storage: HashMap::default(),
    };
    let bertha = EstablishedAccount {
        address: wallet::defaults::bertha_address(),
        vp_code_path: vp_user_path.into(),
        public_key: Some(wallet::defaults::bertha_keypair().public),
        storage: HashMap::default(),
    };
    let christel = EstablishedAccount {
        address: wallet::defaults::christel_address(),
        vp_code_path: vp_user_path.into(),
        public_key: Some(wallet::defaults::christel_keypair().public),
        storage: HashMap::default(),
    };
    let matchmaker = EstablishedAccount {
        address: wallet::defaults::matchmaker_address(),
        vp_code_path: vp_user_path.into(),
        public_key: Some(wallet::defaults::matchmaker_keypair().public),
        storage: HashMap::default(),
    };
    let implicit_accounts = vec![ImplicitAccount {
        public_key: wallet::defaults::daewon_keypair().public,
    }];
    let default_user_tokens = token::Amount::whole(1_000_000);
    let balances: HashMap<Address, token::Amount> = HashMap::from_iter([
        (wallet::defaults::albert_address(), default_user_tokens),
        (wallet::defaults::bertha_address(), default_user_tokens),
        (wallet::defaults::christel_address(), default_user_tokens),
        (wallet::defaults::daewon_address(), default_user_tokens),
    ]);
    let token_accounts = address::tokens()
        .into_iter()
        .map(|(address, _)| TokenAccount {
            address,
            vp_code_path: vp_token_path.into(),
            balances: balances.clone(),
        })
        .collect();
    Genesis {
        validators: vec![validator],
        established_accounts: vec![albert, bertha, christel, matchmaker],
        implicit_accounts,
        token_accounts,
        parameters,
        pos_params: PosParams::default(),
    }
}
#[cfg(not(feature = "dev"))]
pub fn genesis() -> Genesis {
    todo!("load from file")
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
