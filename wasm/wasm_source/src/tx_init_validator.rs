//! A tx to initialize a new validator account and staking reward account with a
//! given public keys and a validity predicates.

use anoma_tx_prelude::transaction::InitValidator;
use anoma_tx_prelude::*;

#[transaction]
fn apply_tx(ctx: &mut Ctx, tx_data: Vec<u8>) -> TxResult {
    let signed = SignedTxData::try_from_slice(&tx_data[..])
        .err_msg("failed to decode SignedTxData")?;
    let data = signed.data.ok_or_err_msg("Missing data")?;
    let init_validator = InitValidator::try_from_slice(&data[..])
        .err_msg("failed to decode InitValidator")?;
    debug_log!("apply_tx called to init a new validator account");

    // Register the validator in PoS
    match ctx.init_validator(init_validator) {
        Ok((validator_address, staking_reward_address)) => {
            debug_log!(
                "Created validator {} and staking reward account {}",
                validator_address.encode(),
                staking_reward_address.encode()
            )
        }
        Err(err) => {
            debug_log!("Validator creation failed with: {}", err);
            panic!()
        }
    }
    Ok(())
}
