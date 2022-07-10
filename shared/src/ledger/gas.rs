//! Gas accounting module to track the gas usage in a block for transactions and
//! validity predicates triggered by transactions.

use std::convert::TryFrom;

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
use thiserror::Error;

#[allow(missing_docs)]
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("Transaction gas limit exceeded")]
    TransactionGasExceedededError,
    #[error("Block gas limit exceeded")]
    BlockGasExceeded,
    #[error("Overflow during gas operations")]
    GasOverflow,
}

const COMPILE_GAS_PER_BYTE: u64 = 1;
const BASE_TRANSACTION_FEE: u64 = 2;
const PARALLEL_GAS_DIVIDER: u64 = 10;

/// The maximum value should be less or equal to i64::MAX
/// to avoid the gas overflow when sending this to ABCI
const BLOCK_GAS_LIMIT: u64 = 10_000_000_000_000;
const TRANSACTION_GAS_LIMIT: u64 = 10_000_000_000;

/// The minimum gas cost for accessing the storage
pub const MIN_STORAGE_GAS: u64 = 1;

/// Gas module result for functions that may fail
pub type Result<T> = std::result::Result<T, Error>;

/// Gas metering in a block. Tracks the gas in a current block and a current
/// transaction.
#[derive(Debug, Default, Clone)]
pub struct BlockGasMeter {
    block_gas: u64,
    transaction_gas: u64,
}

/// Gas metering in a validity predicate
#[derive(Debug, Clone, Default)]
pub struct VpGasMeter {
    /// The gas used in the transaction before the VP run
    initial_gas: u64,
    /// The current gas usage in the VP
    pub current_gas: u64,
}

/// Gas meter for VPs parallel runs
#[derive(
    Clone, Debug, Default, BorshSerialize, BorshDeserialize, BorshSchema,
)]
pub struct VpsGas {
    max: Option<u64>,
    rest: Vec<u64>,
}

impl BlockGasMeter {
    /// Add gas cost for the current transaction. It will return error when the
    /// consumed gas exceeds the transaction gas limit, but the state will still
    /// be updated.
    pub fn add(&mut self, gas: u64) -> Result<()> {
        self.transaction_gas = self
            .transaction_gas
            .checked_add(gas)
            .ok_or(Error::GasOverflow)?;

        if self.transaction_gas > TRANSACTION_GAS_LIMIT {
            return Err(Error::TransactionGasExceedededError);
        }
        Ok(())
    }

    /// Add the base transaction fee and the fee per transaction byte that's
    /// charged the moment we try to apply the transaction.
    pub fn add_base_transaction_fee(&mut self, bytes_len: usize) -> Result<()> {
        tracing::info!("add_base_transaction_fee {}", bytes_len);
        self.add(BASE_TRANSACTION_FEE)
    }

    /// Add the compiling cost proportionate to the code length
    pub fn add_compiling_fee(&mut self, bytes_len: usize) -> Result<()> {
        self.add(bytes_len as u64 * COMPILE_GAS_PER_BYTE)
    }

    /// Add the transaction gas to the block's total gas. Returns the
    /// transaction's gas cost and resets the transaction meter. It will return
    /// error when the consumed gas exceeds the block gas limit, but the state
    /// will still be updated.
    pub fn finalize_transaction(&mut self) -> Result<u64> {
        self.block_gas = self
            .block_gas
            .checked_add(self.transaction_gas)
            .ok_or(Error::GasOverflow)?;

        let transaction_gas = self.transaction_gas;
        self.transaction_gas = 0;
        if self.block_gas > BLOCK_GAS_LIMIT {
            return Err(Error::BlockGasExceeded);
        }
        Ok(transaction_gas)
    }

    /// Reset the gas meter.
    pub fn reset(&mut self) {
        self.transaction_gas = 0;
        self.block_gas = 0;
    }

    /// Get the total gas used in the current transaction.
    pub fn get_current_transaction_gas(&self) -> u64 {
        self.transaction_gas
    }

    /// Add the gas cost used in validity predicates to the current transaction.
    pub fn add_vps_gas(&mut self, vps_gas: &VpsGas) -> Result<()> {
        self.add(vps_gas.get_current_gas()?)
    }
}

impl VpGasMeter {
    /// Initialize a new VP gas meter, starting with the gas consumed in the
    /// transaction so far.
    pub fn new(initial_gas: u64) -> Self {
        Self {
            initial_gas,
            current_gas: 0,
        }
    }

    /// Consume gas in a validity predicate. It will return error when the
    /// consumed gas exceeds the transaction gas limit, but the state will still
    /// be updated.
    pub fn add(&mut self, gas: u64) -> Result<()> {
        let gas = self
            .current_gas
            .checked_add(gas)
            .ok_or(Error::GasOverflow)?;

        self.current_gas = gas;

        let current_total = self
            .initial_gas
            .checked_add(self.current_gas)
            .ok_or(Error::GasOverflow)?;

        if current_total > TRANSACTION_GAS_LIMIT {
            return Err(Error::TransactionGasExceedededError);
        }
        Ok(())
    }

    /// Add the compiling cost proportionate to the code length
    pub fn add_compiling_fee(&mut self, bytes_len: usize) -> Result<()> {
        self.add(bytes_len as u64 * COMPILE_GAS_PER_BYTE)
    }
}

impl VpsGas {
    /// Set the gas cost from a single VP run.
    pub fn set(&mut self, vp_gas_meter: &VpGasMeter) -> Result<()> {
        debug_assert_eq!(self.max, None);
        debug_assert!(self.rest.is_empty());
        self.max = Some(vp_gas_meter.current_gas);
        self.check_limit(vp_gas_meter.initial_gas)
    }

    /// Merge validity predicates gas meters from parallelized runs.
    pub fn merge(
        &mut self,
        other: &mut VpsGas,
        initial_gas: u64,
    ) -> Result<()> {
        match (self.max, other.max) {
            (None, Some(_)) => {
                self.max = other.max;
            }
            (Some(this_max), Some(other_max)) => {
                if this_max < other_max {
                    self.rest.push(this_max);
                    self.max = other.max;
                } else {
                    self.rest.push(other_max);
                }
            }
            _ => {}
        }
        self.rest.append(&mut other.rest);

        self.check_limit(initial_gas)
    }

    fn check_limit(&self, initial_gas: u64) -> Result<()> {
        let total = initial_gas
            .checked_add(self.get_current_gas()?)
            .ok_or(Error::GasOverflow)?;
        if total > TRANSACTION_GAS_LIMIT {
            return Err(Error::GasOverflow);
        }
        Ok(())
    }

    /// Get the gas consumed by the parallelized VPs
    fn get_current_gas(&self) -> Result<u64> {
        let parallel_gas = self.rest.iter().sum::<u64>() / PARALLEL_GAS_DIVIDER;
        self.max
            .unwrap_or_default()
            .checked_add(parallel_gas as u64)
            .ok_or(Error::GasOverflow)
    }
}

/// Convert the gas from signed to unsigned int. This will panic on overflow,
/// but it should never occur for our gas limits (see
/// `tests::gas_limits_cannot_overflow_i64`).
pub fn as_i64(gas: u64) -> i64 {
    i64::try_from(gas).expect("Gas should never overflow i64")
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    proptest! {
        #[test]
        fn test_vp_gas_meter_add(gas in 0..TRANSACTION_GAS_LIMIT) {
            let mut meter = VpGasMeter::new(0);
            meter.add(gas).expect("cannot add the gas");
        }

        #[test]
        fn test_block_gas_meter_add(gas in 0..TRANSACTION_GAS_LIMIT) {
            let mut meter = BlockGasMeter::default();
            meter.add(gas).expect("cannot add the gas");
            let result = meter.finalize_transaction().expect("cannot finalize the tx");
            assert_eq!(result, gas);
        }
    }

    #[test]
    fn test_vp_gas_overflow() {
        let mut meter = VpGasMeter::new(1);
        assert_matches!(
            meter.add(u64::MAX).expect_err("unexpectedly succeeded"),
            Error::GasOverflow
        );
    }

    #[test]
    fn test_vp_gas_limit() {
        let mut meter = VpGasMeter::new(1);
        assert_matches!(
            meter
                .add(TRANSACTION_GAS_LIMIT)
                .expect_err("unexpectedly succeeded"),
            Error::TransactionGasExceedededError
        );
    }

    #[test]
    fn test_tx_gas_overflow() {
        let mut meter = BlockGasMeter::default();
        meter.add(1).expect("cannot add the gas");
        assert_matches!(
            meter.add(u64::MAX).expect_err("unexpectedly succeeded"),
            Error::GasOverflow
        );
    }

    #[test]
    fn test_tx_gas_limit() {
        let mut meter = BlockGasMeter::default();
        assert_matches!(
            meter
                .add(TRANSACTION_GAS_LIMIT + 1)
                .expect_err("unexpectedly succeeded"),
            Error::TransactionGasExceedededError
        );
    }

    #[test]
    fn test_block_gas_limit() {
        let mut meter = BlockGasMeter::default();

        // add the maximum tx gas
        for _ in 0..(BLOCK_GAS_LIMIT / TRANSACTION_GAS_LIMIT) {
            meter
                .add(TRANSACTION_GAS_LIMIT)
                .expect("over the tx gas limit");
            meter
                .finalize_transaction()
                .expect("over the block gas limit");
        }

        meter
            .add(TRANSACTION_GAS_LIMIT)
            .expect("over the tx gas limit");
        match meter
            .finalize_transaction()
            .expect_err("unexpectedly succeeded")
        {
            Error::BlockGasExceeded => {}
            _ => panic!("unexpected error happened"),
        }
    }

    /// Test that the function [`as_i64`] cannot fail for transaction and block
    /// gas limit + some "tolerance" for gas exhaustion.
    #[test]
    fn gas_limits_cannot_overflow_i64() {
        let tolerance = 10_000;
        as_i64(BLOCK_GAS_LIMIT + tolerance);
        as_i64(TRANSACTION_GAS_LIMIT + tolerance);
    }
}
