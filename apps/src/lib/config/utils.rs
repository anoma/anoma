//! Configuration utilities

use std::str::FromStr;
use std::{cmp, env};

use itertools::Either;

use crate::cli;

/// Find how many threads to use from an environment variable if it's set and
/// valid (>= 1). If the environment variable is invalid, exits the process with
/// an error. or return 1 if the default value is not >= 1. Otherwise returns
/// the default.
pub fn num_of_threads(env_var: impl AsRef<str>, default: usize) -> usize {
    match num_of_threads_aux(&env_var, default) {
        Either::Left(num) => num,
        Either::Right(num_str) => {
            eprintln!(
                "Invalid env. var {} value: {}. Expecting a positive number.",
                env_var.as_ref(),
                num_str
            );
            cli::safe_exit(1);
        }
    }
}

/// Find how many threads to use from an environment variable if it's set and
/// valid (>= 1). On success, returns the value in `Either::Left`. If the
/// environment variable is invalid, returns `Either::Right` with the env var's
/// string value. or return 1 if the default value is not >= 1. Otherwise
/// returns the default.
fn num_of_threads_aux(
    env_var: impl AsRef<str>,
    default: usize,
) -> Either<usize, String> {
    let env_var = env_var.as_ref();
    if let Ok(num_str) = env::var(env_var) {
        match usize::from_str(&num_str) {
            Ok(num) if num > 0 => Either::Left(num),
            _ => Either::Right(num_str),
        }
    } else {
        Either::Left(cmp::max(1, default))
    }
}

#[cfg(test)]
mod test {
    use std::panic;

    use proptest::prelude::*;

    use super::*;

    proptest! {

        /// Test `num_of_threads_aux` when the env var is set and valid, it is
        /// correctly parsed and returned in `Either::Left`.
        #[test]
        fn test_num_of_threads_from_valid_env_var(value in 1_usize..) {
            let env_var = "anythingXYZ1";
            env::set_var(env_var, value.to_string());
            assert_eq!(num_of_threads_aux(env_var, value), Either::Left(value));
        }

        /// Test `num_of_threads_aux` that when the env var is set but not valid
        /// it returns `Either::Right`.
        #[test]
        fn test_num_of_threads_from_invalid_env_var(value in ..1_usize) {
            let env_var = "anythingXYZ2";
            let val_string = value.to_string();
            env::set_var(env_var, &val_string);
            assert_eq!(
                num_of_threads_aux(env_var, value),
                Either::Right(val_string)
            );
        }

        /// Test `num_of_threads_aux` when the env var is not set, the default
        /// value is returned in `Either::Left`.
        #[test]
        fn test_num_of_threads_from_valid_default(default in 1_usize..) {
            let env_var = "anythingXYZ3";
            assert_eq!(
                num_of_threads_aux(env_var, default),
                Either::Left(default)
            );
        }

        /// Test `num_of_threads_aux` when the env var is not set and the
        /// default is lower than 1, then 1 in `Either::Left` is returned
        /// instead.
        #[test]
        fn test_num_of_threads_from_invalid_default(default in ..1_usize) {
        let env_var = "anythingXYZ4";
            assert_eq!(num_of_threads_aux(env_var, default), Either::Left(1));
        }
    }
}
