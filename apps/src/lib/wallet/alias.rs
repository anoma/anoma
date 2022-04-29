//! Wallet address and key aliases.

use std::convert::Infallible;
use std::fmt::Display;
use std::hash::Hash;
use std::str::FromStr;

use serde::{Deserialize, Serialize};

/// Aliases created from raw strings are kept in-memory as given, but their
/// `Serialize` and `Display` instance converts them to lowercase. Their
/// `PartialEq` instance is case-insensitive.
#[derive(Clone, Debug, Default, Deserialize, PartialOrd, Ord, Eq)]
#[serde(transparent)]
pub struct Alias(String);

impl Alias {
    /// Normalize an alias to lower-case
    pub fn normalize(&self) -> String {
        self.0.to_lowercase()
    }

    /// Returns the length of the underlying `String`.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Is the underlying `String` empty?
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Serialize for Alias {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.normalize().serialize(serializer)
    }
}

impl PartialEq for Alias {
    fn eq(&self, other: &Self) -> bool {
        self.normalize() == other.normalize()
    }
}

impl Hash for Alias {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.normalize().hash(state);
    }
}

impl<T> From<T> for Alias
where
    T: AsRef<str>,
{
    fn from(raw: T) -> Self {
        Self(raw.as_ref().to_owned())
    }
}

impl From<Alias> for String {
    fn from(alias: Alias) -> Self {
        alias.normalize()
    }
}

impl<'a> From<&'a Alias> for String {
    fn from(alias: &'a Alias) -> Self {
        alias.normalize()
    }
}

impl Display for Alias {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.normalize().fmt(f)
    }
}

impl FromStr for Alias {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.into()))
    }
}

/// Default alias of a validator's account key
pub fn validator_key(validator_alias: &Alias) -> Alias {
    format!("{validator_alias}-validator-key").into()
}

/// Default alias of a validator's consensus key
pub fn validator_consensus_key(validator_alias: &Alias) -> Alias {
    format!("{validator_alias}-consensus-key").into()
}

/// Default alias of a validator's staking rewards key
pub fn validator_rewards_key(validator_alias: &Alias) -> Alias {
    format!("{validator_alias}-rewards-key").into()
}

/// Default alias of a validator's Tendermint node key
pub fn validator_tendermint_node_key(validator_alias: &Alias) -> Alias {
    format!("{validator_alias}-tendermint-node-key").into()
}
