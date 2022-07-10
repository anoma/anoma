//! Types for dealing with time and durations.

use std::convert::{TryFrom, TryInto};
use std::fmt::Display;
use std::ops::{Add, Sub};

use borsh::{BorshDeserialize, BorshSchema, BorshSerialize};
pub use chrono::{DateTime, Duration, TimeZone, Utc};
#[cfg(not(feature = "ABCI"))]
use tendermint_proto::google::protobuf;
#[cfg(feature = "ABCI")]
use tendermint_proto_abci::google::protobuf;

use crate::tendermint::time::Time;
use crate::tendermint::Error as TendermintError;

/// Check if the given `duration` has passed since the given `start.
pub fn duration_passed(
    current: DateTimeUtc,
    start: DateTimeUtc,
    duration: DurationSecs,
) -> bool {
    start + duration <= current
}

/// A duration in seconds precision.
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshSerialize,
    BorshDeserialize,
    BorshSchema,
)]
pub struct DurationSecs(pub u64);

impl From<Duration> for DurationSecs {
    fn from(duration_chrono: Duration) -> Self {
        let duration_std = duration_chrono
            .to_std()
            .expect("Duration must not be negative");
        duration_std.into()
    }
}

impl From<std::time::Duration> for DurationSecs {
    fn from(duration_std: std::time::Duration) -> Self {
        DurationSecs(duration_std.as_secs())
    }
}

impl From<DurationSecs> for std::time::Duration {
    fn from(duration_secs: DurationSecs) -> Self {
        std::time::Duration::new(duration_secs.0, 0)
    }
}

impl Display for DurationSecs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A duration in nanos precision.
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    BorshSerialize,
    BorshDeserialize,
    BorshSchema,
)]
pub struct DurationNanos {
    /// The seconds
    pub secs: u64,
    /// The nano seconds
    pub nanos: u32,
}

impl From<std::time::Duration> for DurationNanos {
    fn from(duration_std: std::time::Duration) -> Self {
        DurationNanos {
            secs: duration_std.as_secs(),
            nanos: duration_std.subsec_nanos(),
        }
    }
}

/// An RFC 3339 timestamp (e.g., "1970-01-01T00:00:00Z").
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct Rfc3339String(pub String);

/// A duration in seconds precision.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DateTimeUtc(pub DateTime<Utc>);

impl DateTimeUtc {
    /// Returns a DateTimeUtc which corresponds to the current date.
    pub fn now() -> Self {
        Self(Utc::now())
    }

    /// Returns an rfc3339 string or an error.
    pub fn to_rfc3339(&self) -> String {
        chrono::DateTime::to_rfc3339(&self.0)
    }
}

impl Add<DurationSecs> for DateTimeUtc {
    type Output = DateTimeUtc;

    fn add(self, duration: DurationSecs) -> Self::Output {
        let duration_std = std::time::Duration::from_secs(duration.0);
        let duration_chrono = Duration::from_std(duration_std).expect(
            "Duration shouldn't be larger than the maximum value supported \
             for chrono::Duration",
        );
        (self.0 + duration_chrono).into()
    }
}

impl Sub<Duration> for DateTimeUtc {
    type Output = DateTimeUtc;

    fn sub(self, rhs: Duration) -> Self::Output {
        (self.0 - rhs).into()
    }
}

impl BorshSerialize for DateTimeUtc {
    fn serialize<W: std::io::Write>(
        &self,
        writer: &mut W,
    ) -> std::io::Result<()> {
        let raw = self.0.to_rfc3339();
        raw.serialize(writer)
    }
}

impl BorshDeserialize for DateTimeUtc {
    fn deserialize(buf: &mut &[u8]) -> std::io::Result<Self> {
        use std::io::{Error, ErrorKind};
        let raw: String = BorshDeserialize::deserialize(buf)?;
        let actual = DateTime::parse_from_rfc3339(&raw)
            .map_err(|err| Error::new(ErrorKind::InvalidData, err))?;
        Ok(Self(actual.into()))
    }
}

impl BorshSchema for DateTimeUtc {
    fn add_definitions_recursively(
        definitions: &mut std::collections::HashMap<
            borsh::schema::Declaration,
            borsh::schema::Definition,
        >,
    ) {
        // Encoded as rfc3339 `String`
        let fields =
            borsh::schema::Fields::UnnamedFields(vec!["string".into()]);
        let definition = borsh::schema::Definition::Struct { fields };
        definitions.insert(Self::declaration(), definition);
    }

    fn declaration() -> borsh::schema::Declaration {
        "DateTimeUtc".into()
    }
}

impl From<DateTime<Utc>> for DateTimeUtc {
    fn from(dt: DateTime<Utc>) -> Self {
        Self(dt)
    }
}

impl TryFrom<prost_types::Timestamp> for DateTimeUtc {
    type Error = prost_types::TimestampOutOfSystemRangeError;

    fn try_from(
        timestamp: prost_types::Timestamp,
    ) -> Result<Self, Self::Error> {
        let system_time: std::time::SystemTime = timestamp.try_into()?;
        Ok(Self(system_time.into()))
    }
}

impl From<DateTimeUtc> for prost_types::Timestamp {
    fn from(dt: DateTimeUtc) -> Self {
        let seconds = dt.0.timestamp();
        let nanos = dt.0.timestamp_subsec_nanos() as i32;
        prost_types::Timestamp { seconds, nanos }
    }
}

impl TryFrom<protobuf::Timestamp> for DateTimeUtc {
    type Error = prost_types::TimestampOutOfSystemRangeError;

    fn try_from(timestamp: protobuf::Timestamp) -> Result<Self, Self::Error> {
        Self::try_from(prost_types::Timestamp {
            seconds: timestamp.seconds,
            nanos: timestamp.nanos,
        })
    }
}

impl From<DateTimeUtc> for std::time::SystemTime {
    fn from(dt: DateTimeUtc) -> Self {
        dt.0.into()
    }
}

impl TryFrom<Rfc3339String> for DateTimeUtc {
    type Error = chrono::ParseError;

    fn try_from(str: Rfc3339String) -> Result<Self, Self::Error> {
        let utc = DateTime::parse_from_rfc3339(&str.0)?;
        Ok(Self(utc.into()))
    }
}

impl From<DateTimeUtc> for Rfc3339String {
    fn from(dt: DateTimeUtc) -> Self {
        Self(DateTime::to_rfc3339(&dt.0))
    }
}

impl TryFrom<DateTimeUtc> for Time {
    type Error = TendermintError;

    fn try_from(dt: DateTimeUtc) -> Result<Self, Self::Error> {
        Self::parse_from_rfc3339(&DateTime::to_rfc3339(&dt.0))
    }
}

impl TryFrom<Time> for DateTimeUtc {
    type Error = chrono::ParseError;

    fn try_from(t: Time) -> Result<Self, Self::Error> {
        Rfc3339String(t.to_rfc3339()).try_into()
    }
}
