//! Types for dealing with time and durations.

use std::convert::{TryFrom, TryInto};
use std::ops::{Add, Sub};

use borsh::{BorshDeserialize, BorshSerialize};
pub use chrono::{DateTime, Duration, TimeZone, Utc};

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

/// A duration in seconds precision.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DateTimeUtc(pub DateTime<Utc>);

impl DateTimeUtc {
    /// Returns a DateTimeUtc which corresponds to the current date.
    pub fn now() -> Self {
        Self(Utc::now())
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

impl From<DateTimeUtc> for std::time::SystemTime {
    fn from(dt: DateTimeUtc) -> Self {
        dt.0.into()
    }
}
