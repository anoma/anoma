//! Types that are used in IBC.

#[cfg(any(feature = "ibc-vp", feature = "ibc-vp-abci"))]
pub mod data;
pub mod event;

pub use event::*;
