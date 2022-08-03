//! Helpers for error handling in WASM
//!
//! This module is currently duplicated in tx_prelude and vp_prelude crates to
//! be able to implement `From` conversion on error types from other crates,
//! avoiding `error[E0117]: only traits defined in the current crate can be
//! implemented for arbitrary types`

use anoma::ledger::storage_api;
use thiserror::Error;

#[allow(missing_docs)]
#[derive(Error, Debug)]
pub enum Error {
    #[error("{0}")]
    SimpleMessage(&'static str),
    #[error("{0}")]
    Custom(CustomError),
    #[error("{0}: {1}")]
    CustomWithMessage(&'static str, CustomError),
}

/// Result of transaction or VP.
pub type EnvResult<T> = Result<T, Error>;

pub trait ResultExt<T> {
    /// Replace a possible error with a static message in [`EnvResult`].
    fn err_msg(self, msg: &'static str) -> EnvResult<T>;
}

// This is separate from `ResultExt`, because the implementation requires
// different bounds for `T`.
pub trait ResultExt2<T> {
    /// Convert a [`Result`] into [`EnvResult`].
    fn into_env_result(self) -> EnvResult<T>;

    /// Add a static message to a possible error in [`EnvResult`].
    fn wrap_err(self, msg: &'static str) -> EnvResult<T>;
}

pub trait OptionExt<T> {
    /// Transforms the [`Option<T>`] into a [`EnvResult<T>`], mapping
    /// [`Some(v)`] to [`Ok(v)`] and [`None`] to the given static error
    /// message.
    fn ok_or_err_msg(self, msg: &'static str) -> EnvResult<T>;
}

impl<T, E> ResultExt<T> for Result<T, E> {
    fn err_msg(self, msg: &'static str) -> EnvResult<T> {
        self.map_err(|_err| Error::new_const(msg))
    }
}

impl<T, E> ResultExt2<T> for Result<T, E>
where
    E: std::error::Error + Send + Sync + 'static,
{
    fn into_env_result(self) -> EnvResult<T> {
        self.map_err(Error::new)
    }

    fn wrap_err(self, msg: &'static str) -> EnvResult<T> {
        self.map_err(|err| Error::wrap(msg, err))
    }
}

impl<T> OptionExt<T> for Option<T> {
    fn ok_or_err_msg(self, msg: &'static str) -> EnvResult<T> {
        self.ok_or_else(|| Error::new_const(msg))
    }
}

impl Error {
    /// Create an [`enum@Error`] from a static message.
    #[inline]
    pub const fn new_const(msg: &'static str) -> Self {
        Self::SimpleMessage(msg)
    }

    /// Create an [`enum@Error`] from another [`std::error::Error`].
    pub fn new<E>(error: E) -> Self
    where
        E: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        Self::Custom(CustomError(error.into()))
    }

    /// Wrap another [`std::error::Error`] with a static message.
    pub fn wrap<E>(msg: &'static str, error: E) -> Self
    where
        E: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        Self::CustomWithMessage(msg, CustomError(error.into()))
    }
}

/// A custom error
#[derive(Debug)]
pub struct CustomError(Box<dyn std::error::Error + Send + Sync>);

impl std::fmt::Display for CustomError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<storage_api::Error> for Error {
    fn from(err: storage_api::Error) -> Self {
        // storage_api::Error::Custom(CustomError {Box<Error>})
        // Error:Custom(storage_api::Error::Custom(CustomError {Box<Error>}))
        Self::new(err)
    }
}
