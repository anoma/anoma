use anoma::proto::Intent;
use anoma::vm::wasm;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("error while running the filter: {0}")]
    RunnerError(wasm::run::Error),
    #[error("Failed to read file: {0}")]
    FileFailed(std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Filter;

impl Filter {
    pub fn validate(&self, _intent: &Intent) -> Result<bool> {
        // TODO to be replaced by the matchmaker dylib impl
        Ok(true)
    }
}
