use std::path::Path;

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
pub struct Filter {
    code: Vec<u8>,
}

impl Filter {
    pub fn from_file(path: impl AsRef<Path>) -> Result<Filter> {
        Ok(Filter {
            code: std::fs::read(path).map_err(Error::FileFailed)?,
        })
    }

    pub fn validate(&self, intent: &Intent) -> Result<bool> {
        wasm::run::matchmaker_filter(&self.code, &intent.data)
            .map_err(Error::RunnerError)
    }
}
