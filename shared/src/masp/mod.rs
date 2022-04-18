use std::path::{Path, PathBuf};
use crate::types::chain::ChainId;

pub struct ParamsDirectory(pub PathBuf);

impl ParamsDirectory {
    pub fn from_chain_directory(chain_dir: impl AsRef<Path>) -> Self {
        ParamsDirectory(chain_dir.as_ref().join("masp"))
    }

    pub fn spend_path(&self) -> PathBuf {
        (&self).0.join("masp-spend.params")
    }

    pub fn output_path(&self) -> PathBuf {
        (&self).0.join("masp-output.params")
    }
}
