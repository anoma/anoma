//! Helpers for using MASP
use std::path::{Path, PathBuf};

/// Represents a directory containing MASP parameters e.g.
/// `~/Library/Application Support/MASPParams/`
pub struct ParamsDirectory {
    /// The actual path to the directory
    pub path: PathBuf,
}

impl ParamsDirectory {
    /// Returns the MASP parameters directory for a given chain directory e.g.
    /// `.anoma/anoma-masp-0.3.51d2f83a8412b95/masp/`
    pub fn for_chain_directory(chain_dir: impl AsRef<Path>) -> Self {
        ParamsDirectory {
            path: chain_dir.as_ref().join("masp"),
        }
    }

    /// Returns the path to the spend parameters
    pub fn spend_path(&self) -> PathBuf {
        self.path.join("masp-spend.params")
    }

    /// Returns the path to the output parameters
    pub fn output_path(&self) -> PathBuf {
        self.path.join("masp-output.params")
    }
}

impl AsRef<Path> for ParamsDirectory {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}
