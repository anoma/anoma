//! Helpers for using wasm fixtures in tests.

use std::fs;
use std::path::{Path, PathBuf};

use anoma_apps::wasm_loader::Checksums;
use eyre::{eyre, Context, Result};

/// A directory containing .wasm files and a checksums.json.
pub struct WasmDirectory(PathBuf);

impl WasmDirectory {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self(path.into())
    }

    /// Gets the absolute path to a wasm file.
    pub fn get_abs_path(&self, name: &str) -> Result<PathBuf> {
        let filename = format!("{}.wasm", name);
        let checksums = Checksums::read_checksums(&self.0);
        let filename_with_hash =
            checksums.0.get(&filename).ok_or_else(|| {
                eyre!("Didn't find entry in checksums.json for {}", &filename)
            })?;
        let path = &self.0.join(&filename_with_hash);
        let abs_path = fs::canonicalize(path).wrap_err_with(|| {
            eyre!("Couldn't get absolute path for {}", &filename_with_hash)
        })?;
        Ok(abs_path)
    }
}

impl AsRef<Path> for WasmDirectory {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use serde_json::json;

    use super::*;

    #[test]
    fn test_get_abs_path() {
        // get a tempdir
        let tmp_dir = tempfile::tempdir().unwrap();
        let wasm_name = "tx_enqueue_eth_transfer";
        let wasm_filename_without_hash = format!("{}.wasm", wasm_name);
        let wasm_filename = format!(
            "{}.\
             7d7fa4553ccf115cd82ce59d4e1dc8321c41d357d02ccae29a59865aac2bb77d.\
             wasm", wasm_name);
        let wasm_path = tmp_dir.path().join(&wasm_filename);
        fs::write(&wasm_path, "contents of this file not relevant to test")
            .unwrap();
        let checksums_path = tmp_dir.path().join("checksums.json");
        fs::write(
            &checksums_path,
            json!({
                wasm_filename_without_hash: wasm_filename,
            })
            .to_string(),
        )
        .unwrap();

        let wasm_dir = WasmDirectory::new(tmp_dir.path());
        let wasm_filepath = wasm_dir.get_abs_path(wasm_name).unwrap();
        assert_eq!(
            wasm_filepath,
            fs::canonicalize(tmp_dir.path().join(&wasm_filename)).unwrap(),
        );
    }
}
