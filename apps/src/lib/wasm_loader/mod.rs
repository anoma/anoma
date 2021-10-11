//! A module for loading WASM files and downloading pre-built WASMs.
use core::borrow::Borrow;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

use hex;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use crate::cli::safe_exit;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Not able to download {0}")]
    Download(String),
    #[error("Error writing to {0}")]
    FileWrite(String),
    #[error("Cannot download {0}")]
    WasmNotFound(String),
    #[error("Error while downloading {0}: {1}")]
    ServerError(String, String),
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(transparent)]
struct Checksums(HashMap<String, String>);

const S3_URL: &str = "https://heliax-anoma-wasm-v1.s3.eu-west-1.amazonaws.com";

impl Checksums {
    pub fn read_checksums(wasm_directory: impl AsRef<Path>) -> Self {
        let checksums_path = wasm_directory.as_ref().join("checksums.json");
        match fs::File::open(checksums_path) {
            Ok(file) => match serde_json::from_reader(file) {
                Ok(result) => result,
                Err(_) => {
                    eprintln!(
                        "Can't read checksums.json in {}",
                        wasm_directory.as_ref().to_string_lossy()
                    );
                    safe_exit(1);
                }
            },
            Err(_) => {
                eprintln!(
                    "Can't find checksums.json in {}",
                    wasm_directory.as_ref().to_string_lossy()
                );
                safe_exit(1);
            }
        }
    }
}

/// Download all the pre-build WASMs, or if they're already downloaded, verify
/// their checksums. Download all the pre-build WASMs, or if they're already
/// downloaded, verify their checksums.
pub fn pre_fetch_wasm(wasm_directory: impl AsRef<Path>) {
    // load json with wasm hashes
    let checksums = Checksums::read_checksums(&wasm_directory);

    for (name, hash) in checksums.0 {
        let wasm_path = wasm_directory.as_ref().join(&hash);

        match fs::read(&wasm_path) {
            // if the file exist, first check the hash. If not matching download
            // it again.
            Ok(bytes) => {
                let mut hasher = Sha256::new();
                hasher.update(bytes);
                let result = hex::encode(hasher.finalize());
                let checksum = format!(
                    "{}.{}.wasm",
                    &name.split('.').collect::<Vec<&str>>()[0],
                    result
                );
                if hash == checksum {
                    continue;
                }
                tracing::info!(
                    "Wasm checksum mismatch for {}. Fetching new version...",
                    &name,
                );
                let url = format!("{}/{}", S3_URL, hash);
                match download_wasm(url) {
                    Ok(bytes) => {
                        if let Err(e) = fs::write(wasm_path, &bytes) {
                            panic!(
                                "Error while creating file for {}: {}",
                                &name, e
                            );
                        }
                    }
                    Err(e) => {
                        eprintln!("Error downloading wasm: {}", e);
                        safe_exit(1);
                    }
                }
            }
            // if the doesn't file exist, download it.
            Err(err) => match err.kind() {
                std::io::ErrorKind::NotFound => {
                    let url = format!("{}/{}", S3_URL, hash);
                    match download_wasm(url) {
                        Ok(bytes) => {
                            if let Err(e) = fs::write(wasm_path, &bytes) {
                                panic!(
                                    "Error while creating file for {}: {}",
                                    &name, e
                                );
                            }
                        }
                        Err(e) => {
                            eprintln!("Error downloading wasm: {}", e);
                            safe_exit(1);
                        }
                    }
                }
                _ => {
                    eprintln!(
                        "Can't read {}.",
                        wasm_path.as_os_str().to_string_lossy()
                    );
                    safe_exit(1);
                }
            },
        }
    }
}

pub fn read_wasm(
    wasm_directory: impl AsRef<Path>,
    file_path: impl AsRef<Path>,
) -> Vec<u8> {
    // load json with wasm hashes
    let checksums = Checksums::read_checksums(&wasm_directory);

    if let Some(os_name) = file_path.as_ref().file_name() {
        if let Some(name) = os_name.to_str() {
            match checksums.0.get(name) {
                Some(wasm_filename) => {
                    let wasm_path = wasm_directory.as_ref().join(wasm_filename);
                    match fs::read(&wasm_path) {
                        Ok(bytes) => {
                            return bytes;
                        }
                        Err(_) => {
                            eprintln!(
                                "File {} not found. ",
                                wasm_path.to_string_lossy()
                            );
                            safe_exit(1);
                        }
                    }
                }
                None => {
                    if !file_path.as_ref().is_absolute() {
                        match fs::read(
                            wasm_directory.as_ref().join(file_path.as_ref()),
                        ) {
                            Ok(bytes) => {
                                return bytes;
                            }
                            Err(_) => {
                                eprintln!(
                                    "Could not read file {}. ",
                                    file_path.as_ref().to_string_lossy()
                                );
                                safe_exit(1);
                            }
                        }
                    } else {
                        match fs::read(file_path.as_ref()) {
                            Ok(bytes) => {
                                return bytes;
                            }
                            Err(_) => {
                                eprintln!(
                                    "Could not read file {}. ",
                                    file_path.as_ref().to_string_lossy()
                                );
                                safe_exit(1);
                            }
                        }
                    }
                }
            }
        }
    }
    eprintln!(
        "File  {} does not exist.",
        file_path.as_ref().to_string_lossy()
    );
    safe_exit(1);
}

fn download_wasm(url: String) -> Result<Vec<u8>, Error> {
    let response = reqwest::blocking::get(&url);
    match response {
        Ok(body) => {
            let status = body.status();
            if status.is_success() {
                let bytes = body.bytes().unwrap();
                let bytes: &[u8] = bytes.borrow();
                let bytes: Vec<u8> = bytes.to_owned();

                Ok(bytes)
            } else if status.is_server_error() {
                Err(Error::WasmNotFound(url))
            } else {
                Err(Error::ServerError(url, status.to_string()))
            }
        }
        Err(e) => {
            tracing::error!(
                "Error while downloading file {}. Error: {}",
                url,
                e
            );
            Err(Error::Download(url))
        }
    }
}
