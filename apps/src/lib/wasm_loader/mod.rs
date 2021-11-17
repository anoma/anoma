//! A module for loading WASM files and downloading pre-built WASMs.
use core::borrow::Borrow;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

use futures::future::join_all;
use hex;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;
use tokio::io::AsyncReadExt;

use crate::cli::safe_exit;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Not able to download {0}, failed with {1}")]
    Download(String, reqwest::Error),
    #[error("Error writing to {0}")]
    FileWrite(String),
    #[error("Cannot download {0}")]
    WasmNotFound(String),
    #[error("Error while downloading {0}: {1}")]
    ServerError(String, String),
}

/// A hash map where keys are file names and values their expected sha256 hash
#[derive(Debug, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Checksums(pub HashMap<String, String>);

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

    pub async fn read_checksums_async(
        wasm_directory: impl AsRef<Path>,
    ) -> Self {
        let checksums_path = wasm_directory.as_ref().join("checksums.json");
        match tokio::fs::File::open(checksums_path).await {
            Ok(mut file) => {
                let mut contents = vec![];
                // Ignoring the result, next step will fail if not read
                let _ = file.read_to_end(&mut contents).await;
                match serde_json::from_slice(&contents[..]) {
                    Ok(checksums) => checksums,
                    Err(_) => {
                        eprintln!(
                            "Can't read checksums.json in {}",
                            wasm_directory.as_ref().to_string_lossy()
                        );
                        safe_exit(1);
                    }
                }
            }
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
pub async fn pre_fetch_wasm(wasm_directory: impl AsRef<Path>) {
    // load json with wasm hashes
    let checksums = Checksums::read_checksums_async(&wasm_directory).await;

    join_all(checksums.0.into_iter().map(|(name, hash)| {
        let wasm_directory = wasm_directory.as_ref().to_owned();

        // Async check and download (if needed) each file
        tokio::spawn(async move {
            let wasm_path = wasm_directory.join(&hash);
            match tokio::fs::read(&wasm_path).await {
                // if the file exist, first check the hash. If not matching
                // download it again.
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
                        return;
                    }
                    tracing::info!(
                        "Wasm checksum mismatch for {}. Fetching new \
                         version...",
                        &name,
                    );
                    let url = format!("{}/{}", S3_URL, hash);
                    match download_wasm(url).await {
                        Ok(bytes) => {
                            if let Err(e) =
                                tokio::fs::write(wasm_path, &bytes).await
                            {
                                eprintln!(
                                    "Error while creating file for {}: {}",
                                    &name, e
                                );
                                safe_exit(1);
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
                        match download_wasm(url).await {
                            Ok(bytes) => {
                                if let Err(e) =
                                    tokio::fs::write(wasm_path, &bytes).await
                                {
                                    eprintln!(
                                        "Error while creating file for {}: {}",
                                        &name, e
                                    );
                                    safe_exit(1);
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
        })
    }))
    .await;
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

async fn download_wasm(url: String) -> Result<Vec<u8>, Error> {
    tracing::info!("Downloading WASM {}...", url);
    let response = reqwest::get(&url).await;
    match response {
        Ok(body) => {
            let status = body.status();
            if status.is_success() {
                let bytes = body.bytes().await.unwrap();
                let bytes: &[u8] = bytes.borrow();
                let bytes: Vec<u8> = bytes.to_owned();

                Ok(bytes)
            } else if status.is_server_error() {
                Err(Error::WasmNotFound(url))
            } else {
                Err(Error::ServerError(url, status.to_string()))
            }
        }
        Err(e) => Err(Error::Download(url, e)),
    }
}
