use std::fs::OpenOptions;
use std::path::{Path, PathBuf};

use libp2p::identity::ed25519::Keypair;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::cli;

const P2P_KEY_PATH: &str = "gossiper-p2p-private-key.json";

/// ed255519 keypair + hash of public key. The keypair used to encrypted the
/// data send in the libp2p network.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Identity {
    pub address: String,
    #[serde(with = "keypair_serde")]
    pub key: Keypair,
}

// TODO this is needed because libp2p does not export ed255519 serde
// feature maybe a MR for libp2p to export theses functions ?
mod keypair_serde {
    use libp2p::identity::ed25519::Keypair;
    use serde::de::Error;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn serialize<S>(
        value: &Keypair,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let bytes = value.encode();
        let string = hex::encode(&bytes[..]);
        string.serialize(serializer)
    }
    pub fn deserialize<'d, D>(deserializer: D) -> Result<Keypair, D::Error>
    where
        D: Deserializer<'d>,
    {
        let string = String::deserialize(deserializer)?;
        let mut bytes = hex::decode(&string).map_err(Error::custom)?;
        Keypair::decode(bytes.as_mut()).map_err(Error::custom)
    }
}

impl Identity {
    /// Generates a new gossiper keypair and hash.
    pub fn new() -> Self {
        let key = Keypair::generate();
        let mut hasher = Sha256::new();
        hasher.update(key.public().encode());
        let address = format!("{:.40X}", hasher.finalize());
        Identity { address, key }
    }

    /// Load identity from file or generate a new one if none found.
    pub fn load_or_gen(base_dir: impl AsRef<Path>) -> Identity {
        let file_path = Self::file_path(&base_dir);
        match OpenOptions::new().read(true).open(&file_path) {
            Ok(file) => {
                let gossiper: Identity = serde_json::from_reader(file)
                    .expect("unexpected key encoding");
                gossiper
            }
            Err(err) => {
                if let std::io::ErrorKind::NotFound = err.kind() {
                    tracing::info!(
                        "No P2P key found, generating a new one. This will be \
                         written into {}",
                        file_path.to_string_lossy()
                    );
                    Self::gen(base_dir)
                } else {
                    eprintln!(
                        "Cannot read {}: {}",
                        file_path.to_string_lossy(),
                        err
                    );
                    cli::safe_exit(1);
                }
            }
        }
    }

    /// Generate a new identity.
    pub fn gen(base_dir: impl AsRef<Path>) -> Identity {
        let file_path = Self::file_path(base_dir);
        std::fs::create_dir_all(&file_path.parent().unwrap()).unwrap();
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&file_path)
            .expect("Couldn't open P2P key file");
        let gossiper = Identity::new();
        serde_json::to_writer_pretty(file, &gossiper)
            .expect("Couldn't write private validator key file");
        gossiper
    }

    pub fn file_path(base_dir: impl AsRef<Path>) -> PathBuf {
        base_dir.as_ref().join(P2P_KEY_PATH)
    }

    pub fn peer_id(&self) -> libp2p::PeerId {
        let pk = self.key.public();
        let pk = libp2p::identity::PublicKey::Ed25519(pk);
        libp2p::PeerId::from(pk)
    }

    pub fn key(&self) -> libp2p::identity::Keypair {
        libp2p::identity::Keypair::Ed25519(self.key.clone())
    }
}

impl Default for Identity {
    fn default() -> Self {
        Self::new()
    }
}
