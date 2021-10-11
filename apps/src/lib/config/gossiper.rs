use libp2p::identity::ed25519::Keypair;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

// TODO move this into a sub-module of config
// TODO use conditional compilation to not write private key to file
// TODO The hash is not yet used. It's should be used by the ledger at some
// point ?

/// ed255519 keypair + hash of public key. The keypair used to encrypted the
/// data send in the libp2p network.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Gossiper {
    pub address: String,
    #[serde(with = "keypair_serde")]
    pub key: Keypair,
}
// TODO Here instead of encoding to bytes, it would be nice to encode to hex
// instead. Bytes makes the config file a bit less readable

// TODO this is needed because libp2p does not export ed255519 serde
// feature maybe a MR for libp2p to export theses functions ?
mod keypair_serde {
    use libp2p::identity::ed25519::Keypair;
    use serde::de::Error;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use serde_bytes::{ByteBuf as SerdeByteBuf, Bytes as SerdeBytes};

    pub fn serialize<S>(
        value: &Keypair,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let bytes = &value.encode()[..];
        SerdeBytes::new(bytes).serialize(serializer)
    }
    pub fn deserialize<'d, D>(deserializer: D) -> Result<Keypair, D::Error>
    where
        D: Deserializer<'d>,
    {
        let mut bytes = <SerdeByteBuf>::deserialize(deserializer)?;

        Keypair::decode(bytes.as_mut()).map_err(Error::custom)
    }
}

impl Gossiper {
    /// Generates a new gossiper keypair and hash.
    pub fn new() -> Self {
        let key = Keypair::generate();
        let mut hasher = Sha256::new();
        hasher.update(key.public().encode());
        let address = format!("{:.40X}", hasher.finalize());
        Gossiper { address, key }
    }
}

impl Default for Gossiper {
    fn default() -> Self {
        Self::new()
    }
}
