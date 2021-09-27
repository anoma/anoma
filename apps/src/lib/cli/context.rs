//! CLI input types can be used for command arguments

use std::env;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str::FromStr;

use anoma::types::address::Address;
use anoma::types::key::ed25519::{Keypair, PublicKey, PublicKeyHash};

use super::args;
use crate::cli::safe_exit;
use crate::config::Config;
use crate::wallet::Wallet;

/// A raw address (bech32m encoding) or an alias of an address that may be found
/// in the wallet
pub type WalletAddress = FromContext<Address>;

/// An alias, a public key or a public key hash of a keypair that may be found
/// in the wallet
pub type WalletKeypair = FromContext<Rc<Keypair>>;

/// A raw public key (hex encoding), a public key hash (also hex encoding) or an
/// alias of an public key that may be found in the wallet
pub type WalletPublicKey = FromContext<PublicKey>;

/// Command execution context
#[derive(Debug)]
pub struct Context {
    /// Global arguments
    pub global_args: args::Global,
    /// The wallet
    pub wallet: Wallet,
    /// The configuration
    pub config: Config,
}

impl Context {
    pub fn new(global_args: args::Global) -> Self {
        let wallet = Wallet::load_or_new(&global_args.base_dir);
        let mut config = load_config(&global_args.base_dir);

        // If the WASM dir specified, put it in the config
        match global_args.wasm_dir.as_ref() {
            Some(wasm_dir) => {
                config.ledger.wasm_dir = wasm_dir.clone();
            }
            None => {
                if let Ok(wasm_dir) = env::var("ANOMA_WASM_DIR") {
                    config.ledger.wasm_dir = wasm_dir.into();
                }
            }
        }
        Self {
            global_args,
            wallet,
            config,
        }
    }

    /// Parse and/or look-up the value from the context.
    pub fn get<T>(&self, from_context: &FromContext<T>) -> T
    where
        T: ArgFromContext,
    {
        from_context.from_ctx(self)
    }

    /// Try to parse and/or look-up an optional value from the context.
    pub fn get_opt<T>(&self, from_context: &Option<FromContext<T>>) -> Option<T>
    where
        T: ArgFromContext,
    {
        from_context
            .as_ref()
            .map(|from_context| from_context.from_ctx(self))
    }

    /// Parse and/or look-up the value from the context with cache.
    pub fn get_cached<T>(&mut self, from_context: &FromContext<T>) -> T
    where
        T: ArgFromMutContext,
    {
        from_context.from_mut_ctx(self)
    }

    /// Try to parse and/or look-up an optional value from the context with
    /// cache.
    pub fn get_opt_cached<T>(
        &mut self,
        from_context: &Option<FromContext<T>>,
    ) -> Option<T>
    where
        T: ArgFromMutContext,
    {
        from_context
            .as_ref()
            .map(|from_context| from_context.from_mut_ctx(self))
    }

    /// Read the given WASM file from the WASM directory.
    pub fn read_wasm(
        &self,
        file_name: impl AsRef<Path>,
    ) -> std::io::Result<Vec<u8>> {
        std::fs::read(self.wasm_path(file_name))
    }

    /// Find the path to the given WASM file name.
    pub fn wasm_path(&self, file_name: impl AsRef<Path>) -> PathBuf {
        let file_path = file_name.as_ref();
        if file_path.is_absolute() {
            return file_path.into();
        }
        self.config.ledger.wasm_dir.join(file_name)
    }
}

/// Load config from expected path in the `base_dir` or generate a new one if it
/// doesn't exist.
fn load_config(base_dir: &Path) -> Config {
    match Config::read(base_dir) {
        Ok(config) => config,
        Err(err) => {
            eprintln!(
                "Tried to read config in {} but failed with: {}",
                base_dir.display(),
                err
            );
            super::safe_exit(1)
        }
    }
}

/// Argument that can be given raw or found in the [`Context`].
#[derive(Debug, Clone)]
pub struct FromContext<T> {
    raw: String,
    phantom: PhantomData<T>,
}

impl<T> FromContext<T> {
    pub fn new(raw: String) -> FromContext<T> {
        Self {
            raw,
            phantom: PhantomData,
        }
    }
}

impl<T> FromContext<T>
where
    T: ArgFromContext,
{
    /// Parse and/or look-up the value from the context.
    fn from_ctx(&self, ctx: &Context) -> T {
        T::from_ctx(ctx, &self.raw)
    }
}

impl<T> FromContext<T>
where
    T: ArgFromMutContext,
{
    /// Parse and/or look-up the value from the mutable context.
    fn from_mut_ctx(&self, ctx: &mut Context) -> T {
        T::from_mut_ctx(ctx, &self.raw)
    }
}

/// CLI argument that found via the [`Context`].
pub trait ArgFromContext: Sized {
    fn from_ctx(ctx: &Context, raw: impl AsRef<str>) -> Self;
}

/// CLI argument that found via the [`Context`] and cached (as in case of an
/// encrypted keypair that has been decrypted), hence using mutable context.
pub trait ArgFromMutContext: Sized {
    fn from_mut_ctx(ctx: &mut Context, raw: impl AsRef<str>) -> Self;
}

impl ArgFromContext for Address {
    fn from_ctx(ctx: &Context, raw: impl AsRef<str>) -> Self {
        let raw = raw.as_ref();
        // An address can be either raw (bech32m encoding)
        FromStr::from_str(raw)
            // Or it can be an alias that may be found in the wallet
            .unwrap_or_else(|_| {
                ctx.wallet
                    .find_address(raw)
                    .unwrap_or_else(|| {
                        eprintln!("Unknown address {}", raw);
                        safe_exit(1)
                    })
                    .clone()
            })
    }
}

impl ArgFromMutContext for Rc<Keypair> {
    fn from_mut_ctx(ctx: &mut Context, raw: impl AsRef<str>) -> Self {
        let raw = raw.as_ref();
        ctx.wallet.find_key(raw).unwrap_or_else(|_find_err| {
            eprintln!("Unknown key {}", raw);
            safe_exit(1)
        })
    }
}

impl ArgFromMutContext for PublicKey {
    fn from_mut_ctx(ctx: &mut Context, raw: impl AsRef<str>) -> Self {
        let raw = raw.as_ref();
        // A public key can be either a raw public key in hex string
        FromStr::from_str(raw).unwrap_or_else(|_parse_err| {
            // Or it can be a public key hash in hex string
            FromStr::from_str(raw)
                .map(|pkh: PublicKeyHash| {
                    let key = ctx.wallet.find_key_by_pkh(&pkh).unwrap();
                    key.public.clone()
                })
                // Or it can be an alias that may be found in the wallet
                .unwrap_or_else(|_parse_err| {
                    let key = ctx.wallet.find_key(raw).unwrap();
                    key.public.clone()
                })
        })
    }
}
