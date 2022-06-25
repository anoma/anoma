//! CLI input types can be used for command arguments

use std::env;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str::FromStr;

use anoma::types::address::Address;
use anoma::types::chain::ChainId;
use anoma::types::key::*;
use color_eyre::eyre::Result;

use super::args;
use crate::cli::safe_exit;
use crate::config::genesis::genesis_config;
use crate::config::global::GlobalConfig;
use crate::config::{self, Config};
use crate::wallet::Wallet;
use crate::wasm_loader;

/// Env. var to set chain ID
const ENV_VAR_CHAIN_ID: &str = "ANOMA_CHAIN_ID";
/// Env. var to set wasm directory
pub const ENV_VAR_WASM_DIR: &str = "ANOMA_WASM_DIR";

/// A raw address (bech32m encoding) or an alias of an address that may be found
/// in the wallet
pub type WalletAddress = FromContext<Address>;

/// A raw keypair (hex encoding), an alias, a public key or a public key hash of
/// a keypair that may be found in the wallet
pub type WalletKeypair = FromContext<Rc<common::SecretKey>>;

/// A raw public key (hex encoding), a public key hash (also hex encoding) or an
/// alias of an public key that may be found in the wallet
pub type WalletPublicKey = FromContext<common::PublicKey>;

/// Command execution context
#[derive(Debug)]
pub struct Context {
    /// Global arguments
    pub global_args: args::Global,
    /// The wallet
    pub wallet: Wallet,
    /// The global configuration
    pub global_config: GlobalConfig,
    /// The ledger & intent gossip configuration for a specific chain ID
    pub config: Config,
}

impl Context {
    pub fn new(global_args: args::Global) -> Result<Self> {
        let global_config = read_or_try_new_global_config(&global_args);
        tracing::info!("Chain ID: {}", global_config.default_chain_id);

        let mut config = Config::load(
            &global_args.base_dir,
            &global_config.default_chain_id,
            global_args.mode.clone(),
        );

        let chain_dir = global_args
            .base_dir
            .join(&global_config.default_chain_id.as_str());
        let genesis_file_path = global_args
            .base_dir
            .join(format!("{}.toml", global_config.default_chain_id.as_str()));
        let wallet = Wallet::load_or_new_from_genesis(
            &chain_dir,
            genesis_config::open_genesis_config(&genesis_file_path)?,
        );

        // If the WASM dir specified, put it in the config
        match global_args.wasm_dir.as_ref() {
            Some(wasm_dir) => {
                if wasm_dir.is_absolute() {
                    eprintln!(
                        "The arg `--wasm-dir` cannot be an absolute path. It \
                         is nested inside the chain directory."
                    );
                    safe_exit(1);
                }
                config.wasm_dir = wasm_dir.clone();
            }
            None => {
                if let Ok(wasm_dir) = env::var(ENV_VAR_WASM_DIR) {
                    let wasm_dir: PathBuf = wasm_dir.into();
                    if wasm_dir.is_absolute() {
                        eprintln!(
                            "The env var `{}` cannot be an absolute path. It \
                             is nested inside the chain directory.",
                            ENV_VAR_WASM_DIR
                        );
                        safe_exit(1);
                    }
                    config.wasm_dir = wasm_dir;
                }
            }
        }
        Ok(Self {
            global_args,
            wallet,
            global_config,
            config,
        })
    }

    /// Parse and/or look-up the value from the context.
    pub fn get<T>(&self, from_context: &FromContext<T>) -> T
    where
        T: ArgFromContext,
    {
        from_context.arg_from_ctx(self)
    }

    /// Try to parse and/or look-up an optional value from the context.
    pub fn get_opt<T>(&self, from_context: &Option<FromContext<T>>) -> Option<T>
    where
        T: ArgFromContext,
    {
        from_context
            .as_ref()
            .map(|from_context| from_context.arg_from_ctx(self))
    }

    /// Parse and/or look-up the value from the context with cache.
    pub fn get_cached<T>(&mut self, from_context: &FromContext<T>) -> T
    where
        T: ArgFromMutContext,
    {
        from_context.arg_from_mut_ctx(self)
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
            .map(|from_context| from_context.arg_from_mut_ctx(self))
    }

    /// Get the wasm directory configured for the chain.
    ///
    /// Note that in "dev" build, this may be the root `wasm` dir.
    pub fn wasm_dir(&self) -> PathBuf {
        let wasm_dir =
            self.config.ledger.chain_dir().join(&self.config.wasm_dir);

        // In dev-mode with dev chain (the default), load wasm directly from the
        // root wasm dir instead of the chain dir
        #[cfg(feature = "dev")]
        let wasm_dir =
            if self.global_config.default_chain_id == ChainId::default() {
                "wasm".into()
            } else {
                wasm_dir
            };

        wasm_dir
    }

    /// Read the given WASM file from the WASM directory or an absolute path.
    pub fn read_wasm(&self, file_name: impl AsRef<Path>) -> Vec<u8> {
        wasm_loader::read_wasm(self.wasm_dir(), file_name)
    }
}

/// Load global config from expected path in the `base_dir` or try to generate a
/// new one if it doesn't exist.
pub fn read_or_try_new_global_config(
    global_args: &args::Global,
) -> GlobalConfig {
    GlobalConfig::read(&global_args.base_dir).unwrap_or_else(|err| {
        if let config::global::Error::FileNotFound(_) = err {
            let chain_id = global_args.chain_id.clone().or_else(|| {
                env::var(ENV_VAR_CHAIN_ID).ok().map(|chain_id| {
                    ChainId::from_str(&chain_id).unwrap_or_else(|err| {
                        eprintln!("Invalid chain ID: {}", err);
                        super::safe_exit(1)
                    })
                })
            });

            // If not specified, use the default
            let chain_id = chain_id.unwrap_or_default();

            let config = GlobalConfig::new(chain_id);
            config.write(&global_args.base_dir).unwrap_or_else(|err| {
                tracing::error!("Error writing global config file: {}", err);
                super::safe_exit(1)
            });
            config
        } else {
            eprintln!("Error reading global config: {}", err);
            super::safe_exit(1)
        }
    })
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
    fn arg_from_ctx(&self, ctx: &Context) -> T {
        T::arg_from_ctx(ctx, &self.raw)
    }
}

impl<T> FromContext<T>
where
    T: ArgFromMutContext,
{
    /// Parse and/or look-up the value from the mutable context.
    fn arg_from_mut_ctx(&self, ctx: &mut Context) -> T {
        T::arg_from_mut_ctx(ctx, &self.raw)
    }
}

/// CLI argument that found via the [`Context`].
pub trait ArgFromContext: Sized {
    fn arg_from_ctx(ctx: &Context, raw: impl AsRef<str>) -> Self;
}

/// CLI argument that found via the [`Context`] and cached (as in case of an
/// encrypted keypair that has been decrypted), hence using mutable context.
pub trait ArgFromMutContext: Sized {
    fn arg_from_mut_ctx(ctx: &mut Context, raw: impl AsRef<str>) -> Self;
}

impl ArgFromContext for Address {
    fn arg_from_ctx(ctx: &Context, raw: impl AsRef<str>) -> Self {
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

impl ArgFromMutContext for Rc<common::SecretKey> {
    fn arg_from_mut_ctx(ctx: &mut Context, raw: impl AsRef<str>) -> Self {
        let raw = raw.as_ref();
        // A keypair can be either a raw keypair in hex string
        FromStr::from_str(raw)
            .map(Rc::new)
            .unwrap_or_else(|_parse_err| {
                // Or it can be an alias
                ctx.wallet.find_key(raw).unwrap_or_else(|_find_err| {
                    eprintln!("Unknown key {}", raw);
                    safe_exit(1)
                })
            })
    }
}

impl ArgFromMutContext for common::PublicKey {
    fn arg_from_mut_ctx(ctx: &mut Context, raw: impl AsRef<str>) -> Self {
        let raw = raw.as_ref();
        // A public key can be either a raw public key in hex string
        FromStr::from_str(raw).unwrap_or_else(|_parse_err| {
            // Or it can be a public key hash in hex string
            FromStr::from_str(raw)
                .map(|pkh: PublicKeyHash| {
                    let key = ctx.wallet.find_key_by_pkh(&pkh).unwrap();
                    key.ref_to()
                })
                // Or it can be an alias that may be found in the wallet
                .unwrap_or_else(|_parse_err| {
                    let key = ctx.wallet.find_key(raw).unwrap();
                    key.ref_to()
                })
        })
    }
}
