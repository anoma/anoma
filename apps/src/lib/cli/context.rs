//! CLI input types can be used for command arguments

use std::env;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anoma::types::address::Address;
use anoma::types::chain::ChainId;
use anoma::types::key::*;
use anoma::types::masp::*;

use super::args;
use crate::cli::safe_exit;
use crate::client::tx::ShieldedContext;
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

/// A raw extended spending key (bech32m encoding) or an alias of an extended
/// spending key in the wallet
pub type WalletSpendingKey = FromContext<ExtendedSpendingKey>;

/// A raw payment address (bech32m encoding) or an alias of a payment address
/// in the wallet
pub type WalletPaymentAddr = FromContext<PaymentAddress>;

/// A raw full viewing key (bech32m encoding) or an alias of a full viewing key
/// in the wallet
pub type WalletViewingKey = FromContext<ExtendedViewingKey>;

/// A raw address or a raw extended spending key (bech32m encoding) or an alias
/// of either in the wallet
pub type WalletTransferSource = FromContext<TransferSource>;

/// A raw address or a raw payment address (bech32m encoding) or an alias of
/// either in the wallet
pub type WalletTransferTarget = FromContext<TransferTarget>;

/// A raw keypair (hex encoding), an alias, a public key or a public key hash of
/// a keypair that may be found in the wallet
pub type WalletKeypair = FromContext<common::SecretKey>;

/// A raw public key (hex encoding), a public key hash (also hex encoding) or an
/// alias of an public key that may be found in the wallet
pub type WalletPublicKey = FromContext<common::PublicKey>;

/// A raw address or a raw full viewing key (bech32m encoding) or an alias of
/// either in the wallet
pub type WalletBalanceOwner = FromContext<BalanceOwner>;

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
    /// The context fr shielded operations
    pub shielded: ShieldedContext,
}

impl Context {
    pub fn new(global_args: args::Global) -> Self {
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
        let wallet = Wallet::load_or_new_from_genesis(&chain_dir, move || {
            genesis_config::open_genesis_config(genesis_file_path)
        });

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
        Self {
            global_args,
            wallet,
            global_config,
            config,
            shielded: ShieldedContext::new(chain_dir),
        }
    }

    /// Parse and/or look-up the value from the context.
    pub fn get<T>(&self, from_context: &FromContext<T>) -> T
    where
        T: ArgFromContext,
    {
        from_context.arg_from_ctx(self).unwrap()
    }

    /// Try to parse and/or look-up an optional value from the context.
    pub fn get_opt<T>(&self, from_context: &Option<FromContext<T>>) -> Option<T>
    where
        T: ArgFromContext,
    {
        from_context
            .as_ref()
            .map(|from_context| from_context.arg_from_ctx(self).unwrap())
    }

    /// Parse and/or look-up the value from the context with cache.
    pub fn get_cached<T>(&mut self, from_context: &FromContext<T>) -> T
    where
        T: ArgFromMutContext,
    {
        from_context.arg_from_mut_ctx(self).unwrap()
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
            .map(|from_context| from_context.arg_from_mut_ctx(self).unwrap())
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

impl FromContext<TransferSource> {
    /// Converts this TransferSource argument to an Address. Call this function
    /// only when certain that raw represents an Address.
    pub fn to_address(&self) -> FromContext<Address> {
        FromContext::<Address> {
            raw: self.raw.clone(),
            phantom: PhantomData,
        }
    }

    /// Converts this TransferSource argument to an ExtendedSpendingKey. Call
    /// this function only when certain that raw represents an
    /// ExtendedSpendingKey.
    pub fn to_spending_key(&self) -> FromContext<ExtendedSpendingKey> {
        FromContext::<ExtendedSpendingKey> {
            raw: self.raw.clone(),
            phantom: PhantomData,
        }
    }
}

impl FromContext<TransferTarget> {
    /// Converts this TransferTarget argument to an Address. Call this function
    /// only when certain that raw represents an Address.
    pub fn to_address(&self) -> FromContext<Address> {
        FromContext::<Address> {
            raw: self.raw.clone(),
            phantom: PhantomData,
        }
    }

    /// Converts this TransferTarget argument to a PaymentAddress. Call this
    /// function only when certain that raw represents a PaymentAddress.
    pub fn to_payment_address(&self) -> FromContext<PaymentAddress> {
        FromContext::<PaymentAddress> {
            raw: self.raw.clone(),
            phantom: PhantomData,
        }
    }
}

impl<T> FromContext<T>
where
    T: ArgFromContext,
{
    /// Parse and/or look-up the value from the context.
    fn arg_from_ctx(&self, ctx: &Context) -> Result<T, String> {
        T::arg_from_ctx(ctx, &self.raw)
    }
}

impl<T> FromContext<T>
where
    T: ArgFromMutContext,
{
    /// Parse and/or look-up the value from the mutable context.
    fn arg_from_mut_ctx(&self, ctx: &mut Context) -> Result<T, String> {
        T::arg_from_mut_ctx(ctx, &self.raw)
    }
}

/// CLI argument that found via the [`Context`].
pub trait ArgFromContext: Sized {
    fn arg_from_ctx(
        ctx: &Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String>;
}

/// CLI argument that found via the [`Context`] and cached (as in case of an
/// encrypted keypair that has been decrypted), hence using mutable context.
pub trait ArgFromMutContext: Sized {
    fn arg_from_mut_ctx(
        ctx: &mut Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String>;
}

impl ArgFromContext for Address {
    fn arg_from_ctx(
        ctx: &Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // An address can be either raw (bech32m encoding)
        FromStr::from_str(raw)
            // Or it can be an alias that may be found in the wallet
            .or_else(|_| {
                ctx.wallet
                    .find_address(raw)
                    .cloned()
                    .ok_or_else(|| format!("Unknown address {}", raw))
            })
    }
}

impl ArgFromMutContext for common::SecretKey {
    fn arg_from_mut_ctx(
        ctx: &mut Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // A keypair can be either a raw keypair in hex string
        FromStr::from_str(raw).or_else(|_parse_err| {
            // Or it can be an alias
            ctx.wallet
                .find_key(raw)
                .map_err(|_find_err| format!("Unknown key {}", raw))
        })
    }
}

impl ArgFromMutContext for common::PublicKey {
    fn arg_from_mut_ctx(
        ctx: &mut Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // A public key can be either a raw public key in hex string
        FromStr::from_str(raw).or_else(|_parse_err| {
            // Or it can be a public key hash in hex string
            FromStr::from_str(raw)
                .map(|pkh: PublicKeyHash| {
                    let key = ctx.wallet.find_key_by_pkh(&pkh).unwrap();
                    key.ref_to()
                })
                // Or it can be an alias that may be found in the wallet
                .or_else(|_parse_err| {
                    ctx.wallet
                        .find_key(raw)
                        .map(|x| x.ref_to())
                        .map_err(|x| x.to_string())
                })
        })
    }
}

impl ArgFromMutContext for ExtendedSpendingKey {
    fn arg_from_mut_ctx(
        ctx: &mut Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // Either the string is a raw extended spending key
        FromStr::from_str(raw).or_else(|_parse_err| {
            // Or it is a stored alias of one
            ctx.wallet
                .find_spending_key(raw)
                .map_err(|_find_err| format!("Unknown spending key {}", raw))
        })
    }
}

impl ArgFromMutContext for ExtendedViewingKey {
    fn arg_from_mut_ctx(
        ctx: &mut Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // Either the string is a raw full viewing key
        FromStr::from_str(raw).or_else(|_parse_err| {
            // Or it is a stored alias of one
            ctx.wallet
                .find_viewing_key(raw)
                .map(Clone::clone)
                .map_err(|_find_err| format!("Unknown viewing key {}", raw))
        })
    }
}

impl ArgFromContext for PaymentAddress {
    fn arg_from_ctx(
        ctx: &Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // Either the string is a payment address
        FromStr::from_str(raw).or_else(|_parse_err| {
            // Or it is a stored alias of one
            ctx.wallet
                .find_payment_addr(raw)
                .cloned()
                .ok_or_else(|| format!("Unknown payment address {}", raw))
        })
    }
}

impl ArgFromMutContext for TransferSource {
    fn arg_from_mut_ctx(
        ctx: &mut Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // Either the string is a transparent address or a spending key
        Address::arg_from_ctx(ctx, raw)
            .map(Self::Address)
            .or_else(|_| {
                ExtendedSpendingKey::arg_from_mut_ctx(ctx, raw)
                    .map(Self::ExtendedSpendingKey)
            })
    }
}

impl ArgFromContext for TransferTarget {
    fn arg_from_ctx(
        ctx: &Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // Either the string is a transparent address or a payment address
        Address::arg_from_ctx(ctx, raw)
            .map(Self::Address)
            .or_else(|_| {
                PaymentAddress::arg_from_ctx(ctx, raw).map(Self::PaymentAddress)
            })
    }
}

impl ArgFromMutContext for BalanceOwner {
    fn arg_from_mut_ctx(
        ctx: &mut Context,
        raw: impl AsRef<str>,
    ) -> Result<Self, String> {
        let raw = raw.as_ref();
        // Either the string is a transparent address or a viewing key
        Address::arg_from_ctx(ctx, raw)
            .map(Self::Address)
            .or_else(|_| {
                ExtendedViewingKey::arg_from_mut_ctx(ctx, raw)
                    .map(Box::new)
                    .map(Self::FullViewingKey)
            })
    }
}
