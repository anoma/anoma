# Anoma

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](./LICENSE)
[![Drone CI build status](https://ci.heliax.dev/api/badges/anoma/anoma/status.svg)](https://ci.heliax.dev/anoma/anoma)

## Namada

This repo contains an early prototype of Anoma. A lot of current development is happening on [Namada](https://github.com/anoma/namada). Please go there to check out the latest status.

## Overview

[Anoma](https://anoma.net/) is an intent-centric, privacy-preserving protocol for decentralized counterparty discovery, solving, and multi-chain atomic settlement. To learn more about Anoma's vision, take a look at the [Anoma Vision Paper](https://anoma.net/vision-paper.pdf).

This is an implementation of the Anoma protocol in Rust.

## Warning

> Here lay dragons: this codebase is still experimental, try at your own risk!

## 💾 Installing

There is a single command to build and install Anoma executables from source (the node, the client and the wallet). This command will also verify that a compatible version of [Tendermint](#dependencies) is available and if not, attempt to install it. Note that currently at least 16GB RAM is needed to build from source.

```shell
make install
```

After installation, the main `anoma` executable will be available on path.

To find how to use it, check out the [User Guide section of the docs](https://docs.anoma.net/user-guide/).

If you have Nix, you may opt to build and install Anoma using Nix. The Nix
integration also takes care of making a compatible version of Tendermint
available.

```shell
# Nix 2.4 and later
nix profile install

# All versions of Nix
nix-env -f . -iA anoma
```

For more detailed instructions and more install options, see the [Install
section](https://docs.anoma.net/user-guide/install.html) of the User
Guide.

## ⚙️ Development

```shell
# Build the provided validity predicate, transaction and matchmaker wasm modules
make build-wasm-scripts-docker

# Development (debug) build Anoma, which includes a validator and some default 
# accounts, whose keys and addresses are available in the wallet
ANOMA_DEV=true make
```

### Using Nix

You may opt to get all of the dependencies to develop Anoma by entering the
development shell:

```shell
# Nix 2.4 and above
nix develop

# All versions of Nix
nix-shell
```

Inside the shell, all of the `make` targets work as usual:

```shell
# Build the WASM modules without docker
make build-wasm-scripts

# Development build (uses cargo)
ANOMA_DEV=true make
```

---

It is also possible to use the Nix Rust infrastructure instead of Cargo to
build the project crates. This method uses `crate2nix` to derive Nix
expressions from `Cargo.toml` and `Cargo.lock` files. The workspace members are
exposed as packages in `flake.nix` with a `rust_` prefix. Variants where the
`ABCI-plus-plus` feature flag is enabled are exposed with a `:ABCI-plus-plus`
suffix.

```shell
# List all packages
nix flake show

# Build the `anoma_apps` crate with `ABCI-plus-plus` feature
nix build .#rust_anoma_apps:ABCI-plus-plus

# Build the (default) anoma package. It consists of wrappers for the Anoma
# binaries (`rust_anoma_apps`) that ensure `tendermint` is in `PATH`.
nix build .#anoma
```

Advantages:

- Excellent build reproducibility (all dependencies pinned).
- Individual crates are stored as Nix derivations and therefore cached in the
  Nix store.
- Makes it possible to build Nix derivations of the binaries. Cargo build
  doesn't work in the Nix build environment because network access is not
  allowed, meaning that Cargo can't fetch dependencies; `cargo vendor` could be
  used to prefetch everything for Cargo, but `cargo vendor` does not work on
  our project at the moment.

Disadvantages:

- Only works for Linux and Darwin targets. WASM builds in particular are not
  possible with this method. Although, while `crate2nix` doesn't support
  targeting WASM, we should be able to build the WASM modules via Cargo - if
  only `cargo vendor` worked.

__Note:__ If you have modified the Cargo dependencies (changed `Cargo.lock`),
it is necessary to recreate the `Cargo.nix` expressions with `crate2nix`.
Helpers are provided as flake apps (Nix 2.4 and later):

```shell
nix run .#generateCargoNix
nix run .#generateCargoNixABCI-plus-plus
```

### Before submitting a PR, pls make sure to run the following

```shell
# Format the code
make fmt

# Lint the code
make clippy
```

## 🧾 Logging

To change the log level, set `ANOMA_LOG` environment variable to one of:

- `error`
- `warn`
- `info`
- `debug`
- `trace`

The default is set to `info` for all the modules, expect for Tendermint ABCI, which has a lot of `debug` logging.

For more fine-grained logging levels settings, please refer to the [tracing subscriber docs](https://docs.rs/tracing-subscriber/0.2.18/tracing_subscriber/struct.EnvFilter.html#directives) for more information.

To switch on logging in tests that use `#[test]` macro from `test_log::test`, use `RUST_LOG` with e.g. `RUST_LOG=info cargo test -- --nocapture`.

## How to contribute

Please see the [contributing page](./CONTRIBUTING.md).

### Dependencies

The ledger currently requires that [Tendermint version 0.34.x](https://github.com/tendermint/tendermint) is installed and available on path. [The pre-built binaries and the source for 0.34.8 are here](https://github.com/tendermint/tendermint/releases/tag/v0.34.8), also directly available in some package managers.

This can be installed by `make install` command (which runs [scripts/install/get_tendermint.sh](scripts/install/get_tendermint.sh) script).
