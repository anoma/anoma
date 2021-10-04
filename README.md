# Anoma

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](./LICENSE)
[![Drone CI build status](https://ci.heliax.dev/api/badges/anoma/anoma/status.svg)](https://ci.heliax.dev/anoma/anoma)

## Overview

[Anoma](https://anoma.network/) is a sovereign, proof-of-stake blockchain protocol that enables private, asset-agnostic cash and private bartering among any number of parties. To learn more about Anoma's vision, take a look at the [Anoma Vision Paper](https://anoma.network/papers/vision-paper.pdf) or [Anoma's Whitepaper](https://anoma.network/papers/whitepaper.pdf).

This is an implementation of the Anoma protocol in Rust.

## 📓Docs

- [docs](https://anoma.github.io/anoma/): built from [docs mdBook](./docs/)
- [rustdoc](https://anoma.github.io/anoma/rustdoc/anoma/): built from the source

## Warning

> Here lay dragons: this codebase is still experimental, try at your own risk!

## 💾Installing

There's a single command to build and install Anoma executables from source (the node, the client and the wallet). This command will also verify that [Tendermint](#dependencies) with a compatible version is available and if not, attempt to install it.

```shell
make install
```

After installation, the main `anoma` executable will be available on path.

To find how to use it, check out the [User Guide section of the docs](https://anoma.github.io/anoma/user-guide/).

## ⚙️Development

```shell
# Build the provided validity predicate, transaction and matchmaker wasm modules
docker build -t anoma-wasm wasm
make build-wasm-scripts-docker

# Development build Anoma
make
```

### Using Nix

```shell
nix-shell -p crate2nix --command "crate2nix generate"
nix-build -A apps
nix-env -i ./result
```

### Before submitting a PR, pls make sure to run the following:

```shell
# Format the code
make fmt

# Lint the code
make clippy
```

## 🧾Logging

To change the log level, set `ANOMA_LOG` environment variable to one of:

- `error`
- `warn`
- `info`
- `debug`
- `trace`

The default is set to `info` for all the modules, expect for Tendermint ABCI, which has a lot of `debug` logging.

For more fine-grained logging levels settings, please refer to the [tracing subscriber docs](https://docs.rs/tracing-subscriber/0.2.18/tracing_subscriber/struct.EnvFilter.html#directives) for more information.

To switch on logging in tests that use `#[test]` macro from `test_env_log::test`, use `RUST_LOG` with e.g. `RUST_LOG=info cargo test -- --nocapture`.

## How to contribute

Please see the [contributing page](./CONTRIBUTING.md).

### Dependencies

The ledger currently requires that [Tendermint version 0.34.x](https://github.com/tendermint/tendermint) is installed and available on path. [The pre-built binaries and the source for 0.34.8 are here](https://github.com/tendermint/tendermint/releases/tag/v0.34.8), also directly available in some package managers.

This can be installed by `make install` command (which runs [scripts/install/get_tendermint.sh](scripts/install/get_tendermint.sh) script).
