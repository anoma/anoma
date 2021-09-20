# Anoma

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](./LICENSE)
[![Drone CI build status](https://ci.heliax.dev/api/badges/anoma/anoma/status.svg)](https://ci.heliax.dev/anoma/anoma)

## Overview

[Anoma](https://anoma.network/) is a sovereign, proof-of-stake blockchain protocol that enables private, asset-agnostic cash and private bartering among any number of parties.

This is an implementation of the Anoma ledger in Rust.

## Docs

- [docs](https://anoma.github.io/anoma/): built from [docs mdBook](./docs/)
- [rustdoc](https://anoma.github.io/anoma/rustdoc/anoma/): built from the source

## Warning

> Here lay dragons: this codebase is still experimental, try at your own risk!

## Installing

### Dependencies

The ledger currently requires that [Tendermint version 0.34.x](https://github.com/tendermint/tendermint) is installed and available on path. [The pre-built binaries and the source for 0.34.8 are here](https://github.com/tendermint/tendermint/releases/tag/v0.34.8), also directly available in some package managers.

### Notes

The transaction code can currently be built from [tx_template](wasm/tx_template) and validity predicates from [vp_template](wasm/vp_template), which is Rust code compiled to wasm.

The transaction template calls functions from the host environment. The validity predicate template can validate a transaction and the storage key changes that is has performed.

The matchmaker template receives intents with the borsh encoding define in `data_template` and crafts data to be sent with `tx_intent_template` to the ledger. It matches only two intents that are the exact opposite.

### Instructions

```shell
# Build the provided validity predicate, transaction and matchmaker wasm modules
docker build -t anoma-wasm wasm
make build-wasm-scripts-docker

# Build Anoma
make
```

### Using Nix

```shell
nix-shell -p crate2nix --command "crate2nix generate"
nix-build -A apps
nix-env -i ./result
```

## Running an Anoma node

```shell
# Run Anoma node (this will also initialize and run Tendermint node)
make run-ledger

# Reset the state (resets Tendermint too)
make reset-ledger

```

## Interacting with Anoma

### Anoma Wallet

The wallet is stored under `.anoma/wallet.toml` (with the default `--base-dir`), which will be created if it doesn't already exist. A newly created wallet will be pre-loaded with some default keys and addresses for development.

The ledger and intent gossip commands that use keys and addresses may use their aliases as defined in the wallet.

```shell
# Manage keys, various sub-commands are available, see the commands' `--help`
cargo run --bin anomaw key
# List all known keys
cargo run --bin anomaw key list

# Manage addresses, again, various sub-commands are available
cargo run --bin anomaw address
# List all known addresses
cargo run --bin anomaw address list
```

### Anoma Ledger

```shell
# Submit a token transfer
cargo run --bin anomac transfer --source Bertha --target Albert --token XAN --amount 10.1

# Query token balances (various options are available, see the command's `--help`)
cargo run --bin anomac balance --token XAN

# Query the current epoch
cargo run --bin anomac epoch

# Submit a transaction to update an account's validity predicate
cargo run --bin anomac update --address Bertha --code-path wasm/vp_user.wasm
```

### Interacting with the PoS system

The PoS system is using the `XAN` token.

```shell
# Submit a self-bond of tokens for a validator
cargo run --bin anomac bond --validator validator --amount 3.3

# Submit a delegation of tokens for a source address to the validator
cargo run --bin anomac bond --source Bertha --validator validator --amount 3.3

# Submit an unbonding of a self-bond of tokens from a validator
cargo run --bin anomac unbond --validator validator --amount 3.3

# Submit an unbonding of a delegation of tokens from a source address to the validator
cargo run --bin anomac unbond --source Bertha --validator validator --amount 3.3

# Submit a withdrawal of tokens of unbonded self-bond back to its validator validator
cargo run --bin anomac withdraw --validator validator

# Submit a withdrawal of unbonded delegation of tokens back to its source address
cargo run --bin anomac withdraw --source Bertha --validator validator

# Queries (various options are available, see the commands' `--help`)
cargo run --bin anomac bonds
cargo run --bin anomac slashes
cargo run --bin anomac voting-power
```

### Anoma Intent Gossip

```shell
# run gossip node with intent gossip system and rpc server (use default config)
cargo run --bin anoma gossip --rpc "127.0.0.1:39111"

# run gossip node with intent gossip system, matchmaker and rpc (use default config)
cargo run --bin anoman gossip --rpc "127.0.0.1:39111" --matchmaker-path wasm/mm_token_exch.wasm --tx-code-path wasm/tx_from_intent.wasm --ledger-address "127.0.0.1:26657"

# Prepare intents:
# 1) We'll be using these addresses in the intents:
export ALBERT=a1qq5qqqqqg4znssfsgcurjsfhgfpy2vjyxy6yg3z98pp5zvp5xgersvfjxvcnx3f4xycrzdfkak0xhx
export BERTHA=a1qq5qqqqqxv6yydz9xc6ry33589q5x33eggcnjs2xx9znydj9xuens3phxppnwvzpg4rrqdpswve4n9
export CHRISTEL=a1qq5qqqqqxsuygd2x8pq5yw2ygdryxs6xgsmrsdzx8pryxv34gfrrssfjgccyg3zpxezrqd2y2s3g5s
export XAN=a1qq5qqqqqxuc5gvz9gycryv3sgye5v3j9gvurjv34g9prsd6x8qu5xs2ygdzrzsf38q6rss33xf42f3
export BTC=a1qq5qqqqq8q6yy3p4xyurys3n8qerz3zxxeryyv6rg4pnxdf3x3pyv32rx3zrgwzpxu6ny32r3laduc
export ETH=a1qq5qqqqqx3z5xd3ngdqnzwzrgfpnxd3hgsuyx3phgfry2s3kxsc5xves8qe5x33sgdprzvjptzfry9
# 2) Create file containing the json representation of the intent:
echo '[{"addr":"'$ALBERT'","key":"'$ALBERT'","max_sell":"300","min_buy":"50","rate_min":"0.7","token_buy":"'$BTC'","token_sell":"'$ETH'"}]' > intent.A.data

echo '[{"addr":"'$BERTHA'","key":"'$BERTHA'","max_sell":"70","min_buy":"100","rate_min":"2","token_buy":"'$XAN'","token_sell":"'$BTC'","vp_path": "wasm_for_tests/vp_always_true.wasm"}]' > intent.B.data

echo '[{"addr":"'$CHRISTEL'","key":"'$CHRISTEL'","max_sell":"200","min_buy":"20","rate_min":"0.5","token_buy":"'$ETH'","token_sell":"'$XAN'"}]' > intent.C.data

# 3) Instruct the matchmaker to subscribe to new network:
cargo run --bin anomac subscribe-topic --node "http://127.0.0.1:39111" --topic "asset_v1"

# 4) Submit the intents (the target gossip node need to run an RPC server):
cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.A.data --topic "asset_v1" --signing-key Albert
cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.B.data --topic "asset_v1" --signing-key Bertha
cargo run --bin anomac intent --node "http://127.0.0.1:39111" --data-path intent.C.data --topic "asset_v1" --signing-key Christel
```

## Development

```shell
# Format the code
make fmt

# Lint the code
make clippy
```

## Logging

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
