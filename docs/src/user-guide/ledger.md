# The Anoma Ledger

To start a local ledger node, run:

```bash
anoma ledger
```

This will start up the Anoma ledger, which should attempt to connect to the persistent validator nodes and other peers in the network.

By default, the ledger will store its configuration and state in the `.anoma` directory relative to the current working directory. One can use `--base-dir` CLI global argument or `ANOMA_BASE_DIR` environment variable to change it.

If not found on start up, the ledger will generate a configuration file `.anoma/config.toml`.

The ledger also needs access to the built WASM files that are used in the genesis block. These files are included in release and shouldn't be modified, otherwise your node will fail with consensus error on the genesis block. By default, these are expected to be in the "wasm" directory, relative to the current working directory. This can also be set by `--wasm-dir` CLI global argument, `ANOMA_WASM_DIR` environment variable or the configuration file.

## Basic transactions and queries

Submit a token transfer:

```bash
anoma client transfer --source Bertha --target Albert --token XAN --amount 10.1
```

Query token balances (various options are available, see the command's `--help`):

```bash
anoma client balance --token XAN
```

Query the current epoch:

```bash
anoma client epoch
```

Submit a transaction to update an account's validity predicate:

```bash
anoma client update --address Bertha --code-path wasm/vp_user.wasm
```

## Interacting with the PoS system

The PoS system is using the `XAN` token as the only staking token.

Submit a self-bond of tokens for a validator:

```bash
anoma client bond --validator validator --amount 3.3
```

Submit a delegation of tokens for a source address to the validator:

```bash
anoma client bond --source Bertha --validator validator --amount 3.3
```

Submit an unbonding of a self-bond of tokens from a validator:

```bash
anoma client unbond --validator validator --amount 3.3
```

Submit an unbonding of a delegation of tokens from a source address to the validator:

```bash
anoma client unbond --source Bertha --validator validator --amount 3.3
```

Submit a withdrawal of tokens of unbonded self-bond back to its validator validator:

```bash
anoma client withdraw --validator validator
```

Submit a withdrawal of unbonded delegation of tokens back to its source address:

```bash
anoma client withdraw --source Bertha --validator validator
```

Queries (various options are available, see the commands' `--help`):

```bash
anoma client bonds
anoma client slashes
anoma client voting-power
```

### TODO: validator registration

## TODO: custom tx/VPs