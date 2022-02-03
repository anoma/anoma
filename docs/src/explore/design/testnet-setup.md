# Testnet setup

Starting from a release branch, we configure the network that will run on this release.

## Step 1: Prepare a genesis configuration file

Prepare a genesis configuration file. You can start from one of the source files in the [anoma-network-config repo](https://github.com/heliaxdev/anoma-network-config/tree/master/src) or the source files inside the `genesis` directory in this repository, or start from scratch. Note that in this file, for any account for which you don't specify address and/or keys, they will be automatically generated in the next step and saved in wallet(s) in the network "setup" directory.

Additionally, for validator accounts you should also specify their `net_address`. Note that for each validator node we're using up to 5 ports (3 for the ledger and 2 for the intent gossip), so if multiple validators are running behind the same public IP, their ports should be increments of 5 (e.g. `26656`, `26661`, ...). A port supplied in the `net_address` will be used for the node's P2P address. The ledger's RPC address address is its `{port + 1}` and the Anoma ledger's port is `{port + 2}`. The intent gossip will run on `{port + 3}` and its RPC server at `{post + 4}`.

In the genesis file, also set the `genesis_time` in [RFC3339](https://www.ietf.org/rfc/rfc3339.txt) format, e.g. `2021-09-30T10:00:00Z`. It's the time the blockchain started or will start. If nodes are started before this time they will sit idle until the time specified.

## Step 2: Initialize new network using the utils command

- Choose a `chain_prefix` for a new chain ID (e.g. `anoma-feigenbaum-0`). Allows up to 19 alphanumeric characters and `.`, `-` and `_`.
- Run `anomac utils init-network --genesis-path genesis/{file_from_last_step}.toml --chain-prefix {chain_prefix}` to (note that you can also specify other options, for example `--localhost` to setup a local network, `--allow-duplicate-ip` to allow multiple ledger nodes to run under the same IP address, which is useful for testnets):
  - Generate a new `chain_id` with the chosen `chain_prefix` (up to 19 chars), a separator char `.` and the hash of the genesis data, 30 characters long in total (`shared/src/types/chain.rs`)
  - For each validator (index `n`), prepare the base directory under `{base_dir}/{chain_prefix}/setup/validator_{n}` with:
    - A wallet with the validator's addresses and keys
    - Tendermint config with its private validator key (consensus key) and node key (from which its node ID is derived)
    - Tendermint data private validator state file (this is required by Tendermint)
    - Add global config, chain config and genesis file
    - Set ledger's config `p2p_pex` to `false`
    - Update chain config ledger's address, P2P and RPC address and intent gossip's config P2P and RPC address.
  - Write genesis config file, and ledger and intent gossip configs
  - Save the genesis file to the `{base_dir}/{chain_id}.toml` and print the full path
  - Generate a global config in `{base_dir}/global-config.toml` with the {chain_id}
  - Print the chain ID and the path to the network's genesis file
  - Create a public release archive file with the genesis file and global and chain config files and print its name (`{chain_id}.tar.gz`)
- Verify that the configs are valid and can be parsed by running `cargo run --package anoma_apps --no-default-features --features std --bin anoman ledger` (TODO add a sub-cmd to verify the genesis config before its finalized - at the end of step 1)

## Step 3: Deploy

- Upload the validator directories to host(s)
  - each validator's wallet and config is prepared under `{base_dir}/{chain_id}/setup/{validator_alias}`
- Distribute:
  - `{base_dir}/global-config.toml`
  - `{base_dir}/{chain_id}/config.toml`
  - `{base_dir}/{chain_id}/genesis.toml`
