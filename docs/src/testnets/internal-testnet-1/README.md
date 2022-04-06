# Internal Testnet 1
*Last updated on **2022-04-06** by **James Hiew***

Latest values regarding the testnet that would be useful to have in your shell:

```bash
export ANOMA_TESTNET_CHAIN_ID='anoma-masp-0.3.51d2f83a8412b95'
export ANOMA_TESTNET_BRANCH='internal/testnet-n1'
export ANOMA_TESTNET_COMMIT='0184e64e044366ec370d1431ddf4691b4bd3a5b4'
```

## Getting the binaries
Specific `anoma` binaries should be used when joining or interacting with the testnet.

### Downloading prebuilt binaries
There may be prebuilt binaries available for your OS and architecture [in Google Drive](https://drive.google.com/drive/folders/1MM-HOkxDgcbgKbTn8E2xVHVKPhiKBI9C?usp=sharing). If not, you will have to compile from source.

### Compiling from source
- Checkout the commit being used with the testnet -e.g. `git checkout $ANOMA_TESTNET_COMMIT`
- Run `make package` which should result in an archive containing the binaries named like `anoma-v0.5.0-49-g0184e64e0-Linux-x86_64.tar.gz`

## Downloading parameters
- Download `masp-params.tar.gz` [from Google Drive](https://drive.google.com/drive/folders/1MM-HOkxDgcbgKbTn8E2xVHVKPhiKBI9C?usp=sharing)
- Extract masp params file `masp-params.tar.gz`
    - Linux: into home dir as follow `~/.masp-params`
    - Mac OS: into `~/Library/Application Support/MASPParams`

## Run MASP Testnet
**NOTE** Check the [prerequisites](#prerequisites) before trying to start a node from binaries.

Executing the commands below inside the folder with the testnet anoma binaries should start a node:
```bash
./anomac utils join-network --chain-id $ANOMA_TESTNET_CHAIN_ID
./anoma ledger
```

## Using shielded transfers

Shielded balances are owned by a particular spending key. In this
testnet, spending keys are just arbitrary hexadecimal strings, provided
on the command line.

To try out shielded transfers, you will first need an ordinary
transparent account with some token balance. Example commands for that:

```
anomaw address gen --alias my-implicit
anomac init-account --source my-implicit --public-key my-implicit --alias my-established
anomac transfer --token btc --amount 1000 --source faucet --target my-established --signer my-established
```

The testnet tokens which the faucet can provide you are named `XAN`,
`BTC`, `ETH`, `DOT`, `Schnitzel`, `Apfel`, and `Kartoffel`. The faucet
will transfer these in increments of 1000 at a time.

Once you have an ordinary transparent account with some tokens, you
should select a spending key to hold your shielded balances. This is
just some hexadecimal string, like `1234`, `abcd`, or
`030cdcc0a43765d4645e22adbf9944b58c646d162c9a08890a08cc49a9580c9e`. Try
to select one that is unique to you (i.e., probably don't use `1234`);
you could randomly generate one with e.g. `openssl rand -hex 32`. The
wallet does not yet support spending keys, so make a note of yours
somewhere.

Shielded transfers work with the `anomac transfer` command, but either
`--source`, `--target`, or both are replaced. `--source` may be replaced
with `--spending-key` to spend a shielded balance, but if you are
following along, you don't have a shielded balance to spend yet.
`--target` may be replaced with `--payment-address` to create a shielded
balance.

To create a payment address from your spending key, use:

```
anomaw masp gen-payment-addr --spending-key [your spending key]
```

This will generate a different payment address each time you run it.
Payment addresses can be reused or discarded as you like, and can't be
correlated with one another.

Once you have a payment address, transfer a balance from your
transparent account to your shielded spending key with something like:

```
anomac transfer --source my-established --payment-address [your payment address] --token btc --amount 100
```

Once this transfer goes through, you can view your spending key's
balance:

```
anomac balance --spending-key [your spending key]
```

However, your spending key is the secret key to all your shielded
balances, and you may not want to use it just to view balances. For this
purpose, you can derive the viewing key:

```
anomaw masp derive-view-key --spending-key [your spending key]
anomac balance --viewing-key [your viewing key]
```

The viewing key can also be used to generate payment addresses, with
e.g. `anomaw masp gen-payment-addr --viewing-key [your viewing key]`.

Now that you have a shielded balance, it can either be transferred to a
different shielded payment address (shielded to shielded):

```
anomac transfer --spending-key [your spending key] --payment-address [someone's payment address] --token btc --amount 50 --signer my-established
```

or to a transparent account (shielded to transparent):

```
anomac transfer --spending-key [your spending key] --target [some transparent account] --token btc --amount 50 --signer my-established
```

Note that for both of these types of transfer, `--signer` must be
specified. However, any transparent account can sign these transactions.

## Hardware Requirements

This section covers the minimum and recommended hardware requirements for engaging with the Namada as a validator node.

### Minimal Hardware Requirements

| Hardware | Minimal Specifications |
| -------- | -------- |
| CPU     | x86_64 (Intel, AMD) processor with at least 4 physical cores     |
| RAM     | 16GB DDR4     |
| Storage     | at least 60GB SSD (NVMe SSD is recommended. HDD will be enough for localnet only)    |

## Run a Node from Prebuilt Binaries

This is a quickstart guide to install and run Anoma. If you want to install Anoma from Source, Docker or Nix check section [Install Anoma](https://docs.anoma.network/v0.5.0/user-guide/install.html) and then come back to this section to run your node.

### Install Anoma

#### From Prebuilt Binaries

##### Prerequisites

- [Rust](https://www.rust-lang.org/tools/install)
- [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
- Tendermint 0.34.x pre-installed:
  - [Download any of the pre-built binaries for versions 0.34.x](https://github.com/tendermint/tendermint/releases) and install it using the instructions on the [Tendermint guide](https://docs.tendermint.com/master/introduction/install.html)
  - or use the script from the Anoma repo [`scripts/install/get_tendermint.sh`](https://github.com/anoma/anoma/blob/master/scripts/install/get_tendermint.sh). This is used by the make install command (if you’re installing from the source).
- GLIBC v2.29 or higher
  - On Ubuntu, you can check your glibc version with `ldd --version`
  - By default, the highest version of GLIBC for Ubuntu 18.04 should be 2.27. The best way to go for Ubuntu 18.04 is to build from source, but if you still want to upgrade to a higher version then follow one of the following points:
    - The second option is to migrate your application to a system that supports GLIBC higher than or equal to 2.29. This would mean a lot of work though. It seems Ubuntu 19.04 actually uses that version.
    - The thired option would be to actually build your GLIBC from source using the version you want or need. I’ve researched it a bit and found a website which actually gives you the steps for you to build the package from source : <http://www.linuxfromscratch.org/lfs/view/9.0-systemd/chapter05/glibc.html>

###### Optional prerequisites

- [Tmux](https://github.com/tmux/tmux/wiki/Installing) Terminal Multiplexer (to keep the node running in the background)

*Note: we don't support Windows at this point in time.*

- Open your favorite Terminal
- Install the [Anoma release v0.3.1](https://github.com/anoma/anoma/releases/tag/v0.3.1) on your machine by running:

**Linux**

```bash
curl -LO https://github.com/anoma/anoma/releases/download/v0.3.1/anoma-v0.3.1-Linux-x86_64.tar.gz
tar -xzf anoma-v0.3.1-Linux-x86_64.tar.gz
cd anoma-v0.3.1-Linux-x86_64
```

**MacOS**

```bash
curl -LO https://github.com/anoma/anoma/releases/download/v0.3.1/anoma-v0.3.1-Darwin-x86_64.tar.gz
tar -xzf anoma-v0.3.1-Darwin-x86_64.tar.gz
cd anoma-v0.3.1-Darwin-x86_64
```

#### From Source

##### Prerequisites

To build from source you will have to install some dependencies:

- [Rust](https://www.rust-lang.org/tools/install)
- [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
- Clang
- OpenSSL
- LLVM

For ubuntu users, the following command should install everything necessary:

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
sudo apt-get install -y make git-core libssl-dev pkg-config libclang-12-dev
```

Clone the source code from the [Github Anoma repo](https://github.com/anoma/anoma), then build and install anoma with the following commands:

```
git clone https://github.com/anoma/anoma.git --single-branch --branch internal/testnet-n1
cd anoma
make install
```

### Run Anoma

- Configure your node to join a network:

```
./anomac utils join-network --chain-id=$ANOMA_TESTNET_CHAIN_ID
```

This should output something like the following:

```
Downloading config release from https://github.com/heliaxdev/anoma-network-config/releases/download/anoma-masp-0.3.51d2f83a8412b95/anoma-masp-0.3.51d2f83a8412b95.tar.gz ...
Successfully configured for chain ID anoma-masp-0.3.51d2f83a8412b95
```

- Run the ledger into a tmux session:

```
tmux new -s anoma
./anoma ledger
```

Detach from the anoma tmux session by pressing `Ctrl-B then D`.
Attach again to your anoma tmux session by running:

```
tmux a -t anoma
```

### Troubleshooting

#### Build from Source

This is required to build the wasm validity predicates.

```
rustup target add wasm32-unknown-unknown
```

Build the provided validity predicate, transaction and matchmaker wasm modules

```
make build-wasm-scripts-docker
```

#### Node is not starting

**No state could be found**
If you get the following log, it means that tendermint is not installed properly on your machine. To solve this issue follow the [prerequisites](#prerequisites).

```bash
2022-03-30T07:21:09.212187Z  INFO anoma_apps::cli::context: Chain ID: anoma-masp-0.3.51d2f83a8412b95
2022-03-30T07:21:09.213968Z  INFO anoma_apps::node::ledger: Available logical cores: 8
2022-03-30T07:21:09.213989Z  INFO anoma_apps::node::ledger: Using 4 threads for Rayon.
2022-03-30T07:21:09.213994Z  INFO anoma_apps::node::ledger: Using 4 threads for Tokio.
2022-03-30T07:21:09.217867Z  INFO anoma_apps::node::ledger: VP WASM compilation cache size not configured, using 1/6 of available memory.
2022-03-30T07:21:09.218908Z  INFO anoma_apps::node::ledger: Available memory: 15.18 GiB
2022-03-30T07:21:09.218934Z  INFO anoma_apps::node::ledger: VP WASM compilation cache size: 2.53 GiB
2022-03-30T07:21:09.218943Z  INFO anoma_apps::node::ledger: Tx WASM compilation cache size not configured, using 1/6 of available memory.
2022-03-30T07:21:09.218947Z  INFO anoma_apps::node::ledger: Tx WASM compilation cache size: 2.53 GiB
2022-03-30T07:21:09.218954Z  INFO anoma_apps::node::ledger: Block cache size not configured, using 1/3 of available memory.
2022-03-30T07:21:09.218959Z  INFO anoma_apps::node::ledger: RocksDB block cache size: 5.06 GiB
2022-03-30T07:21:09.218996Z  INFO anoma_apps::node::ledger::storage::rocksdb: Using 2 compactions threads for RocksDB.
2022-03-30T07:21:09.219196Z  INFO anoma_apps::node::ledger: Tendermint node is no longer running.
2022-03-30T07:21:09.232544Z  INFO anoma::ledger::storage: No state could be found
2022-03-30T07:21:09.232709Z  INFO anoma_apps::node::ledger: Tendermint has exited, shutting down...
2022-03-30T07:21:09.232794Z  INFO anoma_apps::node::ledger: Anoma ledger node started.
2022-03-30T07:21:09.232849Z  INFO anoma_apps::node::ledger: Anoma ledger node has shut down.
```
