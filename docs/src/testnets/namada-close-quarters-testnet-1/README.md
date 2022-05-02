# Namada Close Quarters Testnet 1

(WIP)

This testnet introduces the following new features:
- [on-chain governance](../../user-guide/ledger/governance.md) - create and vote for proposals both onchain and offchain
- [MASP (multi-asset shielded pool) transfers](../internal-testnet-1/README.md#shielded-transfers) - make private transfers of any Namada token

Future testnets will include more features as described in [the Namada spec](https://specs.anoma.net/master/architecture/namada.html), like IBC (inter-blockchain communication protocol), bridging to the Ethereum blockchain and more.

## Status
- 2021-05-02 - we are currently preparing to launch the chain. Applications are open for people to be genesis validators.

## Chain information
Chain ID: TBD
Links to download prebuilt binaries: TODO - will be automatically uploaded from the branch tag CI

If you want to compile the binaries from source yourself, make sure you have checked out the specific tag `TODO-x.x.x-prerelease` to build from. i.e.

```shell
git checkout tags/TODO-x.x.x-prerelease -b namada-close-quarters-testnet-1
```

Then follow [the building from source guide](../../user-guide/install.md#from-source).

## Applying to be a genesis validator

### Prerequisites
- a machine that meets the [requirements](../../user-guide/install.md#hardware-requirements) for running a validator node
- an associated public IPv4 address with ports 26656 and 26657 reachable from anywhere

### Set up
Follow [this guide](../../user-guide/genesis-validator-setup.md#Pre-genesis) on how to generate your "pre-genesis" validator files.

After this, you'll have a `validator.toml` file, the contents of which will look something like the following:

```toml
[validator.1337-validator]
consensus_public_key = "00056fff5232da385d88428ca2bb2012a4d83cdf5c697864dde34b393333a72268"
account_public_key = "00f1bd321be2e23b9503653dd50fcd5177ca43a0ade6da60108eaecde0d68abdc8"
staking_reward_public_key = "005725f952115838590fc7c5dd9590bc054ac4bd5af55672a40df4ac7dca50ce97"
protocol_public_key = "0054c213d2f8fe2dd3fc5a41a52fd2839cb49643d960d7f75e993202692c5d8783"
dkg_public_key = "6000000054eafa7320ddebf00c9487e5f7ea5107a8444f042b74caf9ed5679163f854577bf4d0992a8fd301ec4f3438c9934c617a2c71649178e536f7e2a8cdc1f8331139b7fd9b4d36861f0a9915d83f61d7f969219f0eba95bb6fa45595425923d4c0e"
net_address = "1.2.3.4:26656"
tendermint_node_key = "00e1a8fe1abceb700063ab4558baec680b64247e2fd9891962af552b9e49318d8d"
```

This file contains only public information and is safe to share publicly. If you want to be a genesis validator for this testnet, please make a pull request to [https://github.com/anoma/namada-testnets](https://github.com/anoma/namada-testnets) adding your `validator.toml` file to the `namada-close-quarters-testnet-1/` directory, renaming it to `$alias.toml`. e.g. if you chose your alias to be "bertha", submit the file with the name `bertha.toml`. You can see what an example PR looks like [here](https://github.com/anoma/namada-testnets/pull/1).
