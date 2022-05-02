# Namada Close Quarters Testnet 1

(WIP)

## Info
Chain ID: TBD

Link to download prebuilt binaries: TBD

If you want to compile the binaries from source yourself, please usethe head of the [`tomas/gov-and-masp`](https://github.com/anoma/anoma/tree/tomas/gov-and-masp) branch.

### Current status
- 2021-05-02 - we are currently preparing for chain genesis and open for applications from people to be genesis validators

### Useful guides on new features
- [Governance](#TODO)
- [MASP (multi-asset shielded pool) transfers](./masp.md)

## Applying to be a genesis validator
If you want to be a validator on the testnet.

### Prerequisites
- a machine that meets the [requirements](../../user-guide/genesis-validator-setup.md) for running a validator node
- an associated public IPv4 address with ports 26656 and 26657 reachable from anywhere

### Set up
Follow [this guide](../../user-guide/genesis-validator-setup.md) on how to generate. After this, you'll have a `validator.toml` file, the contents of which will look something like the following:

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

This file contains only public information and is safe to share publicly. If you want to be a genesis validator for this testnet, please make a pull request to [https://github.com/anoma/namada-testnets](https://github.com/anoma/namada-testnets) with this `validator.toml` file. You can see what an example PR looks like [here](#TODO).
