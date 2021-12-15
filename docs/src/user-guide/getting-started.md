# Getting started

This guide assumes that the Anoma binaries are [installed](./install.md) and available on path. These are:

- `anoma`: The main binary that can be used to interact with all the components of Anoma
- `anoman`: The ledger and intent gossiper node
- `anomac`: The client
- `anomaw`: The wallet

The main binary `anoma` has sub-commands for all of the other binaries:

- `anoma client = anomac`
- `anoma node   = anoman`
- `anoma wallet = anomaw`

To explore the command-line interface, add `--help` argument at any sub-command level to find out any possible sub-commands and/or arguments.

To configure your node to join the Feigenbaum public testnet, run:

```bash
anoma client utils join-network --chain-id=anoma-feigenbaum-0.ebb9e9f9013
```
