# User Guide

This guide assumes that the Anoma binaries are installed and available on path. These are:

- `anoma`: The main binary that can be used to interact with all the components of Anoma
- `anoman`: The ledger and intent gossiper node
- `anomac`: The client
- `anomaw`: The wallet

The main binary `anoma` has sub-commands for all of the other binaries:

- `anoma client = anomac`
- `anoma node   = anoman`
- `anoma wallet = anomaw`

To explore the command-line interface, add `--help` argument at any sub-command level to find out any possible sub-commands and/or arguments.


### Notes

A custom transaction code can be built from [tx_template](wasm/tx_template) and validity predicates from [vp_template](wasm/vp_template), which is Rust code compiled to WASM.

The transaction template calls functions from the host environment. The validity predicate template can validate a transaction and the storage key changes that is has performed.

Similarly, a custom matchmaker code can be built from [mm_template](wasm/mm_template).