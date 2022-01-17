# CHANGELOG

## Unreleased

### BUG FIXES

- Matchmaker: Fix a matchmaker's state management via a raw pointer
  that was causing segfaults in the matchmaker in release build.
  ([#806](https://github.com/anoma/anoma/pull/806))

### FEATURES

- Client/Ledger: Added a flag  to allow to indicate that client should exit once
  the transaction is in mempool without waiting for it to be applied in a block.
  ([#403](https://github.com/anoma/anoma/issues/403))
- Ledger: Emit and validate IBC events from transactions.
  ([#480](https://github.com/anoma/anoma/issues/480))
- Add `anomac tx-result` command to query the result of a transaction given a
  transaction hash. ([#634](https://github.com/anoma/anoma/issues/634))
- Ledger: Use IBC messages from ibc-rs crate to be used in the relayer.
  ([#699](https://github.com/anoma/anoma/issues/699))

### IMPROVEMENTS

- Ledger: Two-layer merkle tree for the IBC proof verification
  ([#671](https://github.com/anoma/anoma/issues/671))
- Testing: Increments network configuration ports used for E2E
  tests and ABCI++ enabled E2E tests to avoid sharing resources.
  ([#717](https://github.com/anoma/anoma/issues/717))
- Matchmaker: compiling and loading matchmakers to and from dylib instead of
  WASM ([#718](https://github.com/anoma/anoma/pull/718))
- Matchmaker: re-purpose the matchmaker macro to manage state of a custom
  matchmaker implementation ([#746](https://github.com/anoma/anoma/pull/746))

## v0.3.1

Anoma 0.3.1 - first maintenance release in the 0.3.x series. Protocol
compatible with 0.3.0, but changes the on-disk storage format - nodes
will need to resync from scratch.

### BUG FIXES

- Fix the `anoma client utils join-network` to respect `--base-dir` argument, if
  specified ([#723](https://github.com/anoma/anoma/issues/723))
- Ledger: Fix an issue in the default thread count usage calculation that
  was previously causing it to crash for a target with a single logical core.
  ([#726](https://github.com/anoma/anoma/pull/726))
- Ledger: write storage diffs from the correct current block height and ignore
  these on loading last known block's state from persisted state on disk.
  ([#732](https://github.com/anoma/anoma/pull/732))
- Ledger: Handle Unix and Windows interrupt and termination signals to shut down
  cleanly. ([#768](https://github.com/anoma/anoma/issues/768))

### IMPROVEMENTS

- Ledger: enable atomic commits in RocksDB and explicitly flush blocks without
  waiting ([#372](https://github.com/anoma/anoma/issues/372))
- Fix the `anoma client utils join-network` to respect `--base-dir` argument, if
  specified ([#711](https://github.com/anoma/anoma/issues/711))
- Ledger: Write predecessor block's values to be able to integrate Tendermint's
  rollback helper command. ([#729](https://github.com/anoma/anoma/pull/729))
- Include a more accurate build version from git describe in help output version
  strings. ([#733](https://github.com/anoma/anoma/pull/733))
- Ledger: Updated wasmer dependency to [v2.1.1](https://github.com/wasmerio/wasmer/releases/tag/2.1.1).
  ([#756](https://github.com/anoma/anoma/pull/756))
- Config: Enable setting config values via environment variables, add
  variables for configuring Tendermint instrumentation and allow missing
  values in the config file (filled in with defaults defined in the code)
  ([#774](https://github.com/anoma/anoma/pull/774))
- Gossip: Enable peer discovery with libp2p Kademlia and Identify
  protocol and allow to keep the established peer connections open.
  ([#775](https://github.com/anoma/anoma/pull/775))

### MISCELLANEOUS

- Adds missing nix-shell openssl dependency.
  ([#694](https://github.com/anoma/anoma/pull/694))
- Don't include wasm checksums in the package, since the network configuration
  mechanisms now handle this. ([#731](https://github.com/anoma/anoma/pull/731))
- Force non-dev build for make clippy.
  ([#783](https://github.com/anoma/anoma/pull/783))

## v0.3.0

Anoma 0.3.0

### BUG FIXES

- Ledger: Using fixes in `tower-abci` crate that improve shutdown and error handling.
  ([#614](https://github.com/anoma/anoma/pull/614))
- Ledger: Fix rlimit breaking build on non-unix target.
  ([#615](https://github.com/anoma/anoma/pull/615))
- Ledger/client: Fixed an issue with obtaining a result of a transaction.
  ([#668](https://github.com/anoma/anoma/pull/668))
- Ledger: Fixed Windows build. ([#684](https://github.com/anoma/anoma/pull/684))
- Use at least one thread for rocksdb compaction.
  ([#704](https://github.com/anoma/anoma/pull/704))
- Downgrade to an older version of wasmer (1.0.2) to avoid runaway memory usage.
  ([#708](https://github.com/anoma/anoma/pull/708))
- Process ABCI requests in order in the shell.
  ([#713](https://github.com/anoma/anoma/pull/713))

### CI

- Added jobs for ABCI++ feature enabled builds
  ([#661](https://github.com/anoma/anoma/pull/661))

### DOCS

- Fix broken links ([#605](https://github.com/anoma/anoma/issues/605))
- Improved user guide installation section. Improved development scripts.
  ([#613](https://github.com/anoma/anoma/pull/613))
- Updated Anoma prototypes pages.
  ([#642](https://github.com/anoma/anoma/pull/642))
- Fix broken links to WASM sources.
  ([#660](https://github.com/anoma/anoma/pull/660))

### FEATURES

- Ledger: Added storage query (non-)membership proofs
  ([#498](https://github.com/anoma/anoma/issues/498))
- Ledger: Make all transactions encrypted, add in ABCI++, support commit and
  reveal scheme for txs in a block. This feature is disabled by default
  because it requires a custom Tendermint build. The Tendermint version
  required with default features remains unchanged (currently 0.34.x).
  ([#622](https://github.com/anoma/anoma/pull/622))
- Cache compiled wasm modules on disk.
  ([#697](https://github.com/anoma/anoma/pull/697))

### IMPROVEMENTS

- Improve how the `anoma` binary launches a sub-process by replacing itself with
  it. ([#609](https://github.com/anoma/anoma/pull/609))
- Ledger/client: Add archives for release-able networks from init-network utils
  command. ([#616](https://github.com/anoma/anoma/pull/616))
- Ledger: Open the default P2P address for non-localhost networks
  ([#617](https://github.com/anoma/anoma/pull/617))
- Tooling (switched to Rust 1.56.1 and nightly  to 2021-11-01. Many Cargo
  dependencies updates. ([#618](https://github.com/anoma/anoma/pull/618))
- Ledger: Join Anoma networks from GitHub released network configurations.
  ([#619](https://github.com/anoma/anoma/pull/619))
- Ledger/storage: Write values from references.
  ([#627](https://github.com/anoma/anoma/pull/627))
- Build: Avoid build context in Docker image builds.
  ([#629](https://github.com/anoma/anoma/pull/629))
- Ledger: Add the WASM checksums file for the pre-built
  transactions and validity predicates to network releases.
  ([#644](https://github.com/anoma/anoma/pull/644))
- Ledger: Simplified gas addition code. ([#648](https://github.com/anoma/anoma/pull/648))
- Ledger: Improved the "wrapper" transaction type data structures and encoding.
  ([#653](https://github.com/anoma/anoma/pull/653))
- Ledger: Follow-up to the improved "wrapper" transaction type data structures
  and encoding. ([#655](https://github.com/anoma/anoma/pull/655))
- Ledger: Refactored ledger threads usage and made them configurable.
  ([#658](https://github.com/anoma/anoma/pull/658))
- Ledger: Updated dependencies names to nix friendly format.
  ([#664](https://github.com/anoma/anoma/pull/664))
- Ledger: Follow-up to the updated crate names to nix friendly format.
  ([#666](https://github.com/anoma/anoma/pull/666))
- Ledger/client: Set a default fee amount, token, and gas limit for txs.
  ([#667](https://github.com/anoma/anoma/pull/667))
- Remove intent gossiper key from configuration file, storing it separately.
  ([#673](https://github.com/anoma/anoma/pull/673))
- Refactor historical data storage in rocksdb to store diffs
  of key changes, and additional database performance changes.
  ([#683](https://github.com/anoma/anoma/pull/683))
- Improved nix integration. ([#685](https://github.com/anoma/anoma/pull/685))
- Ledger/client: Fix the confirmation dialog for using an existing alias and
  allow to select a new one. ([#690](https://github.com/anoma/anoma/pull/690))
- Refactor debug printouts in wasm modules to use conditional compilation.
  ([#693](https://github.com/anoma/anoma/pull/693))
- Ledger: Updated to wasmer 2.0.1.
  ([#698](https://github.com/anoma/anoma/pull/698))
- Allow zero-balance transactions pending proper gas fee deduction.
  ([#700](https://github.com/anoma/anoma/pull/700))
- Emit more CPU thread usage information.
  ([#705](https://github.com/anoma/anoma/pull/705))

### TESTING

- Tests/E2E: Added PoS tests for bonding and initialization of a validator
  account on-chain. ([#463](https://github.com/anoma/anoma/issues/463))

