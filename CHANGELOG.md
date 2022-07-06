# CHANGELOG

## v0.6.1

Anoma 0.6.1 is a patch release updating the Rust toolchain and various
libraries.

### BUG FIXES

- Fixed PoS `votes_per_token` parameter update validation
  ([#1181](https://github.com/anoma/anoma/issues/1181))

### IMPROVEMENTS

- Ledger: Updated the version of Tendermint used for ABCI++ ([#1088](https://github.com/anoma/anoma/pull/1088))
    - Add full support for ProcessProposal and FinalizeBlock
    - Updated the shims
    - Updated `tendermint-rs`, `ibc-rs`, and `tower-abci` deps
    - Updated the proto definitions
    - Added Tendermint's new method of a BFT timestamping
    - Updated the format of Tendermint's new config
    - Fixed booting up the tendermint node in the ledger with correct settings
    - Refactored storage to account for the fact that tendermint no longer passes in block headers
- Client: Configured Tendermints new event log and JSON RPC API for events querying ([#1088](https://github.com/anoma/anoma/pull/1088))
    - Added necessary config parameters to our tendermint node's configuration
    - Wrote a jsonrpc client for querying tendermint's event logs
    - Refactored how txs are submitted in the client when the `ABCI-plus-plus` feature is
      set to use jsonrpc calls instead of websockets.

### MISCELLANEOUS

- Updated RocksDB dependency version to v0.18.0
  ([#1135](https://github.com/anoma/anoma/issues/1135))

## v0.6.0

Anoma 0.6.0 is a scheduled minor release.

### BUG FIXES

- Ledger: Set the counterparty connection ID when the connection ack
  ([#968](https://github.com/anoma/anoma/issues/968))
- Ledger: Set the counterparty channel ID when the ack
  ([#980](https://github.com/anoma/anoma/issues/980))
- Ledger: Encode a commitment with Protobuf
  ([#988](https://github.com/anoma/anoma/issues/988))
- Client: Fix Tendermint node ID derivation from ed25519 keys in "utils init-
  network" command ([#992](https://github.com/anoma/anoma/issues/992))
- Ledger: Reuse IBC port ([#1011](https://github.com/anoma/anoma/issues/1011))
- Ledger: Fix to write the initial value of IBC sequence
  ([#1014](https://github.com/anoma/anoma/issues/1014))
- Fixes installation of Tendermint on M1 Macs
  ([#1018](https://github.com/anoma/anoma/issues/1018))
- Ledger: Fix IBC VP for packet timeout
  ([#1029](https://github.com/anoma/anoma/issues/1029))
- Ledger: Fix vp read_pre to read from write_log for the previous txs updates
  ([#1032](https://github.com/anoma/anoma/issues/1032))
- Ledger: Fix IBC token VP ([#1040](https://github.com/anoma/anoma/issues/1040))
- Fix loading of validator VP during chain initialization
  ([#1054](https://github.com/anoma/anoma/pull/1054))
- Fix possible overflow when formatting token amount to a string.
  ([#1087](https://github.com/anoma/anoma/pull/1087))

### DOCS

- Add docs for internal testnet for MASP
  ([#1013](https://github.com/anoma/anoma/pull/1013))
- Applied various fixes to MASP internal testnet guide
  ([#1017](https://github.com/anoma/anoma/pull/1017))
- Added docs page for testnet launch procedure.
  ([#1028](https://github.com/anoma/anoma/pull/1028))
- Add links to doc pages sources on Github.
  ([#1030](https://github.com/anoma/anoma/pull/1030))
- User guide and testnets documentation has been
  separated and moved to <https://github.com/anoma/docs>
  ([#1080](https://github.com/anoma/anoma/issues/1080))
- Updated whitepaper and vision paper links
  ([#1116](https://github.com/anoma/anoma/issues/1116))
- Install mdbook-admonish plugin
  ([#1132](https://github.com/anoma/anoma/pull/1132))
- Include Rust nightly version from root file
  ([#1133](https://github.com/anoma/anoma/pull/1133))

### FEATURES

- Client: Add raw bytes query command "query-bytes" from the storage.
  ([#836](https://github.com/anoma/anoma/issues/836))
- Added on-chain and off-chain governance validity predicate with client support
  for submitting proposal transaction, proposal queries and for creating off-
  chain proposals. ([#954](https://github.com/anoma/anoma/pull/954))
- Ledger: Change the storage hasher to SHA256
  ([#968](https://github.com/anoma/anoma/issues/968))
- Governance: Added proposal vote transaction and validity
  predicate support. Also improved the proposal query command.
  ([#975](https://github.com/anoma/anoma/pull/975))
- Ledger: Query with the specified height
  ([#987](https://github.com/anoma/anoma/issues/987))
- Add stub EthBridge internal address
  ([#1066](https://github.com/anoma/anoma/pull/1066))

### IMPROVEMENTS

- Ledger: The validity predicate checks rule has been simplified
  so that every validity predicate triggered by a transaction
  receives all the storage keys touched by the transaction.
  ([#955](https://github.com/anoma/anoma/issues/955))
- Ledger: write/get block header to get an old consensus state
  ([#974](https://github.com/anoma/anoma/issues/974))
- Ledger: Use non-validator full node Tendermint mode by default.
  ([#978](https://github.com/anoma/anoma/pull/978))
- Client: Updated the utils commands `init-genesis-validator` and `join-
  network` to be able to automatically configure a genesis validator node.
  ([#997](https://github.com/anoma/anoma/pull/997))
- Show an error when calling `anomac utils join-network` if trying to download a
  file and it is missing ([#1044](https://github.com/anoma/anoma/pull/1044))
- Wallet: various store and API changes and additions for genesis setup.
  ([#1063](https://github.com/anoma/anoma/pull/1063))

### MISCELLANEOUS

- Fixed Nix build and updated Nix dependencies.
  ([#994](https://github.com/anoma/anoma/pull/994))
- Update `make install` command to respect the Cargo.lock file
  ([#1118](https://github.com/anoma/anoma/issues/1118))

### TESTING

- Replaced E2E tests command runner library with
  [expectrl](https://crates.io/crates/expectrl)
  ([#686](https://github.com/anoma/anoma/issues/686))
- Added state-machine property-based tests for PoS validity predicate.
  ([#927](https://github.com/anoma/anoma/pull/927))
- WASM host environment testing helpers are now pinned to a stable
  memory location to avoid issues in state machine test runner.
  ([#947](https://github.com/anoma/anoma/pull/947))
- More logging in end-to-end tests
  ([#1071](https://github.com/anoma/anoma/pull/1071))

## v0.5.0

Anoma 0.5.0 is a scheduled minor release.

### BUG FIXES

- WASM: Fix WASM cache exponential backoff
  ([#834](https://github.com/anoma/anoma/issues/834))
- Ledger: Temporarily downgrade back to wasmer v1.0.2 until
  [the leak that is affecting Linux](https://github.com/anoma/anoma/issues/871) 
  is found and fixed. ([#870](https://github.com/anoma/anoma/pull/870))
- Ledger: Upgrade to wasmer v2.2.0 and fix memory leak
  caused by a circular reference in the WASM memory
  ([#871](https://github.com/anoma/anoma/issues/871))
- Change the validity predicate main entry-point function to receive
  `changed_keys` and `verifiers` arguments in a deterministic order.
  ([#891](https://github.com/anoma/anoma/issues/891))
- Dependency: Backport libp2p-noise patch that fixes a compilation
  issue from <https://github.com/libp2p/rust-libp2p/pull/2264>
  ([#908](https://github.com/anoma/anoma/issues/908))
- Wasm: Re-add accidentaly removed `tx_ibc` WASM and `vm_env::ibc` module
  ([#916](https://github.com/anoma/anoma/pull/916))
- Ledger & Matchmaker: In "dev" chain with "dev" build, load WASM directly from
  the root `wasm` directory. ([#933](https://github.com/anoma/anoma/issues/933))
- Ledger: Decode signed data in IBC VPs
  ([#938](https://github.com/anoma/anoma/issues/938))
- Ledger: Fixed handling of the Tendermint mode global argument.
  ([#943](https://github.com/anoma/anoma/pull/943))
- Ledger: Set IBC event besides tx_result
  ([#944](https://github.com/anoma/anoma/issues/944))
- Ledger: Fix IBC ClientReader functions
  ([#949](https://github.com/anoma/anoma/issues/949))
- Ledger: Set 0 as IBC height revision number
  ([#950](https://github.com/anoma/anoma/issues/950))
- Ledger: Fix the path via ABCI query
  ([#958](https://github.com/anoma/anoma/issues/958))

### CI

- Move cron pipeline script externally, fix cron scripts dependencies
  ([#906](https://github.com/anoma/anoma/pull/906))
- Fix cron scripts execution ([#912](https://github.com/anoma/anoma/pull/912))
- Build docs without attempting to merge master
  ([#924](https://github.com/anoma/anoma/pull/924))

### DOCS

- Added specifications for ledger, RPC, default transactions
  and encoding, which is partially derived from code.
  ([#887](https://github.com/anoma/anoma/pull/887))

### FEATURES

- Added IBC transaction ([#411](https://github.com/anoma/anoma/issues/411))
- Ledger: Added validity predicate whitelist configurable for a network to limit which validity predicates are permitted ([#875](https://github.com/anoma/anoma/issues/875))
- Ledger: Added transaction whitelist configurable for a network to limit which transactions are permitted ([#876](https://github.com/anoma/anoma/issues/876))
- Added transactions and vp to create and mint nfts.
  ([#882](https://github.com/anoma/anoma/issues/882))
 - Supports a new type of transaction intended to be sent by validators, so called protocol transactions. 
   - New transaction type
   - Generation of keys for validators to sign protocol txs
   - A service to broadcast protocol txs from the ledger
   - Improved client tx broadcasting
     
   (#[913](https://github.com/anoma/anoma/pull/913))

### IMPROVEMENTS

- Added a common signing schemes interface to ease additions/removals of signing
  schemes to the ledger. ([#225](https://github.com/anoma/anoma/issues/225))
- Canonicalize all wallet aliases to lowercase in the CLI.
  ([#564](https://github.com/anoma/anoma/issues/564))
- Ledger: Handle spurious errors on user initiated shutdown.
  ([#716](https://github.com/anoma/anoma/issues/716))
- Dependency: Replace ed22519-dalek with ed22519-consensus.
  ([#753](https://github.com/anoma/anoma/issues/753))
- Network config: Make the WASM checksums optional in network source, as it is 
  filled in by `init-network` utils command.
  ([#777](https://github.com/anoma/anoma/issues/777))
- Network config: The WASM dir were moved inside chain directories.
  ([#838](https://github.com/anoma/anoma/issues/838))
- Ledger: added support for transactions to write temporary data that
  can be read by any VP that is checking the validity of the transaction.
  This is being used in IBC native VP for fungible token transfer.
  ([#848](https://github.com/anoma/anoma/pull/848))
- Ledger: Update tendermint-rs and ibc-rs
  ([#863](https://github.com/anoma/anoma/issues/863))
- Ledger: Sign transaction hash of bytes instead of the bytes themselves.
  ([#886](https://github.com/anoma/anoma/issues/886))
- Update the Rust toolchain to 1.58.1.
  ([#902](https://github.com/anoma/anoma/pull/902))
- Implemented `BorshSchema` for ledger's the public types.
  ([#907](https://github.com/anoma/anoma/pull/907))
- Updated the tx result from an undecryptable tx to give an error code and message stating message was not decryptable
  ([#910](https://github.com/anoma/anoma/pull/910))
- Ledger: Update ibc-rs to v0.12.0
  ([#926](https://github.com/anoma/anoma/issues/926))
- WASM: Use tx/VP specific preludes.
  ([#948](https://github.com/anoma/anoma/pull/948))
- WASM: Split up tx and VP modules into files.
  ([#952](https://github.com/anoma/anoma/pull/952))

## v0.4.0

Anoma 0.4.0 is a scheduled minor release, released 31 January 2022.

### BUG FIXES

- Matchmaker: Fix a matchmaker's state management via a raw pointer
  that was causing segfaults in the matchmaker in release build.
  ([#806](https://github.com/anoma/anoma/pull/806))

### CI

- Build Linux package directly from tagged releases, and upload wasm from tags.
  ([#801](https://github.com/anoma/anoma/pull/801))

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
- Ledger: Added [fungible token transfer](https://github.com/cosmos/ibc/tree/26299580866b80fbdf0ce8a0691ee19a28176795/spec/app/ics-020-fungible-token-transfer)
  support to IBC validity predicate. 
  ([#823](https://github.com/anoma/anoma/issues/823))

### IMPROVEMENTS

- Ledger: Add IbcActions trait to execute IBC operations
  ([#411](https://github.com/anoma/anoma/issues/411))
- Matchmaker has been separated from intent gossiper node. Multiple
  matchmakers can connect to an intent gossiper node over WebSocket.
  ([#579](https://github.com/anoma/anoma/issues/579))
- Wallet: Ask for encryption password confirmation when generating a new key.
  ([#625](https://github.com/anoma/anoma/issues/625))
- Ledger: Two-layer merkle tree for the IBC proof verification
  ([#671](https://github.com/anoma/anoma/issues/671))
- Testing: Increments network configuration ports used for E2E
  tests and ABCI++ enabled E2E tests to avoid sharing resources.
  ([#717](https://github.com/anoma/anoma/issues/717))
- Matchmaker: compiling and loading matchmakers to and from dylib instead of
  WASM ([#718](https://github.com/anoma/anoma/pull/718))
- Ledger: Coding IBC-related data without Borsh
  ([#734](https://github.com/anoma/anoma/issues/734))
- Matchmaker: re-purpose the matchmaker macro to manage state of a custom
  matchmaker implementation ([#746](https://github.com/anoma/anoma/pull/746))
- Testing: Update to a new branch of property-based state machine testing with
  initial state shrinking. ([#765](https://github.com/anoma/anoma/pull/765))
- Port the Nix build to the new Flakes system.
  ([#770](https://github.com/anoma/anoma/pull/770))
- Client/Utils: Respect wasm directory, when specified and non-default in the
  command. The command now doesn't unpack the network config archive into its
  default directories, if any of them are specified with non-default values.
  ([#813](https://github.com/anoma/anoma/issues/813))
- Install the default token exchange matchmaker implemenetation into
  `~/.cargo/lib` directory when building from source. When not absolute, the
  matchmaker will attempt to load the matchmaker from the same path as where the
  binary is being ran from, from `~/.cargo/lib` or the current working 
  directory. ([#816](https://github.com/anoma/anoma/issues/816))

### MISCELLANEOUS

- Force non-dev build for make build-release, check-release & package
  ([#791](https://github.com/anoma/anoma/pull/791))

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

