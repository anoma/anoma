# The ledger

The ledger's main responsibility is to process and apply [transactions](#transactions) over the [distributed ledger's storage](#storage), following the ledger's [protocol](#the-protocol) to reach consensus.

## Accounts

The ledger is backed by an account-based system. Each account has a unique [address](#addresses) and exactly one [validity predicate](#validity-predicates-check) and a [dynamic storage sub-space](#dynamic-storage-sub-space).

### Addresses

There are two main types of address: transparent and shielded.

The transparent addresses are the addresses of accounts associated with dynamic storage sub-spaces, where the address of the account is the prefix key segment of its sub-space.

The shielded addresses are used for private transactions and they are not directly associated with storage sub-spaces.

#### Transparent addresses

Furthermore, there are three types of transparent addresses:

- "implicit" addresses which are derived from [public keys](crypto.md#public-keys)
- "established" addresses which are generated from the current address nonce and hence must be created via a request in the ledger
- "internal" addresses are used for special modules integrated into the ledger such as PoS and IBC.

The addresses are stored on-chain encoded with [bech32m](https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki), which is an improved version of [bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki).

The human-readable prefix (as specified for [bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#specification)) in the transparent address encoding is:

- `"a"` for Anoma live network (80 characters in total)
- `"atest"` for test networks (84 characters in total)

## Transactions

A transaction has two layers, each wrapped inside [`Tx` type encoded with proto3](./encoding.md#transactions).

The outer layer is employed for front-running protection following DKG protocol to wrap the inner layer, which remains encrypted before its block order has been committed. The outer layer MUST contain `data` with a [`TxType::Wrapper`](encoding.md#txtype) that has a [`WrapperTx`](encoding.md#wrappertx) inside it.

The SHA-256 hash of this data [encoded with Borsh](encoding.html#borsh-binary-encoding) MUST be [signed](crypto.md#signatures) by an implicit account's key. The encoded signed data together with the signature should be encoded as a [`SignedTxData`](encoding.md#signedtxdata) and also encoded with Borsh. This data should then be attached to a protobuf encoded transaction's `data` field and the field `code` in this layer MUST be empty. Note that the outer layer's signature is not relevant to the inner layer of the transaction, only itself.

The fields of a `WrapperTx` are:

- `fee`: Fee to be payed by the source implicit account for including the tx in a block.
- `pk`: [Public key](crypto.md#public-keys) of the source implicit account.
- `epoch`: The [epoch](#epochs) in which the transaction is being included. This should be queried from a synchronized ledger node before the transaction is fully constructed.

   Note that this is currently not used and so the default value `0` may be used for now (depends on <https://github.com/anoma/anoma/issues/669>).

- `gas_limit`: Maximum amount of gas that can be used when executing the inner transaction
- `inner_tx`: The inner layer of the transaction. This MUST contain a [`Tx` type encoded with proto3](./encoding.md#transactions), encrypted against a public key that should be queried from a synchronized ledger node.

   The inner transaction's `Tx` MUST contain the WASM code to be executed and optionally any `data` (which will be provided to the transaction and any triggered validity predicates when they're invoked) to be executed and applied in a block (for example the [default transactions](ledger/default-transactions.md)).

   Please refer to the [signing of the default transactions](ledger/default-transactions.md#signing-transactions) to learn how to construct inner transaction's signatures which will be accepted by the [default validity predicates](ledger/default-validity-predicates.md).

   Note that currently the key doesn't change and so it stay constant for the duration of a chain and `<EllipticCurve as PairingEngine>::G1Affine::prime_subgroup_generator()` may be used to encrypt the inner transaction for now as done by the the [`WrapperTx::new` method](https://dev.anoma.net/master/rustdoc/anoma/types/transaction/wrapper/wrapper_tx/struct.WrapperTx.html#method.new) (depends on <https://github.com/anoma/anoma/issues/669>).

- `tx_hash`: A SHA-256 hash of the inner transaction. This MUST match the hash of decrypted `inner_tx`.

TODO: wrapper transactions will include replay protection (this is because we can simply check a counter against the source (i.e. gas payer) of the transaction before the transactions order is committed to by the DKG protocol, which could affect the expected counter order for sources with multiple queued transactions)

## The protocol

When a tx is added to the [mempool](#mempool) and included in block by a block proposer, the [outer transaction is processed](#outer-transaction-processing) and if valid, its inner transaction is added to a transaction FIFO queue that MUST be in the same order as the outer transactions.

An inner transaction popped from the queue is applied in a block executed in two main steps:

1. [Inner transaction execution](#inner-transaction-execution)
1. [Validity predicates check](#validity-predicates-check)

### Epochs

An epoch is a range of blocks whose length is determined by the [epoch duration protocol parameter](#protocol-parameters): minimum epoch duration and minimum number of blocks in an epoch. They are identified by consecutive natural numbers starting at 0. The [Borsh encoded `Epoch`](encoding.md#epoch) for the last committed block can be queried via the [RPC](ledger/rpc.md#read-only-queries).

### Protocol parameters

The parameters are used to dynamically control certain variables in the protocol. They are implemented as an internal address with a native validity predicate. The current value of [Borsh encoded `Parameters`](encoding.md#parameters) is written into and read from the block storage in the parameters account's sub-space.

Initial parameters for a chain are set in the genesis configuration.

#### Epoch duration

The parameters for [epoch](#epochs) duration are:

- Minimum number of blocks in an epoch
- Minimum duration of an epoch

### Mempool

When a request to add a transaction to the mempool is received, it will only be added it's a [`Tx` encoded with proto3](./encoding.md#transactions).

### Outer transaction processing

TODO: describe outer tx fee check and deduction, inner tx decryption, tx queue up to the inner tx execution

### Inner transaction execution

For any error encountered in any of the following steps of transaction execution, the protocol MUST charge the gas used by the transaction and discard any storage changes that the transaction attempted to perform.

1. Charge a base transaction [gas](#gas):
   \\( \verb|BASE_TRANSACTION_FEE| \\)
1. Decode the transaction bytes and validate the data. The field `timestamp` is required.
1. Charge WASM compilation gas, proportional to the bytes `length` of the `code` field of the transaction (this is because the WASM code is compiled with a single-pass compiler):
   \\( \verb|length| * \verb|COMPILE_GAS_PER_BYTE| \\)
1. [Validate the WASM code](#wasm-validation) from the `code` field of the transaction.
1. Inject a [gas counter](#gas) into the `code`.
1. Inject a [stack height](#stack-height-limiter) limiter into the `code`.
1. Compile the transaction `code` with a single-pass compiler (for example, [the Wasmer runtime single-pass compiler](https://medium.com/wasmer/a-webassembly-compiler-tale-9ef37aa3b537)). The compilation computational complexity MUST be linear in proportion to the `code` size.
1. Initialize the WASM linear memory with descriptor having the initial memory size equal to [`TX_MEMORY_INIT_PAGES`](#wasm-constants) and maximum memory size to [`TX_MEMORY_MAX_PAGES`](#wasm-constants).
1. Instantiate the WASM module with imported [transaction host environment functions](#transaction-host-environment-functions) and the instantiated WASM memory.
1. Write the transaction's `data` into the memory exported from the WASM module instance.
1. Attempt to call the module's entrypoint function. The entrypoint MUST have signature:

   ```wat
   func (param i64 i64)
   ```

   The first argument is the offset to the `data` input written into the memory and the second argument is its bytes length.

If the transaction executed successfully, it is followed [Validity predicates check](#validity-predicates-check).

### Validity predicates check

For the transaction to be valid, all the triggered validity predicates must accept it.

First, the addresses whose validity predicates should be triggered by the transaction are determined:

1. The addresses set by the transaction (see `insert_verifier` in [transaction host environment functions](#transaction-host-environment-functions)) are included in the verifiers set.
1. The storage keys that were modified by the transaction are inspected for addresses included in the storage key segments and these are also included in the verifiers set. Note that a storage key may contain more than one address, in which case all its addresses are included. This however excludes addresses of established accounts that were initialized in this transaction as they do not exist prior to transaction execution and a validity predicate will be associated with an initialized account only after the transaction is applied and accepted. This is intended as it allows users to initialize their account's storage without a validity predicate check.

For all these addresses, attempt to read their validity predicate WASM code from the storage. For each validity predicate look-up, charge storage read gas and WASM compilation gas, proportional to the bytes length of the validity predicate. If any of the validity predicates look-ups fails, or any validity rejects the transaction or fails anywhere in the execution, the whole transaction is rejected. If the transaction is rejected, the protocol MUST charge the gas used by the transaction and discard any storage changes that the transaction attempted to perform.

Execute all validity predicates in parallel as follows:

1. Charge WASM compilation gas, proportional to the bytes length of the validity predicate (same as for the transaction, WASM code is compiled with a single-pass compiler).
1. Charge WASM compilation gas, proportional to the bytes `length` of the validity predicate (same as for the transaction, WASM code is compiled with a single-pass compiler): \\( \verb|length| * \verb|COMPILE_GAS_PER_BYTE| \\).
1. [Validate the WASM code](#wasm-validation) of the validity predicate.
1. Inject a [gas counter](#gas) into the `code`.
1. Inject a [stack height](#stack-height-limiter) limiter into the `code`.
1. Compile the validity predicate with single-pass compiler. The compilation computational complexity MUST be linear in proportion to its bytes size.
1. Initialize the WASM linear memory with descriptor having the initial memory size equal to [`VP_MEMORY_INIT_PAGES`](#wasm-constants) and maximum memory size to [`VP_MEMORY_MAX_PAGES`](#wasm-constants).
1. Instantiate the WASM module with imported [validity predicate host environment functions](#validity-predicate-host-environment-functions) and the instantiated WASM memory.
1. Write the address of the validity predicate’s owner, the transaction `data`, the modified storage keys encoded with Borsh, and all the triggered validity predicates owners' addresses encoded with Borsh into the memory exported from the WASM module instance.
1. Attempt to call the module's entrypoint function. The entrypoint MUST have signature:

   ```wat
   func (param i64 i64 i64 i64 i64 i64 i64 i64) (result i64))
   ```

   - The first argument is the offset to the owner’s address written into the memory, the second argument is its bytes length
   - The third is the offset of the transaction’s `data` and fourth is it’s bytes length
   - The fifth is the offset of the modified storage keys and sixth is its bytes length
   - The seventh is the offset of the triggered validity predicates owners' addresses and eighth is its bytes length

### Gas

#### Gas constants

The gas constants are currently chosen arbitrarily and are subject to change following gas accounting estimations.

| Name                   | Value |
|------------------------|-------|
| `COMPILE_GAS_PER_BYTE` |     1 |
| `BASE_TRANSACTION_FEE` |     2 |
| `PARALLEL_GAS_DIVIDER` |    10 |
| `MIN_STORAGE_GAS`      |     1 |

- TODO describe gas accounting, wasm gas counter, limits, what happens if we go over limits and how gas relates to fees

### WebAssembly (WASM)

#### WASM constants

| Name                                 | Unit              | Value |
|--------------------------------------|-------------------|-------|
| `PAGE` (as defined in the WASM spec) | kiB               |    64 |
| `TX_MEMORY_INIT_PAGES`               | number of `PAGE`s |   100 |
| `TX_MEMORY_MAX_PAGES`                | number of `PAGE`s |   200 |
| `VP_MEMORY_INIT_PAGES`               | number of `PAGE`s |   100 |
| `VP_MEMORY_MAX_PAGES`                | number of `PAGE`s |   200 |
| `WASM_STACK_LIMIT`                   | stack depth       | 65535 |

The WASM instantiation, the types, instructions, validation and execution of WASM modules MUST conform to the [WebAssembly specification](https://webassembly.github.io/spec/core/intro/index.html).

#### WASM validation

The WebAssembly code is REQUIRED to only use deterministic instructions. Furthermore, it MUST NOT use features from any of the following WebAssembly proposals:

- The reference types proposal
- The multi-value proposal
- The bulk memory operations proposal
- The module linking proposal
- The SIMD proposal
- The threads proposal
- The tail-call proposal
- The multi memory proposal
- The exception handling proposal
- The memory64 proposal

#### Stack height limiter

To make stack overflows deterministic, set the upper bound of the stack size to [`WASM_STACK_LIMIT`](#wasm-constants). If the stack height exceeds the limit then execution MUST abort.

<!--
cargo test test_tx_stack_limiter
cargo test test_vp_stack_limiter
-->

#### WASM memory

- TODO memory read/write gas costs

#### Transaction host environment functions

The following functions from the host ledger are made available in transaction's WASM code. They MAY be imported in the WASM module as shown bellow and MUST be provided by the ledger's WASM runtime:

```wat
(import "env" "gas" (func (param i32)))
(import "env" "anoma_tx_read" (func (param i64 i64) (result i64)))
(import "env" "anoma_tx_result_buffer" (func (param i64)))
(import "env" "anoma_tx_has_key" (func (param i64 i64) (result i64)))
(import "env" "anoma_tx_write" (func (param i64 i64 i64 i64)))
(import "env" "anoma_tx_delete" (func (param i64 i64)))
(import "env" "anoma_tx_iter_prefix" (func (param i64 i64) (result i64)))
(import "env" "anoma_tx_iter_next" (func (param i64) (result i64)))
(import "env" "anoma_tx_insert_verifier" (func (param i64 i64)))
(import "env" "anoma_tx_update_validity_predicate" (func (param i64 i64 i64 i64)))
(import "env" "anoma_tx_init_account" (func (param i64 i64 i64)))
(import "env" "anoma_tx_get_chain_id" (func (param i64)))
(import "env" "anoma_tx_get_block_height" (func (param ) (result i64)))
(import "env" "anoma_tx_get_block_hash" (func (param i64)))
(import "env" "anoma_tx_log_string" (func (param i64 i64)))
```

Additionally, the WASM module MUST export its memory as shown:

```wat
(export "memory" (memory 0))
```

- `anoma_tx_init_account` TODO newly created accounts' validity predicates aren't used until the block is committed (i.e. only the transaction that created the account may write into its storage in the block in which its being applied).
- TODO describe functions in detail

#### Validity predicate host environment functions

The following functions from the host ledger are made available in validity predicate's WASM code. They MAY be imported in the WASM module as shown bellow and MUST be provided by the ledger's WASM runtime.

```wat
(import "env" "gas" (func (param i32)))
(import "env" "anoma_vp_read_pre" (func (param i64 i64) (result i64)))
(import "env" "anoma_vp_read_post" (func (param i64 i64) (result i64)))
(import "env" "anoma_vp_result_buffer" (func (param i64)))
(import "env" "anoma_vp_has_key_pre" (func (param i64 i64) (result i64)))
(import "env" "anoma_vp_has_key_post" (func (param i64 i64) (result i64)))
(import "env" "anoma_vp_iter_prefix" (func (param i64 i64) (result i64)))
(import "env" "anoma_vp_iter_pre_next" (func (param i64) (result i64)))
(import "env" "anoma_vp_iter_post_next" (func (param i64) (result i64)))
(import "env" "anoma_vp_get_chain_id" (func (param i64)))
(import "env" "anoma_vp_get_block_height" (func (param ) (result i64)))
(import "env" "anoma_vp_get_block_hash" (func (param i64)))
(import "env" "anoma_vp_verify_tx_signature" (func (param i64 i64 i64 i64) (result i64)))
(import "env" "anoma_vp_eval" (func (param i64 i64 i64 i64) (result i64)))
```

- TODO describe functions in detail

Additionally, the WASM module MUST export its memory as shown:

```wat
(export "memory" (memory 0))
```

### Storage

- TODO dynamic key-value storage paths, encoding agnostic, any ledger native keys such as the VP key
- TODO VPs must be written into the storage as raw bytes without any additional encoding

#### Storage keys

- TODO spec the key segments, punct, reserved VP segment `?` and address prefix `#`

#### Dynamic storage sub-space

Each account can have an associated dynamic account state in the storage. This
state may be comprised of keys with a format specified above and values of arbitrary user bytes. The first segment of all the keys must be the account's address.
