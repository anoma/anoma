# The ledger

## The protocol

- TODO describe DKG transactions
- TODO DKG transactions will include replay protection (this is because we can simply check a counter against the source (i.e. gas payer) of the transaction before the transactions order is committed to by the DKG protocol, which could affect the expected counter order for sources with multiple queued transactions)

### Transactions

A transaction [encoded with proto3](./encoding.md#transactions) received from ABCI `DeliverTx` method is executed in two main steps:

1. [Transaction execution](#transaction-execution)
1. [Validity predicates check](#validity-predicates-check)

#### Transaction execution

For any error encountered in any of the following steps of transaction execution, the protocol MUST charge the gas used by the transaction and discard any storage changes that the transaction attempted to perform.

1. Charge a base transaction [gas](#gas):
   \\[ \verb|BASE_TRANSACTION_FEE| \\]
1. Decode the transaction bytes and validate the data. The field `timestamp` is required.
1. Charge WASM compilation gas, proportional to the bytes `length` of the `code` field of the transaction (this is because the WASM code is compiled with a single-pass compiler):
   \\[ \verb|length| * \verb|COMPILE_GAS_PER_BYTE| \\]
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

#### Validity predicates check

For the transaction to be valid, all the triggered validity predicates must accept it.

First, the addresses whose validity predicates should be triggered by the transaction are determined. In this process, the addresses get associated with a set of modified storage keys that are relevant to the address:
1. The addresses set by the transaction (see `insert_verifier` in [transaction host environment functions](#transaction-host-environment-functions)) are associated with *all* the modified storage keys.

   TODO - <https://github.com/anoma/anoma/issues/292>
1. The storage keys that were modified by the transaction are associated with the addresses included in the storage keys. Note that a storage key may contain more than one address, in which case all its addresses are associated with this key. 
1. All these addresses are additionally associated with the storage key to the validity predicates of any newly initialized accounts' by the transaction (see `init_account` in [transaction host environment functions](#transaction-host-environment-functions)).

For all these addresses, attempt to read their validity predicate WASM code from the storage. For each validity predicate look-up, charge storage read gas and WASM compilation gas, proportional to the bytes length of the validity predicate. If any of the validity predicates look-ups fails, or any validity rejects the transaction or fails anywhere in the execution, the whole transaction is rejected. If the transaction is rejected, the protocol MUST charge the gas used by the transaction and discard any storage changes that the transaction attempted to perform.

Execute all validity predicates in parallel as follows:

1. Charge WASM compilation gas, proportional to the bytes length of the validity predicate (same as for the transaction, WASM code is compiled with a single-pass compiler).
1. Charge WASM compilation gas, proportional to the bytes `length` of the validity predicate (same as for the transaction, WASM code is compiled with a single-pass compiler): \\[ \verb|length| * \verb|COMPILE_GAS_PER_BYTE| \\].
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

#### Gas

##### Gas constants

The gas constants are currently chosen arbitrarily and are subject to change following gas accounting estimations.

| Name                   | Value |
|------------------------|-------|
| `COMPILE_GAS_PER_BYTE` |     1 |
| `BASE_TRANSACTION_FEE` |     2 |
| `PARALLEL_GAS_DIVIDER` |    10 |
| `MIN_STORAGE_GAS`      |     1 |

- TODO describe gas accounting, wasm gas counter, limits, what happens if we go over limits and how gas relates to fees

#### WebAssembly (WASM)

##### WASM constants

| Name                                 | Unit              | Value |
|--------------------------------------|-------------------|-------|
| `PAGE` (as defined in the WASM spec) | kiB               |    64 |
| `TX_MEMORY_INIT_PAGES`               | number of `PAGE`s |   100 |
| `TX_MEMORY_MAX_PAGES`                | number of `PAGE`s |   200 |
| `VP_MEMORY_INIT_PAGES`               | number of `PAGE`s |   100 |
| `VP_MEMORY_MAX_PAGES`                | number of `PAGE`s |   200 |
| `WASM_STACK_LIMIT`                   | stack depth       | 65535 |


The WASM instantiation, the types, instructions, validation and execution of WASM modules MUST conform to the [WebAssembly specification](https://webassembly.github.io/spec/core/intro/index.html).

##### WASM validation

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

##### Stack height limiter

To make stack overflows deterministic, set the upper bound of the stack size to [`WASM_STACK_LIMIT`](#wasm-constants). If the stack height exceeds the limit then execution MUST abort.

<!--
cargo test test_tx_stack_limiter
cargo test test_vp_stack_limiter
-->

##### WASM memory

- TODO memory read/write gas costs

##### Transaction host environment functions

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

##### Validity predicate host environment functions

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

- TODO
