# Validity predicates

[Tracking Issue](https://github.com/anoma/anoma/issues/44)

---

Each [account](accounts.md) is associated with exactly one validity predicate (VP).

Conceptually, a VP is a function from the transaction's data and the storage state prior and posterior to a transaction execution returning a boolean value. A transaction may modify any data in the [accounts' dynamic storage sub-space](accounts.md#dynamic-storage-sub-space). Upon [transaction execution](tx.md#tx-execution), the VPs associated with the accounts whose storage has been modified are invoked to verify the transaction. If any of them reject the transaction, all of its storage modifications are discarded.

There are some native VPs for [internal transparent addresses](accounts.md#internal-transparent-addresses) that are built into the ledger. All the other VPs are implemented as [WASM programs](wasm-vm.md). One can build a custom VP using the [VP template](https://github.com/anoma/anoma/tree/master/wasm/vp_template) or use one of the pre-defined VPs.

The VPs must implement the following interface that will be invoked by the protocol:

```rust
fn validate_tx(
    // Data of the transaction that triggered this VP call
    tx_data: Vec<u8>,
    // Address of this VP
    addr: Address,
    // Storage keys that have been modified by the transation, relevant to this VP
    keys_changed: BTreeSet<storage::Key>,
    // Set of all the addresses whose VP was triggered by the transaction
    verifiers: BTreeSet<Address>,
) -> bool;
```

The host functions available to call from inside the VP code can be found in [docs generated from code](https://dev.anoma.net/master/rustdoc/anoma_vm_env/imports/vp/index.html#functions).

## Native VPs

The native VPs follow the same interface as WASM VPs and rules for how they are [triggered by a transaction](tx.md#tx-execution). They can also call the same host functions as those provided in [WASM VPs environment](wasm-vm.md#vps-environment) and must also account any computation for gas usage.

### PoS slash pool VP

The Proof-of-Stake slash pool is a simple account with a native VP which can receive slashed tokens, but no token can ever be withdrawn from it by anyone at this point.

## Fungible token VP

The [fungible token VP](https://github.com/anoma/anoma/tree/master/wasm/wasm_source) allows to associate accounts balances of a specific token under its account.

For illustration, users `Albert` and `Bertha` might hold some amount of token with the address `XAN`. Their balances would be stored in the `XAN`'s storage sub-space under the storage keys `@XAN/balance/@Albert` and `@XAN/balance/@Bertha`, respectively. When `Albert` or `Bertha` attempt to transact with their `XAN` tokens, its validity predicate would be triggered to check:

- the total supply of `XAN` token is preserved (i.e. inputs = outputs)
- the senders (users whose balance has been deducted) are checked that their validity predicate has also been triggered

Note that the fungible token VP doesn't need to know whether any of involved users accepted or rejected the transaction, because if any of the involved users rejects it, the whole transaction will be rejected.

## User VP

The [user VP](https://github.com/anoma/anoma/blob/master/wasm/wasm_source/src/vp_user.rs) currently provides a signature verification against a public key for sending tokens as prescribed by the fungible token VP. In this VP, a transfer of tokens doesn't have to be authorized by the receiving party.

It also allows arbitrary storage modifications to the user's sub-space to be performed by a transaction that has been signed by the secret key corresponding to the user's public key stored on-chain. This functionality also allows one to update their own validity predicate.
