# Customize accounts and transactions

On this page, we'll cover how to tailor your account(s) to your use-case with custom-made validity predicates and transactions.

We currently only support Rust for custom validity predicates and transactions via WASM, but expect many more options to be available in the future!

## 👩🏽‍🏫 Anoma accounts primer

Instead of the common smart contract design, in Anoma, all the accounts follow the same basic principles. Each account has exactly one validity predicate. Any transaction that attempts to make some storage modifications will trigger validity predicates of each account whose storage has been modified by it. Validity predicates are stateless functions that decide if an account accepts the transaction.

Every account also has its dedicated key-value storage. The ledger encodes agnostic, it allows you to use the encoding of your preference for the storage values. Internally and for the pre-built validity predicates and transactions, we use [Borsh](https://github.com/near/borsh-rs), which allows you to simply derive the encoding from your data types. The storage keys are `/` separated path segments, where the first segment is always the address of the account to which the storage key belongs. In storage keys, addresses use a reserved prefix `#`.

To illustrate with an example storage key used for fungible tokens (with addresses shortened for clarity), let's say:

- XAN token's address is `atest1v4ehgw36x3prs...`
- A user Bertha has address `atest1v4ehgw36xvcyyv...`
- Then, the balance of Bertha's XAN tokens is stored in the XAN account, with the storage key comprised of `#{token}/balance/#{owner}`, i.e.:

  ```text
  #atest1v4ehgw36x3prs.../balance/#atest1v4ehgw36xvcyy...
  ```

Any transaction can attempt to make changes to the storage of any account(s). Only if all the involved accounts accept, the transaction will it be committed. Otherwise, the transaction is rejected and its modifications discarded.

This approach allows multiparty transactions to be applied atomically, without any a priority coordination. It also gives accounts complete and fine-grained control over how they can be used in transactions in themselves and in relation to other accounts.

In fact, most of the functionality in the Anoma ledger is being built leveraging the simplicity and flexibility of this account system, from a simple fungible token to more complex accounts that integrate the Inter-blockchain Communication protocol and the Proof of Stake system.

## ☑ Validity predicates

A custom validity predicates can be built from scratch using `vp_template` (from root directory [`wasm/vp_template`](https://github.com/anoma/anoma/tree/master/wasm/vp_template)), which is Rust code compiled to WASM. Consult its `README.md` to find out more.

You can also check out the pre-built validity predicates' source code in the [`wasm/wasm_source`](https://github.com/anoma/anoma/tree/master/wasm/wasm_source), where each sub-module that begins with `vp_` implements a validity predicate. For example the [`vp_user`](https://github.com/anoma/anoma/blob/master/wasm/wasm_source/src/vp_user.rs) is the default validity predicate used for established accounts (created with `init-account` command).

A validity predicate's must contain the following function:

```rust
use anoma_vm_env::vp_prelude::*;

#[validity_predicate]
fn validate_tx(
    // The data attached to the transaction
    tx_data: Vec<u8>,
    // The address of the account where this validity predicate is used
    addr: Address,
    // The storage keys that were modified by the transaction
    keys_changed: BTreeSet<storage::Key>,
    // The addresses of all the accounts that are verifying the current 
    // transaction
    verifiers: BTreeSet<Address>,
) -> bool {
  // Returning `true` allows any key change
  true
}
```

You can think of it as its `main` function. When this VP is deployed to an account, this function will be called for every transaction that:

- Modifies a storage key that contains the account's address to which the validity predicate belongs
- Inserts the account's address into the verifier set with [`tx_prelude::insert_verifiers` function](https://docs.anoma.network/rustdoc/anoma_vm_env/imports/tx/fn.insert_verifier.html)

Inside the validity predicate function, you can read any storage value with the functions provided in the `vp_prelude` from the storage prior to the transaction (functions with name suffix `_pre`) and from the storage state after the transaction is applied (suffixed with `_post`).

To find out about the host interface available in a validity predicate, please check out [Rust docs for `vp_prelude`](https://docs.anoma.network/rustdoc/anoma_vm_env/vp_prelude/index.html).

To compile the validity predicate's code from the template:

```shell
make -C wasm/vp_template
```

This will output a WASM file that can be found in `wasm/vp_template/target/wasm32-unknown-unknown/release/vp_template.wasm`.

You can, for example, copy it into your `wasm` directory (the default directory used by the ledger's node and the client, which can be changed with `--wasm-dir` global argument or `ANOMA_WASM_DIR`):

```shell
cp \
  wasm/vp_template/target/wasm32-unknown-unknown/release/vp_template.wasm \
  wasm/my_vp.wasm
```

To submit a transaction that updates an account's validity predicate:

```shell
anoma client update --address my-new-acc --code-path my_vp.wasm
```

## 📩 Custom transactions

A transaction must contain a WASM code that can perform arbitrary storage changes. It can also contain arbitrary data, which will be passed onto the transaction and validity predicates when the transaction is being applied.

A custom transaction can be built from scratch using `tx_template` (from root directory [`wasm/tx_template`](https://github.com/anoma/anoma/tree/master/wasm/tx_template)), which is Rust code compiled to WASM. Consult its `README.md` to find out more.

For some inspiration, check out the pre-built transactions source code in the [`wasm/wasm_source`](https://github.com/anoma/anoma/tree/master/wasm/wasm_source), where each sub-module that begins with `tx_` implements a transaction.

A transaction code must contain the following function, which will be called when the transaction is being applied:

```rust
use anoma_vm_env::tx_prelude::*;

#[transaction]
fn apply_tx(tx_data: Vec<u8>) {
  // Do anything here
}
```

Inside the validity predicate function, you can read, write or delete any storage value with the functions provided in the `tx_prelude` from the storage of any account.

To find out about the interface available in a transaction, please check out [Rust docs for `tx_prelude`](https://docs.anoma.network/rustdoc/anoma_vm_env/tx_prelude/index.html).

Compile the transaction's code from the template:

```shell
make -C wasm/tx_template
```

This will output a WASM file that can be found in `wasm/tx_template/target/wasm32-unknown-unknown/release/tx_template.wasm`.

Submit the transaction to the ledger:

```shell
anoma client tx --code-path tx_template/tx.wasm
```

Optionally, you can also attach some data to the transaction from a file with the `--data-path` argument.
