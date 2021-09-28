# Data schema

At high level, all the data in the [accounts' dynamic
sub-spaces](../accounts.md#dynamic-storage-sub-space) is just keys associated with
arbitrary bytes and intents are just wrapper around arbitrary data. To help the
processes that read and write this data (transactions, validity predicates,
matchmaker) interpret it and implement interesting functionality on top it, the
ledger could provide a way to describe the schema of the data.

For storage data encoding, we're currently using the borsh library, which
provides a way to derive schema for data that can describe its structure in a
very generic way that can easily be consumed in different data-exchange formats
such as JSON. In Rust code, the data can be composed with Rust native ADTs
(`struct` and `enum`) and basic collection structures (fixed and dynamic sized
array, hash map, hash set). Borsh already has a decent coverage of different
implementations in e.g. JS and TypeScript, JVM based languages and Go, which
we'll hopefully be able to support in wasm in near future too.

Note that the borsh data schema would not be forced upon the users as they can
still build and use custom data with arbitrary encoding.

A naive implementation could add optional `schema` field to each stored key. To
reduce redundancy, there could be some "built-in" schemas and/or specific
storage space for commonly used data schema definitions. Storage fees apply, but
perhaps they can be split between all the users, so some commonly used data
schema may be almost free.

A single address in the ledger is define with all schema. A specific schema can
be looked up with a key in its subspace. The schema variable is not yet
implemented and the definition might change to something more appropiate.

## Schema derived library code

### account example
Let's start with an example, in which some users want to deploy a
multi-signature account to some shared asset. They create a transaction, which
would initialize a new account with an address `shared-savings` and write into
its storage sub-space the initial funds for the account and data under the key
`"multisig"` with the following definition:

```rust
#[derive(Schema)]
struct MultiSig {
    threshold: u64,
    counter: u64,
    keys: Vec<PublicKey>,
}
```

When the transaction is applied, the data is stored together with a reference to
the derived data schema, e.g.:

```json
{
  "MultiSig": {
    "struct": {
      "named_fields": {
        "threshold": "u64",
        "counter": "u64",
        "keys": {
          "sequence": "PublicKey"
        }
      }
    }
  }
}
```

Now any transaction that wants to interact with this account can look-up and use its data schema. We can also use this information to display values read from storage from e.g. RPC or indexer.

What's more, when the data has schema attached on-chain, with borsh we have bijective mapping between the data definitions and their schemas. We can use this nice property to generate code for data definitions back from the schema in any language supported by borsh and that we'll able to support in wasm.

We can take this a step further and even generate some code for data access on top of our wasm environment functions to lift the burden of encoding/decoding data from storage. For our example, from the key `"multisig"`, in Rust we can generate this code:

```rust
fn read_multisig() -> MultiSig;
fn write_multisig(MultiSig);
fn with_multisig(FnMut(MultiSig) -> MultiSig);
```

Which can be imported like regular library code in a transaction and arbitrarily extended by the users. Similarly, the schema could be used to derive some code for validity predicates and intents.

We can generate the code on demand (e.g. we could allow to query a node to generate library code for some given accounts for a given language), but we could also provide some helpers for e.g. foundation's or validator's node to optionally automatically publish generated code via git for all the accounts in the current state. In Rust, using this library could look like this:

```rust
// load the account(s) code where the identifier is the account's address.
use anoma_accounts::SharedSavings;

fn transaction(...) {
  let multisig = SharedSavings::read_multisig();
  ...
}
```
