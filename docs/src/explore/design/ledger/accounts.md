# Accounts

[Tracking Issue](https://github.com/anoma/anoma/issues/45)

---

There's only a single account type. Each account is associated with:

- a unique [transparent address](../../../specs/ledger.html#transparent-addresses)
- a [validity predicate](./vp.md)
- [dynamic storage sub-space](#dynamic-storage-sub-space)

## Shielded addresses

Similar to [Zcash Sapling protocol payment addresses and keys (section 3.1)](https://raw.githubusercontent.com/zcash/zips/master/protocol/protocol.pdf), users can generate spending keys for private payments. A shielded payment address, incoming viewing key and full viewing key are derived from a spending key. In a private payment, a shielded payment address is hashed with a diversifier into a diversified transmission key. When a different diversifier function is chosen for different transactions, it prevents the transmission key from being matched across the transactions.

The encoding of the shielded addresses, spending and viewing keys is not yet decided, but for consistency we'll probably use a the same schema with different prefixes for anything that can use an identifier.

- TODO consider using a schema similar to the [unified addresses proposed in Zcash](https://github.com/zcash/zips/issues/482), that are designed to unify the payment addresses across different versions by encoding a typecode and the length of the payment address together with it. This may be especially useful for the protocol upgrade system and fractal scaling system.

## Dynamic storage sub-space

Each account can have an associated dynamic account state in the storage. This
state may be comprised of keys of the built-in supported types and values of arbitrary user bytes.

The dynamic storage sub-space could be a unix filesystem-like tree under the
account's address key-space with `read, write, delete, has_key, iter_prefix`
(and maybe a few other convenience functions for hash-maps, hash-sets, optional values, etc.) functions parameterized with the the account's address.

In addition, the storage sub-space would provide:

- a public type/trait for storage keys and key segments:
  - this should allow to turn types to storage key segments, key segments back to types
  - combine key segments into keys
  - can be extended with custom types in the code in a transaction
- a public type/trait for storage values:
  - values need to implement encoding traits, e.g. `BorshSerialize, BorshDeserialize`
    - this allows composition of types as specified for [Borsh](https://borsh.io)
    - the Merkle tree hashing function should hash values from the encoded bytes of this trait (the encoded value may be cached, because we update the Merkle tree in-memory before we commit the finalized block to the DB)
- functions to get the size of a key and an encoded value (for storage fees)
- the updates to account storage should be immediately visible to the transaction that performed the updates
  - validity predicate modifications have to be handled a little differently -
    the old validity predicate should be run to check that the new validity
    predicate (and other state changes included in the transaction) is valid

## Initializing a new account

A new account can be initialized on-chain with a transaction:

- anything be written into its storage (initial parameter)
- a validity predicate has to be provided (we can have a default out-of-band)
- at minimum, accounts need to be enumerated on chain, this could be done with an address or a counter

A newly created account should be validated by all the VPs triggered by the transaction, i.e. it should be included in the set of changed keys passed to each VP. If the VPs are not interested in the newly created account, they can choose to ignore it.
