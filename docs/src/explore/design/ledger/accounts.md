# Accounts

[Tracking Issue](https://github.com/anoma/anoma/issues/45)

---

There's only a single account type. Each account is associated with:
- a unique [address](#addresses)
- a [validity predicate](./vp.md)
- [dynamic storage sub-space](#dynamic-storage-sub-space)

## Addresses

There are two main types of address: transparent and shielded.

The transparent addresses are the addresses of accounts associated with dynamic storage sub-spaces, where the address of the account is the prefix key segment of its sub-space.

The shielded addresses are used for private transactions and they are not directly associated with storage sub-spaces.

### Transparent addresses

Furthermore, there are three types of transparent addresses:
- "implicit" addresses which are derived from public keys
- "established" addresses which are generated from the current address nonce and hence must be created via a request in the ledger
- "internal" addresses are used for special modules integrated into the ledger such as PoS and IBC.

The addresses are stored on-chain encoded with [bech32m](https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki) ([not yet adopted in Zcash](https://github.com/zcash/zips/issues/484)), which is an improved version of [bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki).

The human-readable prefix (as specified for [bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#specification)) in the address encoding is:
- `"a"` for Anoma live network
- `"atest"` for test network

#### Implicit transparent addresses

As implied by their name, accounts for implicit addresses exist as a possibility and not as a matter of fact. These addresses allow users to interact with public keys which may or may not have a registered on-chain account, e.g. allowing to send some fungible token to an address derived from a public key. An implicit address is derived from a hash of a public key, which also helps to protect keys for which the public key has not been revealed publicly.

#### Established transparent addresses

Established addresses are created by a ledger transaction, which can create any number of new account addresses. The users are not in control of choosing the address as it's derived from the current address nonce, which is changed after every newly established address.

#### Internal transparent addresses

There will be a static set of internal addresses that integrate certain functionality into the ledger via a dedicated module, such as the proof-of-stake module and the IBC module. The internal accounts use [native validity predicates](vp.md#native-vps) to validate transactions that interact with their module. A native module will use the [dynamic storage sub-space](#dynamic-storage-sub-space) to store all the data relevant to their functionality (e.g. PoS parameters, bond pool, IBC state and proofs).

### Shielded addresses

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
