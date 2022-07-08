# Cryptographic schemes

Anoma currently supports Ed25519 for signing transactions or any other arbitrary data, with support for more signature schemes to be added:

- [`Secp256k1`](https://github.com/anoma/anoma/issues/162)
- [`Sr25519`](https://github.com/anoma/anoma/issues/646)

## Public keys

A public key is a [Borsh encoded `PublicKey`](encoding.md#publickey).

## Signatures

A signature in Anoma is a [Borsh encoded `Signature`](encoding.md#signature).
