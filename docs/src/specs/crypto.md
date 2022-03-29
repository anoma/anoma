# Cryptographic schemes

Anoma currently supports Ed25519 signatures with more to be added:

- [`Secp256k1`](https://github.com/anoma/anoma/issues/162)
- [`Sr25519`](https://github.com/anoma/anoma/issues/646)

Please note that the Anoma's crypto public API and encoding is currently undergoing some breaking changes with <https://github.com/anoma/anoma/issues/225>.

## Public keys

A public key is a [Borsh encoded `PublicKey`](encoding.md#publickey). For the Ed25519 scheme, this is 32 bytes of Ed25519 public key, prefixed with `32` in little endian encoding (`[32, 0, 0, 0]` in raw bytes or `20000000` in hex). (TODO this will change with <https://github.com/anoma/anoma/issues/225>)

## Signatures

A signature in Anoma is a [Borsh encoded `Signature`](encoding.md#signature). For the Ed25519 scheme, this is 64 bytes of Ed25519 signature, prefixed with `64` in little endian encoding (`[64, 0, 0, 0]` in raw bytes or `40000000` in hex). (TODO this will change with <https://github.com/anoma/anoma/issues/225>)
