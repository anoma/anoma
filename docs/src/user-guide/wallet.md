# The Anoma Wallet

The Anoma wallet allows you to store and use addresses and keys by their alias.

The wallet's state is stored under `.anoma/{chain_id}/wallet.toml` (with the default `--base-dir`), which will be created for you if it doesn't already exist when you run any command that accesses the wallet. A newly created wallet will be pre-loaded with some default addresses.

For the ledger and intent gossip commands that use keys and addresses, you can enter their aliases as as defined in the wallet (case-sensitive).

By default, keys are stored encrypted (unless the `--unsafe-dont-encrypt` flag is used). Currently, the Anoma client can load the password via:
- file, by exporting an ENV variable called `ANOMA_WALLET_PASSWORD_FILE` with value containing the path to a file containing the password.
- env variable, by exporting a ENV variable called `ANOMA_WALLET_PASSWORD` with value of the actual password.
- stdin, the client will prompt for a password.

## 🔐 Keys

For cryptographic signatures, we currently support ed25519 keys. More will be added in future.

To manage keys, various sub-commands are available, see the commands `--help`:

```shell
anoma wallet key
```

List all known keys:

```shell
anoma wallet key list
```

Generate a new key:

```shell
anoma wallet key gen --alias my-key
```

Note that this will also save an implicit address derived from this public key under the same alias. More about addresses below. This command has the same effect as `address gen`.

## 📇 Addresses

All accounts in the Anoma ledger have a unique address, exactly one validity predicate and optionally any additional data in its dynamic storage sub-space.

There are currently 3 types of account addresses:

- Established: Used for accounts that allow the deployment of custom validation logic. These must be created on-chain via a transaction (see [the Ledger guide](./ledger.md#-initialize-an-account)). The address is generated on-chain and is not known until the transaction is applied.
- Implicit, *not yet fully supported in the ledger*: Derived from a key, which can be used to authorize certain transactions from the account. They can be used as recipients of transactions without even when the account has not been used on-chain before.
- Internal: Special internal accounts, such as protocol parameters account, PoS and IBC

To manage addresses, similar to keys, various sub-commands are available:

```shell
anoma wallet address
```

List all known addresses:

```shell
anoma wallet address list
```

Generate a new implicit address:

```shell
anoma wallet address gen --alias my-account
```

Note that this will also generate and save a key from which the address derived and save it under the same alias. Thus, this command has the same effect as `key gen`.
