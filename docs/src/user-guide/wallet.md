# The Anoma wallet

The wallet's state is stored under `.anoma/wallet.toml` (with the default `--base-dir`), which will be created if it doesn't already exist. A newly created wallet will be pre-loaded with some default addresses.

The ledger and intent gossip commands that use keys and addresses may use their aliases as defined in the wallet.

Manage keys, various sub-commands are available, see the commands `--help`:

```bash
anoma wallet key
```

List all known keys:

```bash
anoma wallet key list
```

Generate a new key:

```bash
anoma wallet key gen --alias my-key
```

Manage addresses, again, various sub-commands are available:

```bash
anoma wallet address
```

List all known addresses:

```bash
anoma wallet address list
```
