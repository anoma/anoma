# MASP

## Using MASP

### Shielded transfers

Shielded balances are owned by a particular spending key.

To try out shielded transfers, you will first need an ordinary
transparent account with some token balance. Example commands for that:

```
anomaw address gen --alias my-implicit
anomac init-account --source my-implicit --public-key my-implicit --alias my-established
anomac transfer --token btc --amount 1000 --source faucet --target my-established --signer my-established
```

The testnet tokens which the faucet can provide you are named `XAN`,
`BTC`, `ETH`, `DOT`, `Schnitzel`, `Apfel`, and `Kartoffel`. The faucet
will transfer these in increments of 1000 at a time.

Once you have an ordinary transparent account with some tokens, you
should generate a spending key to hold your shielded balances. You
can randomly generate one with e.g. `anomaw masp gen-key --alias [your spending key alias]`.
This command will also generate a corresponding viewing key sharing
the same alias.

Shielded transfers work with the `anomac transfer` command. `--source`
can be supplied with a spending key alias to spend a shielded balance, but
if you are following along, you don't have a shielded balance to spend yet.
`--target` can be supplied with a shielded payment address to create a shielded
balance.

To create a payment address from your spending key, use:

```
anomaw masp gen-addr --key [your viewing key alias] --alias [your payment address alias]
```

This will generate a different payment address each time you run it.
Payment addresses can be reused or discarded as you like, and can't be
correlated with one another.

Once you have a payment address, transfer a balance from your
transparent account to your shielded spending key with something like:

```
anomac transfer --source my-established --target [your payment address alias] --token btc --amount 100
```

Once this transfer goes through, you can view your viewing key's
balance:

```
anomac balance --owner [your viewing key alias]
```

Now that you have a shielded balance, it can either be transferred to a
different shielded payment address (shielded to shielded):

```
anomac transfer --source [your spending key] --target [someone's payment address] --token btc --amount 50 --signer my-established
```

or to a transparent account (shielded to transparent):

```bash
anomac transfer --source [your spending key] --target [some transparent account] --token btc --amount 50 --signer my-established
```

Note that for the second last type of transfer, `--signer` must be
specified. However, any transparent account can sign these transactions.

## Troubleshooting

### Setup the MASP parameters

Namada uses a multi-asset shielded pool (MASP) to enable private transfers. The pool relies on three circuits which require each individually their randomly generated parameters to work.

>⚠️ Normally, the parameters are downloaded through the `masp` crate by the client, but in case of troubles you should get them from someone in the team and follow the instructions below.

<!-- You can download the parameters with:
```bash
[command]
``` -->

The parameters need to be extracted to the correct folder where the node will read the parameters from. 

**Ubuntu**
```bash
mkdir ~/.masp-params
tar -xvf masp-params.tar.gz ~/.masp-params
```
**Mac**
```bash
mkdir ~/Library/Application\ Support/MASPParams/
tar -xvf masp-params.tar.gz ~/Library/Application\ Support/MASPParams/
```
