# Genesis validator setup

## Pre-genesis
To setup all the [required keys](#required-keys) for a genesis validator for an upcoming network, you can execute the following command with an alias of your choice. Note that this alias is public (the address of your validator account will be visible in every wallet) and must be unique within the network.

You must also provide a static `{IP:port}` to the `--net-address` argument of your future node's P2P address.

```shell
export ALIAS="1337-validator"
anoma client utils init-genesis-validator \
    --alias $ALIAS \
    --net-address 1.2.3.4:26656
```

After generating your keys, the command will print something like this:

```shell
Pre-genesis TOML written to .anoma/pre-genesis/1337-validator/validator.toml
```

This file is the public configuration of your validator. You can safely share this file with the network's organizer, who is responsible for setting up and publishing the finalized genesis file and Anoma configuration for the chain.

Note that the wallet containing your private keys will also be written into this directory.

## After network config release
Once the network is finalized, a new chain ID will be created and released on [anoma-network-config/releases](https://github.com/heliaxdev/anoma-network-config/releases) (a custom configs URL can be used instead with `ANOMA_NETWORK_CONFIGS_SERVER` env var). You can use it to setup your genesis validator node for the `--chain-id` argument in the command below.

```shell
anoma client utils join-network \
    --chain-id $CHAIN_ID \
    --genesis-validator $ALIAS
```

This command will use your pre-genesis wallet for the given chain and take care of setting up Anoma with Tendermint.

If you run this command in the same directory that you ran `anoma client utils init-genesis-validator`, it should find the pre-genesis wallet for you, otherwise you can pass the path to the pre-genesis directory using `--pre-genesis-path`. e.g.

```shell
anoma client utils join-network \
    --chain-id $CHAIN_ID \
    --pre-genesis-path workspace/.anoma/pre-genesis/$ALIAS
```

Once setup, you can start the ledger as usual with e.g.:

```shell
anoma ledger
```

## Required keys

- Account key: Can be used to sign transactions that require authorization in the default validator validity predicate, such as a balance transfer.
- Staking rewards key: Can be used to sign transactions on the PoS staking rewards account.
- Protocol key: This key is used by the validator's ledger itself to sign protocol transaction on behalf of the validator.
- DKG key: Special key needed for participation in the DKG protocol
- Consensus key: Used in Tendermint consensus layer. Currently, this key is written to a file which is read by Tendermint.
- Tendermint node key: This key is used to derive Tendermint node ID for P2P connection authentication. This key is also written to a file which is read by Tendermint.
