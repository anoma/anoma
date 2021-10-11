# 🔏 Interacting with the Proof-of-Stake system

The Anoma Proof of Stake system is using the XAN token as the staking token. It features delegation to any number of validators and customizable validator validity predicates.

The PoS system is implemented as an account with a validity predicate that governs the rules of the system. You can find its address in your wallet:

```shell
anoma wallet address find --alias PoS
```

The system relies on the concept of epochs. An epoch is a range of consecutive blocks identified by consecutive natural numbers. Each epoch last a minimum duration and includes a minimum number of blocks since the beginning of the last epoch. These are defined by protocol parameters.

To query the current epoch:

```shell
anoma client epoch
```

You can delegate to any number of validators at any time. When you delegate tokens, the delegation won't count towards the validator's stake (which in turn determines its voting power) until the beginning of epoch `n + 2` in the current epoch `n` (the literal `2` is set by PoS parameter `pipeline_len`). The delegated amount of tokens will be deducted from your account immediately and will be credited to the PoS system's account.

To submit a delegation that bonds tokens from the source address to a validator with alias `validator-1`:

```shell
anoma client bond \
  --source my-new-acc \
  --validator validator-1 \
  --amount 12.34
```

You can query your delegations:

```shell
anoma client bonds --owner my-new-acc
```

The result of this query will inform the epoch from which your delegations will be active.

Because the PoS system is just an account, you can query its balance, which is the sum of all staked tokens:

```shell
anoma client balance --owner PoS
```

Should a validator exhibit punishable behavior, the delegations towards this validator are also liable for slashing. Only the delegations that were active in the epoch in which the fault occurred will be slashed by the slash rate of the fault type. If any of your delegations have been slashed, this will be displayed in the `bonds` query. You can also find all the slashes applied with:

```shell
anoma client slashes
```

While your tokens are being delegated, they are locked-in the PoS system and hence are not liquid until you withdraw them. To do that, you first need to send a transaction to “unbond” your tokens. You can unbond any amount, up to the sum of all your delegations to the given validator, even before they become active.

To submit an unbonding of a delegation of tokens from a source address to the validator:

```shell
anoma client unbond \
  --source my-new-acc \
  --validator validator-1 \
  --amount 1.2
```

When you unbond tokens, you won't be able to withdraw them immediately. Instead, tokens unbonded in the epoch `n` will be withdrawable starting from the epoch `n + 6` (the literal `6` is set by PoS parameter `unbonding_len`). After you unbond some tokens, you will be able to see when you can withdraw them via `bonds` query:

```shell
anoma client bonds --owner my-new-acc
```

When the chain reaches the epoch in which you can withdraw the tokens (or anytime after), you can submit a withdrawal of unbonded delegation of tokens back to your account:

```shell
anoma client withdraw \
  --source my-new-acc \
  --validator validator-1
```

On success, the withdrawn tokens will be credited back your account and debited from the PoS system.

To see all validators and their voting power, you can query:

```shell
anoma client voting-power
```

With this command, you can specify `--epoch` to find the voting powers at some future epoch. Note that only the voting powers for the current and the next epoch are final.

## 📒 PoS Validators

To register a new validator account, run:

```shell
anoma client init-validator \
  --alias my-validator \
  --source my-new-acc
```

This command will generate the keys required for running a validator:

- Consensus key, which is used in [signing blocks in Tendermint](https://docs.tendermint.com/master/nodes/validators.html#validator-keys).
- Validator account key for signing transactions on the validator account, such as token self-bonding, unbonding and withdrawal, validator keys, validity predicate, state and metadata updates.
- Staking reward account key for signing transactions on the staking reward accounts. More on this account below.

Then, it submits a transaction to the ledger that generates two new accounts with established addresses:

- Validator account with the main validator address, which can be used to receive new delegations
- Staking reward account, which will receive rewards for participation in the PoS system. In future, the validity predicate of this account will be able to control how the rewards are to be distributed to the validator's delegators. *Staking rewards are not yet implemented*.

These keys and addresses of the addresses will be saved in your wallet. You local ledger node will also be setup to run this validator, you just have to shut down it down with e.g. `Ctrl + C`, then start it again with the same command:

```shell
anoma ledger
```

The ledger will then use the validator consensus key to sign blocks, should your validator account acquire enough voting power to be included in the active validator set. The size of the active validator set is limited to `128` (the limit is set by the PoS `max_validator_slots` parameter).

Note that the balance of XAN tokens that is in your validator account doesn't count towards your validator's stake and voting power:

```shell
anoma client balance --owner my-validator --token XAN
```

That is, the balance of your account's address is a regular liquid balance that you can transfer using your validator account key, depending on the rules of the validator account's validity predicate. The default validity predicate allows you to transfer it by a signed transaction and/or stake it in the PoS system.

You can submit a self-bonding transaction of tokens from a validator account to the PoS system with:

```shell
anoma client bond \
  --validator my-validator \
  --amount 3.3
```

Validator's voting power is determined from the sum of all their active self-bonds and delegations of tokens, with slashes applied, if any, divided by `1000` (PoS `votes_per_token` parameter, with current value set to `10‱` in parts per ten thousand).

Same rules apply as with delegations. When you self-bond tokens, the bonded amount won't count towards your validator's stake (which in turn determines your power) until the beginning of epoch `n + 2` in the current epoch `n`. The bonded amount of tokens will be deducted from the validator's account immediately and will be credited to the PoS system's account.

While your tokens are being self-bonded, they are locked-in the PoS system and hence are not liquid until you withdraw them. To do that, you first need to send a transaction to “unbond” your tokens. You can unbond any amount, up to the sum of all your self-bonds, even before they become active.

To submit an unbonding of self-bonded of tokens from your validator:

```shell
anoma client unbond \
  --validator my-validator \
  --amount 0.3
```

Again, when you unbond tokens, you won't be able to withdraw them immediately. Instead, tokens unbonded in the epoch `n` will be withdrawable starting from the epoch `n + 6`. After you unbond some tokens, you will be able to see when you can withdraw them via `bonds` query:

```shell
anoma client bonds --validator my-validator
```

When the chain reaches the epoch in which you can withdraw the tokens (or anytime after), you can submit a withdrawal of unbonded tokens back to your validator account:

```shell
anoma client withdraw --validator my-validator
```
