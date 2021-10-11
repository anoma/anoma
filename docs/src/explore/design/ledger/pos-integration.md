# PoS integration

The [PoS system](../pos.md) is integrated into Anoma ledger at 3 different layers:
- base ledger that performs genesis initialization, validator set updates on new epoch and applies slashes when they are received from ABCI
- an account with an internal address and a [native VP](vp.md#native-vps) that validates any changes applied by transactions to the PoS account state
- transaction WASMs to perform various PoS actions, also available as a library code for custom made transactions

The `votes_per_token` PoS system parameter must be chosen to satisfy the [Tendermint requirement](https://github.com/tendermint/spec/blob/60395941214439339cc60040944c67893b5f8145/spec/abci/apps.md#validator-updates) of `MaxTotalVotingPower = MaxInt64 / 8`.

All [the data relevant to the PoS system](../pos.md#storage) are stored under the PoS account's storage sub-space, with the following key schema (the PoS address prefix is omitted for clarity):

- `params` (required): the system parameters
- for any validator, all the following fields are required:
  - `validator/{validator_address}/consensus_key`
  - `validator/{validator_address}/state`
  - `validator/{validator_address}/total_deltas`
  - `validator/{validator_address}/voting_power`
- `slash/{validator_address}` (optional): a list of slashes, where each record contains epoch and slash rate
- `bond/{bond_source}/{bond_validator} (optional)`
- `unbond/{unbond_source}/{unbond_validator} (optional)`
- `validator_set (required)`
- `total_voting_power (required)`

- standard validator metadata (these are regular storage values, not epoched data):
  - `validator/{validator_address}/staking_reward_address` (required): an address that should receive staking rewards
  - `validator/{validator_address}/address_raw_hash` (required): raw hash of validator's address associated with the address is used for look-up of validator address from a raw hash
  - TBA (e.g. alias, website, description, delegation commission rate, etc.)

Only XAN tokens can be staked in bonds. The tokens being staked (bonds and unbonds amounts) are kept in the PoS account under `{xan_address}/balance/{pos_address}` until they are withdrawn.

## Initialization

The PoS system is initialized via the shell on chain initialization. The genesis validator set is given in the genesis configuration. On genesis initialization, all the epoched data is set to be immediately active for the current (the very first) epoch.

## Staking rewards and transaction fees

Staking rewards for validators are rewarded in Tendermint's method `BeginBlock` in the base ledger. A validator must specify a `validator/{validator_address}/staking_reward_address` for its rewards to be credited to this address.

To a validator who proposed a block (`block.header.proposer_address`), the system rewards tokens based on the `block_proposer_reward` PoS parameter and each validator that voted on a block (`block.last_commit_info.validator` who `signed_last_block`) receives `block_vote_reward`.

All the fees that are charged in a transaction execution (DKG transaction wrapper fee and transactions applied in a block) are transferred into a fee pool, which is another special account controlled by the PoS module. Note that the fee pool account may contain tokens other than the staking token XAN.

- TODO describe the fee pool, related to <https://github.com/anomanetwork/anoma/issues/48>, <https://github.com/anomanetwork/anoma/issues/51> and <https://github.com/anomanetwork/anoma/issues/72>

## Transactions

The transactions are assumed to be applied in epoch `n`. Any transaction that modifies [epoched data](../pos.md#epoched-data) updates the structure as described in [epoched data storage](../pos.md#storage).

For slashing tokens, we implement a [PoS slash pool account](vp.md#pos-slash-pool-vp). Slashed tokens should be credited to this account and, for now, no tokens can be be debited by anyone.

### Validator transactions

The validator transactions are assumed to be applied with an account address `validator_address`.

- `become_validator(consensus_key, staking_reward_address)`:
  - creates a record in `validator/{validator_address}/consensus_key` in epoch `n + pipeline_length`
  - creates a record in `validator/{validator_address}/staking_reward_address`
  - sets `validator/{validator_address}/state` for to `pending` in the current epoch and `candidate` in epoch `n + pipeline_length`
- `deactivate`:
  - sets `validator/{validator_address}/state` for to `inactive` in epoch `n + pipeline_length`
- `reactivate`:
  - sets `validator/{validator_address}/state` for to `pending` in the current epoch and `candidate` in epoch `n + pipeline_length`
- `self_bond(amount)`:
  - let `bond = read(bond/{validator_address}/{validator_address}/delta)`
  - if `bond` exist, update it with the new bond amount in epoch `n + pipeline_length`
  - else, create a new record with bond amount in epoch `n + pipeline_length`
  - debit the token `amount` from the `validator_address` and credit it to the PoS account
  - add the `amount` to `validator/{validator_address}/total_deltas` in epoch `n + pipeline_length`
  - update the `validator/{validator_address}/voting_power` in epoch `n + pipeline_length`
  - update the `total_voting_power` in epoch `n + pipeline_length`
  - update `validator_set` in epoch `n + pipeline_length`
- `unbond(amount)`:
  - let `bond = read(bond/{validator_address}/{validator_address}/delta)`
  - if `bond` doesn't exist, panic
  - let `pre_unbond = read(unbond/{validator_address}/{validator_address}/delta)`
  - if `total(bond) - total(pre_unbond) < amount`, panic
  - decrement the `bond` deltas starting from the rightmost value (a bond in a future-most epoch) until whole `amount` is decremented
  - for each decremented `bond` value write a new `unbond` with the key set to the epoch of the source value
  - decrement the `amount` from `validator/{validator_address}/total_deltas` in epoch `n + unbonding_length`
  - update the `validator/{validator_address}/voting_power` in epoch `n + unbonding_length`
  - update the `total_voting_power` in epoch `n + unbonding_length`
  - update `validator_set` in epoch `n + unbonding_length`
- `withdraw_unbonds`:
  - let `unbond = read(unbond/{validator_address}/{validator_address}/delta)`
  - if `unbond` doesn't exist, panic
  - if no `unbond` value is found for epochs <= `n`, panic
  - for each `((bond_start, bond_end), amount) in unbond where unbond.epoch <= n`:
    - let `amount_after_slash = amount`
    - for each `slash in read(slash/{validator_address})`:
      - if `bond_start <= slash.epoch && slash.epoch <= bond_end)`, `amount_after_slash *= (10_000 - slash.rate) / 10_000`
    - credit the `amount_after_slash` to the `validator_address` and debit the whole `amount` (before slash, if any) from the PoS account
    - burn the slashed tokens (`amount - amount_after_slash`), if not zero
- `change_consensus_key`:
  - creates a record in `validator/{validator_address}/consensus_key` in epoch `n + pipeline_length`

For `self_bond`, `unbond`, `withdraw_unbonds`, `become_validator` and `change_consensus_key` the transaction must be signed with the validator's public key. Additionally, for `become_validator` and `change_consensus_key` we must attach a signature with the validator's consensus key to verify its ownership. Note that for `self_bond`, signature verification is also performed because there are tokens debited from the validator's account.

### Delegator transactions

The delegator transactions are assumed to be applied with an account address `delegator_address`.

- `delegate(validator_address, amount)`:
  - let `bond = read(bond/{delegator_address}/{validator_address}/delta)`
  - if `bond` exist, update it with the new bond amount in epoch `n + pipeline_length`
  - else, create a new record with bond amount in epoch `n + pipeline_length`
  - debit the token `amount` from the `delegator_address` and credit it to the PoS account
  - add the `amount` to `validator/{validator_address}/total_deltas` in epoch `n + pipeline_length`
  - update the `validator/{validator_address}/voting_power` in epoch `n + pipeline_length`
  - update the `total_voting_power` in epoch `n + pipeline_length`
  - update `validator_set` in epoch `n + pipeline_length`
- `undelegate(validator_address, amount)`:
  - let `bond = read(bond/{delegator_address}/{validator_address}/delta)`
  - if `bond` doesn't exist, panic
  - let `pre_unbond = read(unbond/{delegator_address}/{validator_address}/delta)`
  - if `total(bond) - total(pre_unbond) < amount`, panic
  - decrement the `bond` deltas starting from the rightmost value (a bond in a future-most epoch) until whole `amount` is decremented
  - for each decremented `bond` value write a new `unbond` with the key set to the epoch of the source value
  - decrement the `amount` from `validator/{validator_address}/total_deltas` in epoch `n + unbonding_length`
  - update the `validator/{validator_address}/voting_power` in epoch `n + unbonding_length`
  - update the `total_voting_power` in epoch `n + unbonding_length`
  - update `validator_set` in epoch `n + unbonding_length`
- `redelegate(src_validator_address, dest_validator_address, amount)`:
  - `undelegate(src_validator_address, amount)`
  - `delegate(dest_validator_address, amount)` but set in epoch `n + unbonding_length` instead of `n + pipeline_length`
- `withdraw_unbonds`:
  - for each `validator_address in iter_prefix(unbond/{delegator_address})`:
    - let `unbond = read(unbond/{validator_address}/{validator_address}/delta)`
    - if no `unbond` value is found for epochs <= `n`, `continue` to the next `validator_address`
    - for each `((bond_start, bond_end), amount)` in epochs <= `n`:
      - let `amount_after_slash = amount`
      - for each `slash in read(slash/{validator_address})`:
        - if `bond_start <= slash.epoch && slash.epoch <= bond_end)`, `amount_after_slash *= (10_000 - slash.rate) / 10_000`
      - credit the `amount_after_slash` to the `delegator_address` and debit the whole `amount` (before slash, if any) from the PoS account
      - burn the slashed tokens (`amount - amount_after_slash`), if not zero

For `delegate`, `undelegate`, `redelegate` and `withdraw_unbonds` the transaction must be signed with the delegator's public key. Note that for `delegate`, signature verification is also performed because there are tokens debited from the delegator's account.

## Slashing

Evidence for byzantine behaviour is received from Tendermint ABCI on `BeginBlock`. For each evidence:

- append the `evidence` into `slash/{evidence.validator_address}`
- calculate the slashed amount from deltas in and before the `evidence.epoch` in `validator/{validator_address}/total_deltas` for the `evidence.validator_address` and the slash rate
- deduct the slashed amount from the `validator/{validator_address}/total_deltas` at `pipeline_length` offset
- update the `validator/{validator_address}/voting_power` for the `evidence.validator_address` in and after epoch `n + pipeline_length`
- update the `total_voting_power` in and after epoch `n + pipeline_length`
- update `validator_set` in and after epoch `n + pipeline_length`

## Validity predicate

In the following description, "pre-state" is the state prior to transaction execution and "post-state" is the state posterior to it.

Any changes to PoS epoched data are checked to update the structure as described in [epoched data storage](../pos.md#storage).

Because some key changes are expected to relate to others, the VP also accumulates some values that are checked for validity after key specific logic:
- `balance_delta: token::Change`
- `bond_delta: HashMap<Address, token::Change>`
- `unbond_delta: HashMap<Address, token::Change>`
- `total_deltas: HashMap<Address, token::Change>`
- `total_stake_by_epoch: HashMap<Epoch, HashMap<Address, token::Amount>>`
- `expected_voting_power_by_epoch: HashMap<Epoch, HashMap<Address, VotingPower>>`: calculated from the validator's total deltas
- `expected_total_voting_power_delta_by_epoch: HashMap<Epoch, VotingPowerDelta>`: calculated from the validator's total deltas
- `voting_power_by_epoch: HashMap<Epoch, <HashMap<Address, VotingPower>>`
- `validator_set_pre: Option<ValidatorSets<Address>>`
- `validator_set_post: Option<ValidatorSets<Address>>`
- `total_voting_power_delta_by_epoch: HashMap<Epoch, VotingPowerDelta>`
- `new_validators: HashMap<Address, NewValidator>`

The accumulators are initialized to their default values (empty hash maps and hash set). The data keyed by address are using the validator addresses.

For any updated epoched data, the `last_update` field must be set to the current epoch.

The validity predicate triggers a validation logic based on the storage keys modified by a transaction:

- `validator/{validator_address}/consensus_key`:
  ```rust,ignore
  match (pre_state, post_state) {
    (None, Some(post)) => {
      // - check that all other required validator fields have been initialized
      // - check that the `state` sub-key for this validator address has been set
      // correctly, i.e. the value should be initialized at `pipeline_length` offset
      // - insert into or update `new_validators` accumulator
    },
    (Some(pre), Some(post)) => {
      // - check that the new consensus key is different from the old consensus
      // key and that it has been set correctly, i.e. the value can only be changed at `pipeline_length` offset
    },
    _ => false,
  }
  ```
- `validator/{validator_address}/state`:
  ```rust,ignore
  match (pre_state, post_state) {
    (None, Some(post)) => {
      // - check that all other required validator fields have been initialized
      // - check that the `post` state is set correctly:
      //   - the state should be set to `pending` in the current epoch and `candidate` at pipeline offset
      // - insert into or update `new_validators` accumulator
    },
    (Some(pre), Some(post)) => {
      // - check that a validator has been correctly deactivated or reactivated
      // - the `state` should only be changed at `pipeline_length` offset
      // - if the `state` becomes `inactive`, it must have been `pending` or `candidate`
      // - if the `state` becomes `pending`, it must have been `inactive`
      // - if the `state` becomes `candidate`, it must have been `pending` or `inactive`
    },
    _ => false,
  }
  ```
- `validator/{validator_address}/total_deltas`:
  - find the difference between the pre-state and post-state values and add it to the `total_deltas` accumulator and update `total_stake_by_epoch`, `expected_voting_power_by_epoch` and `expected_total_voting_power_delta_by_epoch`
- `validator/{validator_address}/voting_power`:
  - find the difference between the pre-state and post-state value and insert it into the `voting_power_by_epoch` accumulator
- `bond/{bond_source}/{bond_validator}/delta`:
  - for each difference between the post-state and pre-state values:
    - if the difference is not in epoch `n` or `n + pipeline_length`, panic
    - find slashes for the `bond_validator`, if any, and apply them to the delta value
    - add it to the `bond_delta` accumulator
- `unbond/{unbond_source}/{unbond_validator}/deltas`:
  - for each difference between the post-state and pre-state values:
    - if the difference is not in epoch `n` or `n + unboding_length`, panic
    - find slashes for the `bond_validator`, if any, and apply them to the delta value
    - add it to the `unbond_delta` accumulator
- `validator_set`:
  - set the accumulators `validator_set_pre` and `validator_set_post`
- `total_voting_power`:
  - find the difference between the post-state and pre-state
  - add it to the `total_voting_power_delta_by_epoch` accumulator
- PoS account's balance:
  - find the difference between the post-state and pre-state
  - add it to the `balance_delta` accumulator

No other storage key changes are permitted by the VP.

After the storage keys iteration, we check the accumulators:

- For each `total_deltas`, there must be the same delta value in `bond_delta`.
- For each `bond_delta`, there must be validator's change in `total_deltas`.
- Check that all positive `unbond_delta` also have a `total_deltas` update. Negative unbond delta is from withdrawing, which removes tokens from unbond, but doesn't affect total deltas.
- Check validator sets updates against validator total stakes.
- Check voting power changes against validator total stakes.
- Check expected voting power changes against `voting_power_by_epoch`.
- Check expected total voting power change against `total_voting_power_delta_by_epoch`.
- Check that the sum of bonds and unbonds deltas is equal to the balance delta.
- Check that all the new validators have their required fields set and that they have been added to the validator set
