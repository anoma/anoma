# Introduction to governance.

The anoma governance mechanism gives users the possibility to upgrade the protocol dynamically.
There are two different mechanism to create a proposal:
- Onchain
- Offchain

## On chain proposals

### Create a proposal

Assuming you have an account with at least 500 NAM token (in this example we are going to use `my-new-acc`), lets get the corresponding address

```shell
anoma wallet address find `my-new-acc`
```

Now, we need to create a json file `proposal.json` holding the content of our proposal. Copy the below text into a json file.

```json
{
    "content": {
        "title": "Proposal title",
        "authors": "email@proposal.n",
        "discussions-to": "www.github.com/anoma/aip/1",
        "created": "2022-03-10T08:54:37Z",
        "license": "MIT",
        "abstract": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices. Quisque viverra varius cursus. Praesent sed mauris gravida, pharetra turpis non, gravida eros. Nullam sed ex justo. Ut at placerat ipsum, sit amet rhoncus libero. Sed blandit non purus non suscipit. Phasellus sed quam nec augue bibendum bibendum ut vitae urna. Sed odio diam, ornare nec sapien eget, congue viverra enim.",
        "motivation": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices.",
        "details": "Ut convallis eleifend orci vel venenatis. Duis vulputate metus in lacus sollicitudin vestibulum. Suspendisse vel velit ac est consectetur feugiat nec ac urna. Ut faucibus ex nec dictum fermentum. Morbi aliquet purus at sollicitudin ultrices. Quisque viverra varius cursus. Praesent sed mauris gravida, pharetra turpis non, gravida eros.",
        "requires": "2"
    },
    "author": "TODO",
    "voting_start_epoch": 3,
    "voting_end_epoch": 6,
    "grace_epoch": 12,
    "proposal_code_path": "./wasm_for_tests/tx_no_op.wasm"
}
```
You should change the value of:
- `Author` field with the address of `my-new-acc`.
- `voting_start_epoch` with future epoch (must be a multiple of 3)
- `voting_end_epoch` with an epoch greater of `voting_start_epoch`, multiple of 3.
- `grace_epoch` with an epoch greater of `voting_end_epoch` + 6
- `proposal_code_path` with the absolute path of the wasm file to execute (or remove the field completely)

As soon as your `proposal.json` file is ready, you can submit the proposal with (making sure to be in the same directory as the `proposal.json` file):

```shell
anoma client init-proposal --data-path proposal.json 
```

The transaction should have been accepted. You can query all the proposals with:

```shell
anoma client query-proposal
```

or a single proposal with
```shell
anoma client query-proposal --proposal-id 0
```

where `0` is the proposal id.

### Vote a proposal

Only validator and delegators can vote. Assuming you have a validator/delegator account (in this example we are going to use `validator`), you can send a vote with the following command:

```shell
anoma client vote-proposal --proposal-id 0 --vote yay --signer validator
```

where `--vote` can be either `yay` or `nay`.

### Check the result

As soon as the ledger reaches epoch definied in the json as `voting_end_epoch`, you can no longer vote. The code definied in `proposal_code` json field will be executed at the beginning of `grace_epoch` epoch. You can use the following commands to check the status of a proposal:

```shell
anoma client query-proposal --proposal-id 0
```

or to just check the result:

```shell
anoma client query-proposal-result --proposal-id 0
```

## Offchain proposals

### Create proposal

Create the same json file as in the on-chain proposal and use the following command:

```shell
anoma client init-proposal --data-path proposal.json --offline
```

This command will create a `proposal` file same directory where the command was launched.

### Vote proposal

To vote an offline proposal use the following command:
```shell
anoma client vote-proposal --data-path proposal --vote yay --signer validator --offline
```

This command will create a `proposal-vote-${address}` file (where address is the `--signer` address).

### Tally offline proposal

To compute the tally for an offline proposal we need to collect
- `proposal` file (must have this name)
- all the `proposal-vote-${address}` files

All those files will have to be in a folder (lets call it `offline-proposal`).

Now you can use the following command:
```shell
anoma client query-proposal-result --offline --data-path `offline-proposal`
```

which will tell you the proposal result.