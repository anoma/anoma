# Default transactions

The Anoma client comes with a set of pre-built transactions. Note that the Anoma ledger is agnostic about the format of the transactions beyond the format described in [ledger transaction section](../ledger.md#transactions).

The [default validity predicates](default-validity-predicates.md) can be used to initialize the network's genesis block. These expect the data in the storage to be encoded with [Borsh](../encoding.md#borsh-binary-encoding) and are fully compatible with the default transactions described below.

## Rust-to-WASM transactions

The following transactions are pre-built from Rust code and can be used by clients interacting with the Anoma ledger.

The pre-built WASM code's raw bytes should be attached to the transaction's `code` field. The transactions expect certain variables to be provided via the transaction's `data` field encoded with [Borsh](../encoding.md#borsh-binary-encoding).

### tx_init_account

Initialize a new [established account](../../explore/design/ledger/accounts.md#established-transparent-addresses) on the chain.

To use this transaction, attach [InitAccount](../encoding.md#initaccount) to the `data`.

### tx_init_validator

Initialize a new validator account on the chain.

Attach [InitValidator](../encoding.md#initvalidator) to the `data`.

### tx_transfer

Transparently transfer `amount` of fungible `token` from the `source` to the `target`.

Attach [Transfer](../encoding.md#transfer) to the `data`.

### tx_update_vp

Update a validity predicate of an established account.

Attach [UpdateVp](../encoding.md#updatevp) to the `data`.

### tx_bond

Self-bond `amount` of XAN token from `validator` (without `source`) or delegate to `validator` from `source`.

Attach [Bond](../encoding.md#bond) to the `data`.

### tx_unbond

Unbond self-bonded `amount` of XAN token from the `validator` (without `source`) or unbond delegation from the `source` to the `validator`.

Attach [Bond](../encoding.md#bond) to the `data`.

### tx_withdraw

Withdraw unbonded self-bond from the `validator` (without `source`) or withdraw unbonded delegation from the `source` to the `validator`.

Attach [Withdraw](../encoding.md#withdraw) to the `data`.

## Signing transactions

To sign transactions in format that is understood and thus can be verified by the [default validity predicates](default-validity-predicates.md), the SHA-256 hash of the `data` [encoded with Borsh](../encoding.html#borsh-binary-encoding) MUST be [signed](../crypto.md#signatures) by an implicit or established account's key. The encoded signed data together with the signature should be encoded as a [`SignedTxData`](../encoding.md#signedtxdata) and also encoded with Borsh. This data should then be attached to a protobuf encoded transaction's `data` field.
