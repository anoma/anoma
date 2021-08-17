# Front-running prevention

[Tracking Issue](https://github.com/anomanetwork/anoma/issues/42)

A problem that many blockchains face is that block proposers can use their
knowledge of the next transactions to be included in a block to their 
advantage e.g, by inserting transactions of their own ahead of other user's
or censoring transactions not advantageous to them. This is called 
*front-running*.

Anoma seeks to address this issue by encrypting transactions so that their
contents are not known by block proposers. Only after the particular transactions
to be included and their orders have been agreed upon via the consensus layer
can we then decrypt the transactions, revealing their contents.

In practice, the encrypted transactions will be included via a wrapper
transaction in a block. Then in the subsequent block, the decrypted inner 
contents will be included as a separate transaction. Thus, a user's 
"transaction" is really two transactions from the view of the blockchain: the
outer transaction which wraps an inner transaction within to be included 
after it is decrypted.

## Wrapper transactions

A wrapper transaction contains an encrypted payload that will be another
transaction upon decryption. In addition to this payload, the wrapper 
transaction will contain the following data:
 * fee: the price of including the block onto the chain
 * fee payer: the account paying the above fee
 * fee payer signature: the above account's signature for verification
 * gas limit: A limit on the amount of gas the encrypted transaction can
   consume when it is applied.
 * A ciphertext public key that can be used for deriving the decryption key later by 
   using the decryption shares from enough validators.
 
The encryption of the wrapped transaction using the public key from the DKG
and the derivation of the ciphertext public key are provided by the Ferveo
library.

For a wrapped transaction to be included in a block proposal, the following 
conditions must be met:
 * The fee payers signature must be checked
 * The fee payer must have sufficient balance to pay the fee
 * The fee payer must have sufficient balance to pay
 * The sum of all gas limits of transactions to be included cannot exceed
   the total gas limit of the block.
 * The ciphertext public key must be checked for validity (handled by Ferveo)

We note a few things above. Firstly, it is not required that the encrypted
payload be a valid transaction or even able to be decrypted. In either case,
the wrapper transaction may still be included onto the blockchain and the fee
for its inclusion deducted from the fee payer.

Secondly, the fee payer of wrapper transaction does not have to be the same
as the fee payer of the encrypted transaction. The whole point is that as
little as possible is known about the inner transaction.

However, the gas limits do leak some information about the encrypted transaction.
To mitigate this somewhat the following approach is taken: A constant, 
`GAS_LIMIT_RESOLUTION` is defined and gas limits must be specified as multiples
of this constant. The larger `GAS_LIMIT_RESOLUTION`, the less information is
leaked about the inner transaction.

However, to prevent users from setting gas limits higher than necessary, if the gas limit of a transaction is specified as 
k * `GAS_LIMIT_RESOLUTION`, then the user will have to pay for at least 
(k - 1) * `GAS_LIMIT_RESOLUTION` units of gas. Otherwise, they are charged only
for the units of gas that their transaction actually consumes. And as usual, if
their transaction exceeds the gas limit, the application of the transaction
is halted.

### Encrypting a transaction
The key used to encrypt transactions for the current is created by the
validators during the previous epoch and posted onto the blockchain. The
process for doing this is described in the chapter on [Distributed key generation](../dkg.md).

## Decrypting a transaction

### Verifying that a transaction could not be decrypted