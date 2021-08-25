# Front-running prevention

[Tracking Issue](https://github.com/anoma/anoma/issues/42)

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
 * fee payer: an implicit account paying the above fee
 * fee payer signature: the above account's signature for verification
 * gas limit: A limit on the amount of gas the encrypted transaction can
   consume when it is applied.
 * A ciphertext public key that can be used for deriving the decryption key later by 
   using the decryption shares from enough validators.
 * A commitment to the encrypted payload (e.g. a BLAKE2b hash of the decrypted
   transaction)
 * The epoch number the tx is being encrypted to
 
The encryption of the wrapped transaction using the public key from the DKG
and the derivation of the ciphertext public key are provided by the Ferveo
library.

For a wrapped transaction to be included in a block proposal, the following 
conditions must be met:
 * The fee payers signature must be checked
 * The fee payer must have sufficient balance to pay the fee
 * The sum of all gas limits of transactions to be included cannot exceed
   the total gas limit of the block
 * The ciphertext public key must be checked for validity (handled by Ferveo)
 * The epoch included has not already finished

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
The key used to encrypt transactions for the current epoch is created from the
result of a protocol run by the validators during the previous epoch and posted onto the blockchain. The
process for doing this is described in the chapter on [Distributed key generation](../dkg.md).

A user uses the public information as well as randomly generated salt to create
a key for a symmetric-key cipher (e.g. ChaCha20).  This key can be recovered
later from the ciphertext public key and decryption shares from at least 
\\( \frac{2}{3} \\) of validators by staking weight.

## Decrypting a transaction

Wrapped transactions that are included in block `n` shall be decrypted and 
applied in block `n+1`. During the [Vote Extension](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#vote-extensions)
phase, the aggregated decryption shares needed to decrypt the transactions 
in the currently proposed block must be submitted from each validator (and be 
verified by other validators via the [VerifyVoteExtension](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#vote-extensions)
method). If an invalid decryption share is received, it is ignored.

Validators must collect the decryption shares they receive in case they 
are chosen as the next block proposer and must perform the decryption. The 
aggregation and validation of decryption shares is handled by Ferveo.

Since choosing the next block proposer is deterministic, validators can
know with reasonable certainty that  they are likely to be chosen next as
block proposer for block `n+1`. As such, when they finalize a block `n`, 
they know the transactions to be decrypted and should have the necessary 
decryption shares.

They may then begin the decryption and after this is finished may continue 
with their [Prepare Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#prepare-proposal)
phase. One expects that in the majority of cases that the decryption is 
completed before the next round begins. As part of their [Prepare Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#prepare-proposal)
phase, the decrypted payloads are included into the proposed block. 

If the decryption was not successful, the block proposer includes a proof 
that the wrapped transaction could not be decrypted. If the proof is 
accepted by the other validators, that transaction is rejected. If the proof
is not accepted, the other validators reject the proposal and may choose to
slash the proposer.

### Verifying transaction decryption

The verification of each validators decryption shares is performed by the
[VerifyVoteExtension](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#vote-extensions)
call. Thus, upon block finalization, each validator
has a set of verified decryption shares from validators whose combined 
weight exceeds \\(\frac{2}{3}\\) of the total.

These decryption shares should be sufficient to decrypt the wrapped 
transactions but this can fail if:
 * The derived key does not decrypt the transaction
 * The decrypted payload does not agree with the commitment included in the
   wrapper transaction.

The block proposer must then include a proof that the transactions could not
be decrypted that other validators can verify. This proof consists of the 
decryption shares the validator attempted to use to decrypt the payload.

The other validators must check that these decryption shares are valid,
otherwise the fault lies with the block proposer for accepting them as
valid vote extensions in the previous round. Validators will reject the 
proposal and may choose to slash the block proposer.

If the decryption shares are indeed valid, the validators can use these to 
attempt to decrypt the payload and see that it failed for one of the two
above reasons. Again, if they produce a valid transaction, the block is 
rejected and the block proposer may be slashed. Otherwise, the proof is 
accepted and the transaction is not applied. Validators do this check as 
part of their Process Proposal phase.

If the block proposer claims that the decryption succeeded, validators must
check that the decrypted payload agrees with the included commitment, 
otherwise the proposer has incorrectly decrypted the transaction. In that
case, the proposal must be rejected and the block proposer may be slashed.
