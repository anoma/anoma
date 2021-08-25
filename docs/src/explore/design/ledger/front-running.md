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
 * fee payer: the account paying the above fee
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
 * The fee payer must have sufficient balance to pay
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
The key used to encrypt transactions for the current is created from the
result of a protocol run by the validators during the previous epoch and posted onto the blockchain. The
process for doing this is described in the chapter on [Distributed key generation](../dkg.md).

A user uses the public information as well as randomly generated salt to create
a key for a symmetric-key cipher (e.g. ChaCha20).  This key can be recovered
later from the ciphertext public key and decryption shares from at least 
\\( \frac{2}{3} \\) of validators by staking weight.

## Decrypting a transaction

Wrapped transactions that are included in block `n` shall be decrypted and 
applied in block `n+1` depending on the chosen decryption protocol.
We outline both protocols here.

We first state that in both protocols, during the Vote Extension phase, the
aggregated decryption shares needed to decrypt the transactions in the currently proposed
block must be submitted from each validator (and be verified by other 
validators). Validators must collect the decryption shares they receive in
case they must perform the decryption. The aggregation and validation of 
decryption shares is handled by Ferveo.

### Decrypt-ahead protocol

*__This protocol assumes that a validator knows that they are very likely to be
the next block proposer__*. 


After a validator who knows they are likely to be chosen next as block proposer
finalizes a block, they know the transactions to be decrypted and should
have the necessary decryption shares. They may then begin the decryption
and after this is finished may begin their Prepare Proposal phase. As
part of the Prepare Proposal phase, the decrypted payloads are included
into the proposed block.

#### Pros:
 - No alteration to block after Prepare Proposal phase
 - Less communication overhead
#### Cons:
 - Validators need to now in advance that they are likely to be the next proposer
 - Preparing new proposals is blocked until decryption is finished (which can be slow)
 - More than one validator will do decryption even though there will be a
   single proposer (wasted effort)

### Async-decrypt protocol
###TODO: This section doesn't make sense yet, need to figure out the details better
*__This protocol assumes that a validator can query the state of the
proposer after it has processed transactions__*.

As opposed to the above protocol, the current block proposer decrypts the 
wrapped transactions from the previous block as part of their Process Proposal
phase and then applies them. This means the computation happens asynchronously.

However, in order to make this work, it should be possible
for validators to query the state after the Process Proposal phase to see how
to update their own state before the block is committed. 

#### Pros:
 - Expensive decryptions are only done by a single party
 - Decryption is done asynchronously
 - Does not require foreknowledge of proposers
#### Cons:
 - More messaging overhead
 - Requires querying processed state of a proposer

### Verifying transaction decryption

The verification of each validators decryption shares is performed by the
VerifyVoteExtension call. Thus, upon block finalization, each validator
has a set of verified decryption shares from validators whose combined 
weight exceeds \\(\frac{2}{3}\\) of the total.

These decryption shares should be sufficient to decrypt the wrapped 
transactions but this can fail if:
 * The derived key does not decrypt the transaction
 * The decrypted payload does not agree with the commitment included in the
   wrapper transaction.

The block proposer must then include a proof that the transactions could not
be decrypted that other validators can verify. This proof consist of the 
decryption shares the validator attempted to use to decrypt the payload.

The other validators must check that these decryption shares are valid,
otherwise the fault lies with the block proposer for accepting them as
valid vote extensions in the previous round. Validators may then issue
a complaint against the block proposer.

If the decryption shares are indeed valid, the validators can use these to 
attempt to decrypt the payload and see that it failed for one of the two
above reasons. Again, if they produce a valid transaction, a complaint
may be issued against the block proposer. Otherwise, the proof is accepted
and the transaction is not applied. Validators do this check as part of their
Process Proposal phase.

If the block proposer claims that the decryption succeeded, validators must
check that the decrypted payload agrees with the included commitment, 
otherwise the proposer has incorrectly decrypted the transaction. In that
case, a complaint may be issued.

Alternatively, validators could simply try to decrypt the transaction with
their own decryption shares and if the transaction turns out to be valid or 
is in any way different from what is reported by the proposer,
the proposer is known to be at fault. This sometimes requires less 
computation and also removes the need for a proof to be added to the block.

__Question / TODO: If the block proposer is at fault, do we try to decrypt
the tx successfully later? And how? I suppose that other validators should
vote against the block and the next block proposer gets the job so that it
just takes more rounds to finalize the block. This means that we may not 
need complaints for this as the loss of proposer rewards provides
economic incentive for good behavior already.__


## Complaints

Complaints are encrypted transactions that validators broadcast when they
observe misbehavior from other validators during the protocol. The current
causes for complaint are as follows:
* `Share_Complaint`: During VerifyVoteExtension, the aggregated decryption
  shares from a validator does not pass validation
* `Decrypt_Complaint`: The block proposer incorrectly decrypted a tx or falsely
   claimed that a transaction could not be decrypted.
  
These transactions must include a proofs of malfeasance. The necessary
 proofs for the above complaints are as follows:
* `Share_Complaint`: The decryption share that failed verification
* `Decrypt_Complaint`: The alleged decrypted payload, the original wrapper tx,
  and the decryption shares that should have been used to derive the 
  decryption key.

The above proofs can be used to verify that the protocol was not adhered to.
Checking these proofs will be part of executing the Complaint transaction.
If the transaction succeeds and is added to the blockchain, a slashing
penalty will be determined based on the type of infraction, likely by a
native VP.
 