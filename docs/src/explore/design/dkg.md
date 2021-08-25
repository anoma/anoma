# Distributed key generation

Here we give an overview of how the DKG protocol works with the ledger and 
the roles of each node in the network per this protocol. This portion of the 
book focuses solely on the production of a new public encryption key via the
DKG scheme. It does not cover encryption, decryption, or validation of transactions.

## Starting the Protocol

The protocol to generate a new key will be scheduled to begin at specific 
block heights, in practice this will be at the beginning of every epoch.
In  addition to starting at the pre-determined blocks heights, if the protocol 
fails, all validators know to immediately start a new round of the protocol.

It is important to note that the key that is being generated is to be used
to encrypt transactions in the epoch following the current one. However,
this relies on the assumption that changes in the validator set between 
epochs leaves an overlap or validators whose combined weight exceeds 
\\( \frac{2}{3} \\) of \\W\\. Otherwise, it will be impossible to decrypt
transactions. If this does not happen a new key will need to be generated. 
Until the new key is generated, transactions will not be accepted.

##  Computing weight shares

In order for the DKG protocol to work, it requires the contribution of 
 \\(\frac{2}{3} \\) of the total staked weight among validators. The Proof
of stake system maintains a list of validators and their voting powers in
storage. This  can be queried by each validator to get the set of validators,
their associated address, and voting power (or weight).

The total weight will be scaled down to a fixed parameter 
\\( W\\) (and fractional weights rounded appropriately to achieve integral
values). After this procedure, each validator has a corresponding number of
weight shares. 

The DKG protocol requires a canonical ordering of validators. The ordering
will be chosen by ordering validators in terms of decreasing voting power.
If there is a tie, it is broken by considering the lexicographic ordering
on their associated addresses. 

This allows each validator to independently compute the partition of weight
shares themselves which will be needed for the following steps of the DKG 
protocol. This partitioning is computed by Ferveo.

## Dealers
In a single DKG instance, a subset of validators holding at least
\\( \frac{2}{3} \\) of the weight shares must each act as dealers. It
is the responsibility of a dealer to post a signed message to the blockchain
which includes the session id (\\( \tau \\)) as well the data of the
PVSS (we do not concern ourselves with the details here). The creation of this
message is handled by the Ferveo library.

The signed message will be returned as part of the vote extension. However,
for performance reasons, these transcripts will not be verified at this stage.
Rather, the verification will be performed when an aggregation is attempted
(discussed next). This verification is provided by the Ferveo library. The

## Aggregation

At the end of the vote extension phase, enough validators 
(meaning their combined weight  shares exceeds \\( \frac{2}{3}\\) of the 
total) *should* have shared their PVSS to the current proposer and hopefully
enough of these transcripts are valid. This is because the vote extension 
phase requires at least \\( \frac{2}{3}\\) of validators to send a vote
extension before it can move to the finalization phase.

At this point, the next block proposer can aggregate the messages into a 
single instance using Ferveo during its Prepare Proposal phase at the beginning
of the next round. If the aggregation succeeds, the result of this is
included in the header of the proposed block along with the PVSS transcripts.
Once this is on the blockchain, it can be used for encryption of transactions until the next 
DKG instance is run.

When deciding to aggregate the PVSS instances into a single one, we shall
consider the instances ranked in terms of their validators as was also used
for partitioning the weight shares (described above).

If the aggregation fails, the proposer should instead include a 
special transaction into its proposed block (a transaction that only proposers
can make). This transaction states that the aggregation could not succeed 
because some PVSS transcripts are invalid. In the Process Proposal phase,
each node should check each PVSS transcript and if any are found to be invalid,
they update their local state, slashing the author of the offending transcript
appropriately. This amount should be significant to mitigate denial of service
attack vectors.

If the block proposer is found to be in error instead, other validators should
simply reject the proposed block. __TBD: (And do we slash the proposer as well?)__

## Storage considerations

The storage of a node must be adapted to accommodate the DKG protocol. Each
instance of a DKG protocol from Ferveo is a state machine that stores
much of the necessary data from the protocol instance. The following data
needs to be stored by each node
 - The partition of the weight shares among the current validator set
 - A session keypair for the current DKG session. Used for encrypting
   PVSS transcripts and signing DKG protocol messages.
 - A list of session ids from the last completed DKG instance up until the
   latest started (we may prune ids for failed instances however) 
 - The DKG state machine associated to each of these session ids.

##  The DKG state machine

We first describe each new message type needed for the protocol.
- `Deal`: This message contains the PVSS transcript from a validator
  acting as a dealer
- `Aggregate`: Add the combined PVSS transcripts as well as each transcript
  included in the aggregation in the header of a proposed block
- `Complain`: A special transaction stating that the block proposer could
  not aggregate the PVSS transcripts and an inclusion of the transcripts. 

In Ferveo, the DKG protocol is modelled as a state machine. This allows a
validator to initialize an instance and drive it forward by reacting to
each of the above message in the appropriate way. All necessary state
is persisted by this state machine. We describe the  appropriate action for
each of the above transactions.

 - `Deal`: Each validator adds every deal message
   it receives to the state machine and when the threshold is reached,
   the state machine says that it is ready for aggregation. The subsequent
   block proposer asks the DKG state machine to perform the aggregation. If
   successful, it adds an `Aggregate` message to the block it is proposing.
   The includes the aggreations as wells as  the PVSS transcripts and is 
   added tto the block header. Otherwise, a `Complain` transaction is added
   to the proposed block along with the PVSS transcripts that it attempted
   to aggregate.
 - `Aggregate`: This simply needs to be verified and the resulting key added
   to the DKG state machine instance along with the input PVSS transcripts. 
   If validation of the aggregated key fails, the proposed block is rejected.
 - `Complain`: Each validator verifies the validity of each of the included
   PVSS transcripts. They slash those validators who authored invalid 
   transcripts. If none are invalid, the block proposer is in error and the
   block should be rejected. __TBD: We could also slash the block proposer__

While every validator must send and react to `Deal` messages, `Aggregate` / `Complain`
messages must be made by the current block proposer if their current DKG 
state machine is in the  appropriate state and/or the appropriate conditions
are met. Thus block proposers must check the state machines of their active
DKG instances.

## ABCI++

The creation of the public key and the shared secrets requires ABCI++ as we
need control over block preparation as well extra data from the Vote Extension
phase. 

We list the phase of ABCI++ in which each of the above actions takes place.

 - `Deal`: `Deal` happens during the Vote Extension phase.
 - `Aggregate` / `Complain`: This is done during the Prepare Proposal phase 
   (it could be done earlier if validators know they are likely to be the
   next proposer). `Aggregate` messages will be  included in the header of 
   the proposed block while `Complain` is a transaction. The final result 
   should be verified by validators as part of their Process Proposal phase.
