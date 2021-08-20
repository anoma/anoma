# Distributed key generation

Here we give an overview of how the DKG protocol works with the ledger and 
the roles of each node in the network per this protocol. This portion of the 
book focuses solely on the production of a new public encryption key via the
DKG scheme. It does not cover encryption, decryption, or validation of transactions.

Some parts of the DKG protocol may be done through the gossip layer or through
off-chain communication. However, the entire DKG cannot exist solely in the 
gossip layer as consensus on the final shared secrets must be achieved. This 
will require posting information on chain.

## Starting the Protocol

The protocol to generate a new key will be scheduled to begin at specific 
block heights, in pratices this will be at the beginning of every eposh.
This way all the validators know when to begin the announcement phase. In 
addition to starting at the pre-determined blocks heights, if the protocol 
fails, all validators know to immediately start a new round of the protocol.

It is important to note that the key that is being generated is to be used
encrypting transactions in the epoch following the current one. However,
this relies on the assumption that after the validators sets change between
epochs that there is still validators whose combined weight exceeds 
\\( \frac{2}{3} \\) of \\W\\. Otherwise, it will be impossible to decrypt
transactions. If this does not happen a new key will need to be generated.

__TBD__: Do we accept unencrypted transactions while the new key is generated
or do we stop accepting transactions until the new key is available? Or 
something else (that is hopefully smarter)?

##  Distributing weight shares

In order for the DKG protocol to work, it requires the contribution of 
 \\(\frac{2}{3} \\) of the total staked weight among validators. At the
beginning of the DKG protocol, Tendermint can be queried each validator
to get the set of validators, their associated address, and voting power
(or weight).

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
protocol.

## Dealers
In a single DKG instance, a subset of validators holding at least
\\( \frac{2}{3} \\) of the weight shares must each act as dealers. It
is the responsibility of a dealer to post a signed message to the blockchain
which includes the session id (\\( \tau \\)) as well the data of the
PVSS (we do not concern ourselves with the details here). The creation of this
message is handled by the Ferveo library.

The signed message will be returned as part of the vote extension and there
is a special verification that must be done, otherwise the PVSS transcript 
should not be included in the aggregation (discussed next). This verification
is provided by the Ferveo library. The PVSS transcripts will be included in
the header of the finalized block along with the aggregation after the verify
vote extension phase completes. This is necessary so that validators can
verify the aggregation.

## Aggregation

At the end of the vote extension phase, enough validators 
(meaning their combined weight  shares exceeds \\( \frac{2}{3}\\) of the 
total) *should* have shared their PVSS to the current proposer and hopefully
enough of these transcripts are valid. This is because the vote extension 
phase requires at least \\( \frac{2}{3}\\) of validators to send a vote
extension before it can move to the finalization phase.

At this point, the next block proposer can aggregate the messages into a 
single instance using Ferveo during its Finalize Block phase (assuming that
the validator knows it will be the next block proposer) or during its
Prepare Proposal phase at the beginning of the next round. The result of 
this is then included in the header of the next block along with the PVSS 
transcripts. Once this DKG is on the blockchain, it can be used for 
encryption of transactions until the next DKG instance is run.

When deciding to aggregate the PVSS instances into a single one, we shall 
consider the instances ranked in terms of their validators as was also used
for partitioning the weight shares (described above).

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
- `Complain`: A message stating that a given validator's PVSS transcript or
  a block proposer's aggregation failed verification. 

In Ferveo, the DKG protocol is modelled as a state machine. This allows a
validator to initialize an instance and drive it forward by reacting to
each of the above message in the appropriate way. All necessary state
is persisted by this state machine. We describe the  appropriate action for
each of the above transactions.

 - `Deal`: These need to be verified. Each validator adds every deal message
   it receives to the state machine and when the threshold is reached,
   the state machine automatically performs the aggregation. The subsequent
   block proposer checks if the aggregation was performed. If so, it adds 
   the result to the block it is proposing. In either case, it also adds the
   PVSS transcripts to the block header. If aggregation was successful, these
   act as verifications. Otherwise, the next block proposer will need the 
   information to try and complete the  protocol in the next block. For any
   PVSS transcript that fails verification, a `Complain` message is also 
   included in the header.
 - `Aggregate`: This simply needs to be verified and the resulting key added
   to the DKG instance along with the input PVSS transcripts. If validation
   of the aggregated key fails, a `Complain` should be issued.

While every validator must send and react to `Deal` messages, `Aggregate` 
must be made by the current block proposer if their current DKG state machine 
is in the  appropriate state and/or the appropriate conditions are met. Thus 
block proposers must check the state machines of their active DKG instances.

## ABCI++

The creation of the public key and the shared secrets requires ABCI++ as we
need control over block preparation as well extra data from the Vote Extension
phase. 

We list the phase of ABCI++ in which each of the above actions takes place.

 - `Deal`: `Deal` happens during the Vote Extension phase.
 - `Aggregate`: This is done during the Prepare Proposal phase (it could be
   done earlier if validators know they are likely to be the next proposer)
   and will be  included in the header of the proposed block. The final result 
   should be verified by validators. If validation fails, the next block 
   proposer can try to aggregate the the PVSS transcripts correctly and 
   include the result in the header of the subsequent block. Furthermore, 
   `Complains` may be issued against the validator that incorrectly
   aggregated the PVSS transcripts. Alternatively, the block could be
   rejected by other validators and the next round begins immediately.
   The faulty proposer thus loses out on the propose block rewards.

## Complaints

Complaints are encrypted transactions that validators broadcast when they
observe misbehavior from other validators during the protocol. The current
causes for complaint are as follows:
 * `PVSS_Complaint`: During VerifyVoteExtension, the PVSS transcript from a validator does not
   pass validation
 * `Aggregate_Complaint`: The aggregated key in a proposed block is not valid
 
These transactions must include a proofs of malfeasance. The necessary 
proofs for the above complaints are as follows:
 * `PVSS_Complaint`: The transcript that failed verification
 * `Aggregate_Complaint`: The alleged incorrect key and the PVSS transcripts
    that should have been used to derive it. 

The above proofs can be used to verify that the protocol was not adhered to.
Checking these proofs will be part of executing the Complaint transaction.
If the transaction succeeds and is added to the blockchain, a slashing
penalty will be determined based on the type of infraction, likely by a
native VP.