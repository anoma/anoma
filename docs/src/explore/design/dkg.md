# Distributed key generation

Here we give an overview of how the DKG protocol works with the ledger and 
the roles of each node in the network per this protocol. This portion of the 
book focuses solely on the production of a new public encryption key via the
DKG scheme. It does not cover encryption, decryption, or validation of transactions.

## Starting the Protocol

The protocol to generate a new key is scheduled to begin at specific 
block heights, in practice this will be at the beginning of every epoch.


It is important to note that the key being generated will be used to encrypt
transactions in the epoch following the current one. This means that the 
validator set of epoch `n+1` must participate in the DKG protocol
during epoch `n` using the voting powers they will have during epoch `n+1`.
Because of the pipelining of validator set changes, the validator sets
and their voting powers for epoch `n+1` will be known at the beginning of
epoch `n`.

##  Computing weight shares

The DKG protocol requires the contribution of \\(\frac{2}{3} \\) of the 
total voting power among validators, mirroring the proof of stake
requirements. The proof of stake system maintains a list of validators and 
their voting powers in storage. This can be queried by each validator to 
get the set of validators, their associated address, and voting power (or 
weight).

The corresponding voting power will be scaled down to a fixed parameter 
\\( W\\) (and fractional results rounded appropriately to achieve integral
values). After this procedure, the scaled voting power represents the number of
__*weight shares*__ possessed by a validator. Ferveo uses an index for
each weight share, so a __partition__ of these indices must be computed
to know which weight share belongs to which validator.

The partition of the weight shares requires a canonical ordering of 
validators. This ordering is so that computations done by individual 
validators offline (and thus do not use the consensus layer) produce
deterministic results. The ordering will be chosen by ordering validators 
in terms of decreasing voting power. If there is a tie, it is broken by 
considering the lexicographic ordering on their associated addresses. 

This allows each validator to independently compute a canonical partition
of the weight shares of all validators which will be needed for the following 
steps of the DKG protocol. This partition is computed by Ferveo.

## Dealers
In a single DKG instance, a subset of validators holding at least
\\( \frac{2}{3} \\) of the weight shares must each act as dealers. It
is the responsibility of a dealer to post a signed message to the blockchain
which includes the session id (\\( \tau \\)) as well the data of the
PVSS (we do not concern ourselves with the details here). The creation of this
message is handled by the Ferveo library.

The signed message will be returned as part of the [Vote Extension](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#vote-extensions). However,
for performance reasons, these transcripts will not be verified at this stage.
Rather, the verification will be performed when an aggregation is attempted
(discussed next). This verification is provided by the Ferveo library.

However, not all PVSS transcripts necessary to complete the protocol will be
posted in a single block due to the size of the transcripts. A parameter,
`PVSS_PER_BLOCK`, will specify at most how many transcripts will be added to
each block during the protocol. 

Using the ordering of the weight shares computed by Ferveo, we schedule the block at which a 
PVSS transcript should be added. For the PVSS transcript corresponding to 
weight share with index \\(i\\), it is scheduled in block 

\\[ B(i) := s + \bigg\lfloor \frac{i}{\text{PVSS_PER_BLOCK}}\bigg\rfloor \\]

where \\(s\\) is the block for which which the protocol started. The dealing
phase of the protocol is thus expected to end at block \\(B(W)\\).

## Aggregation

During the dealing phase of the DKG protocol, PVSS transcripts recieved
are continuously stored by each validator. Once a block proposer detects
that they have enough PVSS transcripts ( at least \\(\frac{2}{3}W\\)), they
should attempt to aggregate them.

The block proposer aggregates the messages into a 
single instance using Ferveo during its [Prepare Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#prepare-proposal)
phase. If the aggregation succeeds, the result of this is included in the 
header of the proposed block along with the PVSS transcripts. Once this is
on the blockchain, it can be used for encryption of transactions until the
next epoch.

When deciding to aggregate the PVSS instances into a single one, we shall
consider the instances ranked in terms of their validators as was also used
for partitioning the weight shares (described above).

If the aggregation fails, the proposer should instead include a 
special transaction into its proposed block (a transaction that only proposers
can make). This transaction states that the aggregation could not succeed 
because some PVSS transcripts are invalid. In the [Process Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#process-proposal)
phase, each node should check each PVSS transcript and if any are found to be 
invalid, they update their local state, slashing the author of the offending 
transcript appropriately. This amount should be significant to mitigate denial of 
service attack vectors.

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
 - The current round of the dealing phase (for scheduling the submission
   of their PVSS transcripts)
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
   The includes the aggregations as wells as  the PVSS transcripts and is 
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
[ABCI++ RFC](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md)

[ABCI++ specification](https://github.com/sikkatech/spec/blob/abci_spec_md/spec/abci%2B%2B/v4.md)

The creation of the public key and the shared secrets requires ABCI++ as we
need control over block preparation as well extra data from the Vote Extension
phase. 

We list the phase of ABCI++ in which each of the above actions takes place.

 - `Deal`: `Deal` happens during the [Vote Extension](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#vote-extensions) phase.
 - `Aggregate` / `Complain`: This is done during the [Prepare Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#prepare-proposal) phase 
   (it could be done earlier if validators know they are likely to be the
   next proposer). `Aggregate` messages will be  included in the header of 
   the proposed block while `Complain` is a transaction. The final result 
   should be verified by validators as part of their [Process Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#process-proposal) phase.
