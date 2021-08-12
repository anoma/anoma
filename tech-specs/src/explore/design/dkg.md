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
block heights, likely after a fixed number of blocks has passed since the 
last protocol was initiated. This way all the participating validators 
know when to begin the announcement phase. In addition to starting at
the pre-determined blocks heights, if the protocol fails, all validators
know to immediately start a new round of the protocol.

## Announcing and distributing weight shares

In order for the DKG protocol to work, it requires the contribution of 
 \\(\frac{2}{3} \\) of the total staked weight among live validators. In 
order to know when a subset of validators own at least \\(\frac{2}{3}\\)
of the total weight, it is necessary for validators to announce their stake.
Each validator will announce their stake as part of their vote extension.

Furthermore, the total weight will be scaled down to a fixed parameter 
\\( W\\) (and fractional weights rounded appropriately to acheive integral
values). After this procedure, each validator has a corresponding number of
weight shares. 

Lastly, the DKG protocol requires a canonical ordering of validators with
shares must be agreed upon. This will be done via the consensus layer.
The current block proposer will apportion out weight shares as well as the
aforementioned ordering (which induces a natural partition of the weight 
shares) to each validator that made an announcement. This is done as part
of the verify vote extension phase and  is included in the header of the 
finalized  block. Once this block is on the blockchain, the next step of 
the DKG can proceed.

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
single instance using Ferveo as part of the verify vote extension phase. 
This is then included in the header of the finalized block along with the
PVSS transcripts. Once this DKG is on the blockchain, it can be 
used for encryption of transactions until the next DKG instance is run.

When deciding to aggregate the PVSS instances into a single one, we shall 
consider the instances ranked in terms of the weights of their dealers (in 
descending order). The topmost dealers constituting enough weight are then 
chosen and aggregated in their specified order.

## Notes on pipelining

Note that in the decription above, at least two blocks must be submitted to the blockchain in order to
produce the next public encryption key. The first block determines the 
partition of weight shares of the validators and the last aggregates the 
PVSS instances from the dealers leading to the final key.

In practice, it may take even longer as the protocol waits for enough PVSS 
instances to get onto the blockchain. Depending on the chosen refresh rate
of the keys, another instance of the DKG protocol may need to be started 
before the current one finishes. This is possible so long as all data
posted for a DKG instance includes the session id \\( \tau \\).

This means that the storage of a node must be adapted. Each instance of a
DKG protocol from Ferveo is a state machine that keeps all necessary data
from the protocol instance. We thus need to store the following data
 - A list of sesssion ids from the last completed DKG instance up until the
   latest started (we may prune ids for failed instances however) 
 - The DKG state machine associated to each of these session ids.

##  The DKG state machine

We first describe each new message type needed for the protocol.
- `AnnounceStake`: Validators participating in this instance of the DKG
  broadcast their stake
- `PartitionShares`: Once the block containing all of the `AnnounceStake`
  messages are finalized, the set of participants is fixed and the
  partition of the weight shares is added to the block.
- `Deal`: This message contains the PVSS transcript from a validator
  acting as a dealer
- `Aggregate`: Combine the PVSS instances into a single one, thereby
  producing the final key.
- `Complain`: A message stating that a given validator's PVSS transcript or
  a block proposer's aggregation failed verification. 

In Ferveo, the DKG protocol is modelled as a state machine. This allows a
validator to initialize an instance and drive it forward by reacting to
each of the above message in the appropriate way. All necessary state
is persisted by this state machine. We describe the  appropriate action for
each of the above transactions.

 - `AnnounceStake`: These need to be verified. The current block proposer
   adds a `PartitionShares` message to the block header of the 
   block  starting the DKG instance. It is based off all the 
   `AnnounceStake` transactions which will also be included in the same
   block header (for verification).
 - `PartitionShares`: Validators verify the partitions and weights and
   should extract their weight shares and store them in the DKG state machine.
   Then they should send out `Deal` transactions.
 - `Deal`: These need to be verified. The current block proposer checks if 
   enough valid PVSS transcripts have been reached to meet the security 
   threshold. If so, it adds a `Aggregate` transaction to the block header. 
   In either case, it also adds the PVSS transcipts to the block header. 
   If aggregation was successful, these act as verifications. Otherwise, the
   next block proposer will need the information to try and complete the
   protocol in the next block.   For any PVSS transcript that fails verification, a `Complain` message is
   also included in the header.
 - `Aggregate`: This simply needs to be verified and the resulting key added
   to the DKG instance along with the input PVSS transcripts. 

Most of the above transactions occur as a reaction to other messages
and/or data in the proposed block header. The two exceptions are the
`PartitionShares`, and `Aggregate` transactions. These must be 
made by the current block proposer if their current DKG state machine is 
in the  appropriate state and/or the appropriate conditions are met. Thus 
block proposers must check the state machines of their active DKG instances.

## ABCI++

The creation of the public key and the shared secrets requires ABCI++ as we
need control over block preparation as well extra data from the Vote Extension
phase. 

We list the phase of ABCI++ in which each of the above actions takes place.
 - `AnnounceStake`: These are sent out by validators during the Vote
    Extension phase.
 - `PartitionShares`: This also happens in the Verify Vote Extension phase
    and the result will be included in the finalized block's header.
 - `Deal`: Validators should verify the weight shares included in the block 
    header. If validation fails, the DKG instance fails.  Otherwise, `Deal` 
    happens during the Vote Extension phase.
 - `Aggregate`: This is done during the Verify Vote Extension and will be 
   included in the header of the finalized block. The final result should 
   be verified by validators in the next round. If validation fails, the 
   now current block proposer can try to aggregate the the PVSS transcripts
   correctly during the VerifyVoteExtension phase and include the result in
   the header of the finalized (next) block. Furthermore, `Complains` may 
   be issued against the validator that incorrectly aggregated the PVSS
   transcripts

## Complaints

TBD