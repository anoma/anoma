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
during epoch `n` using the voting powers they will have during epoch `n+1` in 
their computations. Because of the pipelining of validator set changes, the validator sets
and their voting powers for epoch `n+1` will be known at the beginning of
epoch `n`. 

For validators who start for the first time at epoch `n+1`, they
must participate in the broadcasting their PVSS transcripts 
in epoch `n`. The job of using the consensus layer to validate the DKG
protocol will be done by the current validator set with their current 
voting powers.

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

The signed message will be broadcast as a special transaction. and  will be
posted on the blockchain once they are verified. This verification is 
provided by the Ferveo library.

Validators will need to keep track of the current weight of shared PVSS
transcripts on the blockchain so that they know when enough have been 
posted to perform the next step, aggregation. This may require waiting
for several blocks.

In particular, since PVSS transcripts are large with respect to the size of 
a block, a sensible limit of PVSS transcripts that can be included in a
single block must be chosen. It must also be considered that enough room
for user transactions be left so as not to throttle throughput. However,
even a limit of one PVSS transcript per block will be sufficient for the
most probable choices of epoch length and \\( W \\).

A parameter called `retry_after` is passed in to the DKG state machine
that facilitates a schedule for validators to issue (or re-issue) PVSS
transcripts so as not to overcrowd the gossip network.

This parameters specifies a number of blocks after which all DKG participants
will reissue their PVSS transcripts if they are not already on chain. 
The validator set is partitioned into chunks of size
\\(\frac{1}{2}\times\\)`retry_after`. The DKG determines which partition a
given validator is in. If the number of blocks since the DKG instance began
reaches a validators partition, they issue a PVSS transcript. They wait
until the next partition is reached and if their transcript has not made it
onto the blockchain, they re-issue it. The state machine has a method for advising
validators when to issue a PVSS transcript and when to wait.

## Aggregation

During the dealing phase of the DKG protocol, PVSS transcripts observed on
the blockchain are continuously stored by each validator. Once a validator
detects that they have enough PVSS transcripts ( at least \\(\frac{2}{3}W\\)),
they should attempt to aggregate them. 

If aggregation cannot happen and the next epoch begins, the protocol is 
restarted from scratch. Furthermore, since a new key was not created, 
no new transactions can be accepted until a new key is generated. The 
inability to include transactions represents a lost opportunity cost (in
 terms of transaction fees if chosen as block proposer) and should incentivize
correct and timely reporting of PVSS transcripts in the previous epoch.

The validator aggregates the transcripts into a 
single instance using Ferveo during its [Finalize Block](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#delivertx-rename-to-finalizeblock)
phase. If the aggregation succeeds, the result is kept stored by the validator
and  can be used for the encryption of transactions until the next epoch. If
a block proposer has successfully created the public key, and it is not
posted on the blockchain yet, this should be included as a special transaction
during the [Prepare Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#prepare-proposal) phase.

When deciding to aggregate the PVSS instances into a single one, we shall
consider the instances ranked in terms of their validators as was also used
for partitioning the weight shares (described above). Since the PVSS 
transcripts to be used are all on the blockchain, this should produce a
deterministic result.

## Storage considerations

The storage of a node must be adapted to accommodate the DKG protocol. Each
instance of a DKG protocol from Ferveo is a state machine that stores
much of the necessary data from the protocol instance. The following data
needs to be stored by each node
 - The partition of the weight shares among the current validator set
 - A session keypair for the current DKG session. Used for encrypting
   PVSS transcripts and signing DKG protocol messages.
 - The issued PVSS transcripts
 - Info on the validators, including their weight, voting power, address, 
   and public session keys
 - The PVSS issuing schedule for a validator.

However, some necessary data must be persisted in the public storage. This includes
the following:
 - A public key for every validator which is used for signing all relevant 
   transactions for this protocol.
 - The resulting encryption key
 - The public DKG session keys.
 
Furthermore, this data is kept in memory for a validator as it must be accessed often.
This also necessitates special transactions and validity predicates for updating the
protocol signing keys and DKG session keys.

##  The DKG state machine
The DKG state machine can verify and process two types of messages, which are
posted on the blockchain inside of special protocol transactions.
- `Deal`: This message contains the PVSS transcript from a validator
  acting as a dealer
- `Aggregate`: Add the public key from combined PVSS transcripts in the 
   proposed block 

In Ferveo, the DKG protocol is modelled as a state machine. This allows a
validator to initialize an instance and drive it forward by reacting to
each of the above message in the appropriate way.  We describe the 
appropriate action for each of the above transactions.

 - `Deal`: Each validator adds every verified deal message
   it observes to the DKG state machine and when the threshold is reached,
   the state machine says that it is ready for aggregation. The subsequent
   block proposer asks the DKG state machine to perform the aggregation. It
   then adds an `Aggregate` message to the block it is proposing.
 - `Aggregate`: This simply needs to be verified and the resulting key added
   to the DKG state machine instance. Since every validator should aggregate
   the PVSS shares, verification is simply checking that their result equals
   that in the aggregate transaction. If validation of the aggregated key
   fails, the proposed block is rejected.

While every validator must send and react to `Deal` messages, `Aggregate`
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

 - `Deal`: `Deal` happens when validators detect a new epoch has begun. In practice,
    this happens during or just after the [Finalize Block](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#delivertx-rename-to-finalizeblock) phase.
 - `Aggregate`: This is done during the [Prepare Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#prepare-proposal) phase 
   `Aggregate` messages will be included in the proposed block and 
    should be verified by validators as part of their [Process Proposal](https://github.com/tendermint/spec/blob/master/rfc/004-abci%2B%2B.md#process-proposal) phase.
