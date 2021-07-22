# Distributed key generation

Here we given an overview of how the DKG protocol works with the ledger and 
the roles of each node in the network per this protocol. This portion of the 
book focuses solely on the production of a new public encryption key via the
DKG sheme. It does not cover encryption, decrpytion, or validation of transactions.

Some parts of the DKG protocol may be done through the gossip layer or through
off-chain communication. However, the entire DKG cannot exist solely in the 
gossip layer as consensus on the final shared secrets must be achieved. This 
will require posting information on chain.

## Starting the Protocol

There are two natural possibilities for initializing the protocol to generate
a new key. The first is that the protocol is started at fixed intervals (number
of blocks perhaps.) Since the protocols will be pipelined (see below), the 
only issue that can arise is that a newer instance may succeed before an older 
one does. One policy is to consider the older instance failed and to 
user the newer instance.

The second natural option is that a current block proposer proposes a new instance. This
would be posted on the blockchain and notify all the participating validators
to begin the announcement phase. However, when should the proposer make this
decision? In all likelihood it will be a combination of blocks transpired since
the last instance completed as well as special cases (such as the an instance
failing to complete).

## Announcing and distributing weight shares

In order for the DKG protocol to work, it requires the contribution of 
\\(\frac{2}{3} \\) of the total staked weight among live validators. 

In order to know when a subset of validators own at least\\(\frac{2}{3}\\)
of the total weight, it is necessary for validators to announce their stake.

Furthermore, the total weight will be scaled down to a fixed parameter 
\\( W\\) (and fractional weights rounded appropriately to acheive integral
values). After this procedure, each validator has a corresponding number of
weight shares. 

In order for the DKG protocol to work, a canonical ordering of validators with
shares must be agreed upon. This will be done via the consensus layer.

The current block proposer will apportion out weight shares as well as the
aforementioned ordering (which induces a natural partition of the weight 
shares) to eachvalidator that made an announcement. This is included in the
block. Once this block is on the blockchain, the next step of the DKG can proceed.

## Dealers
In a single DKG instance, a subset of validators whose staked weight exceeds
\\( \frac{2}{3} \\) of the total staked weight must each act as dealers. It
is the responsibility of a dealer to post a signed message to the blockchain
which includes the session id (\\( \tau \\)) as well the data of the
PVSS (we do not concern ourselves with the details here). The creation of this
message is handled by the Ferveo library.

This means that the signed message is a transaction that must be gossiped and
eventually included on the blockchain. For this particular transaction, there
is a special verification that must be done, otherwise the transaction should
not be included in any proposed block. This verification is provided by the
Ferveo library.

## Aggregation

At some point, enough validators (meaning their combined weight exceeds
\\( \frac{2}{3}\\) of the total weight) have their PVSS on chain 
for the session with id \\( \tau \\). At this point, the next block proposer can
aggregate the messages into a single instance using Ferveo. This is then 
included in the next block. Once this DKG is on the blockchain, it can be 
used for encryption of transactions until the next DKG instance is run.

When deciding to aggregate the PVSS instances into a single one, we shall 
consider the instances ranked in terms of the weights of their dealers (in 
descending order). The topmost dealers constituting enough weight are then 
chosen and aggregated in their specified order.

We note that enough weight may be gossiped to the current proposer and thus
they can put the PVSS transactions and aggregation all in the same proposed 
block.

## Notes on pipelining

Note that in the decription above, depending on how the protocol is started,
at least two or three blocks must be submitted to the blockchain in order to
produce the next public encryption key. The (optional) first block kicks off
the protocol instance. The next determines the partition of weight shares of
the validators and the last aggregates the PVSS instances from the dealers 
leading to the final key.

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

We first describe each new transaction type needed for the protocol.
- `StartDKG`: A message from the current block proposer to start the protocol
- `AnnounceStake`: Validators participating in this instance of the DKG
  broadcast their stake
- `PartitionShares`: Once the block containing all of the `AnnounceStake`
  transactions is finalized, the set of participants is fixed and the
  partition of the weight shares is added to the block.
- `Deal`: This transaction contains the PVSS transcript from a validator
  acting as a dealer
- `Aggregate`: Combine the PVSS instances into a single one, thereby
  producing the final key.

In Ferveo, the DKG protocol is modelled as a state machine. This allows a
validator to initialize an instance and drive it forward by reacting to
each of the above transactions in the appropriate way. All necessary state
is persisted by this state machine. We describe the  appropriate action for
each of the above transactions.

 - `StartDKG`: Initialize a fresh instace of the DKG protocol from Ferveo.
   Then send out an `AnnounceStake` transaction.
 - `AnnounceStake`: These simply need to be verified.
 - `PartitionShares`: This is added to the block after the block starting
   DKG instance by the block proposer. It is based off all the `AnnounceStake`
   transactions that it is including in the block. When other validators see
   this proposed block, they should extract their weight shares and store them
   in the DKG state machine. Then they should send out `Deal` transactions.
 - `Deal`: These need to be verified. The current block proposer checks if 
   enough PVSS instances have been reached to meet the security threshold.
   If so, it adds a `Aggregate` transaction to the block. Otherwise, the
   next block proposer will take on the job. 
 - `Aggregate`: This simply needs to be verified and the resulting key added
   to the DKG instance. 

Most of the above transactions occur as a reaction to other transactions. The
two exceptions are the `PartitionShares` and `Aggregate` transactions. This
must be made by the current block if their current DKG state machine is in
the appropriate state (and for the `Aggregate` transaction if enough PVSS
instances have been committed). Thus block proposers must check the state
machines of their active DKG instances.

## ABCI vs ABCI++

The creation of the public key and the shared secrets requires ABCI++ as we
need control over block preparation and finalization. 

### ABCI++