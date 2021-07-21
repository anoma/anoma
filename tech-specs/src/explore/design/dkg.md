# Distributed key generation

There are (potentially several) stages in the DKG protocol where gossiping
comes into play. However, the entire DKG cannot exist solely in the gossip
layer as consensus on the final shared secrets must be achieved. This will
require posting information on chain.

## Announcing and distributing weight shares

In order for the DKG protocol to work, it requires the contribution of 
\\(\frac{2}{3} \\) of the total staked weight among live validators. In 
practice, this weight will be scaled and rounded, but we do not cover those
details here. 

In order to know when a subset of validators own at least\\(\frac{2}{3}\\)
of the total weight, it is necessary for validators to announce their stake.
The current block proposer will then apportion out weight shares to each
validator that made an announcement and include this on the block. Once this
block is one chain, the next step of the DKG can proceed.

## Dealers
In a single DKG instance, a subset of validators whose staked weight exceeds
\\( \frac{2}{3} \\) of the total staked weight must each act as dealers. It
is the responsibility of a dealer to post a signed message to the blockchain
which includes the following the session id (\\( \tau \\)) as well data for
VSS (we do not concern ourselves with the details here). The creation of this
message is handled by the Ferveo library.

This means that the signed message is a transaction that must be gossiped and
eventually included on the blockchain. For this particular transaction, there
is a special verification that must be done, otherwise the transaction should
not be included in any proposed block. This verification is provided by the
Ferveo library.

## Aggregation

At some point, enough validators (meaning their combined weight exceeds
\\( \frac{2}{3}\\) of the total weight) have their signed message on chain 
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

Note that in the decription above, at least two blocks must be submitted to 
the blockchain in order to produce the next public encryption key. The first
block determines the weight shares of the validators and the second aggregates
the PVSS instances from the dealers leading to the final key.