# Distributed key generation

There are (potentially several) stages in the DKG protocol where gossiping
comes into play. However, the entire DKG cannot exist solely in the gossip
layer as consensus on the final shared secrets must be achieved. This will
require posting information on chain.

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
\\( \frac{2}{3}\\) of the total weight) have their signed message on chain for
the session with id \\( \tau \\). At this point, the next block proposer can 
aggregate the messages into a single instance using Ferveo. This is then included
in the next block. Once this DKG is on the blockchain, it can be used for encryption
of transactions until the next DKG instance is run.