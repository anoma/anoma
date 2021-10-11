# Intent gossip network

The intent gossip network enables counterparty discovery for bartering. The
users can express any sort of intents that might be matched and transformed into
a transaction that fulfills the intents on the Anoma ledger.

An [intent](./intent.md) describes the desire of a user, from asset exchange to a
green tax percent for selling shoes. These intents are picked up by a matchmaker
that composes them into transactions to send to the ledger network. A matchmaker
is optionally included in the intent gossip node.

Each node connects to a specified intent gossip network, either a public or a
private one. Anyone can create their own network where they decide all aspects
of it: which type of intents is propagated, which nodes can participate, the
matchmaker logic, etc. It is possible, for example, to run the intent gossip system
over bluetooth to have it off-line.

An intent gossip node is a peer in the intent gossip network that has the role
of propagating intents to all other connected nodes.

The network uses the
[gossipsub](https://github.com/libp2p/specs/tree/512accdd81e35480911499cea14e7d7ea019f71b/pubsub/gossipsub)
network behaviour. This system aggregates nodes around topics of interest. Each
node subscribes to a set of topics and connects to other nodes that are also
subscribed to the same topics. A topic defines a sub-network for a defined
interest, e.g. “asset_exchange”. see
[gossipsub](https://github.com/libp2p/specs/tree/512accdd81e35480911499cea14e7d7ea019f71b/pubsub/gossipsub)
for more information on the network topology.

Each node has an incentive to propagate intents and will obtain a small portion
of the fees if the intent is settled. (TODO: update when logic is found) See
[incentive](./incentive.md) for more information.

### Flow diagram: asset exchange

This example shows three intents matched together by the intent gossip network.
These three intents express user desires to exchange assets.

![intent gossip and ledger network
interaction](./example.svg "intent gossip network")
[Diagram on Excalidraw](https://excalidraw.com/#room=257e44f4b4b5867bf541,XDEKyGVIpqCrfq55bRqKug)

# Flow diagram: life cycle of intent and global process

This diagram shows the process flow for intents, from users expressing their
desire to the ledger executing the validity predicate to check the crafted
transaction.

![intent life cycle](./intent_life_cycle.svg "intent life
cycle") 
[Diagram on Excalidraw](https://excalidraw.com/#room=7ac107b3757c64049003,cdMInfvdLtjaGWSZWEKrhw)
