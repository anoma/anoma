# Intent gossip

## High level Overview

The intent gossip use the gossip network to propagates intents. An intent
describe the desire of a user, from asset exchange to a green tax percent for
selling shoes. These intent are picked up by a matchmaker that compose them into
transactions to send to the ledger network. A matchmaker is optionally included
in the intent gossip node.

The intent gossip network must propagate a large number of intent quickly.  In
order to scale and reduce the required bandwidth, the network is split into
multiple sub network. Each subnetwork is defined by a topic and each node can
connect to as many topic as desired.

### example
![intent gossip and ledger network
interaction](./intent_gossip/example.svg "intent gossip network")
[exilidraw link](https://excalidraw.com/#room=257e44f4b4b5867bf541,XDEKyGVIpqCrfq55bRqKug)

# Network topology

The intent gossip makes use of the gossipsub topic functionality. A topic
specifies a sub-network that each node can connect/subscribe to. A sub-network
only propagates messages of that topic. Any node can subscribe to a new or
existing topic, when doing so it informs all connected nodes.  All node defines
a topic filter. That filter is used when a node receives a subscription to a
topic; the node subscribes to the topic if it passes the filter.  see
[gossipsub](https://github.com/libp2p/specs/tree/master/pubsub/gossipsub) for
more information on the network topology.

# life cycle of intent and process description

![intent life cycle](./intent_gossip/intent_life_cycle.svg "intent life
cycle") [exilidraw
link](https://excalidraw.com/#room=7ac107b3757c64049003,cdMInfvdLtjaGWSZWEKrhw)
