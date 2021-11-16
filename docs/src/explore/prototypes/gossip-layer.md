# Intent Gossip system prototype

## Version 2

tracking issue <https://github.com/anoma/anoma/issues/620>

- Separate matchmakers from intent gossiper nodes
- Various fixes and improvements
- fixes for issues found in the Feigenbaum testnet
- Persistent storage
- Intent gossip and matching of complex txs
- multi-party trades (e.g. 10)
- multi-asset trades (FT, NFT)
- NFT swaps
- Benchmarking base load for the entire network
- Incentives
- Docs

## Version 1

tracking issue <https://github.com/anoma/anoma/issues/35>

### Goals

- learning rust
- usable node + client setup :
  - intent
  - incentive function
  - mempool and white list
- basic matchmaker

### components

The intent gossip is build conjointly to the ledger and share the same binary.

#### Node

The node is built into `anoman`, it runs all the necesarry part, rpc server,
libp2p, intent gossip app.

##### Intent gossip application

The intent gossip application

###### Mempool

###### Filter

##### Network behaviour
The network behaviour is the part that react on network event. It creates a
channel (e.g. `tokio::mpsc::channel`) with the intent gossip to communicate all
intent it receive.

##### Rpc server
If the rpc command line option is set it creates a tonic server that receive
command from a client and send theses through a channel
(e.g. `tokio::mpsc::channel`) to the the intent gossip.

#### Client
Allow to submit a intent :
`anoma gossip --data "data"`
