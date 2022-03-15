# Overview

At a high level, Anoma is composed of two main components: the distributed ledger and the intent gossip / matchmaking system. While they are designed to complement each other, they can be operated separately.

## The ledger

The [ledger](ledger.md) is a distributed state machine, relying on functionality provided by [Tendermint](https://docs.tendermint.com/master/spec/) such as its BFT consensus algorithm with instant finality, P2P networking capabilities, transaction mempool and more. The ledger state machine is built on top the [ABCI](https://docs.tendermint.com/master/spec/abci/).

For block validator voting power assignment, the ledger employs a proof-of-stake system.

The ledger's key-value storage is organized into blocks and user specific state is organized into accounts. The state machine executes transactions, which can apply arbitrary changes to the state that are validated by validity predicates associated with the accounts involved in the transaction.

To prevent transaction front-running, the ledger employs a DKG scheme as implemented in [Ferveo](https://github.com/anoma/ferveo). Using this scheme, transactions are encrypted before being submitted to the ledger. The encrypted transactions are committed by a block proposer to a specific order in which they must be executed once decrypted.

- TODO add fractal scaling & protocol upgrade system overview

## The intent gossip with matchmaking system

- TODO add an overview
