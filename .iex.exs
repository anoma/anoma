alias Anoma.{Node, Block, Dump, Mnesia}

alias Node.{
  Router,
  Transport,
  Ordering,
  Executor,
  Mempool,
  Pinger,
  Clock,
  Logger,
  Storage
}

alias Router.Engine

alias Examples.{
  EBlock,
  EClient,
  ECommitmentTree,
  EConfiguration,
  ECrypto,
  EIdentity,
  ENock,
  ENode,
  EParser,
  EProofRecord,
  EResource,
  ESerialisation,
  ETransaction,
}

alias Examples.ENode.{
  EClock,
  EDumper,
  EIntent,
  EMempool,
  EPinger,
  EStorage,
  ETransport.ETCP
}

import_file_if_available("~/.iex.exs")
