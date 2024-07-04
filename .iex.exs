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
  ECommitmentTree,
  EConfiguration,
  EIdentity,
  ENock,
  ENode,
  EProofRecord,
  EResource,
  ESerialisation,
  ETransaction
}

alias Examples.ENode.{
  EClock,
  EDumper,
  EIntent,
  EPinger,
  EStorage
}

import_file_if_available("~/.iex.exs")
