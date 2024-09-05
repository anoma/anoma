alias Anoma.{Node, Block, Dump, Mnesia}

alias Node.{
  Clock,
  Dumper,
  EventLogger,
  Executor,
  LiveConfiguration,
  Mempool,
  Ordering,
  Pinger,
  Router,
  Storage,
  Transport
}

alias Router.{Addr, Engine}

alias Anoma.Crypto
alias Anoma.Crypto.{
  Encrypt,
  Id,
  Randomness,
  Sign,
  Symmetric
}

alias Anoma.Identity
alias Anoma.Identity.{
  Backend,
  Capabilities,
  Encapsulated,
  Evidence,
  Manager,
  Name,
  Parameters,
  SignsFor,
  Verification
}

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
