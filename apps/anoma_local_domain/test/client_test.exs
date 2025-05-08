defmodule Anoma.LocalDomainTest do
  # these tests cannot run async
  # they require the grpc proxy and there can only be one instance of this at a time.
  use ExUnit.Case, async: false

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.ELocalDomain.Intents

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.ELocalDomain.Mempool

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.ELocalDomain.Nock

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.ELocalDomain.Executor

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.ELocalDomain.Nock.Scry

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.ELocalDomain.Nock.Run

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.ELocalDomain.Nock.Prove
end
