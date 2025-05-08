defmodule ProxyTest do
  # these tests cannot run async
  # they require the grpc proxy and there can only be one instance of this at a time.
  use ExUnit.Case, async: false

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.EProxy

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.EProxy.Intents

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.EProxy.Mempool

  use TestHelper.GenerateExampleTests,
    for: Anoma.LocalDomain.Examples.EProxy.Executor
end
