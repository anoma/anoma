defmodule ProxyTest do
  # these tests cannot run async
  # they require the grpc proxy and there can only be one instance of this at a time.
  use ExUnit.Case, async: false

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.EProxy
end
