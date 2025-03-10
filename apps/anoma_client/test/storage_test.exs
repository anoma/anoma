defmodule StorageTest do
  # these tests cannot run async
  # they require the grpc proxy and there can only be one instance of this at a time.
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Anoma.Client.Examples.EStorage
end
