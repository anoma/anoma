defmodule Examples.SerializeTest do
  use ExUnit.Case, async: true
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.Serializing.Structs.Mempool

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.Serializing.Events.Mempool

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.Serializing.Events.Backends

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.Serializing.Events.Ordering
end
