defmodule Examples.TcpTest do
  use ExUnit.Case, async: true
  use TestHelper.TestMacro

  alias Anoma.CommitmentTree

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.ETransport.ETcp

  doctest CommitmentTree
end
