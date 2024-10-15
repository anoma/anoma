defmodule Examples.TcpTest do
  use TestHelper.TestMacro
  doctest CommitmentTree

  alias Anoma.Node.Examples.ETransport.ETcp

  test "tcp client and server examples" do
    ETcp.connect_nodes()
  end
end
