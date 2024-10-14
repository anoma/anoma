defmodule Examples.TcpTest do
  use TestHelper.TestMacro
  doctest CommitmentTree

  alias Anoma.Node.Examples.ETransport.ETcp

  test "sha256 examples" do
    ETcp.create_nodes_and_cleanup_nodes()
    ETcp.connect_nodes()
  end
end
