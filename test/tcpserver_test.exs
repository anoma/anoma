defmodule AnomaTest.Node.Transport.TCPServer do
  use TestHelper.TestMacro, async: true

  doctest(Anoma.Node.Transport.TCPServer)

  test "server creation and shutdown" do
    Examples.ENode.ETransport.ETCP.server_shutdown()
  end
end
