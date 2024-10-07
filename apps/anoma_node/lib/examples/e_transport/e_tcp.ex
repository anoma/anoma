defmodule Anoma.Node.Examples.ETransport.ETcp do
  use Memoize

  import ExUnit.Assertions

  alias Anoma.Node.Transport.Router

  require ExUnit.Assertions
  require ExUnit.Assertions

  ############################################################
  #    Nodes talking via TCP                                 #
  ############################################################

  def test_tcp_server_1() do
    # ask the router to start up a new tcp server on the port thats specified
    # in its config
    # note: if you want to change/fix this port, you have to change this in the config
    #       in anoma.ex
    :ok = Router.start_tcp_server()

    IO.puts("""
    startup a second node to connect to this node:

    `iex --name node_b@127.0.0.1 --cookie mycookie -S mix`

    and run the following command:

    `Examples.ENode.ETransport.ETCP.test_tcp_server_2`


    Press any key to continue
    """)

    IO.read(:stdio, :line)

    # there should be a proxy to the router in the registry
    # and a tcp connection
    assert Enum.count(Router.dump_register()) == 2
  end

  def test_tcp_server_2() do
    # attempt to connect to the node that was started in the previous test
    Router.start_tcp_client({0, 0, 0, 0}, 1234)
  end
end
