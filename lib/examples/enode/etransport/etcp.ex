defmodule Examples.ENode.ETransport.ETCP do
  use Memoize

  alias Anoma.Node.Transport
  alias Examples.ENode
  alias Anoma.Node.Transport2.Router

  require ExUnit.Assertions

  ############################################################
  #    Nodes talking via TCP                                 #
  ############################################################

  def test_tcp_server_1() do
    # ask the router to start up a new tcp server on the port thats specified
    # in its config
    # note: if you want to change/fix this port, you have to change this in the config
    #       in anoma.ex
    Router.start_tcp_server()
  end

  def test_tcp_server_2() do
    # attempt to connect to the node that was started in the previous test
    Router.start_tcp_client({0, 0, 0, 0}, 1234)
  end

  # ── Section ──

  def shutdown_from_outside(cleanup \\ true) do
    anode = ENode.simple_router()
    {anode, socks} = ENode.attach_socks(anode, cleanup: false)

    process_identity = Transport.lookup_server(anode.transport, socks)

    listener = Transport.TCPServer.listener(process_identity)

    :gen_tcp.close(listener)

    cleanup(socks, cleanup)

    # We should assert that the TCP Server and the pool is dead when
    # that code is in, for now don't assert anything

    anode
  end

  @spec cleanup(Transport.transport_addr(), boolean()) :: any()
  def cleanup(_, false) do
    nil
  end

  def cleanup({:unix, socks_path}, true) do
    File.rm(socks_path)
  end
end
