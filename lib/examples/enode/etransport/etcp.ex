defmodule Examples.ENode.ETransport.ETCP do
  use Memoize

  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  alias Examples.ENode

  require ExUnit.Assertions
  import ExUnit.Assertions

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

  def server_shutdown(cleanup \\ true) do
    anode = ENode.simple_router()

    {anode, socks1} = ENode.attach_socks(anode, cleanup: false)
    server_1 = Transport.lookup_server(anode.transport, socks1)
    pid_1 = Router.Addr.pid(server_1)
    assert Process.alive?(pid_1) == true

    {anode, socks2} = ENode.attach_socks(anode, cleanup: false)
    server_2 = Transport.lookup_server(anode.transport, socks2)
    pid_2 = Router.Addr.pid(server_2)
    assert Process.alive?(pid_2) == true

    Transport.TCPServer.shutdown(server_1)
    Process.sleep(500)
    assert Process.alive?(pid_1) == false
    assert Process.alive?(pid_2) == true

    Transport.TCPServer.shutdown(server_2)
    Process.sleep(500)
    assert Process.alive?(pid_1) == false
    assert Process.alive?(pid_2) == false

    cleanup(socks1, cleanup)
    cleanup(socks2, cleanup)

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
