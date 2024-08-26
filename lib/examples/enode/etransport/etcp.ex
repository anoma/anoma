defmodule Examples.ENode.ETransport.ETCP do
  use Memoize

  alias Anoma.Node.Transport
  alias Examples.ENode

  require ExUnit.Assertions

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
