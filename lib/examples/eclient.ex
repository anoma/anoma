defmodule Examples.EClient do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Cli
  alias Examples.EParser
  alias Anoma.Node.Mempool
  alias Examples.ENode.EStorage
  alias Examples.ENode

  alias Anoma.Cli.Client
  alias Anoma.Symbol

  alias Anoma.Node
  alias Anoma.Node.{Router, Transport}

  @type common() :: {Node.t(), Transport.transport_addr(), Router.addr()}

  @spec get_from_other() :: common()
  @spec get_from_other(Symbol.s()) :: common()
  @spec get_from_other(Symbol.s(), boolean()) :: common()
  def get_from_other(storage_name \\ "get_from_other", cleanup \\ true) do
    anode = EStorage.august_node(storage_name)
    {anode, socks} = ENode.attach_socks(anode, cleanup: false)
    client_node = ENode.simple_router()

    silent = true

    {:ok, c_addr} =
      Router.start_engine(
        client_node.router,
        Client,
        {client_node.router, client_node.transport, anode, socks,
         {:get_key, EStorage.miki_key()}, silent}
      )

    assert 0 == Client.error_code(c_addr)
    assert EStorage.lucky_value() == Client.return_value(c_addr)
    cleanup(socks, cleanup)
    {anode, socks, c_addr}
  end

  @spec storage_423_from_cli() :: common()
  @spec storage_423_from_cli(Symbol.s()) :: common()
  @spec storage_423_from_cli(Symbol.s(), boolean()) :: common()
  def storage_423_from_cli(
        storage_name \\ "storage_423_from_c",
        cleanup \\ true
      ) do
    anode = EStorage.snapshot_then_put(storage_name)
    client_node = ENode.simple_router()

    {anode, socks} = ENode.attach_socks(anode, cleanup: false)
    client_info = {client_node.router, client_node.transport, anode, socks}

    sub_memexec(anode)

    assert {:ok, c_addr} =
             EParser.zero_submit_423()
             |> Cli.run_commands(client_info)

    assert 0 == Client.error_code(c_addr)

    assert_receive {:"$gen_cast", {_, _, {:submitted, zero}}}

    Mempool.execute(anode.mempool)

    TestHelper.Worker.wait_for_worker(zero.addr)

    assert {:ok, c_addr} =
             EParser.get_423()
             |> Cli.run_commands(client_info)

    assert 0 == Client.return_value(c_addr)

    unsub_memexec(anode)
    cleanup(socks, cleanup)

    {anode, socks, c_addr}
  end

  @spec cleanup(Transport.transport_addr(), boolean()) :: any()
  def cleanup(_, false) do
    nil
  end

  def cleanup({:unix, socks_path}, true) do
    File.rm(socks_path)
  end

  @spec sub_memexec(Node.t()) :: :ok
  def sub_memexec(anode) do
    Router.call(anode.router, {:subscribe_topic, anode.mempool_topic, :local})

    Router.call(
      anode.router,
      {:subscribe_topic, anode.executor_topic, :local}
    )

    :ok
  end

  @spec unsub_memexec(Node.t()) :: :ok
  def unsub_memexec(anode) do
    Router.call(
      anode.router,
      {:unsubscribe_topic, anode.mempool_topic, :local}
    )

    Router.call(
      anode.router,
      {:unsubscribe_topic, anode.executor_topic, :local}
    )

    :ok
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################
  # Use Storage like everyone else
end
