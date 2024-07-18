defmodule Examples.EClient do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Examples.ENode.EStorage
  alias Examples.ENode

  alias Anoma.Cli.Client
  alias Anoma.Symbol

  alias Anoma.Node
  alias Anoma.Node.{Router, Transport}

  @type common() :: {Node.t(), Transport.transport_addr()}

  @spec get_from_other() :: common()
  @spec get_from_other(Symbol.s()) :: common()
  @spec get_from_other(Symbol.s(), boolean()) :: common()
  def get_from_other(storage_name \\ "get_from_other", cleanup \\ true) do
    anode = EStorage.august_node(storage_name)
    {anode, socks} = ENode.attach_socks(anode, cleanup: false)
    client_node = ENode.simple_router()

    {:ok, c_addr} =
      Router.start_engine(
        client_node.router,
        Client,
        {client_node.router, client_node.transport, anode, socks,
         {:get_key, EStorage.miki_key()}}
      )

    assert 0 == Client.error_code(c_addr)
    assert EStorage.lucky_value() == Client.return_value(c_addr)
    cleanup(socks, cleanup)
    {anode, socks}
  end

  @spec cleanup(Transport.transport_addr(), boolean()) :: any()
  def cleanup(_, false) do
    nil
  end

  def cleanup({:unix, socks_path}, true) do
    File.rm(socks_path)
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################
  # Use Storage like everyone else
end
