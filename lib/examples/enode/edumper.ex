defmodule Examples.ENode.EDumper do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Configuration
  alias Anoma.Node.Dumper
  alias Examples.EConfiguration
  alias Examples.ENode
  alias Anoma.Symbol
  alias Anoma.Node
  alias Anoma.Node.{Router, Storage, Mempool}

  def dumped_node(storage_name \\ "dumped_node") do
    {anode, config} = anode(storage_name)
    IO.puts("STORAGE: #{inspect(raw_storage(storage_name))}")
    path = config["dump"]["dump"]
    log_top = anode.logger_topic.id

    Dumper.set_count(anode.dumper, 1)
    Dumper.start(anode.dumper)
    assert File.exists?(path) == false

    assert :ok ==
             Router.call(anode.router, {:subscribe_topic, log_top, :local})

    # :observer.start()
    assert {:ok, 0} == Mempool.execute(anode.mempool)

    msg = "Dumping call succesful from worker."

    assert_receive({:"$gen_cast", {_, _, {:logger_add, _, ^msg}}}, 5000)

    msg2 =
      "Dump succesfull. Snapshot path: #{inspect(path)}. Node name: :anoma"

    send(self(), {:foo_budy, anode.storage.server, Process.whereis(anode.storage.server)})
    IO.puts("""
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA #{anode.router.server} AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


    abcdef #{anode.storage.server} AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    """)
    IO.puts("")
    assert_receive(
      {:"$gen_cast", {_, _, {:logger_add, _task, ^msg2}}},
      500000
    )

    assert File.exists?(path) == true

    assert Anoma.Node.Configuration.delete_dump(anode.configuration) ==
             :ok

    assert :ok ==
             Router.call(anode.router, {:unsubscribe_topic, log_top, :local})

    # :observer.stop()
    {anode, config}
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################

  @spec anode() :: {Node.t(), Configuration.configuration_map()}
  @spec anode(Symbol.s()) :: {Node.t(), Configuration.configuration_map()}
  @spec anode(Symbol.s(), Storage.t()) ::
          {Node.t(), Configuration.configuration_map()}
  def anode(arg \\ "none") do
    anode(arg, raw_storage(arg))
  end

  def anode(arg, storage) do
    name = Symbol.append(__MODULE__, "." <> to_string(arg))
    config = EConfiguration.dumper_config(name |> to_string())

    {storage
     |> ENode.fresh_full_node(
       Symbol.append(__MODULE__, "." <> to_string(arg)),
       config
     ), config}
  end

  @spec raw_storage() :: Storage.t()
  @spec raw_storage(Symbol.s()) :: Storage.t()
  def raw_storage(arg \\ "none") do
    %Storage{
      qualified: Symbol.append(__MODULE__.Qualified, "." <> to_string(arg)),
      order: Symbol.append(__MODULE__.Order, "." <> to_string(arg))
    }
  end
end
