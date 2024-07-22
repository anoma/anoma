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

  def dumped_node(name \\ "dumped_node") do
    {anode, config} = anode(name)
    path = config["dump"]["dump"]
    log_top = anode.logger_topic
    node_name = node_name(name) |> String.to_atom()

    Dumper.set_count(anode.dumper, 1)
    Dumper.start(anode.dumper)
    assert File.exists?(path) == false

    # Enforce the wait for task-spawn
    # should be replaced when topics become a thing
    Anoma.Node.Router.Engine.get_state(anode.dumper)

    assert :ok ==
             Router.call(anode.router, {:subscribe_topic, log_top, :local})

    Mempool.execute(anode.mempool)

    msg = "Dumping call succesful from worker."

    assert_receive({:"$gen_cast", {_, _, {:logger_add, _, ^msg}}}, 5000)

    msg2 =
      "Dump succesfull. Snapshot path: #{inspect(path)}. Node name: #{inspect(node_name)}"

    assert_receive(
      {:"$gen_cast", {_, _, {:logger_add, _task, ^msg2}}},
      5000
    )

    assert File.exists?(path) == true

    assert Anoma.Node.Configuration.delete_dump(anode.configuration) ==
             :ok

    assert :ok ==
             Router.call(anode.router, {:unsubscribe_topic, log_top, :local})

    {anode, config}
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################

  @spec anode(Symbol.s()) :: {Node.t(), Configuration.configuration_map()}

  def anode(arg \\ "none") do
    name = arg |> node_name()
    config = name |> EConfiguration.dumper_config()

    storage = config["node"] |> raw_storage()

    {storage
     |> ENode.fresh_full_node(
       name |> String.to_atom(),
       config
     ), config}
  end

  @spec raw_storage(%{}) :: Storage.t()
  def raw_storage(map) do
    %Storage{
      qualified: map["qualified"] |> String.to_atom(),
      order: map["order"] |> String.to_atom()
    }
  end

  @spec node_name(Symbol.s()) :: Symbol.s()
  def node_name(arg \\ "none") do
    Symbol.append(__MODULE__, "." <> to_string(arg)) |> to_string()
  end
end
