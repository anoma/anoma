defmodule AnomaTest.Node.Dumper do
  use TestHelper.TestMacro, async: true

  alias Anoma.Node.{Mempool, Dumper, Router}
  alias Anoma.Node.Router.Engine
  alias Anoma.System.Directories
  alias Anoma.Configuration

  setup_all do
    storage = %Anoma.Node.Storage{
      qualified: AnomaTest.Dumper.Qualified,
      order: AnomaTest.Dumper.Order
    }

    node_name = :dumper
    snapshot_path = [:my_special_nock_snaphsot | 0]

    config =
      Configuration.configuration(%{
        "node" => %{"block_storage" => "dumper_blocks"}
      })

    {:ok, nodes} =
      Anoma.Node.start_link(
        name: node_name,
        use_rocks: false,
        settings:
          {:new_storage,
           [
             snapshot_path: snapshot_path,
             storage_data: storage,
             block_storage: :dumper_blocks,
             ping_time: :no_timer,
             configuration: config,
             count: 1
           ]
           |> Anoma.Node.start_min()}
      )

    node = Anoma.Node.state(nodes)

    [node: node]
  end

  test "Dump automatically", %{node: node} do
    assert Dumper.start(node.dumper) == :ok

    config = node.configuration |> Engine.get_state()
    state = node.dumper |> Engine.get_state()
    path = config.configuration["dump"]["dump"] |> Directories.data()
    table = config.configuration["node"]["block_storage"] |> String.to_atom()
    task = state.task.pid
    my_node = node()
    mempool = node.mempool

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.logger_topic.id, :local}
      )

    assert File.exists?(path) == false

    assert :mnesia.subscribe({:table, table, :simple}) == {:ok, my_node}

    assert Mempool.execute(mempool) == {:ok, 0}

    assert_receive(
      {:mnesia_table_event, {:write, {^table, _, _, 0, _, _}, {:tid, _, _}}},
      5000
    )

    assert :mnesia.unsubscribe({:table, table, :simple}) == {:ok, my_node}

    msg =
      "Dumping succesful from worker."

    assert_receive(
      {:"$gen_cast", {_, _, {:logger_add, ^task, ^msg}}},
      5000
    )

    assert Anoma.Node.Logger.get(node.logger, task)
           |> Enum.any?(fn {_info, log} ->
             log == msg
           end) == true

    Dumper.set_count(node.dumper, 1)

    assert File.exists?(path) == true

    assert Anoma.Node.Configuration.delete_dump(node.configuration) ==
             :ok

    assert (node.dumper |> Engine.get_state()).count == 1
  end
end
