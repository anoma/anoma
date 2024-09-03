defmodule AnomaTest.Node.Dump do
  use TestHelper.TestMacro, async: true

  alias Anoma.Node.{Mempool, Router}
  alias Anoma.Mnesia
  alias Anoma.System.Directories
  import TestHelper.{Nock, Mempool}

  setup_all do
    storage = %Anoma.Node.Storage{
      qualified: AnomaTest.Dump.Qualified,
      order: AnomaTest.Dump.Order
    }

    name = :dump
    snapshot_path = [:my_special_nock_snaphsot | 0]

    {:ok, nodes} =
      Anoma.Node.start_link_or_find_instance(
        name: name,
        use_rocks: false,
        testing: true,
        settings:
          {:new_storage,
           [
             snapshot_path: snapshot_path,
             storage_data: storage,
             block_storage: :dump_blocks,
             logger_table: :dump_log,
             ping_time: :no_timer
           ]
           |> Anoma.Node.start_min()}
      )

    [nodes: nodes, name: name]
  end

  test "loading keeps addresses and storage", %{nodes: nodes, name: name} do
    node = Anoma.Node.state(nodes)
    key = 555
    zero = zero_counter(key)

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.mempool_topic, :local}
      )

    Mempool.hard_reset(node.mempool)

    wait_for_tx(node.mempool, {:kv, zero}, 5000)

    Mempool.execute(node.mempool)

    assert_receive {:"$gen_cast", {_, _, {:executed, {:ok, _}}}}

    block_store_old = Mnesia.dump(:dump_blocks)

    dump_file = "dump_test.dmp"
    Anoma.Dump.dump(dump_file, name)

    id = node.router.id
    sname = Anoma.Node.Router.process_name(:supervisor, id)

    assert sname |> Process.whereis() |> Process.alive?() == true

    DynamicSupervisor.stop(sname, :normal)
    GenServer.stop(nodes, :normal)

    assert Process.whereis(sname) == nil

    {:ok, pid} =
      Anoma.Dump.launch(Directories.data(dump_file), :dump_new, testing: true)

    new_node = Anoma.Node.state(:dump_new)

    assert new_node == node
    assert Mnesia.dump(:dump_blocks) == block_store_old

    assert sname |> Process.whereis() |> Process.alive?() == true

    DynamicSupervisor.stop(sname, :normal)
    GenServer.stop(pid, :normal)

    Anoma.Dump.remove_dump(dump_file)
  end
end
