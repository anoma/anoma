defmodule AnomaTest.Node.Dump do
  use ExUnit.Case, async: true

  alias Anoma.Node.Mempool
  alias Anoma.Mnesia
  alias Anoma.System.Directories
  import TestHelper.Nock

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
    Mempool.hard_reset(node.mempool)

    Mempool.tx(node.mempool, {:kv, zero})

    Mempool.execute(node.mempool)

    block_store_old = Mnesia.dump(:dump_blocks)

    Anoma.Dump.dump("dump_test.dmp", name)

    id = node.router.id
    sname = Anoma.Node.Router.process_name(:supervisor, id)

    assert sname |> Process.whereis() |> Process.alive?() == true

    DynamicSupervisor.stop(sname, :normal)
    GenServer.stop(nodes, :normal)

    assert Process.whereis(sname) == nil

    {:ok, pid} =
      Anoma.Dump.launch(Directories.data("dump_test.dmp"), :dump_new)

    new_node = Anoma.Node.state(:dump_new)

    assert new_node == node
    assert Mnesia.dump(:dump_blocks) == block_store_old

    assert sname |> Process.whereis() |> Process.alive?() == true

    DynamicSupervisor.stop(sname, :normal)
    GenServer.stop(pid, :normal)

    Anoma.Dump.remove_dump("dump_test.dmp")
  end
end
