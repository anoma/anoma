defmodule AnomaTest.Node.Dump do
  use ExUnit.Case, async: true

  alias Anoma.Node.Mempool
  alias Anoma.Mnesia
  import TestHelper.Nock

  setup_all do
    storage = %Anoma.Node.Storage{
      qualified: AnomaTest.Dump.Qualified,
      order: AnomaTest.Dump.Order
    }

    name = :dump
    snapshot_path = [:my_special_nock_snaphsot | 0]

    {:ok, nodes} =
      Anoma.Node.start_link(
        new_storage: true,
        name: name,
        settings:
          [
            snapshot_path: snapshot_path,
            storage_data: storage,
            block_storage: :dump_blocks,
            ping_time: :no_timer
          ]
          |> Anoma.Node.start_min()
      )

    node = Anoma.Node.state(nodes)

    [node: node, name: name]
  end

  test "loading keeps addresses and storage", %{node: node, name: name} do
    key = 555
    zero = zero_counter(key)
    Mempool.hard_reset(node.mempool)

    Mempool.tx(node.mempool, {:kv, zero})

    Mempool.execute(node.mempool)

    block_store_old = Mnesia.dump(:dump_blocks)

    Anoma.Dump.dump("dump_test.dmp", name)

    id = node.router.id
    sname = Anoma.Node.Router.process_name(:supervisor, id)

    DynamicSupervisor.stop(sname, :normal)

    Anoma.Dump.launch("dump_test.dmp", :dump_new)

    new_node = Anoma.Node.state(:dump_new)

    assert new_node == node
    assert Mnesia.dump(:dump_blocks) == block_store_old

    DynamicSupervisor.stop(sname, :normal)

    Anoma.Dump.remove_dump("dump_test.dmp")
  end
end
