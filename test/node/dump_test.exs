defmodule AnomaTest.Node.Dump do
  use ExUnit.Case, async: true

  setup_all do
    storage = %Anoma.Storage{
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

    [node: node]
  end

  test "loading keeps addresses", %{node: node} do
    Anoma.Dump.dump("dump_test", :dump)

    id = node.router.id
    sname = Anoma.Node.Router.process_name(:supervisor, id)

    DynamicSupervisor.stop(sname, :normal)

    Anoma.Dump.launch("dump_test.txt", :dump_new)

    new_node = Anoma.Node.state(:dump_new)

    assert new_node == node

    DynamicSupervisor.stop(sname, :normal)
  end
end
