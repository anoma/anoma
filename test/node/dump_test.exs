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
            storage: storage,
            block_storage: :dump_blocks,
            ping_time: :no_timer
          ]
          |> Anoma.Node.start_min()
      )

    node = Anoma.Node.state(nodes)

    [node: node]
  end

  test "loading keeps addresses", %{node: node} do
    Anoma.Node.dump(:dump, "dump_test")
    # Stop the GenServer directly to ensure correct node termination
    GenServer.stop(:dump, :normal)

    # Make sure that the next created node dumps on shutdown
    System.put_env(Anoma.Node.shutdown_save_path(), "shutdown_dump_test")

    Anoma.Dump.launch("dump_test.txt", :dump_new)

    new_node = Anoma.Node.state(:dump_new)

    assert new_node == node

    # Stop the GenServer directly to ensure correct node termination
    GenServer.stop(:dump_new, :normal)

    # Make sure that the next created node does not dump on shutdown
    System.delete_env(Anoma.Node.shutdown_save_path())

    Anoma.Dump.launch("shutdown_dump_test.txt", :dump_new_new)

    new_node = Anoma.Node.state(:dump_new_new)

    assert new_node == node
    # Stop the GenServer directly to ensure correct node termination
    GenServer.stop(:dump_new_new, :normal)
  end
end
