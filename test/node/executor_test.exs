defmodule AnomaTest.Node.Executor do
  use ExUnit.Case, async: true

  alias Anoma.{Storage, Order}
  alias Anoma.Node.Storage.Communicator
  alias Anoma.Node.Executor.Communicator, as: CCom
  import TestHelper.Nock

  setup_all do
    storage = %Anoma.Storage{
      qualified: AnomaTest.Exectuor.Qualified,
      order: AnomaTest.Executor.Order
    }

    ordering = :executor_storage_com
    executor = :executor_test_com

    snapshot_path = [:my_special_nock_snaphsot | 0]
    env = %Nock{snapshot_path: snapshot_path, ordering: ordering}

    unless Process.whereis(ordering) do
      Anoma.Node.Storage.start_link(name: :executor_storage, table: storage)
    end

    unless Process.whereis(executor) do
      Anoma.Node.Executor.start_link(
        env
        |> Map.to_list()
        |> Keyword.put(:name, :executor_test)
      )
    end

    [env: env, executor: executor]
  end

  test "successful worker pool", %{env: env, executor: executor} do
    # very similar to the standalone worker test, but we have pools!
    key = 555
    id_1 = System.unique_integer([:positive])
    id_2 = System.unique_integer([:positive])

    storage = Communicator.get_storage(env.ordering)
    increment = increment_counter_val(key)

    Storage.ensure_new(storage)
    Communicator.reset(env.ordering)

    spawn_1 = CCom.new_transaction(executor, id_1, {:kv, increment})
    spawn_2 = CCom.new_transaction(executor, id_2, {:kv, increment})

    # simulate sending in 2 different orders
    ord_1 = Communicator.next_order(env.ordering)

    Communicator.new_order(env.ordering, [Order.new(ord_1, id_1, spawn_1.pid)])

    ord_2 = Communicator.next_order(env.ordering)

    Communicator.new_order(env.ordering, [Order.new(ord_2, id_2, spawn_2.pid)])

    # Setup default value for storage
    Storage.put(storage, key, 0)
    # Now set the snapshot up that scry expects
    Storage.put_snapshot(storage, hd(env.snapshot_path))
    # tell the first spawn it can write
    send(spawn_1.pid, {:write_ready, 1})
    assert :ok == Task.await(spawn_1)
    assert {:ok, 1} == Storage.get(storage, key)

    send(spawn_2.pid, {:write_ready, 2})
    assert :ok == Task.await(spawn_2)
    assert {:ok, 2} == Storage.get(storage, key)
  end
end
