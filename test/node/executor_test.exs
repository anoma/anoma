defmodule AnomaTest.Node.Executor do
  use ExUnit.Case, async: true

  alias Anoma.{Storage, Order}
  alias Anoma.Node.Storage.Ordering
  alias Anoma.Node.Executor
  alias Anoma.Node.Router
  alias Router
  import TestHelper.Nock

  setup_all do
    storage = %Anoma.Storage{
      qualified: AnomaTest.Executor.Qualified,
      order: AnomaTest.Executor.Order
    }

    {:ok, router} = Router.start()

    {:ok, ordering} =
      Router.start_engine(router, Anoma.Node.Storage.Ordering, table: storage)

    snapshot_path = [:my_special_nock_snaphsot | 0]
    env = %Nock{snapshot_path: snapshot_path, ordering: ordering}

    {:ok, executor} =
      Router.start_engine(
        router,
        Anoma.Node.Executor,
        {env, Router.new_topic(router), nil}
      )

    [env: env, executor: executor]
  end

  test "successful worker pool", %{env: env, executor: executor} do
    # very similar to the standalone worker test, but we have pools!
    key = 555
    id_1 = System.unique_integer([:positive])
    id_2 = System.unique_integer([:positive])

    storage = Ordering.get_storage(env.ordering)
    increment = increment_counter_val(key)

    Storage.ensure_new(storage)
    Ordering.reset(env.ordering)

    spawn_1 = Executor.new_transaction(executor, id_1, {:kv, increment})
    spawn_2 = Executor.new_transaction(executor, id_2, {:kv, increment})

    # simulate sending in 2 different orders
    ord_1 = Ordering.next_order(env.ordering)

    Ordering.new_order(env.ordering, [Order.new(ord_1, id_1, spawn_1.pid)])

    ord_2 = Ordering.next_order(env.ordering)

    Ordering.new_order(env.ordering, [Order.new(ord_2, id_2, spawn_2.pid)])

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
