defmodule AnomaTest.Node.Mempool do
  use TestHelper.TestMacro, async: true

  alias Anoma.Node.Storage
  alias Anoma.Node.Ordering
  alias Anoma.Node.Mempool
  alias Anoma.Node.Router
  import TestHelper.{Nock, Mempool}

  setup_all do
    storage = %Storage{
      qualified: AnomaTest.Mempool.Qualified,
      order: AnomaTest.Mempool.Order
    }

    name = :mempool
    snapshot_path = [:my_special_nock_snaphsot | 0]

    {:ok, nodes} =
      Anoma.Node.start_link_or_find_instance(
        name: name,
        testing: true,
        use_rocks: false,
        settings:
          {:new_storage,
           [
             snapshot_path: snapshot_path,
             storage_data: storage,
             block_storage: :mempool_blocks,
             ping_time: :no_timer
           ]
           |> Anoma.Node.start_min()}
      )

    node = Anoma.Node.state(nodes)

    [node: node]
  end

  test "successful process", %{node: node} do
    key = 555
    storage = Ordering.get_storage(node.ordering)
    increment = increment_counter_val(key)
    zero = zero_counter(key)

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.executor_topic, :local}
      )

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.mempool_topic, :local}
      )

    Mempool.hard_reset(node.mempool)

    worker_zero = wait_for_tx(node.mempool, {:kv, zero}, 5000).addr

    Mempool.execute(node.mempool)

    worker_one = wait_for_tx(node.mempool, {:kv, increment}, 5000).addr

    worker_two = wait_for_tx(node.mempool, {:kv, increment}, 5000).addr

    Mempool.execute(node.mempool)

    Enum.each(
      [worker_zero, worker_one, worker_two],
      &TestHelper.Worker.wait_for_worker/1
    )

    assert {:ok, 2} = Storage.get(storage, key)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic, :local}
      )

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.mempool_topic, :local}
      )
  end

  test "Processes still snapshots", %{node: node} do
    key = 555
    storage = Ordering.get_storage(node.ordering)
    increment = increment_counter_val(key)

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.executor_topic, :local}
      )

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.mempool_topic, :local}
      )

    Mempool.hard_reset(node.mempool)

    worker_one = wait_for_tx(node.mempool, {:kv, increment}, 5000).addr

    worker_two = wait_for_tx(node.mempool, {:kv, increment}, 5000).addr

    Mempool.execute(node.mempool)
    Enum.each([worker_one, worker_two], &TestHelper.Worker.wait_for_worker/1)
    assert :absent = Storage.get(storage, key)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic, :local}
      )

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.mempool_topic, :local}
      )
  end

  test "Processes gets killed", %{node: node} do
    key = 333
    storage = Ordering.get_storage(node.ordering)

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.mempool_topic, :local}
      )

    Mempool.hard_reset(node.mempool)

    worker_zero =
      wait_for_tx(node.mempool, {:kv, zero_counter(key)}, 5000).addr

    Mempool.soft_reset(node.mempool)

    Mempool.execute(node.mempool)

    assert_receive({:"$gen_cast", {_, _, {:executed, {:ok, 0}}}}, 5000)

    pid = Router.Addr.pid(worker_zero)
    assert pid == nil or not Process.alive?(pid)
    assert :absent = Storage.get(storage, key)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.mempool_topic, :local}
      )
  end

  test "Transaction print", %{node: node} do
    key = 666

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.executor_topic, :local}
      )

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.mempool_topic, :local}
      )

    Mempool.hard_reset(node.mempool)

    zero_tx = wait_for_tx(node.mempool, {:kv, zero_counter(key)}, 5000)
    assert Router.Engine.get_state(node.mempool).transactions == [zero_tx]
    Mempool.soft_reset(node.mempool)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic, :local}
      )

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.mempool_topic, :local}
      )
  end
end
