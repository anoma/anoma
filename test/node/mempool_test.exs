defmodule AnomaTest.Node.Mempool do
  use TestHelper.TestMacro, async: true

  alias Anoma.Node.Storage
  alias Anoma.Node.Ordering
  alias Anoma.Node.Mempool
  alias Anoma.Node.Router
  import TestHelper.Nock

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

    Mempool.hard_reset(node.mempool)

    worker_zero = Mempool.tx(node.mempool, {:kv, zero}).addr

    Mempool.execute(node.mempool)
    worker_one = Mempool.tx(node.mempool, {:kv, increment}).addr
    worker_two = Mempool.tx(node.mempool, {:kv, increment}).addr

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

    Mempool.hard_reset(node.mempool)

    worker_one = Mempool.tx(node.mempool, {:kv, increment}).addr
    worker_two = Mempool.tx(node.mempool, {:kv, increment}).addr

    Mempool.execute(node.mempool)
    Enum.each([worker_one, worker_two], &TestHelper.Worker.wait_for_worker/1)
    assert :absent = Storage.get(storage, key)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic, :local}
      )
  end

  test "Processes gets killed", %{node: node} do
    key = 333
    storage = Ordering.get_storage(node.ordering)

    Mempool.hard_reset(node.mempool)

    worker_zero = Mempool.tx(node.mempool, {:kv, zero_counter(key)}).addr

    Mempool.soft_reset(node.mempool)

    Mempool.execute(node.mempool)

    pid = Router.Addr.pid(worker_zero)
    assert pid == nil or not Process.alive?(pid)
    assert :absent = Storage.get(storage, key)
  end

  test "Transaction print", %{node: node} do
    key = 666

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.executor_topic, :local}
      )

    Mempool.hard_reset(node.mempool)

    zero_tx = Mempool.tx(node.mempool, {:kv, zero_counter(key)})
    assert Mempool.pending_txs(node.mempool) == [zero_tx]
    Mempool.soft_reset(node.mempool)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic, :local}
      )
  end

  test "Transactions broadcast", %{node: node} do
    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.mempool_topic, :local}
      )

    Mempool.hard_reset(node.mempool)

    zero_tx = Mempool.tx(node.mempool, {:kv, zero_counter(777)})

    assert_receive {:"$gen_cast", {_, _, {:submitted, ^zero_tx}}}
    Mempool.soft_reset(node.mempool)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.mempool_topic, :local}
      )
  end
end
