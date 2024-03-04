defmodule AnomaTest.Node.Mempool do
  use ExUnit.Case, async: true

  alias Anoma.Storage
  alias Anoma.Node.Storage.Ordering
  alias Anoma.Node.Mempool
  alias Anoma.Node.Router
  import TestHelper.Nock

  setup_all do
    storage = %Anoma.Storage{
      qualified: AnomaTest.Mempool.Qualified,
      order: AnomaTest.Mempool.Order
    }

    name = :mempool
    snapshot_path = [:my_special_nock_snaphsot | 0]

    {:ok, nodes} =
      Anoma.Node.start_link(
        name: name,
        snapshot_path: snapshot_path,
        storage: storage,
        block_storage: :mempool_blocks,
        ping_time: :no_timer
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
        {:subscribe_topic, node.executor_topic.id, :local}
      )

    Mempool.hard_reset(node.mempool)

    pid_zero = Mempool.tx(node.mempool, {:kv, zero}).pid

    Mempool.execute(node.mempool)
    pid_one = Mempool.tx(node.mempool, {:kv, increment}).pid
    pid_two = Mempool.tx(node.mempool, {:kv, increment}).pid

    Mempool.execute(node.mempool)
    assert_receive {:"$gen_cast", {_, {:process_done, ^pid_zero}}}
    assert_receive {:"$gen_cast", {_, {:process_done, ^pid_one}}}
    assert_receive {:"$gen_cast", {_, {:process_done, ^pid_two}}}
    assert {:ok, 2} = Storage.get(storage, key)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic.id, :local}
      )
  end

  test "Processes still snapshots", %{node: node} do
    key = 555
    storage = Ordering.get_storage(node.ordering)
    increment = increment_counter_val(key)

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.executor_topic.id, :local}
      )

    Mempool.hard_reset(node.mempool)

    pid_one = Mempool.tx(node.mempool, {:kv, increment}).pid
    pid_two = Mempool.tx(node.mempool, {:kv, increment}).pid

    Mempool.execute(node.mempool)
    assert_receive {:"$gen_cast", {_, {:process_done, ^pid_one}}}
    assert_receive {:"$gen_cast", {_, {:process_done, ^pid_two}}}
    assert :absent = Storage.get(storage, key)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic.id, :local}
      )
  end

  test "Processes gets killed", %{node: node} do
    key = 333
    storage = Ordering.get_storage(node.ordering)

    Mempool.hard_reset(node.mempool)

    pid_zero = Mempool.tx(node.mempool, {:kv, zero_counter(key)}).pid

    Mempool.soft_reset(node.mempool)

    Mempool.execute(node.mempool)

    assert not Process.alive?(pid_zero)
    assert :absent = Storage.get(storage, key)
  end

  test "Transaction print", %{node: node} do
    key = 666

    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.executor_topic.id, :local}
      )

    Mempool.hard_reset(node.mempool)

    zero_tx = Mempool.tx(node.mempool, {:kv, zero_counter(key)})
    assert Mempool.pending_txs(node.mempool) == [zero_tx]
    Mempool.soft_reset(node.mempool)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic.id, :local}
      )
  end

  test "Transactions broadcast", %{node: node} do
    :ok =
      Router.call(
        node.router,
        {:subscribe_topic, node.mempool_topic.id, :local}
      )

    Mempool.hard_reset(node.mempool)

    zero_tx = Mempool.tx(node.mempool, {:kv, zero_counter(777)})

    assert_receive {:"$gen_cast", {_, {:submitted, ^zero_tx}}}
    Mempool.soft_reset(node.mempool)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.mempool_topic.id, :local}
      )
  end
end
