defmodule AnomaTest.Node.Mempool do
  use ExUnit.Case, async: true

  alias Anoma.Storage
  alias Anoma.Node.Storage.Communicator, as: Scom
  alias Anoma.Node.Executor.Communicator, as: Ccom
  alias Anoma.Node.Mempool.Communicator, as: Mcom
  import TestHelper.Nock

  setup_all do
    storage = %Anoma.Storage{
      qualified: AnomaTest.Mempool.Qualified,
      order: AnomaTest.Mempool.Order
    }

    name = :mempool
    snapshot_path = [:my_special_nock_snaphsot | 0]

    node = Anoma.Node.com_names(name)

    unless Process.whereis(:mempool_mempool_com) do
      Anoma.Node.start_link(
        name: name,
        snapshot_path: snapshot_path,
        storage: storage,
        block_storage: :mempool_blocks
      )
    end

    [node: node]
  end

  test "successful process", %{node: node} do
    key = 555
    storage = Scom.get_storage(node.ordering)
    increment = increment_counter_val(key)
    zero = zero_counter(key)

    Ccom.subscribe(node.executor, self())
    Mcom.hard_reset(node.mempool)

    pid_zero = Mcom.tx(node.mempool, {:kv, zero}).pid

    Mcom.execute(node.mempool)
    pid_one = Mcom.tx(node.mempool, {:kv, increment}).pid
    pid_two = Mcom.tx(node.mempool, {:kv, increment}).pid

    Mcom.execute(node.mempool)
    assert_receive {:"$gen_cast", {:process_done, ^pid_zero}}
    assert_receive {:"$gen_cast", {:process_done, ^pid_one}}
    assert_receive {:"$gen_cast", {:process_done, ^pid_two}}
    assert {:ok, 2} = Storage.get(storage, key)
    Ccom.reset(node.executor)
  end

  test "Processes still snapshots", %{node: node} do
    key = 555
    storage = Scom.get_storage(node.ordering)
    increment = increment_counter_val(key)

    Ccom.subscribe(node.executor, self())
    Mcom.hard_reset(node.mempool)

    pid_one = Mcom.tx(node.mempool, {:kv, increment}).pid
    pid_two = Mcom.tx(node.mempool, {:kv, increment}).pid

    Mcom.execute(node.mempool)
    assert_receive {:"$gen_cast", {:process_done, ^pid_one}}
    assert_receive {:"$gen_cast", {:process_done, ^pid_two}}
    assert :absent = Storage.get(storage, key)
    Ccom.reset(node.executor)
  end

  test "Processes gets killed", %{node: node} do
    key = 333
    storage = Scom.get_storage(node.ordering)

    Mcom.hard_reset(node.mempool)

    pid_zero = Mcom.tx(node.mempool, {:kv, zero_counter(key)}).pid

    Mcom.soft_reset(node.mempool)

    Mcom.execute(node.mempool)

    assert not Process.alive?(pid_zero)
    assert :absent = Storage.get(storage, key)
  end

  test "Transaction print", %{node: node} do
    key = 666
    Ccom.subscribe(node.executor, self())
    Mcom.hard_reset(node.mempool)

    zero_tx = Mcom.tx(node.mempool, zero_counter(key))
    assert Mcom.pending_txs(node.mempool) == [zero_tx]
    Mcom.soft_reset(node.mempool)
  end

  test "Transactions broadcast", %{node: node} do
    Mcom.subscribe(node.mempool, self())

    Mcom.hard_reset(node.mempool)

    zero_tx = Mcom.tx(node.mempool, zero_counter(777))

    assert_receive {:"$gen_cast", {:submitted, ^zero_tx}}
    Mcom.soft_reset(node.mempool)
  end
end
