defmodule Anoma.Node.Examples.ETransaction do
  alias Anoma.Node
  alias Node.Transaction.{Storage, Ordering, Mempool}

  require ExUnit.Assertions
  import ExUnit.Assertions

  ## storage

  def start_storage(node_id \\ Node.example_random_id()) do
    Anoma.Node.Transaction.Storage.start_link(node_id: node_id)
  end

  def write_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.write(node_id, {1, [{["abc"], 123}]})
    {:ok, 123} = Storage.read(node_id, {1, ["abc"]})
  end

  def write_then_read_other(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.write(node_id, {1, [{["abc"], 123}]})
    :absent = Storage.read(node_id, {1, ["def"]})
  end

  def read_future_then_write(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    task = Task.async(fn -> Storage.read(node_id, {1, ["abc"]}) end)
    Storage.write(node_id, {1, [{["abc"], 123}]})
    {:ok, 123} = Task.await(task)
  end

  def read_other_future_then_write(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    task = Task.async(fn -> Storage.read(node_id, {1, ["def"]}) end)
    Storage.write(node_id, {1, [{["abc"], 123}]})
    :absent = Task.await(task)
  end

  def write_future_then_write_present(node_id \\ Node.example_random_id()) do
    start_storage(node_id)

    _task1 =
      Task.async(fn -> Storage.write(node_id, {2, [{["abc"], 123}]}) end)

    task2 = Task.async(fn -> Storage.read(node_id, {2, ["abc"]}) end)
    Storage.write(node_id, {1, [{["other"], 999}]})

    {:ok, 123} = Task.await(task2)
  end

  def write_multiple_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.write(node_id, {1, [{["abc"], 123}, {["bcd"], 231}]})
    {:ok, 123} = Storage.read(node_id, {1, ["abc"]})
    {:ok, 231} = Storage.read(node_id, {1, ["bcd"]})
  end

  def write_future_multiple_then_write_present(
        node_id \\ Node.example_random_id()
      ) do
    start_storage(node_id)

    _task1 =
      Task.async(fn ->
        Storage.write(node_id, {2, [{["abc"], 123}, {["bcd"], 231}]})
      end)

    task2 = Task.async(fn -> Storage.read(node_id, {2, ["bcd"]}) end)
    Storage.write(node_id, {1, [{["other"], 999}]})

    {:ok, 231} = Task.await(task2)
  end

  def append_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.append(node_id, {1, [{:set, "value"}]})
    new_set = MapSet.new(["value"])
    {:ok, ^new_set} = Storage.read(node_id, {1, :set})
  end

  def append_then_read_same(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.append(node_id, {1, [{:set, "value"}, {:set, "value"}]})
    new_set = MapSet.new(["value"])
    {:ok, ^new_set} = Storage.read(node_id, {1, :set})
  end

  def append_then_read_several(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.append(node_id, {1, [{:set, "value1"}, {:set, "value2"}]})
    new_set = MapSet.new(["value1", "value2"])
    {:ok, ^new_set} = Storage.read(node_id, {1, :set})
  end

  def append_twice_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.append(node_id, {1, [{:set, "value1"}]})
    new_set = MapSet.new(["value1"])
    {:ok, ^new_set} = Storage.read(node_id, {1, :set})
    Storage.append(node_id, {2, [{:set, "value2"}]})
    appended_set = MapSet.new(["value1", "value2"])
    {:ok, ^appended_set} = Storage.read(node_id, {2, :set})
  end

  def append_twice_then_read_with_commit(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.append(node_id, {1, [{:set, "value1"}]})
    new_set = MapSet.new(["value1"])
    {:ok, ^new_set} = Storage.read(node_id, {1, :set})

    Storage.commit(node_id, 1, nil)

    Storage.append(node_id, {2, [{:set, "value2"}]})
    appended_set = MapSet.new(["value1", "value2"])
    {:ok, ^appended_set} = Storage.read(node_id, {2, :set})
  end

  def complicated_storage(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    task1 = Task.async(fn -> Storage.read(node_id, {3, ["abc"]}) end)
    task2 = Task.async(fn -> Storage.read(node_id, {2, ["abc"]}) end)
    task3 = Task.async(fn -> Storage.read(node_id, {1, ["abc"]}) end)
    task4 = Task.async(fn -> Storage.read(node_id, {0, ["abc"]}) end)

    _blocking_write_task =
      Task.async(fn -> Storage.write(node_id, {2, [{["abc"], 123}]}) end)

    Storage.write(node_id, {1, [{["def"], 999}]})
    Storage.write(node_id, {3, [{["abc"], 401}]})

    %{
      task1: {:ok, 401} = Task.await(task1),
      task2: {:ok, 123} = Task.await(task2),
      task3: :absent = Task.await(task3),
      task4: :absent = Task.await(task4)
    }
  end

  def complicated_storage_with_commit(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    task1 = Task.async(fn -> Storage.read(node_id, {3, ["abc"]}) end)
    task2 = Task.async(fn -> Storage.read(node_id, {2, ["abc"]}) end)
    task3 = Task.async(fn -> Storage.read(node_id, {1, ["abc"]}) end)
    task4 = Task.async(fn -> Storage.read(node_id, {0, ["abc"]}) end)

    _blocking_write_task =
      Task.async(fn -> Storage.write(node_id, {2, [{["abc"], 123}]}) end)

    Storage.write(node_id, {1, [{["def"], 999}]})
    Storage.commit(node_id, 1, nil)
    Storage.write(node_id, {3, [{["abc"], 401}]})

    %{
      task1: {:ok, 401} = Task.await(task1),
      task2: {:ok, 123} = Task.await(task2),
      task3: :absent = Task.await(task3),
      task4: :absent = Task.await(task4)
    }
  end

  ## ordering
  def start_ordering(node_id \\ Node.example_random_id()) do
    Anoma.Node.Transaction.Ordering.start_link(node_id: node_id)
  end

  def ord_write_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    start_ordering(node_id)

    _write_task =
      Task.async(fn ->
        Ordering.write(node_id, {"tx id 1", [{["abc"], 123}]})
      end)

    read_task =
      Task.async(fn -> Ordering.read(node_id, {"tx id 2", ["abc"]}) end)

    order = ["tx id 1", "tx id 2"]

    Ordering.order(node_id, order)
    {:ok, 123} = Task.await(read_task)
  end

  def ord_read_future_then_write(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    start_ordering(node_id)

    read_task =
      Task.async(fn -> Ordering.read(node_id, {"tx id 2", ["abc"]}) end)

    write_task =
      Task.async(fn ->
        Ordering.write(node_id, {"tx id 1", [{["abc"], 123}]})
      end)

    Ordering.order(node_id, ["tx id 1", "tx id 2"])
    :ok = Task.await(write_task)
    {:ok, 123} = Task.await(read_task)
  end

  def ord_order_first(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    start_ordering(node_id)

    Ordering.order(node_id, ["tx id 1", "tx id 2"])

    Ordering.write(node_id, {"tx id 1", [{["abc"], 123}]})
    {:ok, 123} = Ordering.read(node_id, {"tx id 2", ["abc"]})
  end

  def start_mempool(node_id \\ Node.example_random_id()) do
    Anoma.Node.Transaction.Mempool.start_link(node_id: node_id)
  end

  def start_executor(node_id \\ Node.example_random_id()) do
    Anoma.Node.Transaction.Executor.start_link(node_id: node_id)
  end

  def start_tx_module(node_id \\ Node.example_random_id()) do
    start_ordering(node_id)
    start_storage(node_id)
    start_executor(node_id)
    start_mempool(node_id)
  end

  # to be moved to nock
  def zero(key \\ "key") do
    zero_counter_arm = [1, key | 0]
    arm = [10, [2 | zero_counter_arm], 1, 0 | 0]
    sample = 0
    zero_tx = [[8, [1 | sample], [1 | arm], 0 | 1] | 999]

    {:debug_term_storage, zero_tx}
  end

  def inc(key \\ "key") do
    increment_value_arm = [[1 | key], 4, 12, [1 | 0], [0 | 6], 1, key | 0]
    # Place the result in a list
    arm = [10, [2 | increment_value_arm], 1, 0 | 0]
    sample = 0
    inc = [[8, [1 | sample], [1 | arm], 0 | 1] | 999]

    {:debug_term_storage, inc}
  end

  def zero_counter_submit(node_id \\ Node.example_random_id()) do
    key = "key"
    start_tx_module(node_id)
    {back, zero} = zero(key)

    Mempool.tx(node_id, {back, zero}, "id 1")
    :mnesia.subscribe({:table, Storage.blocks_table(node_id), :simple})
    dump = Mempool.tx_dump(node_id)
    Mempool.execute(node_id, dump)

    blocks_table = Storage.blocks_table(node_id)

    assert_receive(
      {:mnesia_table_event, {:write, {^blocks_table, 0, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Storage.blocks_table(node_id), :simple})

    [
      {^blocks_table, 0,
       [
         %Mempool.Tx{
           code: ^zero,
           backend: ^back,
           vm_result: {:ok, [[^key | 0] | 0]},
           tx_result: {:ok, [[^key | 0]]}
         }
       ]}
    ] = :mnesia.dirty_read({Storage.blocks_table(node_id), 0})
  end

  def inc_counter_submit_with_zero(node_id \\ Node.example_random_id()) do
    blocks_table = Storage.blocks_table(node_id)
    key = "key"
    start_tx_module(node_id)
    {back1, zero} = zero(key)
    {back2, inc} = inc(key)

    Mempool.tx(node_id, {back1, zero}, "id 1")
    Mempool.tx(node_id, {back2, inc}, "id 2")
    :mnesia.subscribe({:table, blocks_table, :simple})
    dump = Mempool.tx_dump(node_id)
    Mempool.execute(node_id, dump)

    assert_receive(
      {:mnesia_table_event, {:write, {^blocks_table, 0, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, blocks_table, :simple})

    [
      {^blocks_table, 0,
       [
         %Mempool.Tx{
           code: ^zero,
           backend: ^back1,
           vm_result: {:ok, [[^key | 0] | 0]},
           tx_result: {:ok, [[^key | 0]]}
         },
         %Mempool.Tx{
           code: ^inc,
           backend: ^back2,
           vm_result: {:ok, [[^key | 1] | 0]},
           tx_result: {:ok, [[^key | 1]]}
         }
       ]}
    ] = :mnesia.dirty_read({blocks_table, 0})
  end

  def inc_counter_submit_after_zero(node_id \\ Node.example_random_id()) do
    blocks_table = Storage.blocks_table(node_id)
    key = "key"
    zero_counter_submit(node_id)
    {back, inc} = inc(key)
    Mempool.tx(node_id, {back, inc}, "id 2")
    :mnesia.subscribe({:table, blocks_table, :simple})
    Mempool.execute(node_id, ["id 2"])

    assert_receive(
      {:mnesia_table_event, {:write, {^blocks_table, 1, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, blocks_table, :simple})

    [
      {^blocks_table, 1,
       [
         %Mempool.Tx{
           code: ^inc,
           backend: ^back,
           vm_result: {:ok, [[^key | 1] | 0]},
           tx_result: {:ok, [[^key | 1]]}
         }
       ]}
    ] = :mnesia.dirty_read({blocks_table, 1})
  end

  def inc_counter_submit_after_read(node_id \\ Node.example_random_id()) do
    blocks_table = Storage.blocks_table(node_id)

    key = "key"
    zero_counter_submit(node_id)
    {:debug_term_storage, zero} = zero(key)
    {back, inc} = inc(key)
    Mempool.tx(node_id, {{:debug_read_term, self()}, zero}, "id 2")
    Mempool.tx(node_id, inc(key), "id 3")
    :mnesia.subscribe({:table, blocks_table, :simple})
    Mempool.execute(node_id, ["id 2", "id 3"])

    assert_receive(
      {:mnesia_table_event, {:write, {^blocks_table, 1, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, blocks_table, :simple})

    [
      {^blocks_table, 1,
       [
         %Mempool.Tx{
           code: ^zero,
           backend: {:debug_read_term, _},
           vm_result: {:ok, [[^key | 0] | 0]},
           tx_result: {:ok, {:read_value, [[^key | 0] | 0]}}
         },
         %Mempool.Tx{
           code: ^inc,
           backend: ^back,
           vm_result: {:ok, [[^key | 1] | 0]},
           tx_result: {:ok, [[^key | 1]]}
         }
       ]}
    ] = :mnesia.dirty_read({blocks_table, 1})
  end

  def bluf() do
    [0 | 0]
  end

  def bluf_transaction_errors(node_id \\ Node.example_random_id()) do
    blocks_table = Storage.blocks_table(node_id)
    start_tx_module(node_id)
    # todo: ideally we wait for the event broker message
    # before execution
    Mempool.tx(node_id, {:debug_term_storage, bluf()}, "id 1")
    :mnesia.subscribe({:table, blocks_table, :simple})
    Mempool.execute(node_id, ["id 1"])

    assert_receive(
      {:mnesia_table_event, {:write, {^blocks_table, 0, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, blocks_table, :simple})

    [
      {^blocks_table, 0,
       [
         %Mempool.Tx{
           code: [0 | 0],
           backend: :debug_term_storage,
           vm_result: :error,
           tx_result: :error
         }
       ]}
    ] = :mnesia.dirty_read({blocks_table, 0})
  end

  def read_txs_write_nothing(node_id \\ Node.example_random_id()) do
    blocks_table = Storage.blocks_table(node_id)
    key = "key"
    start_tx_module(node_id)
    {_backend, code} = zero(key)

    Mempool.tx(node_id, {{:debug_read_term, self()}, code}, "id 1")
    :mnesia.subscribe({:table, blocks_table, :simple})
    Mempool.execute(node_id, ["id 1"])

    assert_receive(
      {:mnesia_table_event, {:write, {^blocks_table, 0, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, blocks_table, :simple})

    [] = :mnesia.dirty_all_keys(Storage.values_table(node_id))
    [] = :mnesia.dirty_all_keys(Storage.updates_table(node_id))
  end

  def bluff_txs_write_nothing(node_id \\ Node.example_random_id()) do
    bluf_transaction_errors(node_id)

    [] = :mnesia.dirty_all_keys(Storage.values_table(node_id))
    [] = :mnesia.dirty_all_keys(Storage.updates_table(node_id))
  end
end
