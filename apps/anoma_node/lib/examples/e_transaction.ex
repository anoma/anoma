defmodule Anoma.Node.Examples.ETransaction do
  alias Anoma.Node.Transaction.{Storage, Ordering, Mempool}

  require ExUnit.Assertions
  import ExUnit.Assertions

  ## storage

  def restart_storage do
    if GenServer.whereis(Anoma.Node.Transaction.Storage) do
      GenServer.stop(Anoma.Node.Transaction.Storage)
    end

    :mnesia.clear_table(Anoma.Node.Transaction.Storage.Values)
    :mnesia.clear_table(Anoma.Node.Transaction.Storage.Updates)
    :mnesia.clear_table(Anoma.Node.Transaction.Storage.Blocks)

    Anoma.Node.Transaction.Storage.start_link()
  end

  def write_then_read do
    restart_storage()
    Storage.write({1, [{["abc"], 123}]})
    {:ok, 123} = Storage.read({1, ["abc"]})
  end

  def write_then_read_other do
    restart_storage()
    Storage.write({1, [{["abc"], 123}]})
    :absent = Storage.read({1, ["def"]})
  end

  def read_future_then_write do
    restart_storage()
    task = Task.async(fn -> Storage.read({1, ["abc"]}) end)
    Storage.write({1, [{["abc"], 123}]})
    {:ok, 123} = Task.await(task)
  end

  def read_other_future_then_write do
    restart_storage()
    task = Task.async(fn -> Storage.read({1, ["def"]}) end)
    Storage.write({1, [{["abc"], 123}]})
    :absent = Task.await(task)
  end

  def write_future_then_write_present do
    restart_storage()
    _task1 = Task.async(fn -> Storage.write({2, [{["abc"], 123}]}) end)
    task2 = Task.async(fn -> Storage.read({2, ["abc"]}) end)
    Storage.write({1, [{["other"], 999}]})

    {:ok, 123} = Task.await(task2)
  end

  def write_multiple_then_read do
    restart_storage()
    Storage.write({1, [{["abc"], 123}, {["bcd"], 231}]})
    {:ok, 123} = Storage.read({1, ["abc"]})
    {:ok, 231} = Storage.read({1, ["bcd"]})
  end

  def write_future_multiple_then_write_present do
    restart_storage()

    _task1 =
      Task.async(fn ->
        Storage.write({2, [{["abc"], 123}, {["bcd"], 231}]})
      end)

    task2 = Task.async(fn -> Storage.read({2, ["bcd"]}) end)
    Storage.write({1, [{["other"], 999}]})

    {:ok, 231} = Task.await(task2)
  end

  def append_then_read do
    restart_storage()
    Storage.append({1, [{:set, "value"}]})
    new_set = MapSet.new(["value"])
    {:ok, ^new_set} = Storage.read({1, :set})
  end

  def append_then_read_same do
    restart_storage()
    Storage.append({1, [{:set, "value"}, {:set, "value"}]})
    new_set = MapSet.new(["value"])
    {:ok, ^new_set} = Storage.read({1, :set})
  end

  def append_then_read_several do
    restart_storage()
    Storage.append({1, [{:set, "value1"}, {:set, "value2"}]})
    new_set = MapSet.new(["value1", "value2"])
    {:ok, ^new_set} = Storage.read({1, :set})
  end

  def append_twice_then_read do
    restart_storage()
    Storage.append({1, [{:set, "value1"}]})
    new_set = MapSet.new(["value1"])
    {:ok, ^new_set} = Storage.read({1, :set})
    Storage.append({2, [{:set, "value2"}]})
    appended_set = MapSet.new(["value1", "value2"])
    {:ok, ^appended_set} = Storage.read({2, :set})
  end

  def append_twice_then_read_with_commit do
    restart_storage()
    Storage.append({1, [{:set, "value1"}]})
    new_set = MapSet.new(["value1"])
    {:ok, ^new_set} = Storage.read({1, :set})

    Storage.commit(1, nil)

    Storage.append({2, [{:set, "value2"}]})
    appended_set = MapSet.new(["value1", "value2"])
    {:ok, ^appended_set} = Storage.read({2, :set})
  end

  def complicated_storage do
    restart_storage()
    task1 = Task.async(fn -> Storage.read({3, ["abc"]}) end)
    task2 = Task.async(fn -> Storage.read({2, ["abc"]}) end)
    task3 = Task.async(fn -> Storage.read({1, ["abc"]}) end)
    task4 = Task.async(fn -> Storage.read({0, ["abc"]}) end)

    _blocking_write_task =
      Task.async(fn -> Storage.write({2, [{["abc"], 123}]}) end)

    Storage.write({1, [{["def"], 999}]})
    Storage.write({3, [{["abc"], 401}]})

    %{
      task1: {:ok, 401} = Task.await(task1),
      task2: {:ok, 123} = Task.await(task2),
      task3: :absent = Task.await(task3),
      task4: :absent = Task.await(task4)
    }
  end

  def complicated_storage_with_commit do
    restart_storage()
    task1 = Task.async(fn -> Storage.read({3, ["abc"]}) end)
    task2 = Task.async(fn -> Storage.read({2, ["abc"]}) end)
    task3 = Task.async(fn -> Storage.read({1, ["abc"]}) end)
    task4 = Task.async(fn -> Storage.read({0, ["abc"]}) end)

    _blocking_write_task =
      Task.async(fn -> Storage.write({2, [{["abc"], 123}]}) end)

    Storage.write({1, [{["def"], 999}]})
    Storage.commit(1, nil)
    Storage.write({3, [{["abc"], 401}]})

    %{
      task1: {:ok, 401} = Task.await(task1),
      task2: {:ok, 123} = Task.await(task2),
      task3: :absent = Task.await(task3),
      task4: :absent = Task.await(task4)
    }
  end

  ## ordering
  def restart_ordering do
    if GenServer.whereis(Anoma.Node.Transaction.Ordering) do
      GenServer.stop(Anoma.Node.Transaction.Ordering)
    end

    Anoma.Node.Transaction.Ordering.start_link()
  end

  def ord_write_then_read do
    restart_storage()
    restart_ordering()

    _write_task =
      Task.async(fn -> Ordering.write({"tx id 1", [{["abc"], 123}]}) end)

    read_task = Task.async(fn -> Ordering.read({"tx id 2", ["abc"]}) end)
    order = ["tx id 1", "tx id 2"]

    Ordering.order(order)
    {:ok, 123} = Task.await(read_task)
  end

  def ord_read_future_then_write do
    restart_storage()
    restart_ordering()

    read_task = Task.async(fn -> Ordering.read({"tx id 2", ["abc"]}) end)

    write_task =
      Task.async(fn -> Ordering.write({"tx id 1", [{["abc"], 123}]}) end)

    Ordering.order(["tx id 1", "tx id 2"])
    :ok = Task.await(write_task)
    {:ok, 123} = Task.await(read_task)
  end

  def ord_order_first do
    restart_storage()
    restart_ordering()

    Ordering.order(["tx id 1", "tx id 2"])

    Ordering.write({"tx id 1", [{["abc"], 123}]})
    {:ok, 123} = Ordering.read({"tx id 2", ["abc"]})
  end

  def restart_mempool do
    if GenServer.whereis(Anoma.Node.Transaction.Mempool) do
      GenServer.stop(Anoma.Node.Transaction.Mempool)
    end

    Anoma.Node.Transaction.Mempool.start_link()
  end

  def restart_executor do
    if GenServer.whereis(Anoma.Node.Transaction.Executor) do
      GenServer.stop(Anoma.Node.Transaction.Executor)
    end

    Anoma.Node.Transaction.Executor.start_link(nil)
  end

  def restart_tx_module do
    restart_ordering()
    restart_storage()
    restart_executor()
    restart_mempool()
  end

  def mempool_startup_txs do
    restart_storage()
    restart_ordering()

    if GenServer.whereis(Anoma.Node.Transaction.Mempool) do
      GenServer.stop(Anoma.Node.Transaction.Mempool)
    end

    Mempool.start_link(txs: [{"id 1", {:debug_term_storage, "code"}}])
    ["id 1"] = Mempool.tx_dump()
  end

  def mempool_startup_consensus(round \\ 0) do
    restart_storage()
    restart_ordering()

    if GenServer.whereis(Anoma.Node.Transaction.Mempool) do
      GenServer.stop(Anoma.Node.Transaction.Mempool)
    end

    Mempool.start_link(
      txs: [{"id 1", {:debug_term_storage, "code"}}],
      consensus: ["id 1"],
      round: round
    )

    [] = Mempool.tx_dump()

    [
      {Storage.Blocks, ^round,
       [
         %Mempool.Tx{
           code: "code",
           backend: :debug_term_storage,
           vm_result: :error
         }
       ]}
    ] = :mnesia.dirty_read({Storage.Blocks, round})
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

  def zero_counter_submit(key \\ "key") do
    restart_tx_module()
    {back, zero} = zero(key)

    Mempool.tx({back, zero}, "id 1")
    :mnesia.subscribe({:table, Storage.Blocks, :simple})
    Mempool.tx_dump() |> Mempool.execute()

    assert_receive(
      {:mnesia_table_event, {:write, {Storage.Blocks, 0, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Storage.Blocks, :simple})

    [
      {Storage.Blocks, 0,
       [
         %Mempool.Tx{
           code: ^zero,
           backend: ^back,
           vm_result: {:ok, [[^key | 0] | 0]},
           tx_result: {:ok, [[^key | 0]]}
         }
       ]}
    ] = :mnesia.dirty_read({Storage.Blocks, 0})
  end

  def inc_counter_submit_with_zero(key \\ "key") do
    restart_tx_module()
    {back1, zero} = zero(key)
    {back2, inc} = inc(key)

    Mempool.tx({back1, zero}, "id 1")
    Mempool.tx({back2, inc}, "id 2")
    :mnesia.subscribe({:table, Storage.Blocks, :simple})
    Mempool.tx_dump() |> Mempool.execute()

    assert_receive(
      {:mnesia_table_event, {:write, {Storage.Blocks, 0, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Storage.Blocks, :simple})

    [
      {Storage.Blocks, 0,
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
    ] = :mnesia.dirty_read({Storage.Blocks, 0})
  end

  def inc_counter_submit_after_zero(key \\ "key") do
    zero_counter_submit(key)
    {back, inc} = inc(key)
    Mempool.tx({back, inc}, "id 2")
    :mnesia.subscribe({:table, Storage.Blocks, :simple})
    ["id 2"] |> Mempool.execute()

    assert_receive(
      {:mnesia_table_event, {:write, {Storage.Blocks, 1, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Storage.Blocks, :simple})

    [
      {Storage.Blocks, 1,
       [
         %Mempool.Tx{
           code: ^inc,
           backend: ^back,
           vm_result: {:ok, [[^key | 1] | 0]},
           tx_result: {:ok, [[^key | 1]]}
         }
       ]}
    ] = :mnesia.dirty_read({Storage.Blocks, 1})
  end

  def inc_counter_submit_after_read(key \\ "key") do
    zero_counter_submit(key)
    {:debug_term_storage, zero} = zero(key)
    {back, inc} = inc(key)
    Mempool.tx({{:debug_read_term, self()}, zero}, "id 2")
    Mempool.tx(inc(key), "id 3")
    :mnesia.subscribe({:table, Storage.Blocks, :simple})
    ["id 2", "id 3"] |> Mempool.execute()

    assert_receive(
      {:mnesia_table_event, {:write, {Storage.Blocks, 1, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Storage.Blocks, :simple})

    [
      {Storage.Blocks, 1,
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
    ] = :mnesia.dirty_read({Storage.Blocks, 1})
  end

  def bluf() do
    [0 | 0]
  end

  def bluf_transaction_errors do
    restart_tx_module()
    # todo: ideally we wait for the event broker message
    # before execution
    Mempool.tx({:debug_term_storage, bluf()}, "id 1")
    :mnesia.subscribe({:table, Storage.Blocks, :simple})
    ["id 1"] |> Mempool.execute()

    assert_receive(
      {:mnesia_table_event, {:write, {Storage.Blocks, 0, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Storage.Blocks, :simple})

    [
      {Storage.Blocks, 0,
       [
         %Mempool.Tx{
           code: [0 | 0],
           backend: :debug_term_storage,
           vm_result: :error,
           tx_result: :error
         }
       ]}
    ] = :mnesia.dirty_read({Storage.Blocks, 0})
  end

  def read_txs_write_nothing(key \\ "key") do
    restart_tx_module()
    Mempool.tx({{:debug_read_term, self()}, zero(key)}, "id 1")
    :mnesia.subscribe({:table, Storage.Blocks, :simple})
    ["id 1"] |> Mempool.execute()

    assert_receive(
      {:mnesia_table_event, {:write, {Storage.Blocks, 0, _}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Storage.Blocks, :simple})

    [] = :mnesia.dirty_all_keys(Storage.Values)
    [] = :mnesia.dirty_all_keys(Storage.Updates)
  end

  def bluff_txs_write_nothing() do
    bluf_transaction_errors()

    [] = :mnesia.dirty_all_keys(Storage.Values)
    [] = :mnesia.dirty_all_keys(Storage.Updates)
  end
end
