defmodule Anoma.Node.Examples.ETransaction do
  alias Anoma.Node
  alias Node.Transaction.{Storage, Ordering, Mempool, Backends}
  alias Anoma.TransparentResource.Transaction
  alias Anoma.Node.Config
  alias Anoma.Node.Events
  alias Examples.{ENock, ETransparent.ETransaction}

  alias Anoma.Node.Examples.ENode
  require ExUnit.Assertions
  import ExUnit.Assertions
  import ExUnit.CaptureLog

  ############################################################
  #                          Storage                         #
  ############################################################

  @spec start_storage(String.t()) :: GenServer.on_start()
  def start_storage(node_id \\ Node.example_random_id()) do
    Anoma.Node.Transaction.Storage.start_link(node_id: node_id)
  end

  @spec write_then_read(String.t()) :: String.t()
  def write_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.write(node_id, {1, [{["abc"], 123}]})
    {:ok, 123} = Storage.read(node_id, {1, ["abc"]})
    node_id
  end

  @spec write_then_read_other(String.t()) :: String.t()
  def write_then_read_other(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.write(node_id, {1, [{["abc"], 123}]})
    :absent = Storage.read(node_id, {1, ["def"]})
    node_id
  end

  @spec read_future_then_write(String.t()) :: String.t()
  def read_future_then_write(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    task = Task.async(fn -> Storage.read(node_id, {1, ["abc"]}) end)
    Storage.write(node_id, {1, [{["abc"], 123}]})
    {:ok, 123} = Task.await(task)
    node_id
  end

  @spec read_other_future_then_write(String.t()) :: String.t()
  def read_other_future_then_write(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    task = Task.async(fn -> Storage.read(node_id, {1, ["def"]}) end)
    Storage.write(node_id, {1, [{["abc"], 123}]})
    :absent = Task.await(task)
    node_id
  end

  @spec write_future_then_write_present(String.t()) :: String.t()
  def write_future_then_write_present(node_id \\ Node.example_random_id()) do
    start_storage(node_id)

    _task1 =
      Task.async(fn -> Storage.write(node_id, {2, [{["abc"], 123}]}) end)

    task2 = Task.async(fn -> Storage.read(node_id, {2, ["abc"]}) end)
    Storage.write(node_id, {1, [{["other"], 999}]})

    {:ok, 123} = Task.await(task2)
    node_id
  end

  @spec write_multiple_then_read(String.t()) :: String.t()
  def write_multiple_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    Storage.write(node_id, {1, [{["abc"], 123}, {["bcd"], 231}]})
    {:ok, 123} = Storage.read(node_id, {1, ["abc"]})
    {:ok, 231} = Storage.read(node_id, {1, ["bcd"]})
    node_id
  end

  @spec write_future_multiple_then_write_present(String.t()) :: String.t()
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
    node_id
  end

  @spec append_then_read(String.t()) :: String.t()
  def append_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    new_set = MapSet.new(["value"])
    Storage.append(node_id, {1, [{["set"], new_set}]})
    {:ok, ^new_set} = Storage.read(node_id, {1, ["set"]})
    node_id
  end

  @spec append_then_read_same(String.t()) :: String.t()
  def append_then_read_same(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    new_set = MapSet.new(["value"])
    Storage.append(node_id, {1, [{["set"], new_set}, {["set"], new_set}]})
    {:ok, ^new_set} = Storage.read(node_id, {1, ["set"]})
    node_id
  end

  @spec append_then_read_several(String.t()) :: String.t()
  def append_then_read_several(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    set1 = MapSet.new(["value1"])
    set2 = MapSet.new(["value2"])
    Storage.append(node_id, {1, [{["set"], set1}, {["set"], set2}]})
    new_set = MapSet.new(["value1", "value2"])
    {:ok, ^new_set} = Storage.read(node_id, {1, ["set"]})
    node_id
  end

  @spec append_twice_then_read(String.t()) :: String.t()
  def append_twice_then_read(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    set1 = MapSet.new(["value1"])
    Storage.append(node_id, {1, [{["set"], set1}]})
    {:ok, ^set1} = Storage.read(node_id, {1, ["set"]})
    set2 = MapSet.new(["value2"])
    Storage.append(node_id, {2, [{["set"], set2}]})
    appended_set = MapSet.new(["value1", "value2"])
    {:ok, ^appended_set} = Storage.read(node_id, {2, ["set"]})
    node_id
  end

  @spec append_twice_then_read_with_commit(String.t()) :: String.t()
  def append_twice_then_read_with_commit(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    set1 = MapSet.new(["value1"])
    Storage.append(node_id, {1, [{["set"], set1}]})
    {:ok, ^set1} = Storage.read(node_id, {1, ["set"]})

    Storage.commit(node_id, 1, nil)

    set2 = MapSet.new(["value2"])
    Storage.append(node_id, {2, [{["set"], set2}]})
    appended_set = MapSet.new(["value1", "value2"])
    {:ok, ^appended_set} = Storage.read(node_id, {2, ["set"]})
    node_id
  end

  @spec add_rewrites(String.t()) :: String.t()
  def add_rewrites(node_id \\ Node.example_random_id()) do
    write_then_read(node_id)
    new_set = MapSet.new(["value1"])

    Storage.add(
      node_id,
      {2, %{write: [{["abc"], 234}], append: [{["set"], new_set}]}}
    )

    {:ok, 234} = Storage.read(node_id, {2, ["abc"]})
    {:ok, ^new_set} = Storage.read(node_id, {2, ["set"]})
    node_id
  end

  @spec add_append(String.t()) :: String.t()
  def add_append(node_id \\ Node.example_random_id()) do
    append_then_read(node_id)
    new_value_set = MapSet.new(["new_value"])

    Storage.add(
      node_id,
      {2, %{write: [{["abc"], 234}], append: [{["set"], new_value_set}]}}
    )

    {:ok, 234} = Storage.read(node_id, {2, ["abc"]})
    new_set = MapSet.new(["new_value", "value"])
    {:ok, ^new_set} = Storage.read(node_id, {2, ["set"]})
    node_id
  end

  @spec complicated_storage(String.t()) :: String.t()
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

    node_id
  end

  @spec complicated_storage_with_commit(String.t()) :: String.t()
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

    node_id
  end

  ############################################################
  #                         Ordering                         #
  ############################################################

  @spec start_ordering(String.t()) :: GenServer.on_start()
  def start_ordering(node_id \\ Node.example_random_id()) do
    Anoma.Node.Transaction.Ordering.start_link(node_id: node_id)
  end

  @spec ord_write_then_read(String.t()) :: String.t()
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
    node_id
  end

  @spec ord_read_future_then_write(String.t()) :: String.t()
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
    node_id
  end

  @spec ord_order_first(String.t()) :: String.t()
  def ord_order_first(node_id \\ Node.example_random_id()) do
    start_storage(node_id)
    start_ordering(node_id)

    Ordering.order(node_id, ["tx id 1", "tx id 2"])

    Ordering.write(node_id, {"tx id 1", [{["abc"], 123}]})
    {:ok, 123} = Ordering.read(node_id, {"tx id 2", ["abc"]})
    node_id
  end

  @spec start_tx_module(String.t()) :: ENode.t() | any()
  def start_tx_module(node_id \\ Node.example_random_id()) do
    %ENode{} = ENode.start_noded(node_config: Config.node(node_id))
  end

  @spec zero(String.t()) :: {Backends.backend(), Noun.t()}
  def zero(key \\ "key") do
    {:debug_term_storage, Examples.ENock.zero(key)}
  end

  @spec inc(String.t()) :: {Backends.backend(), Noun.t()}
  def inc(key \\ "key") do
    {:debug_term_storage, Examples.ENock.inc(key)}
  end

  @spec trivial_transparent_transaction() :: {Backends.backend(), Noun.t()}
  def trivial_transparent_transaction() do
    {:transparent_resource, ENock.transparent_core(ENock.trivial_swap())}
  end

  @spec trivial_transparent_transaction_no_eph() ::
          {Backends.backend(), Noun.t()}
  def trivial_transparent_transaction_no_eph() do
    {:transparent_resource,
     ENock.transparent_core(ENock.trivial_swap_no_eph())}
  end

  ############################################################
  #                        Transactions                      #
  ############################################################

  @spec submit_successful_trivial_swap(String.t()) :: String.t()
  def submit_successful_trivial_swap(node_id \\ Node.example_random_id()) do
    start_tx_module(node_id)

    code = trivial_transparent_transaction()

    EventBroker.subscribe_me([])

    Mempool.tx(node_id, code, "id 1")
    Mempool.execute(node_id, Mempool.tx_dump(node_id))

    recieve_round_event(node_id, 0)

    base_swap = ETransaction.swap_from_actions()

    assert {:ok, base_swap |> Transaction.nullifiers()} ==
             Storage.read(node_id, {1, ["anoma", "nullifiers"]})

    assert {:ok, base_swap |> Transaction.commitments()} ==
             Storage.read(node_id, {1, ["anoma", "commitments"]})

    cms = base_swap |> Transaction.commitments()

    assert {:ok, cms} == Storage.read(node_id, {1, ["anoma", "commitments"]})

    assert {:ok, Backends.value(cms)} ==
             Storage.read(node_id, {1, ["anoma", "anchor"]})

    EventBroker.unsubscribe_me([])

    node_id
  end

  @spec resubmit_trivial_swap(String.t()) :: String.t()
  def resubmit_trivial_swap(node_id \\ Node.example_random_id()) do
    submit_successful_trivial_swap(node_id)

    code = trivial_transparent_transaction()

    EventBroker.subscribe_me([])

    log =
      capture_log(fn ->
        Mempool.tx(node_id, code, "id 2")
        Mempool.execute(node_id, Mempool.tx_dump(node_id))
        recieve_logger_failure(node_id, "nullifier already")
      end)

    # Occasionally, the log might be empty because `Anoma.Node.Logging`
    # hasn't finished writing the log entries yet.
    assert log =~ "nullifier already" || log == ""

    EventBroker.unsubscribe_me([])

    node_id
  end

  @spec submit_failed_trivial_swap(String.t()) :: String.t()
  def submit_failed_trivial_swap(node_id \\ Node.example_random_id()) do
    start_tx_module(node_id)
    code = trivial_transparent_transaction_no_eph()
    EventBroker.subscribe_me([])

    log =
      capture_log(fn ->
        Mempool.tx(node_id, code, "id 1")
        Mempool.execute(node_id, Mempool.tx_dump(node_id))
        recieve_logger_failure(node_id, "not committed")
      end)

    # Occasionally, the log might be empty because `Anoma.Node.Logging`
    # hasn't finished writing the log entries yet.
    assert log =~ "not committed" || log == ""

    node_id
  end

  @spec zero_counter_submit(String.t()) :: String.t()
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

    {:atomic, block} =
      :mnesia.transaction(fn ->
        :mnesia.read({Storage.blocks_table(node_id), 0})
      end)

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
    ] = block

    node_id
  end

  @spec inc_counter_submit_with_zero(String.t()) :: String.t()
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

    {:atomic, block} =
      :mnesia.transaction(fn -> :mnesia.read({blocks_table, 0}) end)

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
    ] = block

    node_id
  end

  @spec inc_counter_submit_after_zero(String.t()) :: String.t()
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

    {:atomic, block} =
      :mnesia.transaction(fn -> :mnesia.read({blocks_table, 1}) end)

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
    ] = block

    node_id
  end

  @spec inc_counter_submit_after_read(String.t()) :: String.t()
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

    {:atomic, block} =
      :mnesia.transaction(fn -> :mnesia.read({blocks_table, 1}) end)

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
    ] = block

    node_id
  end

  @spec bluf() :: Noun.t()
  def bluf() do
    [0 | 0]
  end

  @spec bluf_transaction_errors(String.t()) :: String.t()
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

    {:atomic, block} =
      :mnesia.transaction(fn -> :mnesia.read({blocks_table, 0}) end)

    [
      {^blocks_table, 0,
       [
         %Mempool.Tx{
           code: [0 | 0],
           backend: :debug_term_storage,
           vm_result: :vm_error,
           tx_result: :error
         }
       ]}
    ] = block

    node_id
  end

  @spec read_txs_write_nothing(String.t()) :: String.t()
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
    node_id
  end

  @spec bluff_txs_write_nothing(String.t()) :: String.t()
  def bluff_txs_write_nothing(node_id \\ Node.example_random_id()) do
    bluf_transaction_errors(node_id)

    [] = :mnesia.dirty_all_keys(Storage.values_table(node_id))
    [] = :mnesia.dirty_all_keys(Storage.updates_table(node_id))
    node_id
  end

  @spec recieve_round_event(String.t(), non_neg_integer()) :: :ok | :error_tx
  def recieve_round_event(node_id, round) do
    receive do
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.BlockEvent{round: ^round}
        }
      } ->
        :ok
    after
      1000 -> :error_tx
    end
  end

  @spec recieve_logger_failure(binary(), String.t()) :: any()
  defp recieve_logger_failure(node_id, exp_message) do
    receive do
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.LoggingEvent{flag: :error, msg: msg}
        }
      } ->
        assert msg =~ exp_message
    after
      1000 -> assert(false, "Failed to find failure message: #{exp_message}")
    end
  end
end
