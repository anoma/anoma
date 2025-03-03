defmodule Anoma.Node.Examples.ELogging do
  alias Anoma.Node
  alias Node.Logging
  alias Node.Examples.ENode
  alias Anoma.Node.Tables
  alias Anoma.Node.Events
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Storage

  require ExUnit.Assertions
  require Node.Event

  import ExUnit.Assertions

  use EventBroker.WithSubscription

  @spec check_tx_event(String.t()) :: String.t()
  def check_tx_event(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    table_name = Tables.table_events(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    Events.transaction_event(
      {:transparent_resource, "code 1"},
      "id 1",
      node_id
    )

    assert_receive(
      {:mnesia_table_event,
       {:write, {_, "id 1", {:transparent_resource, "code 1"}}, _}},
      5000
    )

    assert {:atomic,
            [{^table_name, "id 1", {:transparent_resource, "code 1"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 1")
             end)

    :mnesia.unsubscribe({:table, table_name, :simple})
    node_id
  end

  @spec check_multiple_tx_events(String.t()) :: String.t()
  def check_multiple_tx_events(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)

    table_name = Tables.table_events(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    Events.transaction_event(
      {:transparent_resource, "code 1"},
      "id 1",
      node_id
    )

    Events.transaction_event(
      {:transparent_resource, "code 2"},
      "id 2",
      node_id
    )

    assert_receive(
      {:mnesia_table_event,
       {:write, {^table_name, "id 1", {:transparent_resource, "code 1"}}, _}},
      5000
    )

    assert_receive(
      {:mnesia_table_event,
       {:write, {^table_name, "id 2", {:transparent_resource, "code 2"}}, _}},
      5000
    )

    assert {:atomic,
            [{^table_name, "id 1", {:transparent_resource, "code 1"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 1")
             end)

    assert {:atomic,
            [{^table_name, "id 2", {:transparent_resource, "code 2"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 2")
             end)

    :mnesia.unsubscribe({:table, table_name, :simple})
    node_id
  end

  ############################################################
  #                      Consensus event                     #
  ############################################################

  @spec check_consensus_event(String.t()) :: String.t()
  def check_consensus_event(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_tx_event(node_id)
    table_name = Tables.table_events(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    Events.consensus_event(["id 1"], node_id)

    assert_receive(
      {:mnesia_table_event,
       {:write, {^table_name, :consensus, [["id 1"]]}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, table_name, :simple})

    assert {:atomic, [{^table_name, :consensus, [["id 1"]]}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, :consensus)
             end)

    node_id
  end

  @spec check_consensus_event_multiple(String.t()) :: String.t()
  def check_consensus_event_multiple(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_multiple_tx_events(node_id)
    table_name = Tables.table_events(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    Events.consensus_event(["id 1"], node_id)
    Events.consensus_event(["id 2"], node_id)

    assert_receive(
      {:mnesia_table_event,
       {:write, {^table_name, :consensus, [["id 1"], ["id 2"]]}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, table_name, :simple})

    assert {:atomic, [{^table_name, :consensus, [["id 1"], ["id 2"]]}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, :consensus)
             end)

    node_id
  end

  ############################################################
  #                         Block event                      #
  ############################################################

  @spec check_block_event(String.t()) :: String.t()
  def check_block_event(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_consensus_event(node_id)
    table_name = Tables.table_events(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    Events.block_event(["id 1"], 0, node_id)

    assert_receive(
      {:mnesia_table_event, {:delete, {^table_name, "id 1"}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, table_name, :simple})

    assert {:atomic, [{^table_name, :consensus, []}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, :consensus)
             end)

    assert {:atomic, []} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 1")
             end)

    node_id
  end

  @spec check_block_event_multiple(String.t()) :: String.t()
  def check_block_event_multiple(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_consensus_event_multiple(node_id)
    table_name = Tables.table_events(node_id)

    :mnesia.subscribe({:table, table_name, :simple})
    Events.block_event(["id 1"], 0, node_id)

    assert_receive(
      {:mnesia_table_event, {:delete, {^table_name, "id 1"}, _}},
      5000
    )

    assert {:atomic, [{^table_name, :consensus, [["id 2"]]}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, :consensus)
             end)

    assert {:atomic, []} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 1")
             end)

    Events.block_event(["id 2"], 0, node_id)

    assert_receive(
      {:mnesia_table_event, {:delete, {^table_name, "id 2"}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, table_name, :simple})

    assert {:atomic, [{^table_name, :consensus, []}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, :consensus)
             end)

    assert {:atomic, []} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 2")
             end)

    node_id
  end

  @spec check_block_event_leave_one_out(String.t()) :: String.t()
  def check_block_event_leave_one_out(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_consensus_event_multiple(node_id)
    table_name = Tables.table_events(node_id)

    :mnesia.subscribe({:table, table_name, :simple})
    Events.block_event(["id 1"], 0, node_id)

    assert_receive(
      {:mnesia_table_event, {:delete, {^table_name, "id 1"}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, table_name, :simple})

    assert {:atomic, [{^table_name, :consensus, [["id 2"]]}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, :consensus)
             end)

    assert {:atomic, []} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 1")
             end)

    assert {:atomic,
            [{^table_name, "id 2", {:transparent_resource, "code 2"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 2")
             end)

    node_id
  end

  @spec replay_corrects_result(String.t()) :: String.t()
  def replay_corrects_result(node_id \\ Node.example_random_id()) do
    replay_ensure_created_tables(node_id)
    table = Storage.blocks_table(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write(
        {table, 0, [%Mempool.Tx{backend: :debug_bloblike, code: "code 1"}]}
      )
    end)

    write_consensus_leave_one_out(node_id)
    filter = [%Mempool.TxFilter{}]

    with_subscription [filter] do
      Logging.restart_with_replay(node_id)

      :ok =
        wait_for_tx(node_id, "id 2", "code 2")

      :error_tx =
        wait_for_tx(node_id, "id 1", "code 1")
    end

    state = Anoma.Node.Registry.whereis(node_id, Mempool) |> :sys.get_state()
    nil = Map.get(state.transactions, "id 1")
    1 = state.round

    node_id
  end

  @spec replay_consensus_leave_one_out(String.t()) :: String.t()
  def replay_consensus_leave_one_out(node_id \\ Node.example_random_id()) do
    write_consensus_leave_one_out(node_id)
    replay_ensure_created_tables(node_id)

    filter = [%Mempool.TxFilter{}]

    with_subscription [filter] do
      Logging.restart_with_replay(node_id)

      :ok =
        wait_for_tx(node_id, "id 1", "code 1")

      :ok =
        wait_for_tx(node_id, "id 2", "code 2")

      :ok =
        wait_for_consensus(node_id, ["id 1"])

      Mempool.execute(node_id, ["id 2"])

      :ok =
        wait_for_consensus(node_id, ["id 2"])

      node_id
    end
  end

  @spec replay_several_consensus(String.t()) :: String.t()
  def replay_several_consensus(node_id \\ Node.example_random_id()) do
    write_several_consensus(node_id)
    replay_ensure_created_tables(node_id)

    txfilter = [%Mempool.TxFilter{}]
    consensus_filter = [%Mempool.ConsensusFilter{}]

    with_subscription [txfilter, consensus_filter] do
      Logging.restart_with_replay(node_id)

      :ok =
        wait_for_tx(node_id, "id 1", "code 1")

      :ok =
        wait_for_tx(node_id, "id 2", "code 2")

      :ok =
        wait_for_consensus(node_id, ["id 1"])

      :ok =
        wait_for_consensus(node_id, ["id 2"])

      node_id
    end
  end

  @spec replay_consensus_with_several_txs(String.t()) :: String.t()
  def replay_consensus_with_several_txs(node_id \\ Node.example_random_id()) do
    write_consensus_with_several_tx(node_id)
    replay_ensure_created_tables(node_id)

    txfilter = [%Mempool.TxFilter{}]
    consensus_filter = [%Mempool.ConsensusFilter{}]

    with_subscription [txfilter, consensus_filter] do
      Logging.restart_with_replay(node_id)

      :ok =
        wait_for_tx(node_id, "id 1", "code 1")

      :ok =
        wait_for_tx(node_id, "id 2", "code 2")

      :ok =
        wait_for_consensus(node_id, ["id 1", "id 2"])

      node_id
    end
  end

  @spec replay_consensus(String.t()) :: String.t()
  def replay_consensus(node_id \\ Node.example_random_id()) do
    write_consensus(node_id)
    replay_ensure_created_tables(node_id)

    txfilter = [%Mempool.TxFilter{}]
    consensus_filter = [%Mempool.ConsensusFilter{}]

    with_subscription [txfilter, consensus_filter] do
      Logging.restart_with_replay(node_id)

      :ok =
        wait_for_tx(node_id, "id 1", "code 1")

      :ok =
        wait_for_consensus(node_id, ["id 1"])

      node_id
    end
  end

  @spec replay_several_txs(String.t()) :: String.t()
  def replay_several_txs(node_id \\ Node.example_random_id()) do
    write_several_tx(node_id)
    replay_ensure_created_tables(node_id)

    txfilter = [%Mempool.TxFilter{}]

    with_subscription [txfilter] do
      Logging.restart_with_replay(node_id)

      :ok =
        wait_for_tx(node_id, "id 1", "code 1")

      :ok =
        wait_for_tx(node_id, "id 2", "code 2")

      node_id
    end
  end

  @spec replay_tx(String.t()) :: String.t()
  def replay_tx(node_id \\ Node.example_random_id()) do
    write_tx(node_id)
    replay_ensure_created_tables(node_id)

    txfilter = [%Mempool.TxFilter{}]

    with_subscription [txfilter] do
      {:ok, _pid} = Logging.restart_with_replay(node_id)

      :ok =
        wait_for_tx(node_id, "id 1", "code 1")

      node_id
    end
  end

  @spec write_consensus_leave_one_out(String.t()) :: atom()
  defp write_consensus_leave_one_out(node_id) do
    table = write_several_tx(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, [["id 1"]]})
    end)

    table
  end

  @spec write_several_consensus(String.t()) :: atom()
  defp write_several_consensus(node_id) do
    table = write_several_tx(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, [["id 1"], ["id 2"]]})
    end)

    table
  end

  @spec write_consensus_with_several_tx(String.t()) :: atom()
  defp write_consensus_with_several_tx(node_id) do
    table = write_several_tx(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, [["id 1", "id 2"]]})
    end)

    table
  end

  @spec write_consensus(String.t()) :: atom()
  def write_consensus(node_id) do
    table = write_tx(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, [["id 1"]]})
    end)

    table
  end

  @spec write_several_tx(String.t()) :: atom()
  defp write_several_tx(node_id) do
    table = create_event_table(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, "id 1", {:debug_bloblike, "code 1"}})
      :mnesia.write({table, "id 2", {:debug_bloblike, "code 2"}})
    end)

    table
  end

  @spec write_tx(String.t()) :: atom()
  defp write_tx(node_id) do
    table = create_event_table(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, "id 1", {:debug_bloblike, "code 1"}})
    end)

    table
  end

  @spec wait_for_consensus(String.t(), list(binary())) ::
          :ok | :error_consensus
  defp wait_for_consensus(node_id, consensus) do
    receive do
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.ConsensusEvent{
            order: ^consensus
          }
        }
      } ->
        :ok
    after
      1000 -> :error_consensus
    end
  end

  @spec wait_for_tx(String.t(), binary(), Noun.t()) :: :ok | :error_tx
  defp wait_for_tx(node_id, id, code) do
    receive do
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.TxEvent{
            id: ^id,
            tx: %Mempool.Tx{backend: _, code: ^code}
          }
        }
      } ->
        :ok
    after
      1000 -> :error_tx
    end
  end

  @spec create_event_table(String.t()) :: atom()
  defp create_event_table(node_id) do
    table = Tables.table_events(node_id)
    :mnesia.create_table(table, attributes: [:type, :body])

    :mnesia.transaction(fn ->
      :mnesia.write({table, :round, -1})
    end)

    table
  end

  @spec replay_ensure_created_tables(String.t()) :: [{atom(), atom()}]
  defp replay_ensure_created_tables(node_id) do
    block_table = Storage.blocks_table(node_id)
    values_table = Storage.values_table(node_id)
    updates_table = Storage.updates_table(node_id)

    :mnesia.create_table(values_table, attributes: [:key, :value])
    :mnesia.create_table(updates_table, attributes: [:key, :value])
    :mnesia.create_table(block_table, attributes: [:round, :block])

    [
      block_table: block_table,
      values_table: values_table,
      updates_table: updates_table
    ]
  end
end
