defmodule Anoma.Node.Examples.ELogging do
  alias Anoma.Node.Logging
  alias Anoma.Node.Transaction.{Mempool, Storage}
  alias Anoma.Node.Examples.ENode
  alias Anoma.Crypto.Id

  require EventBroker.Event

  require ExUnit.Assertions
  import ExUnit.Assertions

  def check_tx_event(node_id \\ Id.new_keypair()) do
    ENode.start_node(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    tx_event("id 1", "back 1", "code 1")

    assert_receive(
      {:mnesia_table_event, {:write, {_, "id 1", {"back 1", "code 1"}}, _}},
      5000
    )

    assert {:atomic, [{^table_name, "id 1", {"back 1", "code 1"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 1")
             end)

    :mnesia.unsubscribe({:table, table_name, :simple})
  end

  def check_multiple_tx_events(node_id \\ Id.new_keypair()) do
    ENode.start_node(node_id)

    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    tx_event("id 1", "back 1", "code 1")
    tx_event("id 2", "back 2", "code 2")

    assert_receive(
      {:mnesia_table_event,
       {:write, {^table_name, "id 1", {"back 1", "code 1"}}, _}},
      5000
    )

    assert_receive(
      {:mnesia_table_event,
       {:write, {^table_name, "id 2", {"back 2", "code 2"}}, _}},
      5000
    )

    assert {:atomic, [{^table_name, "id 1", {"back 1", "code 1"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 1")
             end)

    assert {:atomic, [{^table_name, "id 2", {"back 2", "code 2"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 2")
             end)

    :mnesia.unsubscribe({:table, table_name, :simple})
  end

  def check_consensus_event(node_id \\ Id.new_keypair()) do
    check_tx_event(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    consensus_event(["id 1"])

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
  end

  def check_consensus_event_multiple(node_id \\ Id.new_keypair()) do
    check_multiple_tx_events(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    consensus_event(["id 1"])
    consensus_event(["id 2"])

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
  end

  def check_block_event(node_id \\ Id.new_keypair()) do
    check_consensus_event(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    block_event(["id 1"], 0)

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
  end

  def check_block_event_multiple(node_id \\ Id.new_keypair()) do
    check_consensus_event_multiple(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})
    block_event(["id 1"], 0)

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

    block_event(["id 2"], 0)

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
  end

  def check_block_event_leave_one_out(node_id \\ Id.new_keypair()) do
    check_consensus_event_multiple(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})
    block_event(["id 1"], 0)

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

    assert {:atomic, [{^table_name, "id 2", {"back 2", "code 2"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(table_name, "id 2")
             end)
  end

  def replay_consensus(node_id \\ Id.new_keypair()) do
    write_consensus(node_id)
    replay_ensure_created_tables(node_id)

    EventBroker.subscribe_me([])

    {:ok, pid} = Logging.restart_with_replay(node_id)

    :ok =
      receive do
        %EventBroker.Event{
          body: %Mempool.TxEvent{
            id: "id 1",
            tx: %Mempool.Tx{backend: _, code: "code 1"}
          }
        } ->
          :ok
      after
        5000 -> :error_tx
      end

    :ok =
      receive do
        %EventBroker.Event{
          body: %Mempool.ConsensusEvent{
            order: ["id 1"]
          }
        } ->
          :ok
      after
        5000 -> :error_consensus
      end

    Supervisor.stop(pid)
  end

  def replay_several_txs(node_id \\ Id.new_keypair()) do
    write_several_tx(node_id)
    replay_ensure_created_tables(node_id)

    EventBroker.subscribe_me([])

    {:ok, pid} = Logging.restart_with_replay(node_id)

    :ok =
      receive do
        %EventBroker.Event{
          body: %Mempool.TxEvent{
            id: "id 1",
            tx: %Mempool.Tx{backend: _, code: "code 1"}
          }
        } ->
          :ok
      after
        5000 -> nil
      end

    :ok =
      receive do
        %EventBroker.Event{
          body: %Mempool.TxEvent{
            id: "id 2",
            tx: %Mempool.Tx{backend: _, code: "code 2"}
          }
        } ->
          :ok
      after
        5000 -> nil
      end

    Supervisor.stop(pid)
  end

  def replay_tx(node_id \\ Id.new_keypair()) do
    write_tx(node_id)
    replay_ensure_created_tables(node_id)

    EventBroker.subscribe_me([])

    {:ok, pid} = Logging.restart_with_replay(node_id)

    :ok =
      receive do
        %EventBroker.Event{
          body: %Mempool.TxEvent{
            id: "id 1",
            tx: %Mempool.Tx{backend: _, code: "code 1"}
          }
        } ->
          :ok
      after
        5000 -> nil
      end

    Supervisor.stop(pid)
  end

  defp write_consensus_leave_one_out(node_id) do
    table = write_several_tx(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, [["id 1"]]})
    end)

    table
  end

  defp write_several_consensus(node_id) do
    table = write_several_tx(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, [["id 1"], ["id 2"]]})
    end)

    table
  end

  defp write_consensus_with_several_tx(node_id) do
    table = write_several_tx(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, [["id 1", "id 2"]]})
    end)

    table
  end

  def write_consensus(node_id) do
    table = write_tx(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, [["id 1"]]})
    end)

    table
  end

  defp write_several_tx(node_id) do
    table = create_event_table(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, "id 1", {:debug_bloblike, "code 1"}})
      :mnesia.write({table, "id 2", {:debug_bloblike, "code 2"}})
    end)

    table
  end

  defp write_tx(node_id) do
    table = create_event_table(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, "id 1", {:debug_bloblike, "code 1"}})
    end)

    table
  end

  defp create_event_table(node_id \\ Id.new_keypair()) do
    table = Logging.table_name(node_id)
    :mnesia.create_table(table, attributes: [:type, :body])

    :mnesia.transaction(fn ->
      :mnesia.write({table, :round, -1})
    end)

    table
  end

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

  def tx_event(id, backend, code) do
    event =
      EventBroker.Event.new_with_body(%Mempool.TxEvent{
        id: id,
        tx: %Mempool.Tx{backend: backend, code: code}
      })

    EventBroker.event(event)
  end

  def consensus_event(order) do
    event =
      EventBroker.Event.new_with_body(%Mempool.ConsensusEvent{
        order: order
      })

    EventBroker.event(event)
  end

  def block_event(order, round) do
    event =
      EventBroker.Event.new_with_body(%Mempool.BlockEvent{
        order: order,
        round: round
      })

    EventBroker.event(event)
  end
end
