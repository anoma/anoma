defmodule Anoma.Node.Examples.ELogging do
  alias Anoma.Node
  alias Node.Logging
  alias Node.Transaction.Mempool
  alias Node.Examples.ENode

  require Node.Event

  require ExUnit.Assertions
  import ExUnit.Assertions

  def check_tx_event(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    tx_event("id 1", "back 1", "code 1", node_id)

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

  def check_multiple_tx_events(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)

    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    tx_event("id 1", "back 1", "code 1", node_id)
    tx_event("id 2", "back 2", "code 2", node_id)

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

  def check_consensus_event(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_tx_event(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    consensus_event(["id 1"], node_id)

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

  def check_consensus_event_multiple(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_multiple_tx_events(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    consensus_event(["id 1"], node_id)
    consensus_event(["id 2"], node_id)

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

  def check_block_event(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_consensus_event(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})

    block_event(["id 1"], 0, node_id)

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

  def check_block_event_multiple(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_consensus_event_multiple(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})
    block_event(["id 1"], 0, node_id)

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

    block_event(["id 2"], 0, node_id)

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

  def check_block_event_leave_one_out(
        node_id \\ Node.example_random_id()
        |> Base.url_encode64()
      ) do
    check_consensus_event_multiple(node_id)
    table_name = Logging.table_name(node_id)

    :mnesia.subscribe({:table, table_name, :simple})
    block_event(["id 1"], 0, node_id)

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

  def tx_event(id, backend, code, node_id) do
    event =
      Node.Event.new_with_body(node_id, %Mempool.TxEvent{
        id: id,
        tx: %Mempool.Tx{backend: backend, code: code}
      })

    EventBroker.event(event)
  end

  def consensus_event(order, node_id) do
    event =
      Node.Event.new_with_body(node_id, %Mempool.ConsensusEvent{
        order: order
      })

    EventBroker.event(event)
  end

  def block_event(order, round, node_id) do
    event =
      Node.Event.new_with_body(node_id, %Mempool.BlockEvent{
        order: order,
        round: round
      })

    EventBroker.event(event)
  end
end
