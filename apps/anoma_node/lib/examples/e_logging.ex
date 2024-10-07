defmodule Anoma.Node.Examples.ELogging do
  alias Anoma.Node.Logging
  alias Anoma.Node.Transaction.Mempool

  require EventBroker.Event

  require ExUnit.Assertions
  import ExUnit.Assertions

  def restart_logging do
    if GenServer.whereis(Logging) do
      GenServer.stop(Logging)
    end

    :mnesia.clear_table(Anoma.Node.Logging.Events)

    Logging.start_link()
  end

  def check_tx_event() do
    restart_logging()

    :mnesia.subscribe({:table, Logging.Events, :simple})

    tx_event("id 1", "back 1", "code 1")

    assert_receive(
      {:mnesia_table_event,
       {:write, {Anoma.Node.Logging.Events, "id 1", {"back 1", "code 1"}}, _}},
      5000
    )

    assert {:atomic,
            [{Anoma.Node.Logging.Events, "id 1", {"back 1", "code 1"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, "id 1")
             end)

    :mnesia.unsubscribe({:table, Logging.Events, :simple})
  end

  def check_multiple_tx_events() do
    restart_logging()

    :mnesia.subscribe({:table, Logging.Events, :simple})

    tx_event("id 1", "back 1", "code 1")
    tx_event("id 2", "back 2", "code 2")

    assert_receive(
      {:mnesia_table_event,
       {:write, {Anoma.Node.Logging.Events, "id 1", {"back 1", "code 1"}}, _}},
      5000
    )

    assert_receive(
      {:mnesia_table_event,
       {:write, {Anoma.Node.Logging.Events, "id 2", {"back 2", "code 2"}}, _}},
      5000
    )

    assert {:atomic,
            [{Anoma.Node.Logging.Events, "id 1", {"back 1", "code 1"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, "id 1")
             end)

    assert {:atomic,
            [{Anoma.Node.Logging.Events, "id 2", {"back 2", "code 2"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, "id 2")
             end)

    :mnesia.unsubscribe({:table, Logging.Events, :simple})
  end

  def check_consensus_event() do
    check_tx_event()

    :mnesia.subscribe({:table, Logging.Events, :simple})

    consensus_event(["id 1"])

    assert_receive(
      {:mnesia_table_event,
       {:write, {Anoma.Node.Logging.Events, :consensus, [["id 1"]]}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Logging.Events, :simple})

    assert {:atomic, [{Anoma.Node.Logging.Events, :consensus, [["id 1"]]}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, :consensus)
             end)
  end

  def check_consensus_event_multiple() do
    check_multiple_tx_events()

    :mnesia.subscribe({:table, Logging.Events, :simple})

    consensus_event(["id 1"])
    consensus_event(["id 2"])

    assert_receive(
      {:mnesia_table_event,
       {:write, {Anoma.Node.Logging.Events, :consensus, [["id 1"], ["id 2"]]},
        _}},
      5000
    )

    :mnesia.unsubscribe({:table, Logging.Events, :simple})

    assert {:atomic,
            [{Anoma.Node.Logging.Events, :consensus, [["id 1"], ["id 2"]]}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, :consensus)
             end)
  end

  def check_block_event() do
    check_consensus_event()

    :mnesia.subscribe({:table, Logging.Events, :simple})
    block_event(["id 1"], 0)

    assert_receive(
      {:mnesia_table_event,
       {:delete, {Anoma.Node.Logging.Events, "id 1"}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Logging.Events, :simple})

    assert {:atomic, [{Anoma.Node.Logging.Events, :consensus, []}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, :consensus)
             end)

    assert {:atomic, []} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, "id 1")
             end)
  end

  def check_block_event_multiple() do
    check_consensus_event_multiple()

    :mnesia.subscribe({:table, Logging.Events, :simple})
    block_event(["id 1"], 0)

    assert_receive(
      {:mnesia_table_event,
       {:delete, {Anoma.Node.Logging.Events, "id 1"}, _}},
      5000
    )

    assert {:atomic, [{Anoma.Node.Logging.Events, :consensus, [["id 2"]]}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, :consensus)
             end)

    assert {:atomic, []} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, "id 1")
             end)

    block_event(["id 2"], 0)

    assert_receive(
      {:mnesia_table_event,
       {:delete, {Anoma.Node.Logging.Events, "id 2"}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Logging.Events, :simple})

    assert {:atomic, [{Anoma.Node.Logging.Events, :consensus, []}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, :consensus)
             end)

    assert {:atomic, []} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, "id 2")
             end)
  end

  def check_block_event_leave_one_out() do
    check_consensus_event_multiple()

    :mnesia.subscribe({:table, Logging.Events, :simple})
    block_event(["id 1"], 0)

    assert_receive(
      {:mnesia_table_event,
       {:delete, {Anoma.Node.Logging.Events, "id 1"}, _}},
      5000
    )

    :mnesia.unsubscribe({:table, Logging.Events, :simple})

    assert {:atomic, [{Anoma.Node.Logging.Events, :consensus, [["id 2"]]}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, :consensus)
             end)

    assert {:atomic, []} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, "id 1")
             end)

    assert {:atomic,
            [{Anoma.Node.Logging.Events, "id 2", {"back 2", "code 2"}}]} =
             :mnesia.transaction(fn ->
               :mnesia.read(Logging.Events, "id 2")
             end)
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
