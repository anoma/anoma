defmodule Anoma.Node.Examples.EEvent do
  @moduledoc """
  I contain logic to send node events and wait for node events.

  These events are not generic events such as apps/event_broker/lib/examples/e_event_broker.ex.
  Rather, these are specific node events.
  """

  alias Anoma.Node.Examples.ENode

  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Events
  alias Anoma.Node.Event
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Transaction.Backends

  import ExUnit.Assertions

  require Anoma.Node.Event

  ############################################################
  #                       Events                             #
  ############################################################
  # @doc """
  # I create a transaction event
  # """
  @spec transaction_event(ENode.t(), ETransaction.t()) ::
          EventBroker.Event.t()
  def transaction_event(enode \\ ENode.start_node()) do
    transaction = ETransaction.simple_transaction()
    transaction_event(enode, transaction)
  end

  def transaction_event(enode, transaction) do
    # create a transaction event
    event =
      new_tx_event({transaction.backend, transaction.noun}, transaction.id)

    Event.new_with_body(enode.node_id, event)
  end

  @doc """
  I create a consensus event for the given transaction ids.
  """
  @spec consensus_event(ENode.t(), [String.t()]) :: EventBroker.Event.t()
  def consensus_event(enode \\ ENode.start_node(), transaction_ids \\ []) do
    # create a transaction event
    event = new_consensus_event(transaction_ids)

    Event.new_with_body(enode.node_id, event)
  end

  @doc """
  I create an order event for the given transaction id.
  """
  @spec order_event(ENode.t(), String.t() | nil) :: EventBroker.Event.t()
  def order_event(enode \\ ENode.start_node(), transaction_id \\ nil) do
    transaction_id =
      if transaction_id do
        transaction_id
      else
        ETransaction.random_transaction_id()
      end

    # create a transaction event
    event = new_order_event(transaction_id)

    Event.new_with_body(enode.node_id, event)
  end

  @doc """
  I create an execution event for the given transaction id and the given result.
  The transaction should be a tuple with an id and an expected result.
  E.g., {{:ok, [["key" | 0]]}, "id 1"}
        {[error: "id 1"], "id 1"}
  """
  @spec execution_event(ENode.t()) :: EventBroker.Event.t()
  @spec execution_event(ENode.t(), ETransaction.t()) :: EventBroker.Event.t()
  def execution_event(enode \\ ENode.start_node()) do
    transaction = ETransaction.faulty_transaction()
    execution_event(enode, transaction)
  end

  def execution_event(enode, transaction) do
    # create a transaction event
    event = new_execution_event([{transaction.result, transaction.id}])

    Event.new_with_body(enode.node_id, event)
  end

  @doc """
  I create a block event for the given transaction id and the given result.
  The transaction should be a tuple with an id and an expected result.
  """
  @spec block_event(ENode.t()) :: EventBroker.Event.t()
  @spec block_event(ENode.t(), ETransaction.t(), non_neg_integer()) ::
          EventBroker.Event.t()
  def block_event(enode \\ ENode.start_node()) do
    transaction = ETransaction.faulty_transaction()
    order = 0
    block_event(enode, transaction, order)
  end

  def block_event(enode, transaction, order) do
    # create a transaction event
    event = new_block_event([transaction.id], order)

    Event.new_with_body(enode.node_id, event)
  end

  ############################################################
  #                       Send Events                        #
  ############################################################

  @doc """
  I send the given transaction event.
  If no event was given, I send a default event.
  """
  @spec send_transaction_event(ENode.t(), EventBroker.Event.t() | nil) ::
          {ENode.t(), EventBroker.Event.t()}
  def send_transaction_event(enode \\ ENode.start_node(), event \\ nil) do
    # if no event was given, create a default event
    event = if event, do: event, else: transaction_event(enode)

    # send the event
    EventBroker.event(event)

    {enode, event}
  end

  @doc """
  I send the consensus event.
  If no event was given, I send a default event.
  """
  @spec send_consensus_event(ENode.t(), EventBroker.Event.t() | nil) ::
          {ENode.t(), EventBroker.Event.t()}
  def send_consensus_event(enode \\ ENode.start_node(), event \\ nil) do
    # if no event was given, create a default event
    event = if event, do: event, else: consensus_event(enode)

    # send the event
    EventBroker.event(event)

    {enode, event}
  end

  @doc """
  I send the order event.
  If no event was given, I send a default event.
  """
  @spec send_order_event(ENode.t(), EventBroker.Event.t() | nil) ::
          {ENode.t(), EventBroker.Event.t()}
  def send_order_event(enode \\ ENode.start_node(), event \\ nil) do
    # if no event was given, create a default event
    event = if event, do: event, else: order_event(enode)

    # send the event
    EventBroker.event(event)

    {enode, event}
  end

  @doc """
  I send the execution event.
  If no event was given, I send a default event.
  """
  @spec send_execution_event(ENode.t(), EventBroker.Event.t() | nil) ::
          {ENode.t(), EventBroker.Event.t()}
  def send_execution_event(enode \\ ENode.start_node(), event \\ nil) do
    # if no event was given, create a default event
    event = if event, do: event, else: execution_event(enode)

    # send the event
    EventBroker.event(event)

    {enode, event}
  end

  @doc """
  I send the block event.
  If no event was given, I send a default event.
  """
  @spec send_block_event(ENode.t(), EventBroker.Event.t() | nil) ::
          {ENode.t(), EventBroker.Event.t()}
  def send_block_event(enode \\ ENode.start_node(), event \\ nil) do
    # if no event was given, create a default event
    event = if event, do: event, else: block_event(enode)

    # send the event
    EventBroker.event(event)

    {enode, event}
  end

  ############################################################
  #                       Wait For Events                    #
  ############################################################

  @doc """
  I wait for a specific event.
  """
  @spec wait_for_event(EventBroker.Event.t()) :: :ok
  def wait_for_event(event) do
    # the event will be fired from another module,
    # so the source_mdoule attribute has to be ignored.
    expected_body = event.body

    assert_receive %EventBroker.Event{
                     body: ^expected_body,
                     source_module: _
                   },
                   5000

    :ok
  end

  ############################################################
  #                       Helpers                            #
  ############################################################

  @doc """
  Given a transaction and an id, I create a transaction event.
  """
  @spec new_tx_event({Backends.backend(), Noun.t()}, String.t()) ::
          Events.TxEvent.t()
  def new_tx_event({backend, noun}, id) do
    %Events.TxEvent{
      id: id,
      tx: %Mempool.Tx{backend: backend, code: noun}
    }
  end

  @doc """
  I create a new consensus event.
  """
  @spec new_consensus_event([String.t()]) :: Events.ConsensusEvent.t()
  def new_consensus_event(transaction_ids) do
    %Events.ConsensusEvent{
      order: transaction_ids
    }
  end

  @doc """
  I create a new order event.
  """
  @spec new_order_event(String.t()) :: Events.OrderEvent.t()
  def new_order_event(transaction_id) do
    %Events.OrderEvent{
      tx_id: transaction_id
    }
  end

  @doc """
  I create a new execution event.
  I expect a list of tuples that contain a transaction id and a result of the execution
  eof that transaction.

  E.g., {"id 1", {:ok, [["key" | 0]]}}
        {"id 1", [error: "id 1"]}
  """
  @spec new_execution_event([{any(), String.t()}]) ::
          Events.ExecutionEvent.t()
  def new_execution_event(results) do
    %Events.ExecutionEvent{
      result: results
    }
  end

  @doc """
  I create a new block event.
  For this I need the round of the block as well as the order of the transactions in that block.
  """
  @spec new_block_event([String.t()], non_neg_integer()) ::
          Events.BlockEvent.t()
  def new_block_event(transaction_ids, round) do
    %Events.BlockEvent{
      order: transaction_ids,
      round: round
    }
  end
end
