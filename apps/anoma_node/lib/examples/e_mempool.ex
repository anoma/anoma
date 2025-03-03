defmodule Anoma.Node.Examples.Mempool do
  @moduledoc """
  I contain examples on how to interact with the mempool.
  """

  alias Anoma.Node.Examples.EEvent
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Tables
  alias Anoma.Node.Transaction.Mempool

  import ExUnit.Assertions

  use EventBroker.WithSubscription

  # -----------------------------------------------------------
  # Adding transactions

  @doc """
  I add a transaction to the mempool.
  """
  @spec add_transaction(ENode.t()) ::
          {ENode.t(), ETransaction.t()}
  @spec add_transaction(ENode.t(), ETransaction.t()) ::
          {ENode.t(), ETransaction.t()}
  # default arguments
  def add_transaction(enode \\ ENode.start_node()) do
    transaction = ETransaction.simple_transaction()
    add_transaction(enode, transaction)
  end

  def add_transaction(enode, transaction) do
    # subscribe to mnesia events to capture when the transaction
    # is written to the mempool.
    events_table = Tables.table_events(enode.node_id)
    :mnesia.subscribe({:table, events_table, :simple})

    # submit the transaction to the mempool.
    Mempool.tx(
      enode.node_id,
      {transaction.backend, transaction.noun},
      transaction.id
    )

    # assert that the transaction is in the mempool.
    # note: we cannot assert that it is the only transaction, because
    # this example is reused below.
    transactions = Mempool.tx_dump(enode.node_id)

    # wait for the transaction to be present in the events table, too.
    wait_for_transaction_in_table(enode, transaction)

    assert transaction.id in transactions

    {enode, transaction}
  end

  @doc """
  I add a transaction to the mempool that errors when executed.
  """
  @spec add_error_transaction(ENode.t()) ::
          {ENode.t(), ETransaction.t()}

  @spec add_error_transaction(ENode.t(), ETransaction.t()) ::
          {ENode.t(), ETransaction.t()}
  def add_error_transaction(enode \\ ENode.start_node()) do
    transaction = ETransaction.faulty_transaction()
    add_error_transaction(enode, transaction)
  end

  def add_error_transaction(enode, transaction) do
    add_transaction(enode, transaction)
  end

  @doc """
  I add multiple transactions to the mempool.
  """
  @spec add_multiple_transactions(ENode.t()) ::
          {ENode.t(), [ETransaction.t()]}
  @spec add_multiple_transactions(ENode.t(), [ETransaction.t()]) ::
          {ENode.t(), [ETransaction.t()]}

  def add_multiple_transactions(enode \\ ENode.start_node()) do
    transactions =
      Enum.map(1..10, fn _ -> ETransaction.simple_transaction() end)

    add_multiple_transactions(enode, transactions)
  end

  def add_multiple_transactions(enode, transactions) do
    # insert `transaction_count` transactions in the mempool
    Enum.each(transactions, &add_transaction(enode, &1))

    # assert all the transactions are in the mempool.
    mempool_transactions = Mempool.tx_dump(enode.node_id)

    for transaction <- transactions do
      assert transaction.id in mempool_transactions
    end

    {enode, transactions}
  end

  # -----------------------------------------------------------
  # Executing transactions

  @doc """
  I add a transaction to the mempool that executes properly.
  I execute this transaction.
  """
  @spec execute_transaction(ENode.t(), ETransaction.t()) ::
          {ENode.t(), ETransaction.t()}

  def execute_transaction(enode \\ ENode.start_node()) do
    transaction = ETransaction.simple_transaction()
    execute_transaction(enode, transaction)
  end

  def execute_transaction(enode, transaction) do
    # subscribe to events here to be sure the tx events are caught
    with_subscription [[]] do
      # add the transaction to the mempool
      {_enode, _transaction} = add_transaction(enode, transaction)

      # adding a transaction to the mempool has two observable effects.
      # - a transaction event should be fired
      # - there should be a new transaction task running in the dynanamic observer.

      # check that the event has been fired
      event = EEvent.transaction_event(enode, transaction)
      EEvent.wait_for_event(event)

      {enode, transaction}
    end
  end

  @doc """
  I add a transaction to the mempool that fails when executed.
  I execute this transaction.
  """
  @spec execute_multiple_transactions(ENode.t()) ::
          {ENode.t(), [ETransaction.t()]}

  def execute_multiple_transactions(enode \\ ENode.start_node()) do
    transactions =
      Enum.map(1..10, fn _ -> ETransaction.simple_transaction() end)

    execute_multiple_transactions(enode, transactions)
  end

  def execute_multiple_transactions(enode, transactions) do
    # subscribe to events here to be sure the tx events are caught
    with_subscription [[]] do
      # add the transaction to the mempool
      {enode, transactions} = add_multiple_transactions(enode, transactions)

      # adding a transaction to the mempool has two observable effects.
      # - a transaction event should be fired
      # - there should be a new transaction task running in the dynanamic observer.

      # check that the event has been fired for each transaction
      for transaction <- transactions do
        event = EEvent.transaction_event(enode, transaction)
        EEvent.wait_for_event(event)
      end

      {enode, transactions}
    end
  end

  # -----------------------------------------------------------
  # Blocks

  @doc """
  I run a transaction and execute it to create a block.

  I do not wait for the events of the block creation.
  """
  @spec make_block(
          ENode.t(),
          ETransaction.t()
        ) :: {ENode.t(), ETransaction.t()}
  def make_block(enode \\ ENode.start_node()) do
    transaction = ETransaction.simple_transaction()
    make_block(enode, transaction)
  end

  def make_block(enode, transaction) do
    # subscribe to events here to be sure the events are caught
    with_subscription [[]] do
      # fire a transaction
      {_node, transaction} = execute_transaction(enode, transaction)

      # the transaction is currently waiting for an ordering
      # or it has already executed if it did not scry.
      #
      # to ensure that the transaction completes, a consensus event
      # must be fired. This is done by the consensus engine
      # by calling Mempool.execute(node, transaction_ids)
      # there is no consensus in the current branch, so the call is done manually
      #
      # The Mempool.execute call will fire a consensus event
      # and then call the executor to execute the transactions.
      #
      # The executor will order the transactions in the consensus
      # and then wait for all transactions to complete.
      # After this, an execution event is sent.
      Mempool.execute(enode.node_id, [transaction.id])

      {enode, transaction}
    end
  end

  @spec complete_transaction(Anoma.Node.Examples.ENode.t()) ::
          {Anoma.Node.Examples.ENode.t(),
           Anoma.Node.Examples.ETransaction.t()}
  @doc """
  I run a transaction and let it complete.
  I expect a transaction description with the following values:
   - {backend, noun}: the transaction and noun
   - The expected result of executing the transaction
     E.g., {:ok, {:read_value, [["key" | 0] | 0]}}
   - The id of the transaction
  """
  @spec complete_transaction(
          ENode.t(),
          ETransaction.t(),
          non_neg_integer()
        ) :: {ENode.t(), ETransaction.t()}
  def complete_transaction(enode \\ ENode.start_node(), round \\ 1) do
    transaction = ETransaction.simple_transaction()
    complete_transaction(enode, transaction, round)
  end

  def complete_transaction(enode, transaction, round) do
    # subscribe to events here to be sure the events are caught
    with_subscription [[]] do
      # subscribe to mnesia events as well. see below.
      events_table = Tables.table_events(enode.node_id)
      :mnesia.subscribe({:table, events_table, :simple})

      {enode, transaction} = make_block(enode, transaction)

      # to verify that the transaction completed, n observable effects
      # must be assertd.
      # - consensus event is fired
      # - the events table writes a new consensus value
      # - order event is fired
      # - execution event is fired
      # - block event is fired

      # wait for the consensus event
      consensus_event = EEvent.consensus_event(enode, [transaction.id])
      EEvent.wait_for_event(consensus_event)

      # wait for the order event
      order_event = EEvent.order_event(enode, transaction.id)
      EEvent.wait_for_event(order_event)

      # wait for the execution event
      execution_event = EEvent.execution_event(enode, transaction)

      # todo: these should be one function wait_for_event..
      EEvent.wait_for_event(execution_event)

      # wait for the block event
      block_event = EEvent.block_event(enode, transaction, round)
      EEvent.wait_for_event(block_event)

      # wait for the mnesia table to be written fully
      wait_for_consensus_write(enode, transaction)

      # wait for the transaction to be removed from the events table
      wait_for_transaction_removed(enode, transaction)

      {enode, transaction}
    end
  end

  @doc """
  I run a list of transactions and create a block for each of them.
  """
  @spec complete_ten_transactions(ENode.t()) ::
          {ENode.t(), [ETransaction.t()]}
  @spec complete_ten_transactions(ENode.t(), [ETransaction.t()]) ::
          {ENode.t(), [ETransaction.t()]}
  def complete_ten_transactions(enode \\ ENode.start_node()) do
    transactions =
      Enum.map(1..10, fn _ -> ETransaction.simple_transaction() end)

    complete_ten_transactions(enode, transactions)
  end

  def complete_ten_transactions(enode, transactions) do
    transactions =
      for {transaction, round} <- Enum.with_index(transactions) do
        {_node, transaction} =
          complete_transaction(enode, transaction, round + 1)

        transaction
      end

    {enode, transactions}
  end

  # -----------------------------------------------------------
  # Mnesia Sync

  @doc """
  Given a node id and a transaction list, I wait until these
  transactions have been written into the events table its consensus column.
  """
  @spec wait_for_consensus_write(ENode.t(), ETransaction.t()) :: ENode.t()
  def wait_for_consensus_write(enode \\ ENode.start_node(), transaction) do
    events_table = Tables.table_events(enode.node_id)
    transaction_id = transaction.id

    assert_receive {:mnesia_table_event,
                    {:write, {^events_table, :consensus, [[^transaction_id]]},
                     _}},
                   5000

    enode
  end

  @doc """
  Given a node id and a transaction list, I wait until these
  transactions have been written into the events table its consensus column.
  """
  @spec wait_for_transaction_in_table(ENode.t(), ETransaction.t()) ::
          ENode.t()
  def wait_for_transaction_in_table(enode \\ ENode.start_node(), transaction) do
    events_table = Tables.table_events(enode.node_id)
    transaction_id = transaction.id
    transaction_backend = transaction.backend
    transaction_noun = transaction.noun

    assert_receive {:mnesia_table_event,
                    {:write,
                     {^events_table, ^transaction_id,
                      {^transaction_backend, ^transaction_noun}}, _}},
                   5000

    enode
  end

  @doc """
  Given a node id and a transaction, I wait until this transaction is
  removed from the events table.
  """
  @spec wait_for_transaction_removed(ENode.t(), ETransaction.t()) ::
          ENode.t()
  def wait_for_transaction_removed(enode \\ ENode.start_node(), transaction) do
    events_table = Tables.table_events(enode.node_id)
    transaction_id = transaction.id

    assert_receive {:mnesia_table_event,
                    {:delete, {^events_table, ^transaction_id}, _}},
                   5000

    enode
  end
end
