defmodule Anoma.Node.Examples.EReplay.StartState do
  @moduledoc """
  I define examples on how the start state of a node is computed.
  """

  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Examples.Mempool, as: EMempool
  alias Anoma.Node.Replay.State
  alias Anoma.Node.Tables
  alias Anoma.Node.Registry
  alias Anoma.Node.Logging
  alias Anoma.Node.Event
  alias Anoma.Node.Examples.EEvent
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Transaction.Mempool

  import ExUnit.Assertions

  # -----------------------------------------------------------
  # Table states

  @doc """
  I assert that a node that does not exist does not have any tables present.
  """
  @spec no_node_no_tables() :: :ok
  def no_node_no_tables() do
    non_existing_node_id = ENode.random_node_id()
    has_tables? = Tables.has_data?(non_existing_node_id)
    assert has_tables? == {:error, :none_exist}
    :ok
  end

  @doc """
  I check whether a fresh node has all its tables created.
  """
  @spec new_node_has_tables(ENode.t()) :: ENode.t()
  def new_node_has_tables(enode \\ ENode.start_node()) do
    has_tables? = Tables.has_data?(enode.node_id)
    assert has_tables? == {:ok, :exists}

    enode
  end

  @spec partial_state_if_table_deleted() :: Anoma.Node.Examples.ENode.t()
  @doc """
  I check whether a node with some missing tables is marked as partial.
  """
  @spec partial_state_if_table_deleted(ENode.t()) :: ENode.t()
  def partial_state_if_table_deleted(enode \\ ENode.start_node()) do
    # delete a table for the given node
    table_to_delete = Tables.table_blocks(enode.node_id)

    {:atomic, :ok} = :mnesia.delete_table(table_to_delete)

    # there are not partial tables left
    has_tables? = Tables.has_data?(enode.node_id)
    assert has_tables? == {:error, :partial_exist}

    enode
  end

  # -----------------------------------------------------------
  # Mempool

  @doc """
  I start up a new node, or I assume that the given node is empty.

  I compute the startup arguments for this node and verify that they are the default arguments.
  """
  @spec mempool_args_fresh_node(ENode.t()) :: ENode.t()
  def mempool_args_fresh_node(enode \\ ENode.start_node()) do
    # there should be 0 transactions
    {:ok, mempool_start_args} = State.mempool_arguments(enode.node_id)

    # assert values in the arguments
    assert mempool_start_args[:transactions] == []
    assert mempool_start_args[:round] == 1
    assert mempool_start_args[:consensus] == []

    enode
  end

  @doc """
  I start up a new node, or I assume that the given node is empty.

  I compute the startup arguments for this node and verify that they are the default arguments.
  """
  @spec mempool_args_added_transaction(ENode.t()) :: ENode.t()
  def mempool_args_added_transaction(enode \\ ENode.start_node()) do
    {_node, transaction} = EMempool.add_transaction(enode)
    # there should be 0 transactions
    {:ok, mempool_start_args} = State.mempool_arguments(enode.node_id)

    # assert values in the arguments
    assert mempool_start_args[:transactions] == [
             {transaction.id, {transaction.backend, transaction.noun}}
           ]

    assert mempool_start_args[:round] == 1
    assert mempool_start_args[:consensus] == []

    enode
  end

  @doc """
  I start up a new node, or assume the given node is empty.
  I add ten transactions to the mempool and complete all of them so that they are in a block.
  When I compute the startup arguments for this node's mempool, I expect to have the default arguments.
  """
  @spec mempool_args_non_fresh_node(ENode.t()) :: ENode.t()
  def mempool_args_non_fresh_node(enode \\ ENode.start_node()) do
    # run ten separate transactions in a block through the node.
    EMempool.complete_ten_transactions(enode)

    {:ok, mempool_start_args} = State.mempool_arguments(enode.node_id)

    # assert values in the arguments
    assert mempool_start_args[:transactions] == []
    assert mempool_start_args[:round] == 11
    assert mempool_start_args[:consensus] == []

    enode
  end

  @doc """
  I start up a new node, or assume the given node is empty.
  I add a transaction to the mempool.
  The startup arguments for this mempool should contain the transaction I added.
  """
  @spec mempool_args_non_block_transaction(ENode.t()) ::
          {ENode.t(), ETransaction.t()}
  def mempool_args_non_block_transaction(enode \\ ENode.start_node()) do
    # run a transaction, but do not create a block
    # this will make sure the transaction is still present in the mempool's tables
    # and it should be restored.
    {_node, transaction} = EMempool.add_transaction(enode)

    # compute the mempool startup arguments
    {:ok, mempool_start_args} = State.mempool_arguments(enode.node_id)

    # assert the transaction I just added is in the list of the startup arguments.
    assert mempool_start_args[:transactions] == [
             {transaction.id, {transaction.backend, transaction.noun}}
           ]

    assert mempool_start_args[:round] == 1
    assert mempool_start_args[:consensus] == []

    {enode, transaction}
  end

  @doc """
  I start up a new node, or assume the given node is empty.
  I add a bunch of transactions to the mempool.
  The startup arguments for this mempool should contain the transactions I added.
  """
  @spec mempool_args_non_block_transactions(ENode.t()) :: ENode.t()
  def mempool_args_non_block_transactions(enode \\ ENode.start_node()) do
    # run 10 transactions, but do not create a block
    # this will make sure the transactions are still present in the mempool's tables
    # and they should be restored.
    transaction_list =
      for _ <- 1..10 do
        {_node, transaction} = EMempool.add_transaction(enode)
        {transaction.id, {transaction.backend, transaction.noun}}
      end

    # compute the mempool startup arguments
    {:ok, mempool_start_args} = State.mempool_arguments(enode.node_id)

    # assert the transaction I just added is in the list of the startup arguments.
    assert mempool_start_args[:transactions] -- transaction_list == []
    assert transaction_list -- mempool_start_args[:transactions] == []
    assert mempool_start_args[:round] == 1
    assert mempool_start_args[:consensus] == []

    enode
  end

  @doc """
  I want to create a mempool that has a consensus in its state, but for which no block was created.

  To achieve this, I have to stop a block from being created.
  To do this, I force unsubscribe the mempool from execution events.
  This ensures that no commits happen, and no block event is generated.
  """
  @spec mempool_todo_consensus(ENode.t()) :: {ENode.t(), ETransaction.t()}
  def mempool_todo_consensus(enode \\ ENode.start_node()) do
    # stop the logging engine from processing block events.
    mempool_engine = Registry.whereis(enode.node_id, Mempool)

    filter = [
      Event.node_filter(enode.node_id),
      Mempool.filter_for_mempool_execution_events()
    ]

    EventBroker.unsubscribe(mempool_engine, filter)

    # create a block from a transaction
    # this call does not wait for any events, so its async.
    # to be sure the block is created, I wait for the block event here myself.
    {_enode, transaction} = EMempool.make_block(enode)

    # wait for the block event
    order_event = EEvent.order_event(enode, transaction.id)
    EEvent.wait_for_event(order_event)

    # compute the mempool arguments.
    {:ok, mempool_start_args} = State.mempool_arguments(enode.node_id)

    # assert values in the arguments
    assert mempool_start_args[:transactions] == [
             {transaction.id, {transaction.backend, transaction.noun}}
           ]

    assert mempool_start_args[:round] == 1
    assert mempool_start_args[:consensus] == [[transaction.id]]

    {enode, transaction}
  end

  @doc """
  I start up a new node, or assume the given node is empty.
  I add a number of transactions to the mempool but do not let them form into a block.

  When a block is created, the Logging engine does the following:
  - It removes the transactions from the events table.
  - It removes the consensus for these transactions from the events table
  - It writes the round into the events table.

  When the node stops before this ceremony has happened, the following is true.
  - There is a block in the blocks table that holds a list of transactions
  - That same list of transactions is still present in the events table
  - The round of the events table is outdated.

  The startup arguments do not take into account transactions and consensi that are
  in an actual block, these are dropped. This test asserts that this is the case.

  It does so by doing the following.

  If a block event is fired, the Logging engine will remove these values from the database.
  We make sure that this not happen by mocking this behaviour.
  """
  def mempool_obsolete_consensus(enode \\ ENode.start_node()) do
    # create ten blocks
    {_enode, transactions} = EMempool.complete_ten_transactions(enode)

    # the next round is the total amount of transactions (starts counting from 0)
    next_round = Enum.count(transactions) + 1

    # the highest round for a block is 9
    # events is at round 9
    # consensus is empty

    # stop the logging engine from processing block events.
    logging_engine = Registry.whereis(enode.node_id, Logging)
    filter = [Event.node_filter(enode.node_id), Logging.blocks_filter()]
    EventBroker.unsubscribe(logging_engine, filter)

    # create a block from a transaction
    # this call does not wait for any events, so its async.
    # to be sure the block is created, I wait for the block event here myself.
    {_enode, transaction} = EMempool.make_block(enode)

    # wait for the block event
    block_event = EEvent.block_event(enode, transaction, next_round)
    EEvent.wait_for_event(block_event)

    # compute the mempool arguments.
    {:ok, mempool_start_args} = State.mempool_arguments(enode.node_id)

    # assert values in the arguments
    assert mempool_start_args[:transactions] == []
    assert mempool_start_args[:round] == next_round + 1
    assert mempool_start_args[:consensus] == []

    enode
  end

  def mempool_obsolete_consensi(enode \\ ENode.start_node()) do
    # create ten blocks
    {_enode, transactions} = EMempool.complete_ten_transactions(enode)

    # the next round is the total amount of transactions (starts counting from 0)
    next_round = Enum.count(transactions) + 1

    # the highest round for a block is 9
    # events is at round 9
    # consensus is empty

    # stop the logging engine from processing block events.
    logging_engine = Registry.whereis(enode.node_id, Logging)
    filter = [Event.node_filter(enode.node_id), Logging.blocks_filter()]
    EventBroker.unsubscribe(logging_engine, filter)

    # create 5 blocks, creating 5 stale consensi in the logging engine.
    for block <- 0..4 do
      # create a block from a transaction
      # this call does not wait for any events, so its async.
      # to be sure the block is created, I wait for the block event here myself.
      {_enode, transaction} = EMempool.make_block(enode)

      # wait for the block event
      block_event = EEvent.block_event(enode, transaction, next_round + block)
      EEvent.wait_for_event(block_event)
    end

    # compute the mempool arguments.
    {:ok, mempool_start_args} = State.mempool_arguments(enode.node_id)

    # assert values in the arguments
    assert mempool_start_args[:transactions] == []
    assert mempool_start_args[:round] == next_round + 5
    assert mempool_start_args[:consensus] == []

    enode
  end

  # -----------------------------------------------------------
  # Storage

  @spec storage_args_fresh_node() :: Anoma.Node.Examples.ENode.t()
  @doc """
  I check whether the storage arguments for a fresh node are the default arguments.
  """
  @spec storage_args_fresh_node(ENode.t()) :: ENode.t()
  def storage_args_fresh_node(enode \\ ENode.start_node()) do
    # there should be 0 transactions, and the committed height should be 0.
    {:ok, storage_start_args} = State.storage_arguments(enode.node_id)

    assert storage_start_args == [uncommitted_height: 0]

    enode
  end

  @doc """
  I check whether the storage arguments for a fresh node are the default arguments.
  """
  @spec storage_args_non_fresh_node(ENode.t()) :: ENode.t()
  def storage_args_non_fresh_node(enode \\ ENode.start_node()) do
    # run ten separate transactions in a block through the node.
    EMempool.complete_ten_transactions(enode)

    # there should be 10 transactions, and the committed height should be 9.
    {:ok, storage_start_args} = State.storage_arguments(enode.node_id)

    assert storage_start_args == [uncommitted_height: 10]

    enode
  end

  @doc """
  I check whether the storage arguments are default when a transaction is added
  but not executed.
  """
  @spec storage_args_non_block_transaction(ENode.t()) :: ENode.t()
  def storage_args_non_block_transaction(enode \\ ENode.start_node()) do
    # run ten separate transactions in a block through the node.
    EMempool.add_transaction(enode)

    # there should be 10 transactions, and the committed height should be 0.
    {:ok, storage_start_args} = State.storage_arguments(enode.node_id)

    assert storage_start_args == [uncommitted_height: 0]

    enode
  end

  # -----------------------------------------------------------
  # Ordering

  @doc """
  I check whether the ordering arguments for a fresh node are the default arguments.
  """
  @spec ordering_args_fresh_node(ENode.t()) :: ENode.t()
  def ordering_args_fresh_node(enode \\ ENode.start_node()) do
    # there should be 0 transactions, and the committed height should be 0.
    {:ok, ordering_start_args} = State.ordering_arguments(enode.node_id)

    assert ordering_start_args == [next_height: 1]

    enode
  end

  @doc """
  I check whether the ordering arguments for a fresh node are the default arguments.
  """
  @spec ordering_args_non_fresh_node(ENode.t()) :: ENode.t()
  def ordering_args_non_fresh_node(enode \\ ENode.start_node()) do
    # run ten separate transactions in a block through the node.
    EMempool.complete_ten_transactions(enode)

    # there should be 10 transactions, and the committed height should be 9.
    {:ok, ordering_start_args} = State.ordering_arguments(enode.node_id)

    assert ordering_start_args == [next_height: 11]

    enode
  end
end
