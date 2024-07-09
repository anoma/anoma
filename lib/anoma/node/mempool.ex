defmodule Anoma.Node.Mempool do
  @moduledoc """
  I am the Mempool Engine.

  I provide the interface for transaction management including
  communication with the Executor engine to create transaction candidates
  with given code, executing said transactions, and creating appropriate
  blocks.

  ### Public API

  I provide the following public functionality:

  - `execute/1`
  - `soft_reset/1`
  - `hard_reset/1`
  - `tx/2`
  """

  alias Anoma.{Block, Transaction, Serializer}
  alias Anoma.Block.Base
  alias Anoma.Node.{Router, Logger, Executor, Ordering}
  alias Router.Engine
  alias __MODULE__

  use Router.Engine
  use TypedStruct

  @typedoc """
  I am a list of transaction candidates.
  """
  @type transactions :: list(Transaction.t())

  typedstruct do
    @typedoc """
    I am the type of the Mempool Engine.

    My fields store information necessary to succesfully call for
    transaction candidate creation, execution, and ordering requests.

    ### Fields

    - `:ordering` - The Ordering Engine Address.
    - `:executor` - The Executor Engine Address.
    - `:block_storage` - The name of the storage for created blocks.
                         Default: `Anoma.Block`
    - `:transactions` - List of transaction candidates.
                        Default: []
    - `:round` - The block round information.
                 Default: 0
    - `:topic` - The topic address for broadcasting.
    - `:key` - The key used for block creation.
               Default: `:crypto.generate_key(:rsa, {1024, 65537}`
    - `:logger` - The address of the Logger Engine.
                  Enforced: false
    """

    field(:ordering, Router.Addr.t())
    field(:executor, Router.Addr.t())
    field(:block_storage, atom(), default: Anoma.Block)
    field(:transactions, transactions, default: [])
    field(:round, non_neg_integer(), default: 0)
    field(:topic, Router.Addr.t())

    field(:key, {Serializer.public_key(), Serializer.private_key()},
      default: :crypto.generate_key(:rsa, {1024, 65537})
    )

    field(:logger, Router.Addr.t(), enforce: false)
  end

  @doc """
  I am the initialization function for a Mempool Engine instance.

  ### Pattern-Macthing Variations

  - `init(%Mempool{})` - I initialize the Engine with the given state.
  - `init(args)` - I expect a keylist with all keys in the structure fields
                   listed necessary for the Mempool Engine to start. I then
                   create a table with the given `:block_storage` argument
                   and initialize an instance with given parameters.
  """

  def init(%Mempool{} = state) do
    {:ok, state}
  end

  def init(args) do
    primary =
      Map.merge(%Mempool{}, args |> Enum.into(%{}))

    # TODO add a flag in storage for yes if we want rocksdb copies
    Block.create_table(primary.block_storage, false)
    {:ok, primary}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the execution function.

  I execute the block containing top 100 transaction candidates availiable,
  ordering them using the specified Ordering Engine.
  """

  @spec execute(Router.Addr.t()) :: :ok
  def execute(server) do
    Router.cast(server, :execute)
  end

  @doc """
  I am the transaction candidate creating API.

  Given a Mempool Engine address and a transaction candidate code, I ask
  the Executor Engine to fire a new transaction with a random ID created in
  the process. I return the new state, adding the transaction.
  """

  @spec tx(Router.Addr.t(), Transaction.execution()) :: :ok
  def tx(server, tx_code) do
    Router.cast(server, {:tx, tx_code})
  end

  @doc """
  I am the soft reset function.

  I empty the transaction and round fields of the appropriate Mempool
  Engine, asking the linked Executor to kill relevant transaction Workers.
  """

  @spec soft_reset(Router.Addr.t()) :: :ok
  def soft_reset(server) do
    Router.cast(server, :soft_reset)
  end

  @doc """
  I am the hard reset function.

  Similar to `soft_reset/1`, I kill transactions, but also call for an
  Executor snapshot, delete the block table and hard reset the Ordering
  Engine. The block table is then re-launched with the same rocks flag as
  before.
  """

  @spec hard_reset(Router.Addr.t()) :: :ok
  def hard_reset(server) do
    Router.cast(server, :hard_reset)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast(:execute, _from, state) do
    {length_ran, new_state} = handle_execute(state)
    log_info({:execute, state.logger})
    Router.cast(state.topic, {:executed, {:ok, length_ran}})
    {:noreply, new_state}
  end

  def handle_cast({:tx, tx_code}, _from, state) do
    ntrans = handle_tx(tx_code, state)
    nstate = %Mempool{state | transactions: [ntrans | state.transactions]}
    Router.cast(state.topic, {:submitted, ntrans})
    log_info({:tx, nstate.transactions, state.logger})
    {:noreply, nstate}
  end

  def handle_cast(:soft_reset, _from, state) do
    logger = state.logger
    log_info({:soft, logger})
    kill_transactions(state)
    {:noreply, reset_state(state)}
  end

  def handle_cast(:hard_reset, _from, state) do
    logger = state.logger
    log_info({:hard, logger})
    snapshot = Executor.snapshot(state.executor)
    kill_transactions(state)
    reset_blocks(state)
    Ordering.hard_reset(state.ordering, snapshot)
    {:noreply, reset_state(state)}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @doc """
  I handle the transaction candidate creation.

  Given transaction code and the Mempool state, I ask the Executor to fire
  a new transaction with a randomly generated transaction ID.

  I reply with a newly created transaction and specified Worker.
  """

  @spec handle_tx(Transaction.execution(), t()) :: Transaction.t()
  @dialyzer {:nowarn_function, [handle_tx: 2]}
  def handle_tx(tx_code, state) do
    random_tx_id =
      :crypto.strong_rand_bytes(16)
      |> Noun.atom_binary_to_integer()

    ex = state.executor

    log_info({:fire, ex, random_tx_id, state.logger})

    ex_topic = Engine.get_state(ex).topic

    :ok =
      Router.call(
        Engine.get_router(ex),
        {:subscribe_topic, ex_topic, :local}
      )

    Executor.fire_new_transaction(ex, random_tx_id, tx_code)

    receive do
      {:"$gen_cast", {_, _, {:worker_spawned, addr}}} ->
        :ok =
          Router.call(
            Engine.get_router(ex),
            {:unsubscribe_topic, ex_topic, :local}
          )

        Transaction.new(random_tx_id, addr, tx_code)
    end
  end

  @doc """
  I handle Mempool execution.

  I take the top 100 transaction candidates, ask for an ordering on them,
  producing and storing an encoded block.

  I return a tuple with first entry specifying how many transactions got
  into the execution and a new appropriately changed state.
  """

  @spec handle_execute(t()) :: {non_neg_integer(), t()}
  def handle_execute(state) do
    {executing, left} = Enum.split(state.transactions, 100)
    block = produce_block(state, executing)
    choose_and_execute_ordering(state, executing)
    save_block(state, block)
    new_state = %Mempool{state | transactions: left, round: state.round + 1}
    log_info({:execute_handle, new_state, state.logger})
    {length(executing), new_state}
  end

  @doc """
  I kill transactions listed in the fed-in state.

  Given a state with a list of transaction candidates, I call for the
  Executor engine to kill each one in the list.
  """
  @spec kill_transactions(Mempool.t()) :: :ok
  def kill_transactions(state) do
    transactions = state.transactions
    log_info({:kill, length(transactions), state.logger})

    for transaction <- transactions do
      log_info({:killing_pid, transaction, state.logger})
    end

    Executor.kill_transactions(state.executor, transactions)
    :ok
  end

  @doc """
  I provide block-resetting functionality.

  I delete the mnesia table for block storage provided in the given state.

  I then create the table with the same name and rocks flag.
  """

  @spec reset_blocks(Mempool.t()) :: {:aborted, any()} | {:atomic, :ok}
  def reset_blocks(state) do
    storage = state.block_storage
    neg_flag = :mnesia.table_info(storage, :rocksdb_copies) |> Enum.empty?()

    :mnesia.delete_table(storage)
    log_info({:delete_table, storage, state.logger})

    Block.create_table(storage, not neg_flag)
  end

  ############################################################
  #                     Conceptual Helpers                   #
  ############################################################

  @doc """
  I order the transactions based on the current ordering specified by the
  Ordering engine, place a new orderig, and for every ordered transaction,
  send to their appropriate Worker a `:write_ready` message.

  Return :ok.
  """
  @spec choose_and_execute_ordering(t(), list(Transaction.t())) :: :ok
  def choose_and_execute_ordering(state, transactions) do
    # get an ordering for the transactions we are executing
    ordered_transactions =
      order(transactions, Router.Engine.get_state(state.ordering).next_order)

    # send in the ordering for the write ready
    log_info({:order, state.ordering, state.logger})
    Ordering.new_order(state.ordering, ordered_transactions)

    # also send in the logic for write ready
    log_info({:write, length(ordered_transactions), state.logger})

    for ord <- ordered_transactions do
      Router.send_raw(
        Transaction.addr(ord),
        {:write_ready, Transaction.index(ord)}
      )
    end

    :ok
  end

  @doc """
  I produce a block with the key and round from the given state and eturn it.
  """

  @spec produce_block(t(), list(Transaction.t())) :: Block.t()
  def produce_block(state, trans) do
    log_info({:produce, trans, state.logger})

    trans
    |> Enum.map(&persistent_transaction/1)
    |> Base.new()
    |> Block.create(state.key, state.round)
  end

  @doc """
  I save a provided block.

  Given a Mempool Engine state and a block, I encode the block and write it
  at the specified mnesia table.
  """
  @spec save_block(t(), Block.t()) :: {:atomic, :ok} | {:aborted, any()}
  def save_block(state, block) do
    encoded = Block.encode(block, state.block_storage)
    log_info({:encode_block, block, state.logger})
    :mnesia.transaction(fn -> :mnesia.write(encoded) end)
  end

  @doc """
  I order a list of transactions based on a given order.

  Given a list of transaction candidates and an order number, I shuffle the
  transactions, and index them starting with the given order number.
  """

  @spec order(list(Transaction.t()), non_neg_integer()) ::
          Ordering.ordered_transactions()
  def order(executing, next_order) do
    executing
    |> Enum.shuffle()
    |> Enum.with_index(&order_format(&1, &2 + next_order))
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  @doc """
  I return a state with no transactions and 0 round count.
  """

  @spec reset_state(t()) :: t()
  def reset_state(state) do
    %Mempool{state | transactions: [], round: 0}
  end

  @doc """
  I give core information on a transaction candidate.

  Given a transaction candidate, I return a tuple. The first element is the
  id of the transaction. The second element is the transaction code.
  """

  @spec persistent_transaction(Transaction.t()) ::
          {Noun.t(), Transaction.execution()}
  def persistent_transaction(trans) do
    {Transaction.id(trans), Transaction.transaction(trans)}
  end

  @doc """
  I order a transaction structure by giving it an index.

  Given a transaction candidate and an order, I put the index specified by
  the input as its `:index` field value.
  """

  @spec order_format(Transaction.t(), non_neg_integer()) :: Transaction.t()
  def order_format(transaction, order) do
    %Transaction{transaction | index: order}
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  @dialyzer {:nowarn_function, [log_info: 1]}
  defp log_info({:tx, state, logger}) do
    Logger.add(
      logger,
      :info,
      "Transaction added. New pool: #{inspect(state)})"
    )
  end

  defp log_info({:execute, logger}) do
    Logger.add(logger, :info, "Requested execution")
  end

  defp log_info({:soft, logger}) do
    Logger.add(logger, :debug, "Requested soft reset")
  end

  defp log_info({:hard, logger}) do
    Logger.add(logger, :debug, "Requested hard reset")
  end

  defp log_info({:fire, ex, id, logger}) do
    Logger.add(logger, :info, "Requested transaction fire.
      Executor: #{inspect(ex)}.
      Id : #{inspect(id)}")
  end

  defp log_info({:execute_handle, state, logger}) do
    Logger.add(logger, :info, "New state: #{inspect(state)})")
  end

  defp log_info({:order, ordering, logger}) do
    Logger.add(
      logger,
      :info,
      "Requested ordering from: #{inspect(ordering)})"
    )
  end

  defp log_info({:write, length, logger}) do
    Logger.add(
      logger,
      :info,
      "Sending :write_ready to #{inspect(length)} processes"
    )
  end

  defp log_info({:produce, trans, logger}) do
    Logger.add(
      logger,
      :info,
      "Producing block. Transsactions: #{inspect(trans)}"
    )
  end

  defp log_info({:encode_block, block, logger}) do
    Logger.add(logger, :info, "Encoding block: #{inspect(block)}")
  end

  defp log_info({:delete_table, storage, logger}) do
    Logger.add(
      logger,
      :debug,
      "Deleting table: #{inspect(storage)} processes"
    )
  end

  defp log_info({:kill, length, logger}) do
    Logger.add(
      logger,
      :debug,
      "Got Kill Signal killing #{inspect(length)} processes"
    )
  end

  defp log_info({:killing_pid, pid, logger}) do
    Logger.add(logger, :debug, "Killing: #{inspect(pid)}")
  end
end
