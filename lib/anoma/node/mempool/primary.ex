defmodule Anoma.Node.Mempool.Primary do
  use GenServer
  use TypedStruct
  require Logger

  alias Anoma.{Block, Transaction, Order, Serializer}
  alias Anoma.Block.Base
  alias Anoma.Node.Executor.Communicator, as: Ecom
  alias Anoma.Node.Storage.Communicator, as: Scom
  alias Anoma.Node.Storage.Ordering
  alias __MODULE__

  @type transactions :: list(Transaction.t())
  typedstruct do
    field(:ordering, GenServer.server())
    field(:executor, GenServer.server())
    field(:block_storage, atom(), default: Anoma.Block)
    field(:transactions, transactions, default: [])
    field(:round, non_neg_integer(), default: 0)

    field(:key, {Serializer.public_key(), Serializer.private_key()},
      default: :crypto.generate_key(:rsa, {1024, 65537})
    )

    field(:logger, atom(), enforce: false)
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: args[:name])
  end

  def init(args) do
    primary =
      Map.merge(%Primary{}, args |> Enum.into(%{}))

    # TODO add a flag in storage for yes if we want rocksdb copies
    Block.create_table(primary.block_storage, false)
    {:ok, primary}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec execute(GenServer.server()) :: non_neg_integer()
  def execute(server) do
    GenServer.call(server, :execute, 10_000)
  end

  @spec tx(GenServer.server(), Transaction.execution()) :: Transaction.t()
  def tx(server, tx_code) do
    GenServer.call(server, {:tx, tx_code}, 10_000)
  end

  @spec soft_reset(GenServer.server()) :: :ok
  def soft_reset(server) do
    GenServer.cast(server, :soft_reset)
  end

  @spec hard_reset(GenServer.server()) :: :ok
  def hard_reset(server) do
    GenServer.cast(server, :hard_reset)
  end

  @spec state(GenServer.server()) :: t()
  def state(server) do
    GenServer.call(server, :state)
  end

  @spec pending_txs(GenServer.server()) :: transactions
  def pending_txs(server) do
    GenServer.call(server, :pending_txs)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    log_info({:state, state, state.logger})

    {:reply, state, state}
  end

  def handle_call({:tx, tx_code}, _from, state) do
    ntrans = handle_tx(tx_code, state)
    nstate = %Primary{state | transactions: [ntrans | state.transactions]}
    log_info({:tx, nstate.transactions, state.logger})
    {:reply, ntrans, nstate}
  end

  def handle_call(:execute, _from, state) do
    {length_ran, new_state} = handle_execute(state)
    log_info({:execute, state.logger})

    {:reply, {:ok, length_ran}, new_state}
  end

  def handle_call(:pending_txs, _from, state) do
    txs = state.transactions
    log_info({:pending, txs, state.logger})

    {:reply, txs, state}
  end

  def handle_cast(:soft_reset, state) do
    logger = state.logger
    log_info({:soft, logger})

    kill_transactions(state)

    {:noreply, reset_state(state)}
  end

  def handle_cast(:hard_reset, state) do
    logger = state.logger
    log_info({:hard, logger})
    snapshot = Ecom.snapshot(state.executor)
    kill_transactions(state)
    reset_blocks(state)
    Scom.hard_reset(state.ordering, snapshot)
    {:noreply, reset_state(state)}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_tx(Transaction.execution(), t()) :: Transaction.t()
  def handle_tx(tx_code, state) do
    random_tx_id = random_id()
    log_info({:fire, state.executor, random_tx_id, state.logger})
    pid = Ecom.fire_new_transaction(state.executor, random_tx_id, tx_code)

    Transaction.new(random_tx_id, pid, tx_code)
  end

  @spec handle_execute(t()) :: {non_neg_integer(), t()}
  def handle_execute(state) do
    {executing, left} = Enum.split(state.transactions, 100)
    block = produce_block(state, executing)
    choose_and_execute_ordering(state, executing)
    save_block(state, block)
    new_state = %Primary{state | transactions: left, round: state.round + 1}
    log_info({:execute_handle, new_state, state.logger})

    {length(executing), new_state}
  end

  ############################################################
  #                     Conceptual Helpers                   #
  ############################################################

  # 128-bit random id
  def random_id() do
    :crypto.strong_rand_bytes(16)
    |> Noun.atom_binary_to_integer()
  end

  # should we be doing this work, or should we making ordering do
  # this?
  @spec choose_and_execute_ordering(t(), list(Transaction.t())) :: :ok
  def choose_and_execute_ordering(state, transactions) do
    # get an ordering for the transactions we are executing
    ordered_transactions =
      order(transactions, Scom.next_order(state.ordering))

    # send in the ordering for the write ready
    log_info({:order, state.ordering, state.logger})
    Scom.new_order(state.ordering, ordered_transactions)

    # also send in the logic for write ready
    instrument({:write, length(ordered_transactions)})
    log_info({:write, length(ordered_transactions), state.logger})

    for ord <- ordered_transactions do
      send(Order.pid(ord), {:write_ready, Order.index(ord)})
    end

    :ok
  end

  @spec produce_block(t(), list(Transaction.t())) :: Block.t()
  def produce_block(state, trans) do
    log_info({:produce, trans, state.logger})

    trans
    |> Enum.map(&persistent_transaction/1)
    |> Base.new()
    |> Block.create(state.key, state.round)
  end

  @spec save_block(t(), Block.t()) :: {:atomic, :ok} | {:aborted, any()}
  def save_block(state, block) do
    encoded = Block.encode(block, state.block_storage)
    log_info({:encode_block, block, state.logger})
    :mnesia.transaction(fn -> :mnesia.write(encoded) end)
  end

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

  def reset_blocks(state) do
    storage = state.block_storage
    :mnesia.delete_table(storage)
    log_info({:delete_table, storage, state.logger})
    # TODO add a flag in storage for yes if we want rocksdb copies
    Block.create_table(storage, false)
  end

  @spec kill_transactions(Primary.t()) :: :ok
  def kill_transactions(state) do
    transactions = state.transactions
    logger = state.logger

    instrument({:kill, length(transactions)})
    log_info({:kill, length(state.transactions), logger})

    for transaction <- transactions do
      instrument({:killing_pid, transaction})
      log_info({:killing_pid, transaction, logger})
      Process.exit(Transaction.pid(transaction), :kill)
    end

    :ok
  end

  @spec reset_state(t()) :: t()
  def reset_state(state) do
    %Primary{state | transactions: [], round: 0}
  end

  @spec persistent_transaction(Transaction.t()) ::
          {Noun.t(), Transaction.execution()}
  def persistent_transaction(trans) do
    {Transaction.id(trans), Transaction.transaction(trans)}
  end

  @spec order_format(Transaction.t(), non_neg_integer()) :: Order.t()
  def order_format(transaction, order) do
    Order.new(
      order,
      Transaction.id(transaction),
      Transaction.pid(transaction)
    )
  end

  defp instrument({:kill, len}) do
    Logger.info("Got Kill Signal killing #{inspect(len)} processes")
  end

  defp instrument({:killing_pid, pid}) do
    Logger.debug("Killing: #{inspect(pid)}")
  end

  defp instrument({:write, len}) do
    Logger.info("Sending :write_ready to #{inspect(len)} processes")
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:state, state, logger}) do
    Anoma.Node.Logger.add(
      logger,
      self(),
      "Requested state: #{inspect(state)})"
    )
  end

  defp log_info({:tx, state, logger}) do
    Anoma.Node.Logger.add(
      logger,
      self(),
      "Transaction added. New pool: #{inspect(state)})"
    )
  end

  defp log_info({:execute, logger}) do
    Anoma.Node.Logger.add(logger, self(), "Requested execution")
  end

  defp log_info({:pending, txs, logger}) do
    Anoma.Node.Logger.add(
      logger,
      self(),
      "Reqested pending transactions: #{inspect(txs)})"
    )
  end

  defp log_info({:soft, logger}) do
    Anoma.Node.Logger.add(logger, self(), "Requested soft reset")
  end

  defp log_info({:hard, logger}) do
    Anoma.Node.Logger.add(logger, self(), "Requested hard reset")
  end

  defp log_info({:fire, ex, id, logger}) do
    Anoma.Node.Logger.add(logger, self(), "Requested transaction fire.
      Executor: #{inspect(ex)}.
      Id : #{inspect(id)}")
  end

  defp log_info({:execute_handle, state, logger}) do
    Anoma.Node.Logger.add(logger, self(), "New state: #{inspect(state)})")
  end

  defp log_info({:order, ordering, logger}) do
    Anoma.Node.Logger.add(
      logger,
      self(),
      "Requested ordering from: #{inspect(ordering)})"
    )
  end

  defp log_info({:write, length, logger}) do
    Anoma.Node.Logger.add(
      logger,
      self(),
      "Sending :write_ready to #{inspect(length)} processes"
    )
  end

  defp log_info({:produce, trans, logger}) do
    Anoma.Node.Logger.add(
      logger,
      self(),
      "Producing block. Transsactions: #{inspect(trans)}"
    )
  end

  defp log_info({:encode_block, block, logger}) do
    Anoma.Node.Logger.add(logger, self(), "Encoding block: #{inspect(block)}")
  end

  defp log_info({:delete_table, storage, logger}) do
    Anoma.Node.Logger.add(
      logger,
      self(),
      "Deleting table: #{inspect(storage)} processes"
    )
  end

  defp log_info({:kill, length, logger}) do
    Anoma.Node.Logger.add(
      logger,
      self(),
      "Got Kill Signal killing #{inspect(length)} processes"
    )
  end

  defp log_info({:killing_pid, pid, logger}) do
    Anoma.Node.Logger.add(logger, self(), "Killing: #{inspect(pid)}")
  end
end
