defmodule Anoma.Node.Mempool do
  use GenServer
  use TypedStruct
  require Logger

  alias Anoma.{Block, Transaction, Order, Serializer}
  alias Anoma.Block.Base
  alias Anoma.Node.Executor
  alias Anoma.Node.Storage.Ordering
  alias Anoma.Node.Router
  alias __MODULE__

  @type transactions :: list(Transaction.t())
  typedstruct do
    field(:ordering, Router.Addr.t())
    field(:executor, Router.Addr.t())
    field(:block_storage, atom(), default: Anoma.Block)
    field(:transactions, transactions, default: [])
    field(:round, non_neg_integer(), default: 0)
    field(:topic, Router.Addr.t())

    field(:key, {Serializer.public_key(), Serializer.private_key()},
      default: :crypto.generate_key(:rsa, {1024, 65537})
    )
  end

  def init(args) do
    primary =
      Map.merge(%Mempool{}, args |> Enum.into(%{}))

    # TODO add a flag in storage for yes if we want rocksdb copies
    Block.create_table(primary.block_storage, false)
    Logger.info("mempool topic #{inspect(primary.topic)}")
    {:ok, primary}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec execute(Router.Addr.t()) :: non_neg_integer()
  def execute(server) do
    Router.call(server, :execute, 10_000)
  end

  @spec tx(Router.Addr.t(), Transaction.execution()) :: Transaction.t()
  def tx(server, tx_code) do
    Router.call(server, {:tx, tx_code}, 10_000)
  end

  @spec soft_reset(Router.Addr.t()) :: :ok
  def soft_reset(server) do
    Router.cast(server, :soft_reset)
  end

  @spec hard_reset(Router.Addr.t()) :: :ok
  def hard_reset(server) do
    Router.cast(server, :hard_reset)
  end

  @spec state(Router.Addr.t()) :: t()
  def state(server) do
    Router.call(server, :state)
  end

  @spec pending_txs(Router.Addr.t()) :: transactions
  def pending_txs(server) do
    Router.call(server, :pending_txs)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:tx, tx_code}, _from, state) do
    ntrans = handle_tx(tx_code, state)
    nstate = %Mempool{state | transactions: [ntrans | state.transactions]}
    Router.cast(state.topic, {:submitted, ntrans})
    {:reply, ntrans, nstate}
  end

  def handle_call(:execute, _from, state) do
    {length_ran, new_state} = handle_execute(state)
    Router.cast(state.topic, {:executed, {:ok, length_ran}})
    {:reply, {:ok, length_ran}, new_state}
  end

  def handle_call(:pending_txs, _from, state) do
    {:reply, state.transactions, state}
  end

  def handle_cast(:soft_reset, _from, state) do
    kill_transactions(state.transactions, state.executor)
    {:noreply, reset_state(state)}
  end

  def handle_cast(:hard_reset, _from, state) do
    snapshot = Executor.snapshot(state.executor)
    kill_transactions(state.transactions, state.executor)
    reset_blocks(state)
    Ordering.hard_reset(state.ordering, snapshot)
    {:noreply, reset_state(state)}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_tx(Transaction.execution(), t()) :: Transaction.t()
  def handle_tx(tx_code, state) do
    random_tx_id = random_id()
    pid = Executor.fire_new_transaction(state.executor, random_tx_id, tx_code)
    Transaction.new(random_tx_id, pid, tx_code)
  end

  @spec handle_execute(t()) :: {non_neg_integer(), t()}
  def handle_execute(state) do
    {executing, left} = Enum.split(state.transactions, 100)
    block = produce_block(state, executing)
    choose_and_execute_ordering(state, executing)
    save_block(state, block)
    new_state = %Mempool{state | transactions: left, round: state.round + 1}
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
      order(transactions, Ordering.next_order(state.ordering))

    # send in the ordering for the write ready
    Ordering.new_order(state.ordering, ordered_transactions)

    # also send in the logic for write ready
    instrument({:write, length(ordered_transactions)})

    for ord <- ordered_transactions do
      send(Order.pid(ord), {:write_ready, Order.index(ord)})
    end

    :ok
  end

  @spec produce_block(t(), list(Transaction.t())) :: Block.t()
  def produce_block(state, trans) do
    trans
    |> Enum.map(&persistent_transaction/1)
    |> Base.new()
    |> Block.create(state.key, state.round)
  end

  @spec save_block(t(), Block.t()) :: {:atomic, :ok} | {:aborted, any()}
  def save_block(state, block) do
    encoded = Block.encode(block, state.block_storage)
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
    :mnesia.delete_table(state.block_storage)
    # TODO add a flag in storage for yes if we want rocksdb copies
    Block.create_table(state.block_storage, false)
  end

  @spec kill_transactions(list(Transaction.t()), Router.Addr.t()) :: :ok
  def kill_transactions(transactions, executor) do
    instrument({:kill, length(transactions)})

    for transaction <- transactions do
      instrument({:killing_pid, transaction})
    end

    Executor.kill_transactions(executor, transactions)
    :ok
  end

  @spec reset_state(t()) :: t()
  def reset_state(state) do
    %Mempool{state | transactions: [], round: 0}
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
end
