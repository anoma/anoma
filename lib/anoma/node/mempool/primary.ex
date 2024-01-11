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

  typedstruct do
    field(:ordering, GenServer.server(), require: true)
    field(:executor, GenServer.server(), require: true)
    field(:block_storage, atom(), default: Anoma.Block)
    field(:transactions, list(Transaction.t()), default: [])
    field(:round, non_neg_integer(), default: 0)

    field(:key, {Serializer.public_key(), Serializer.private_key()},
      default: :crypto.generate_key(:rsa, {1024, 65537})
    )
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: args[:name])
  end

  def init(args) do
    primary =
      Map.merge(%Primary{}, args |> Enum.into(%{}))
      |> Map.delete(:name)

    # TODO add a flag in storage for yes if we want rocksdb copies
    Block.create_table(primary.block_storage, false)
    {:ok, primary}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec execute(GenServer.server()) :: non_neg_integer()
  def execute(server) do
    GenServer.call(server, :execute)
  end

  @spec tx(GenServer.server(), Noun.t()) :: Transaction.t()
  def tx(server, tx_code) do
    GenServer.call(server, {:tx, tx_code})
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

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:tx, tx_code}, _from, state) do
    ntrans = handle_tx(tx_code, state)
    nstate = %Primary{state | transactions: [ntrans | state.transactions]}
    {:reply, ntrans, nstate}
  end

  def handle_call(:execute, _from, state) do
    {length_ran, new_state} = handle_execute(state)
    {:reply, {:ok, length_ran}, new_state}
  end

  def handle_cast(:soft_reset, state) do
    kill_transactions(state.transactions)
    {:noreply, reset_state(state)}
  end

  def handle_cast(:hard_reset, state) do
    snapshot = Ecom.snapshot(state.executor)
    kill_transactions(state.transactions)
    reset_blocks(state)
    Scom.hard_reset(state.ordering, snapshot)
    {:noreply, reset_state(state)}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_tx(Noun.t(), t()) :: Transaction.t()
  def handle_tx(tx_code, state) do
    random_tx_id = random_id()
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
    Scom.new_order(state.ordering, ordered_transactions)

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

  @spec kill_transactions(list(Transaction.t())) :: :ok
  def kill_transactions(transactions) do
    instrument({:kill, length(transactions)})

    for transaction <- transactions do
      instrument({:killing_pid, transaction})
      Process.exit(Transaction.pid(transaction), :kill)
    end

    :ok
  end

  @spec reset_state(t()) :: t()
  def reset_state(state) do
    %Primary{state | transactions: [], round: 0}
  end

  @spec persistent_transaction(Transaction.t()) :: {Noun.t(), Noun.t()}
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
