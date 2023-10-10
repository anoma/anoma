defmodule Anoma.Node.Mempool.Pool do
  @moduledoc """

  """

  use TypedStruct
  use GenServer

  alias __MODULE__
  alias Anoma.Transaction
  alias Anoma.Node.Utility

  typedstruct do
    field(:pool, :queue.queue(Transaction.t()), default: :queue.new())
  end

  @type transactions() :: Enumerable.t(Transaction.t())

  def init(_init) do
    {:ok, %Pool{}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  Submit a new transaction to the transaction pool
  """
  @spec new_transactions(GenServer.server(), transactions()) :: :ok
  def new_transactions(pool, transactions) do
    GenServer.cast(pool, {:new_transactions, transactions})
  end

  # TODO We should verify who sent this message
  @doc """
  Eject transactions from the mempool that failed validation
  """
  @spec eject(GenServer.server(), transactions()) :: :ok
  def eject(pool, transactions) do
    GenServer.cast(pool, {:eject, transactions})
  end

  @doc """
  Take the first `n` blocks from the mempool
  """
  @spec take(GenServer.server(), pos_integer()) :: list()
  def take(pool, n) do
    GenServer.call(pool, {:take, n})
  end

  @doc """
  Peek at the first `n` blocks from the mempool
  """
  @spec peek(GenServer.server(), pos_integer()) :: list()
  def peek(pool, n) do
    GenServer.call(pool, {:peek, n})
  end

  @doc """
  Dump the state of the node.
  """
  @spec dump(GenServer.server()) :: t()
  def dump(pool) do
    GenServer.call(pool, :dump)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:new_transactions, t}, pool) do
    {:noreply, handle_new_transactions(pool, t)}
  end

  def handle_cast({:eject, transactions}, pool) do
    {:noreply, handle_eject(pool, transactions)}
  end

  def handle_call({:take, n}, _from, pool) do
    {q1, q_rest} = safe_split(pool.pool, n)
    {:reply, :queue.to_list(q1), %Pool{pool | pool: q_rest}}
  end

  def handle_call({:peek, n}, _from, pool) do
    {q1, _} = safe_split(pool.pool, n)
    {:reply, :queue.to_list(q1), pool}
  end

  def handle_call(:dump, _from, pool) do
    {:reply, pool, pool}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_new_transactions(t(), transactions()) :: t()
  defp handle_new_transactions(pool, transactions) do
    transaction_queue =
      transactions
      |> Enum.to_list()
      |> :queue.from_list()

    %Pool{pool | pool: :queue.join(pool.pool, transaction_queue)}
  end

  @spec handle_eject(t(), transactions()) :: t()
  defp handle_eject(pool, transactions) do
    transaction_set = MapSet.new(transactions)

    in_set = fn x -> not MapSet.member?(transaction_set, x) end

    %Pool{pool | pool: :queue.filter(in_set, pool.pool)}
  end

  # Like split but we don't crash!
  @spec safe_split(q1, non_neg_integer()) :: {q2, q3}
        when q1: :queue.queue(term),
             q2: :queue.queue(term),
             q3: :queue.queue(term)
  defp safe_split(pool, amount) do
    try do
      :queue.split(amount, pool)
    rescue
      _ -> {pool, :queue.new()}
    end
  end
end
