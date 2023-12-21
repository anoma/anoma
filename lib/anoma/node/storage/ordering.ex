defmodule Anoma.Node.Storage.Ordering do
  @moduledoc """
  I am a simple mnesia-backed key-value store in an anoma node.

  Currently we do not have a way of propagating what keys users want
  to store, thus we take the following approach to deterministic state
  reading:

    1. We keep a next_order, which represents the next_order a
       transaction will have. From here the scry reads specifically
       [next_order, :key_space | 0] to get a map of the current keys
       saved for this node.

    2. We keep a hash_to_order to cache the id => order mapping
  """

  use TypedStruct
  use GenServer
  alias Anoma.Node.Utility
  alias Anoma.{Storage, Order}
  alias __MODULE__

  @type ordered_transactions() ::
          list(Order.t())

  @type key() :: any()

  typedstruct do
    field(:table, Storage.t(), default: %Anoma.Storage{})
    field(:next_order, non_neg_integer(), default: 1)
    field(:hash_to_order, %{key() => non_neg_integer()}, default: %{})
  end

  def init(opts) do
    return = %Ordering{table: opts[:table]}
    # idempotent
    Storage.setup(return.table)
    :mnesia.subscribe({:table, return.table.qualified, :simple})
    {:ok, return}
  end

  @spec start_link(keyword()) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec state(GenServer.server()) :: t()
  def state(ordering) do
    GenServer.call(ordering, :state)
  end

  @spec next_order(GenServer.server()) :: non_neg_integer()
  def next_order(ordering) do
    GenServer.call(ordering, :next_order)
  end

  @spec true_order(GenServer.server(), any()) :: non_neg_integer() | nil
  def true_order(ordering, id) do
    GenServer.call(ordering, {:true_order, id})
  end

  @spec new_order(GenServer.server(), ordered_transactions()) ::
          :error | {:ok, any()}
  def new_order(ordering, ordered_transactions) do
    new_order(ordering, ordered_transactions, false)
  end

  @spec new_order(GenServer.server(), ordered_transactions(), boolean()) ::
          :error | {:ok, any()}
  def new_order(ordering, ordered, instrumentation) do
    GenServer.call(ordering, {:new_order, ordered, instrumentation})
  end

  @spec reset(GenServer.server()) :: :ok
  def reset(ordering) do
    GenServer.cast(ordering, :reset)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  def handle_call(:next_order, _from, state) do
    {:reply, state.next_order, state}
  end

  def handle_call({:true_order, id}, _from, state) do
    {:reply, Map.get(state.hash_to_order, id), state}
  end

  def handle_call({:new_order, trans, instrumentation}, _from, state) do
    {next_order, new_map} = handle_new_order(trans, state, instrumentation)

    {:reply, :ok, %{state | next_order: next_order, hash_to_order: new_map}}
  end

  def handle_cast(:reset, state) do
    {:noreply, %Ordering{table: state.table}}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_new_order(ordered_transactions(), t(), boolean()) ::
          {non_neg_integer(), %{key() => non_neg_integer()}}
  def handle_new_order(ordered_transactions, state, instrumentation) do
    num_txs = length(ordered_transactions)
    instrument(instrumentation, {:new_tx, num_txs})

    for order <- ordered_transactions do
      instrument(instrumentation, {:ready, Order.pid(order)})
      send(Order.pid(order), {:read_ready, Order.index(order)})
    end

    new_next_order = state.next_order + length(ordered_transactions)

    new_map_elements =
      Map.new(ordered_transactions, &{Order.id(&1), Order.index(&1)})

    new_map = Map.merge(state.hash_to_order, new_map_elements)
    {new_next_order, new_map}
  end

  ############################################################
  #                      Instrumentation                     #
  ############################################################
  def instrument(instrument, {:new_tx, num_txs}) do
    if instrument, do: IO.inspect(num_txs, label: "new tx count")
  end

  def instrument(instrument, {:ready, pid}) do
    if instrument, do: IO.inspect(pid, label: "sending read ready to pid")
  end
end
