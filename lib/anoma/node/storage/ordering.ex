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

  There are two ways to work with me. One is through my communicator
  (Calls to `Anoma.Node.Storage.Communicator`). These will be computed
  on my `GenServer` process. The other way is directly through me in
  my `User blocking API` (done through `Anoma.Node.Storage.Ordering`),
  these will block the caller's process.

  To try to make this extra explicit, caller blocking functions have
  `caller_blocking` prepended to their name. For these functions, you
  should pass the communicator process and not my own, as you should
  always go through my communicator

  ### API (Use me through my Communicator!)
  ### Caller Blocking API
    - `caller_blocking_read_id/2`
    - `caller_blocking_read_id/3`
  """

  use TypedStruct
  use GenServer
  alias Anoma.Node.Storage.Communicator
  alias Anoma.Node.Utility
  alias Anoma.{Storage, Order}
  alias __MODULE__

  require Logger

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
  def new_order(ordering, ordered) do
    GenServer.call(ordering, {:new_order, ordered})
  end

  @spec get_storage(GenServer.server()) :: Storage.t()
  def get_storage(ordering) do
    GenServer.call(ordering, :storage)
  end

  @spec reset(GenServer.server()) :: :ok
  def reset(ordering) do
    GenServer.cast(ordering, :reset)
  end

  @spec hard_reset(GenServer.server(), atom()) :: :ok
  def hard_reset(ordering, initial_snapshot) do
    GenServer.cast(ordering, {:hard_reset, initial_snapshot})
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

  def handle_call({:new_order, trans}, _from, state) do
    {next_order, new_map} = handle_new_order(trans, state)

    {:reply, :ok, %{state | next_order: next_order, hash_to_order: new_map}}
  end

  def handle_call(:storage, _from, state) do
    {:reply, state.table, state}
  end

  def handle_cast(:reset, state) do
    {:noreply, %Ordering{table: state.table}}
  end

  def handle_cast({:hard_reset, initial_snapshot}, state) do
    storage = state.table
    Storage.ensure_new(storage)
    Storage.put_snapshot(storage, initial_snapshot)
    {:noreply, %Ordering{table: state.table}}
  end

  ############################################################
  #                    Caller Blocking API                   #
  ############################################################

  # translate from ids to true order
  @doc """
  Translate from ids to true order

  ### Parameters
    - `coms` - the communicator process

    - `[id | subkey]` -

     the process identification for consistent reads, and a subkey for
     the storage key. This is akin to the `Storage.qualified_key` in
     `Storage.blocking_read/2`

  ### Returns
  returns the given key at a specific value
  """
  @spec caller_blocking_read_id(GenServer.server(), Noun.t()) ::
          :error | {:ok, any()}
  def caller_blocking_read_id(coms, [id | subkey]) do
    maybe_true_order = Communicator.true_order(coms, id)
    storage = Communicator.get_storage(coms)

    read_order =
      case maybe_true_order do
        nil ->
          instrument({:waiting, {self(), id}})

          receive do
            {:read_ready, true_order} ->
              instrument({:read_ready, {self(), id, true_order}})

              true_order
          end

        true_order ->
          true_order
      end

    full_key = [read_order | subkey]
    instrument({:getting_key, full_key})
    Storage.blocking_read(storage, full_key)
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_new_order(ordered_transactions(), t()) ::
          {non_neg_integer(), %{key() => non_neg_integer()}}
  def handle_new_order(ordered_transactions, state) do
    num_txs = length(ordered_transactions)
    instrument({:new_tx, num_txs})

    for order <- ordered_transactions do
      instrument({:ready, Order.pid(order)})
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
  def instrument({:new_tx, num_txs}) do
    Logger.info("New tx count: #{inspect(num_txs)}")
  end

  def instrument({:ready, pid}) do
    Logger.info("sending read ready to #{inspect(pid)}")
  end

  def instrument({:waiting, id}) do
    Logger.info("#{inspect(id)}, Waiting on read ready")
  end

  def instrument({:read_ready, info}) do
    Logger.info("#{inspect(info)}, got read ready")
  end

  def instrument({:getting_key, full_key}) do
    Logger.info("getting at: #{inspect(full_key)}")
  end
end
