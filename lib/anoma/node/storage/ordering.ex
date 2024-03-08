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
  alias Anoma.Node.{Router, Logger}
  alias Anoma.{Storage, Order}
  alias __MODULE__

  @type ordered_transactions() ::
          list(Order.t())

  @type key() :: any()

  typedstruct do
    field(:table, Storage.t(), default: %Anoma.Storage{})
    field(:next_order, non_neg_integer(), default: 1)
    field(:hash_to_order, %{key() => non_neg_integer()}, default: %{})
    field(:logger, Router.Addr.t(), enforce: false)
  end

  def init(opts) do
    return = %Ordering{
      table: opts[:table],
      logger: opts[:logger]
    }

    # idempotent
    Storage.setup(return.table)
    :mnesia.subscribe({:table, return.table.qualified, :simple})
    {:ok, return}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec state(Router.Addr.t()) :: t()
  def state(ordering) do
    Router.call(ordering, :state)
  end

  @spec next_order(Router.Addr.t()) :: non_neg_integer()
  def next_order(ordering) do
    Router.call(ordering, :next_order)
  end

  @spec true_order(Router.Addr.t(), any()) :: non_neg_integer() | nil
  def true_order(ordering, id) do
    Router.call(ordering, {:true_order, id})
  end

  @spec new_order(Router.Addr.t(), ordered_transactions()) ::
          :error | {:ok, any()}
  def new_order(ordering, ordered) do
    Router.call(ordering, {:new_order, ordered})
  end

  @spec get_storage(Router.Addr.t()) :: Storage.t()
  def get_storage(ordering) do
    Router.call(ordering, :storage)
  end

  @spec reset(Router.Addr.t()) :: :ok
  def reset(ordering) do
    Router.cast(ordering, :reset)
  end

  @spec hard_reset(Router.Addr.t(), atom()) :: :ok
  def hard_reset(ordering, initial_snapshot) do
    Router.cast(ordering, {:hard_reset, initial_snapshot})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    log_info({:state, state, state.logger})

    {:reply, state, state}
  end

  def handle_call(:next_order, _from, state) do
    next_order = state.next_order
    log_info({:next, next_order, state.logger})

    {:reply, next_order, state}
  end

  def handle_call({:true_order, id}, _from, state) do
    order = Map.get(state.hash_to_order, id)
    log_info({true, order, state.logger})

    {:reply, order, state}
  end

  def handle_call({:new_order, trans}, _from, state) do
    {next_order, new_map} = handle_new_order(trans, state)
    log_info({:new, next_order, new_map, state.logger})

    {:reply, :ok, %{state | next_order: next_order, hash_to_order: new_map}}
  end

  def handle_call(:storage, _from, state) do
    table = state.table
    log_info({:storage, table, state.logger})

    {:reply, table, state}
  end

  def handle_cast(:reset, _from, state) do
    table = state.table
    log_info({:reset, table, state.logger})

    {:noreply, %Ordering{table: table}}
  end

  def handle_cast({:hard_reset, initial_snapshot}, _from, state) do
    storage = state.table
    Storage.ensure_new(storage)
    Storage.put_snapshot(storage, initial_snapshot)

    log_info({:hard_reset, storage, initial_snapshot, state.logger})

    {:noreply, %Ordering{table: state.table}}
  end

  ############################################################
  #                    Caller Blocking API                   #
  ############################################################

  # translate from ids to true order
  @doc """
  Translate from ids to true order

  ### Parameters
    - `ordering` - the ordering process

    - `[id | subkey]` -

     the process identification for consistent reads, and a subkey for
     the storage key. This is akin to the `Storage.qualified_key` in
     `Storage.blocking_read/2`

  ### Returns
  returns the given key at a specific value
  """
  @spec caller_blocking_read_id(Router.Addr.t(), Noun.t()) ::
          :error | {:ok, any()}
  def caller_blocking_read_id(ordering, [id | subkey]) do
    maybe_true_order = true_order(ordering, id)
    storage = get_storage(ordering)

    read_order =
      case maybe_true_order do
        nil ->
          receive do
            {:read_ready, true_order} ->
              true_order
          end

        true_order ->
          true_order
      end

    full_key = [read_order | subkey]
    Storage.blocking_read(storage, full_key)
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_new_order(ordered_transactions(), t()) ::
          {non_neg_integer(), %{key() => non_neg_integer()}}
  def handle_new_order(ordered_transactions, state) do
    num_txs = length(ordered_transactions)
    log_info({:new_handle, num_txs, state.logger})

    for order <- ordered_transactions do
      log_info({:ready_handle, Order.pid(order), state.logger})
      send(Order.pid(order), {:read_ready, Order.index(order)})
    end

    new_next_order = state.next_order + length(ordered_transactions)

    new_map_elements =
      Map.new(ordered_transactions, &{Order.id(&1), Order.index(&1)})

    new_map = Map.merge(state.hash_to_order, new_map_elements)
    {new_next_order, new_map}
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  # Keeping usual logging above for now

  defp log_info({:state, state, logger}) do
    Logger.add(
      logger,
      :info,
      "Requested state: #{inspect(state)}"
    )
  end

  defp log_info({:next, state, logger}) do
    Logger.add(
      logger,
      :info,
      "Requested next order: #{inspect(state)}"
    )
  end

  defp log_info({true, state, logger}) do
    Logger.add(
      logger,
      :info,
      "Requested true order: #{inspect(state)}"
    )
  end

  defp log_info({:new, order, map, logger}) do
    Logger.add(logger, :info, "Requested new order.
      Next order: #{inspect(order)}. New hash: #{inspect(map)}")
  end

  defp log_info({:storage, state, logger}) do
    Logger.add(
      logger,
      :info,
      "Requested storage table: #{inspect(state)}"
    )
  end

  defp log_info({:reset, state, logger}) do
    Logger.add(
      logger,
      :debug,
      "Requested reset. Table: #{inspect(state)}"
    )
  end

  defp log_info({:hard_reset, table, snap, logger}) do
    Logger.add(logger, :debug, "Requested hard reset.
      Table: #{inspect(table)}. Snapshot: #{inspect(snap)}")
  end

  defp log_info({:new_handle, state, logger}) do
    Logger.add(logger, :info, "New tx count: #{inspect(state)}")
  end

  defp log_info({:ready_handle, state, logger}) do
    Logger.add(
      logger,
      :info,
      "Sending read ready to: #{inspect(state)}"
    )
  end
end
