defmodule Anoma.Storage do
  @moduledoc """
  I am the Anoma Storage engine, I consist of two parts:
     1. An ordering map which tells of the latest order in the
        qualification map
     2. A qualification map which maps from a qualified key to the stored
        value

  ## Types

    A good way to view this is that for the `t()`, the fields for what
    is stored in mnesia, are simply the order_* and qualified_* values

    type `t Anoma.Storage` to find them all.

    Please also type `t Anoma.Storage.t()` to find out more about the
    central type
  ## API
  The important functions for this API are
     - `setup/1`
     - `ensure_new/1`
     - `get/2`
     - `get/3`
     - `put/3`
     - `put/4`
     - `blocking_read/2`
     - `blocking_read/3`

  If one wants to query the tables by hand then there are manual
  functions, but beware, this is an unintended way of using the API
     - `read_order/2`
     - `read_at_order/3`
     - `write_at_order/4`


  Please see my testing module `AnomaTest.Storage` to learn more on
  how to use me

  ### Snapshots
  One can snapshot the keys provided in the code by running the following

    - `snapshot_order/1`
    - `put_snapshot/2`
    - `in_snapshot/2`
    - `get_at_snapshot/2`
  """

  use TypedStruct

  @typedoc """
  I represent the qualified and ordered data of storage

  ## Fields
    - `:qualified` - The key value value map into storage
    - `:order` - A mapping from keys to the properly qualified keys
  """
  typedstruct do
    field(:qualified, atom(), default: Anoma.Node.Storage.Qualified)
    field(:order, atom(), default: Anoma.Node.Storage.Order)
  end

  @type result(res) :: {:atomic, res} | {:aborted, any()}

  @typedoc """
  The key we wish to store, also used for order lookup
  """
  @type order_key() :: Noun.t()
  @type order_value() :: list({any(), Noun.t(), non_neg_integer()})

  @typedoc """
  [non_neg_integer(), Noun.t() | non_neg_integer()]
  """
  @type qualified_key() :: nonempty_improper_list(any(), non_neg_integer())
  @type qualified_value() :: any()

  @type snapshot() :: {t(), list({order_key(), non_neg_integer()})}

  ############################################################
  #                       Creation API                       #
  ############################################################

  @doc """
  I setup storage with the given tables: `t()`.

  I will try to setup all values of storage, even if the first one
  fails due to already being setup, we will try the others.
  """
  @spec setup(t()) :: :ok | {:aborted, any()}
  # If this ever gets big, turn it into a map filter situation
  def setup(t) do
    case {:mnesia.create_table(t.qualified, attributes: [:key, :value]),
          :mnesia.create_table(t.order, attributes: [:key, :order])} do
      {{:atomic, :ok}, {:atomic, :ok}} -> :ok
      {a, {:atomic, :ok}} -> a
      {_a____________, b} -> b
    end
  end

  @spec remove(t()) :: :ok | {:aborted, any()}
  def remove(storage) do
    :mnesia.delete_table(storage.qualified)
    :mnesia.delete_table(storage.order)
  end

  @spec ensure_new(t()) :: :ok | {:aborted, any()}
  def ensure_new(storage) do
    # We don't care about errors that can happen here
    remove(storage)
    setup(storage)
  end

  ############################################################
  #                        Operations                        #
  ############################################################

  @spec get(t(), order_key()) :: :absent | {:ok, qualified_value()}
  def get(storage, key) do
    get(storage, key, false)
  end

  @spec get(t(), order_key(), boolean()) :: :absent | {:ok, qualified_value()}
  def get(storage, key, instrumentation) do
    with {:atomic, [{_, ^key, order}]} <- read_order(storage, key),
         {:atomic, [{_, [^order, ^key | 0], value}]} <-
           read_at_order(storage, key, order) do
      instrument(instrumentation, {:get_order, order})
      {:ok, value}
    else
      _ -> :absent
    end
  end

  @spec put(t(), order_key(), qualified_value()) :: result(:ok)
  def put(storage, key, value) do
    put(storage, key, value, false)
  end

  @spec put(t(), order_key(), qualified_value(), boolean()) :: result(:ok)
  def put(storage, key, value, instrumentation) do
    with {:atomic, order} <- read_order(storage, key),
         new_order = calculate_order(order),
         {:atomic, :ok} <- write_at_order(storage, key, value, new_order) do
      instrument(instrumentation, {:put_order, new_order})
      {:atomic, :ok}
    end
  end

  @spec blocking_read(t(), qualified_key()) :: :error | {:ok, any()}
  def blocking_read(storage, key) do
    blocking_read(storage, key, false)
  end

  @spec blocking_read(t(), qualified_key(), boolean()) ::
          :error | {:ok, any()}
  def blocking_read(storage, key, instrumentation) do
    instrument(instrumentation, {:read, key})

    case key do
      [0 | _] ->
        :error

      [_ | _] ->
        :mnesia.subscribe({:table, storage.qualified, :simple})
        tx = fn -> :mnesia.read(storage.qualified, key) end
        {:atomic, result} = :mnesia.transaction(tx)

        case result do
          [{_, ^key, value}] ->
            {:ok, value}

          [] ->
            receive do
              {:mnesia_table_event, {:write, {_, ^key, value}, _}} ->
                {:ok, value}
            end
        end

      _ ->
        :error
    end
  end

  ############################################################
  #                         Snapshots                        #
  ############################################################

  @spec snapshot_order(t()) :: result(snapshot())
  def snapshot_order(storage) do
    :mnesia.transaction(fn ->
      snapshot = [{{:"$1", :"$2", :"$3"}, [], [{{:"$2", :"$3"}}]}]
      {storage, :mnesia.select(storage.order, snapshot)}
    end)
  end

  @spec put_snapshot(t(), order_key()) :: result(:ok)
  def put_snapshot(storage, key) do
    with {:atomic, snapshot} <- snapshot_order(storage) do
      put(storage, key, snapshot)
    end
  end

  @spec in_snapshot(snapshot(), order_key()) :: nil | non_neg_integer()
  def in_snapshot({_, snapshot}, key) do
    List.keyfind(snapshot, key, 0, {nil, nil})
    |> elem(1)
  end

  @spec get_at_snapshot(snapshot(), order_key()) ::
          :absent | {:ok, qualified_value()}
  def get_at_snapshot(snapshot = {storage, _}, key) do
    position = in_snapshot(snapshot, key)

    with {:atomic, [{_, [^position, ^key | 0], value}]} <-
           read_at_order(storage, key, position) do
      {:ok, value}
    else
      _ -> :absent
    end
  end

  ############################################################
  #                          Queries                         #
  ############################################################

  @spec read_order(t(), order_key()) ::
          result(list({atom(), Noun.t(), non_neg_integer()}))
  def read_order(storage, key) do
    order_tx = fn -> :mnesia.read(storage.order, key) end
    :mnesia.transaction(order_tx)
  end

  @spec read_at_order(t(), Noun.t(), non_neg_integer()) ::
          result(list({atom(), qualified_key(), qualified_value()}))
  def read_at_order(storage, key, order) do
    value_tx = fn -> :mnesia.read(storage.qualified, [order, key | 0]) end
    :mnesia.transaction(value_tx)
  end

  @spec write_at_order(t(), Noun.t(), Noun.t(), non_neg_integer()) ::
          result(:ok)
  def write_at_order(storage, key, value, order) do
    write_tx = fn ->
      :mnesia.write({storage.order, key, order})

      :mnesia.write({storage.qualified, [order, key | 0], value})
    end

    :mnesia.transaction(write_tx)
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  @spec calculate_order([{any(), any(), number()}]) :: number()
  def calculate_order([{_, _, order}]), do: order + 1
  def calculate_order([]), do: 1

  ############################################################
  #                      Instrumentation                     #
  ############################################################
  def instrument(instrument, {:get_order, order}) do
    if instrument, do: IO.inspect(order, label: "getting at order")
  end

  def instrument(instrument, {:put_order, order}) do
    if instrument, do: IO.inspect(order, label: "putting at order")
  end

  def instrument(instrument, {:read, key}) do
    if instrument, do: IO.inspect(key, label: "regular blocking read key")
  end
end
