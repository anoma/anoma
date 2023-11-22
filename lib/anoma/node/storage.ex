defmodule Anoma.Node.Storage do
  @moduledoc """
  I am a simple mnesia-backed key-value store in an anoma node.
  """

  @dialyzer :no_improper_lists

  @behaviour GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, :rocksdb_copies} = Anoma.Mnesia.attach()
    # idempotent
    create_tables()
    :mnesia.subscribe({:table, Anoma.Node.Storage.Qualified, :simple})
    {:ok, %{next_order: 1, hash_to_order: %{}, subscribers: %{}}}
  end

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  def handle_call(:next_order, _from, state) do
    {:reply, state.next_order, state}
  end

  def handle_call({:true_order, id}, _from, state) do
    {:reply, Map.get(state.hash_to_order, id), state}
  end

  def handle_call({:new_order, ordered_transactions}, _from, state) do
    num_txs = length(ordered_transactions)
    IO.inspect(num_txs, label: "new tx count")

    for {index, {id, pid}} <- ordered_transactions do
      IO.inspect(pid, label: "sending read ready to pid")
      send(pid, {:read_ready, index})
    end

    new_next_order = state.next_order + length(ordered_transactions)
    new_map_elements = Map.new(ordered_transactions, fn {index, {id, pid}} -> {id, index} end)
    new_map = Map.merge(state.hash_to_order, new_map_elements)
    {:reply, :ok, %{state | next_order: new_next_order, hash_to_order: new_map}}
  end

  def handle_call({:get, key}, _from, state) do
    value = blocking_read(key)
    {:reply, value, state}
  end

  def handle_call(:reset, _from, _state) do
    {:reply, :ok, %{next_order: 1, hash_to_order: %{}, subscribers: %{}}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(msg, state) do
    IO.inspect(msg, label: "message to storage")
    {:noreply, state}
  end

  # translate from ids to true order
  def blocking_read_id([id | subkey]) do
    maybe_true_order = true_order(id)

    case maybe_true_order do
      nil ->
        IO.inspect({self(), id}, label: "waiting on read ready")

        receive do
          {:read_ready, true_order} ->
            read_order = true_order - 1
            full_key = [read_order | subkey]
            IO.inspect({self(), id, true_order}, label: "got read ready")
            IO.inspect(full_key, label: "getting at key")
            blocking_read(full_key)
        end

      true_order ->
        blocking_read([true_order - 1 | subkey])
    end
  end

  def blocking_read(key) do
    IO.inspect(key, label: "regular blocking read key")

    case key do
      [0 | _] ->
        :error

      [_ | _] ->
        :mnesia.subscribe({:table, Anoma.Node.Storage.Qualified, :simple})
        tx = fn -> :mnesia.read(Anoma.Node.Storage.Qualified, key) end
        {:atomic, result} = :mnesia.transaction(tx)

        case result do
          [{_, ^key, value}] ->
            {:ok, value}

          [] ->
            receive do
              {:mnesia_table_event, {:write, {Anoma.Node.Storage.Qualified, ^key, value}, _}} ->
                {:ok, value}
            end
        end

      _ ->
        :error
    end
  end

  def get(key) do
    maybe_order_tx = fn -> :mnesia.read(Anoma.Node.Storage.Order, key) end
    {:atomic, maybe_order} = :mnesia.transaction(maybe_order_tx)

    case maybe_order do
      [{_, ^key, order}] ->
        IO.inspect(order, label: "getting at order")
        value_tx = fn -> :mnesia.read(Anoma.Node.Storage.Qualified, [order, key | 0]) end
        {:atomic, [{_, [^order, ^key | 0], value}]} = :mnesia.transaction(value_tx)
        {:ok, value}

      [] ->
        :absent
    end
  end

  def put(key, value) do
    maybe_order_tx = fn -> :mnesia.read(Anoma.Node.Storage.Order, key) end
    {:atomic, maybe_order} = :mnesia.transaction(maybe_order_tx)

    new_order =
      case maybe_order do
        [{_, ^key, order}] ->
          order + 1

        [] ->
          1
      end

    IO.inspect(new_order, label: "putting at order")

    write_tx = fn ->
      :mnesia.write({Anoma.Node.Storage.Order, key, new_order})
      :mnesia.write({Anoma.Node.Storage.Qualified, [new_order, key | 0], value})
    end

    :mnesia.transaction(write_tx)
  end

  def create_tables() do
    # fully qualified key-value map
    :mnesia.create_table(Anoma.Node.Storage.Qualified, attributes: [:key, :value])
    # map from keys to qualified keys
    :mnesia.create_table(Anoma.Node.Storage.Order, attributes: [:key, :order])
  end

  def state() do
    GenServer.call(__MODULE__, :state)
  end

  def next_order() do
    GenServer.call(__MODULE__, :next_order)
  end

  def true_order(id) do
    GenServer.call(__MODULE__, {:true_order, id})
  end

  def new_order(ordered_transactions) do
    GenServer.call(__MODULE__, {:new_order, ordered_transactions})
  end

  def reset() do
    :mnesia.delete_table(Anoma.Node.Storage.Qualified)
    :mnesia.delete_table(Anoma.Node.Storage.Order)
    create_tables()
    GenServer.call(__MODULE__, :reset)
  end
end
