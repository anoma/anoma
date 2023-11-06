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
    {:ok, %{}}
  end

  def handle_call({:get, key}, _from, state) do
    value = blocking_read(key)
    {:reply, value, state}
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

  def blocking_read(key) do
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

  def reset() do
    :mnesia.delete_table(Anoma.Node.Storage.Qualified)
    :mnesia.delete_table(Anoma.Node.Storage.Order)
    create_tables()
  end
end
