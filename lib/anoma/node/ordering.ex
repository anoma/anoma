defmodule Anoma.Node.Ordering do
  @moduledoc """
  Dummy ordering service.
  """

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, %{}}
  end

  def handle_call(:execute, _from, state) do
    # order the pending transactions
    next_order = Anoma.Node.Storage.next_order()
    ordered_transactions = order(state, next_order)
    number_of_transactions = length(ordered_transactions)
    IO.inspect(ordered_transactions, label: "ordered transaction set")
    :ok = Anoma.Node.Storage.new_order(ordered_transactions)

    for {index, {id, pid}} <- ordered_transactions do
      IO.inspect(pid, label: "sending write ready to pid")
      send(pid, {:write_ready, index})
    end

    {:reply, {:ok, number_of_transactions}, %{}}
  end

  def handle_call({:execute_manual, tx_list}, _from, state) do
    next_order = Anoma.Node.Storage.next_order()
    tx_pid_list = Enum.map(tx_list, fn tx -> spawn_with_random_id(tx) end)

    ordered_transactions =
      Enum.with_index(tx_pid_list, fn element, index -> {index + next_order, element} end)

    number_of_transactions = length(ordered_transactions)
    IO.inspect(ordered_transactions, label: "ordered transaction set")
    :ok = Anoma.Node.Storage.new_order(ordered_transactions)

    for {index, {id, pid}} <- ordered_transactions do
      IO.inspect(pid, label: "sending write ready to pid")
      send(pid, {:write_ready, index})
    end

    {:reply, {:ok, number_of_transactions}, state}
  end

  def handle_call(:pending, _from, state) do
    {:reply, {:ok, state}, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:tx, tx_code}, state) do
    random_tx_id = random_id()
    worker_pid = spawn(Anoma.Node.Worker, :run, [random_tx_id, tx_code])
    {:noreply, Map.put(state, random_tx_id, worker_pid)}
  end

  def handle_cast(:reset, state) do
    for pid <- Map.values(state) do
      Process.exit(pid, :kill)
    end

    {:noreply, %{}}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def spawn_with_random_id(tx_code) do
    id = random_id()
    pid = spawn(Anoma.Node.Worker, :run, [id, tx_code])
    {id, pid}
  end

  def order(tx_map, next_order) do
    Map.to_list(tx_map)
    |> Enum.shuffle()
    |> Enum.with_index(fn element, index -> {index + next_order, element} end)
  end

  def execute() do
    GenServer.call(__MODULE__, :execute)
  end

  def execute_manual(tx_list) do
    GenServer.call(__MODULE__, {:execute_manual, tx_list})
  end

  def pending() do
    GenServer.call(__MODULE__, :pending)
  end

  def tx(tx_code) do
    GenServer.cast(__MODULE__, {:tx, tx_code})
  end

  def reset() do
    GenServer.cast(__MODULE__, :reset)
  end

  # 128-bit random id
  def random_id() do
    :crypto.strong_rand_bytes(16)
  end
end
