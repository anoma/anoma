defmodule Anoma.Node.AbMempool do
  @moduledoc """
  abmempool genserver
  """

  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.AbStorage
  alias Anoma.Node.AbOrdering

  # placeholder transaction; read at key, write key/value
  @type transaction() :: {AbStorage.bare_key(), term()}
  @type result() :: term()

  typedstruct enforce: true do
    field(:pool, %{binary() => {transaction(), result()}}, default: %{})
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, %AbMempool{}}
  end

  def handle_call(:txs, _from, state) do
    {:reply, :ok, state.pool}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:submit, tx}, state) do
    tx_id = :crypto.strong_rand_bytes(16)
    worker = Task.start(fn -> ab_worker(self(), tx_id, tx) end)
    new_pool = Map.put(state.pool, tx_id, {tx, worker})
    {:noreply, %{state | pool: new_pool}}
  end

  def handle_cast(:clear, _state) do
    # todo: logic to make sure this only clears out completed txs with true as value here
    {:noreply, %__MODULE__{pool: %{}}}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info({:finished, tx_id}, state) do
    {tx, _worker} = Map.get(state.pool, tx_id)
    new_pool = Map.put(state.pool, tx_id, {tx, true})
    {:noreply, %{state | pool: new_pool}}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  # fixme use a result event instead
  def ab_worker(pid, id, _tx = {key, value}) do
    _old_value = AbOrdering.read({id, key})
    AbOrdering.write({id, key}, value)
    send(pid, {:finished, id})
  end

  def submit(tx) do
    GenServer.cast(__MODULE__, {:submit, tx})
  end
end
