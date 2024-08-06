defmodule Anoma.Node.AbStorage do
  @moduledoc """
  abstorage genserver
  """

  use GenServer
  use TypedStruct

  @type bare_key() :: list(String.t())
  @type qualified_key() :: {integer(), bare_key()}

  # invariants this should enforce but does not:
  # - increasing timestamps
  #   - ideally, monotonic timestamps
  # - no writes to the "past"
  #   - uncommitted can sometimes be written out of order, though?
  # - committed/uncommitted distinction
  # - reads to "future" block rather than yielding an old value

  typedstruct enforce: true do
    field(:committed, %{qualified_key() => term()}, default: %{})
    field(:committed_updates, %{bare_key() => list(integer())}, default: %{})
    field(:uncommitted, %{qualified_key() => term()}, default: %{})

    field(:uncommitted_updates, %{bare_key() => list(integer())},
      default: %{}
    )
  end

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    {:ok, %__MODULE__{}}
  end

  def handle_call({:read, {time, key}}, _from, state) do
    real_time =
      Map.get(state.uncommitted_updates, key)
      |> Enum.find(fn a -> a <= time end)

    {:reply, Map.get(state.uncommitted, {real_time, key}), state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:write, {time, key}, value}, state) do
    key_old_updates = Map.get(state.uncommitted_updates, key, [])
    key_new_updates = [time | key_old_updates]
    new_updates = Map.put(state.uncommitted_updates, key, key_new_updates)
    new_kv = Map.put_new(state.uncommitted, {time, key}, value)

    new_state = %{
      state
      | uncommitted: new_kv,
        uncommitted_updates: new_updates
    }

    {:noreply, new_state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  def read({time, key}) do
    GenServer.call(__MODULE__, {:read, {time, key}})
  end

  def write({time, key}, value) do
    GenServer.cast(__MODULE__, {:write, {time, key}, value})
  end
end
