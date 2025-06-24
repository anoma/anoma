defmodule Anoma.LocalDomain.Storage do
  @moduledoc """
  Local storage subsystem. Timestamped, but not in an integrity-requiring way
  very much.

  Writes to all subspaces, but the main API writes to /anoma/local/[local id]/.
  """

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    field(:table, reference())
    # last written time
    field(:time, non_neg_integer(), default: 0)
  end

  def start_link() do
    # just one for now. todo: local ids
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  @doc """
  Writes to /anoma/local/[local id]/ at the current time.
  """
  def write_local(key, value) when is_list(key) do
    GenServer.cast(__MODULE__, {:write, key, value})
  end

  @doc """
  Writes to any possible key, including timestamp. For populating controller
  value cache. Does not update local time (it's intended for non-local values).
  """
  def write_any(full_key, value) when is_list(full_key) do
    GenServer.cast(__MODULE__, {:write_any, full_key, value})
  end

  @doc """
  Reads from any possible key.
  """
  def read(full_key) when is_list(full_key) do
    GenServer.call(__MODULE__, {:read, full_key})
  end

  @doc """
  Reads from /anoma/local/[local id]/ at the current time.
  """
  def read_local(key) when is_list(key) do
    GenServer.call(__MODULE__, {:read_local, key})
  end

  @doc """
  Reads from any possible key, blocking if neither a value nor :absent.
  """
  def read_and_block(full_key) when is_list(full_key) do
    GenServer.call(__MODULE__, {:read_and_block, full_key}, :infinity)
  end

  # callbacks

  @impl true
  def init(_arg) do
    # todo: set this up with a real backend
    table = :ets.new(__MODULE__, [])
    {:ok, %__MODULE__{table: table}}
  end

  @impl true
  def handle_call({:read, full_key}, _from, state) do
    with [{^full_key, value}] <- :ets.lookup(state.table, full_key) do
      {:reply, value, state}
    else
      e -> {:reply, {:error, e}, state}
    end
  end

  @impl true
  def handle_call({:read_local, key}, _from, state) do
    # prefix the key
    key = [
      "anoma", "local",
      Atom.to_string(__MODULE__),
      Integer.to_string(state.time)
    ] ++ key

    with [{^key, value}] <- :ets.lookup(state.table, key) do
      {:reply, value, state}
    else
      e -> {:reply, {:error, e}, state}
    end
  end

  @impl true
  def handle_call({:read_and_block, _full_key}, _from, state) do
    # hangs caller forever until implemented. this is still semantically correct
    {:noreply, state}
  end

  @impl true
  def handle_cast({:write, key, value}, state) do
    # prefix the key
    key = [
      "anoma", "local",
      Atom.to_string(__MODULE__),
      Integer.to_string(state.time + 1)
    ] ++ key

    :ets.insert(state.table, {key, value})

    {:noreply, %{state | time: state.time + 1}}
  end

  @impl true
  def handle_cast({:write_any, full_key, value}, state) do
    :ets.insert(state.table, {full_key, value})
    {:noreply, state}
  end
end
