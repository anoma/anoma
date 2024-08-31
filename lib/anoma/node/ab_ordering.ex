defmodule Anoma.Node.AbOrdering do
  @moduledoc """
  abordering genserver
  """

  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.AbStorage

  typedstruct enforce: true do
    field(:next_height, integer(), default: 1)
    field(:order, %{binary() => integer()}, default: %{})
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, %AbOrdering{}}
  end

  def handle_call({:read, {id, key}}, _from, state) do
    height = Map.get(state.order, id)
    {:ok, value} = AbStorage.read({height, key})
    {:reply, :ok, value}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:write, {id, key}, value}, state) do
    height = Map.get(state.order, id)
    AbStorage.write({height, key}, value)
    {:noreply, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  def read({id, key}) do
    GenServer.call(__MODULE__, {:read, {id, key}}, :infinity)
  end

  def write({id, key}, value) do
    GenServer.cast(__MODULE__, {:write, {id, key}, value})
  end
end
