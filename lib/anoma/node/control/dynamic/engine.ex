defmodule Anoma.Node.Control.Dynamic.Engine do
  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Storage
  alias Anoma.Node.Utility

  typedstruct do
    field(:storage, GenServer.server())
  end

  def init(arg) do
    {:ok, %Engine{storage: arg[:storage]}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  def get(engine, key) do
    GenServer.call(engine, {:get, key})
  end

  def set(engine, key, value) do
    GenServer.call(engine, {:set, key, value})
  end

  # Since storage is persistent this currently sets the value to 0
  def delete(engine, key) do
    GenServer.call(engine, {:delete, key})
  end

  def handle_call({:get, key}, _from, engine) do
    {:reply, Storage.get(engine.storage, [:dynamic, key]), engine}
  end

  def handle_call({:set, key, value}, _from, engine) do
    {:reply, Storage.put(engine.storage, [:dynamic, key], value), engine}
  end

  def handle_call({:delete, key}, _from, engine) do
    with {:ok, _value} <- Storage.get(engine.storage, key) do
      {:reply, Storage.put(engine.storage, [:dynamic, key], 0), engine}
    else
      :absent -> {:reply, :absent, engine}
    end
  end
end
