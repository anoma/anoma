defmodule Anoma.Node.Control.Dynamic.Communicator do
  alias __MODULE__

  use TypedStruct
  use GenServer

  alias Anoma.Node.Control
  alias Anoma.Node.Control.Dynamic.Engine
  alias Anoma.Node.Utility

  typedstruct do
    field(:engine, GenServer.server(), enforce: true)

    field(:subscribers, MapSet.t({GenServer.server(), atom()}),
      default: MapSet.new()
    )
  end

  def init(args) do
    {:ok, %Communicator{engine: args[:name]}}
  end

  def start_link(arg) do
    GenServer.start_link(
      __MODULE__,
      arg,
      Utility.name(arg, &Utility.com_name/1)
    )
  end

  defdelegate get(engine, key), to: Engine
  defdelegate set(engine, key, value), to: Engine
  defdelegate delete(engine, key), to: Engine

  def subscribe(communicator, subscriber, key) do
    GenServer.cast(communicator, {:subscribe, subscriber, key})
  end

  def handle_cast({:subscriber, subscriber, key}, agent) do
    {:noreply,
     %Communicator{
       agent
       | subscribers: MapSet.put(agent.subscribers, {subscriber, key})
     }}
  end

  def handle_call({:get, key}, _from, state) do
    {:reply, Engine.get(state.engine, key), state}
  end

  def handle_call({:set, key, value}, _from, state) do
    {:reply, Engine.set(state.engine, key, value), state,
     {:continue, {:broadcast_get, key, value}}}
  end

  def handle_call({:delete, key}, _from, state) do
    {:reply, Engine.delete(state.engine, key), state,
     {:continue, {:broadcast_delete, key}}}
  end

  def handle_continue({:broadcast_get, key, value}, _from, state) do
    rel_subs = Control.handle_subs(state.subscribers, :dynamic, key)
    Utility.broadcast(rel_subs, {:dynamic_config_changed, key, value})
    {:noreply, state}
  end

  def handle_continue({:broadcast_delete, key}, _from, state) do
    rel_subs = Control.handle_subs(state.subscribers, :dynamic, key)
    Utility.broadcast(rel_subs, {:dynamic_config_unset, key})
    {:noreply, state}
  end
end
