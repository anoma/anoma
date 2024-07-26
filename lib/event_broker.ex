defmodule EventBroker do
  @moduledoc """
  an event broker
  """

  alias __MODULE__

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    field(:subscribers, MapSet.t(pid()), default: MapSet.new())
  end

  def start_link() do
    GenServer.start_link(__MODULE__, %EventBroker{}, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, %EventBroker{}}
  end

  def handle_call({:subscribe, pid}, _from, state) do
    {:reply, :ok, %{state | subscribers: MapSet.put(state.subscribers, pid)}}
  end

  def handle_call({:unsubscribe, pid}, _from, state) do
    {:reply, :ok,
     %{state | subscribers: MapSet.delete(state.subscribers, pid)}}
  end

  def handle_call(:dump, _from, state) do
    {:reply, {:ok, state}, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:event, event}, state) do
    handle_info(event, state)
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(event = %EventBroker.Event{}, state) do
    for pid <- state.subscribers do
      send(pid, event)
    end

    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  def event(event = %EventBroker.Event{}) do
    GenServer.cast(__MODULE__, {:event, event})
  end
end
