defmodule EventBroker.FilterAgent do
  @moduledoc """
  holds a filter and applies it to incoming messages
  """

  alias __MODULE__
  alias EventBroker.FilterSpec

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    field(:spec, FilterSpec.t())
    field(:subscribers, MapSet.t(pid()), default: MapSet.new())
  end

  def start_link(filter_module, filter_params) do
    GenServer.start_link(__MODULE__, {filter_module, filter_params})
  end

  def init({filter_module, filter_params}) do
    {:ok,
     %FilterAgent{
       spec: %FilterSpec{
         filter_module: filter_module,
         filter_params: filter_params
       }
     }}
  end

  def handle_call(:dump, _from, state) do
    {:reply, {:ok, state}, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:subscribe, pid}, state) do
    {:noreply, %{state | subscribers: MapSet.put(state.subscribers, pid)}}
  end

  def handle_cast({:unsubscribe, pid}, state) do
    {:noreply, %{state | subscribers: MapSet.delete(state.subscribers, pid)}}
  end

  def handle_info(event = %EventBroker.Event{}, state) do
    if state.spec.filter_module.filter(event, state.spec.filter_params) do
      for pid <- state.subscribers do
        send(pid, event)
      end
    end

    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end
end
