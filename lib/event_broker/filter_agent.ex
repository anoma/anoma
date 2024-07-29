defmodule EventBroker.FilterAgent do
  @moduledoc """
  holds a filter and applies it to incoming messages
  """

  alias __MODULE__

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    field(:spec, struct())
    field(:subscribers, MapSet.t(pid()), default: MapSet.new())
  end

  def start_link(filter_params) do
    GenServer.start_link(__MODULE__, filter_params)
  end

  def init(filter_params) do
    {:ok,
     %FilterAgent{
       spec: filter_params
     }}
  end

  def handle_call({:subscribe, pid}, _from, state) do
    {:reply, :ok, %{state | subscribers: MapSet.put(state.subscribers, pid)}}
  end

  def handle_call({:unsubscribe, pid}, _from, state) do
    new_subs = MapSet.delete(state.subscribers, pid)

    new_state = %{state | subscribers: state}

    if Enum.empty?(new_subs) do
      EventBroker.Registry
      |> GenServer.cast({:delist, self()})

      {:stop, :normal, "No subscribers left, shutting down", new_state}
    else
      {:reply, :ok, new_state}
    end
  end

  def handle_call(:dump, _from, state) do
    {:reply, {:ok, state}, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_info(event = %EventBroker.Event{}, state) do
    if state.spec.__struct__.filter(event, state.spec) do
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
