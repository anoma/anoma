defmodule EventBroker do
  @moduledoc """
  an event broker
  """

  alias __MODULE__

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    field(:subscribers, list(pid()), default: [])
  end

  def start_link() do
    GenServer.start_link(__MODULE__, %EventBroker{})
  end

  def init(_opts) do
    {:ok, %EventBroker{}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:message, body}, state) do
    for pid <- state.subscribers do
      send(pid, body)
    end

    {:noreply, state}
  end

  def handle_cast({:subscribe, pid}, state) do
    {:noreply, %{state | subscribers: [pid | state.subscribers]}}
  end

  def handle_cast({:unsubscribe, pid}, state) do
    {:noreply, %{state | subscribers: List.delete(state.subscribers, pid)}}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end
end
