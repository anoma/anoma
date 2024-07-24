defmodule EventBroker.FilterAgent do
  @moduledoc """
  holds a filter and applies it to incoming messages
  """

  alias __MODULE__

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    field(:spec, EventBroker.FilterSpec.t())
    field(:subscribers, list(pid()))
  end

  def init(filter_params) do
    {:ok, }
  end

  def handle_cast({:message, msg}, state) do
    if state.spec.filter_module.filter(msg, state.spec.filter_params) do
      for pid <- state.subscribers do
        send(pid, msg)
      end
    end

    {:noreply, state}
  end

  def handle_cast({:subscribe, pid}, state) do
    {:noreply, %{state | subscribers: [pid | state.subscribers]}}
  end

  def handle_cast({:unsubscribe, pid}, state) do
    {:noreply, %{state | subscribers: List.delete(state.subscribers, pid)}}
  end
end
