defmodule EventBroker.Registry do
  @moduledoc """
  an event broker registry
  """

  alias __MODULE__

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    field(:registered_filters, map(list(EventBroker.FilterSpec.t()), pid()),
      default: %{}
    )
  end

  # ["filter for Logger only", "filter for info-marked"]
  # ["filter for Logger only", "filter for error-marked"]
  # [["filter for Logger", "filter for Mempool"], "filter for info"]
  # ["a", "b", "c", "d"]
  # ["filter for Logger only"]
  # <top level broker> <- <logger filter> <- <info filter>
  #                        \- <error filter>

  def init(top_level_pid) do
    {:ok, %Registry{registered_filters: %{[] => top_level_pid}}}
  end

  def handle_cast({:subscribe, pid, filter_spec_list}, state) do
    new_state = unless Map.get(state.registered_filters, filter_spec_list) do
      existing_prefix = state.registered_filters
                        |> Map.keys()
                        |> Enum.filter(fn p -> List.starts_with?(filter_spec_list, p) end)
                        |> Enum.sort(&(length(&1) > length(&2))) |> hd()

      remaining_to_spawn = filter_spec_list -- existing_prefix

      for f <- remaining_to_spawn, reduce: {last(existing_prefix), state} do
        {agent, state} ->
          new_pid = GenServer.start_link(f.filter_module, f.filter_params)
          GenServer.cast(agent, {:subscribe, new_pid})
          {new_pid, state}
      end
    else
      state
    end
    GenServer.cast(pid, {:subscribe, pid})
    {:noreply, new_state}
  end

  def handle_cast({:unsubscribe, pid, filter_spec_list}, state) do
    GenServer.cast(Map.get(state.registered_filters, filter_spec_list), {:unsubscribe, pid})
    {:noreply, state}
  end
end
