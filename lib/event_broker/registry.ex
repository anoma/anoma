defmodule EventBroker.Registry do
  @moduledoc """
  an event broker registry
  """

  alias __MODULE__

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    field(:registered_filters, %{list(struct()) => pid()}, default: %{})
  end

  def start_link(top_level_pid) do
    GenServer.start_link(__MODULE__, top_level_pid, name: __MODULE__)
  end

  def init(top_level_pid) do
    {:ok, %Registry{registered_filters: %{[] => top_level_pid}}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  def subscribe(pid, filter_spec_list) do
    GenServer.call(__MODULE__, {:subscribe, pid, filter_spec_list})
  end

  def subscribe_me(filter_spec_list) do
    subscribe(self(), filter_spec_list)
  end

  def unsubscribe(pid, filter_spec_list) do
    GenServer.call(__MODULE__, {:unsubscribe, pid, filter_spec_list})
  end

  def unsubscribe_me(filter_spec_list) do
    unsubscribe(self(), filter_spec_list)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:subscribe, pid, filter_spec_list}, _from, state) do
    registered = state.registered_filters

    invalid_filters =
      filter_spec_list
      |> Enum.flat_map(fn spec ->
        struct = spec.__struct__

        if Kernel.function_exported?(struct, :filter, 2) do
          []
        else
          [struct]
        end
      end)

    if Enum.empty?(invalid_filters) do
      new_state =
        if Map.has_key?(registered, filter_spec_list) do
          state
        else
          {_, new_registered_filters} =
            iterate_sub(registered, filter_spec_list)

          %{state | registered_filters: new_registered_filters}
        end

      GenServer.call(
        Map.get(new_state.registered_filters, filter_spec_list),
        {:subscribe, pid}
      )

      {:reply, :ok, new_state}
    else
      {:reply,
       "#{inspect(invalid_filters)} do not export filtering functions", state}
    end
  end

  def handle_call({:unsubscribe, pid, filter_spec_list}, _from, state) do
    GenServer.call(
      Map.get(state.registered_filters, filter_spec_list),
      {:unsubscribe, pid}
    )

    {:reply, :ok, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:delist, pid}, state) do
    filters = state.registered_filters

    [{key, _pid}] = Enum.filter(filters, fn {_key, value} -> value == pid end)

    father_spec = Enum.take(key, length(key) - 1)

    filters |> Map.get(father_spec) |> GenServer.call({:unsubscribe, pid})

    {:noreply, %{state | registered_filters: Map.delete(filters, key)}}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  defp iterate_sub(registered, filter_spec_list) do
    existing_prefix =
      registered
      |> Map.keys()
      |> Enum.filter(fn p -> List.starts_with?(filter_spec_list, p) end)
      |> Enum.sort(&(length(&1) > length(&2)))
      |> hd()

    remaining_to_spawn = filter_spec_list -- existing_prefix

    # ["a", "b"] -> ["a", "b", "c", "d"]
    # {["a", "b"], state} iterating over "c"
    # {["a", "b", "c"], state + ["a", "b", "c"] iterating over "d"

    for f <- remaining_to_spawn,
        reduce: {existing_prefix, registered} do
      {parent_spec_list, old_state} ->
        parent_pid = Map.get(old_state, parent_spec_list)

        {:ok, new_pid} =
          GenServer.start_link(
            EventBroker.FilterAgent,
            f
          )

        GenServer.call(parent_pid, {:subscribe, new_pid})
        new_spec_list = parent_spec_list ++ [f]
        new_state = Map.put(old_state, new_spec_list, new_pid)
        {new_spec_list, new_state}
    end
  end
end
