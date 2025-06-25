defmodule Anoma.LocalDomain.HandlerRegistry do
  @moduledoc """
  Registers scry handling functions to key prefixes. e.g., /anoma/local ->
  Scry.scry_local

  Uses the least specific prefix first, then more specific ones?

  e.g. /anoma/local/[node id]/[time]/app-1/abc

  {prev_prefixes_list, prefix} e.g. {[/anoma/local], /app-1}
                                    {[/anoma/local, /app-1], /section-1}

  default registry:
  {[], /anoma/local} -> Scry.scry_local
  {[], /anoma/controller]} -> Scry.scry_controller

  Scry.scry_local({[], /anoma/local})
  App1.scry_app1({[/app-1], key})

  This should go to the /anoma/local handler first. However, this wants to
  later find a handler for app-1.

  If we match the shortest prefix, we always end up with the /anoma/local
  handler.

  If we match the longest prefix, we end up deep in app-1 right away.

  If we chop off the front of the key, app-1 can't be used by other keyspaces.
  """

  use Anoma.LocalDomain
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def match(prev_prefixes, key) do
    GenServer.call(__MODULE__, {:match, prev_prefixes, key})
  end

  def register(prefix, fun) do
    GenServer.cast(__MODULE__, {:register, prefix, fun})
  end

  def deregister(prefix) do
    GenServer.cast(__MODULE__, {:deregister, prefix})
  end

  # callbacks

  @impl true
  def init(_arg) do
    # todo: more searchable backend
    map = %{
      {[], ~k"/anoma/local"} => &Anoma.LocalDomain.Scry.scry_local/2,
      {[], ~k"/anoma/controller"} => &Anoma.LocalDomain.Scry.scry_controller/2
    }

    {:ok, map}
  end

  @impl true
  def handle_cast({:register, prefix, fun}, state) do
    {:noreply, Map.put(state, prefix, fun)}
  end

  @impl true
  def handle_cast({:deregister, prefix}, state) do
    {:noreply, Map.delete(state, prefix)}
  end

  @impl true
  def handle_call({:match, prev_prefixes, key}, _from, state) do
    matches =
      state
      |> Map.filter(fn {{k_prev_prefixes, _}, _} -> k_prev_prefixes == prev_prefixes end)
      |> Map.filter(fn {{_, prefix}, _} -> is_prefix?(prefix, key) end)

    # todo: match longest prefix
    if Enum.count(matches) == 1 do
      [{{_, prefix}, handler}] = Map.to_list(matches)
      {:reply, {prefix, handler}, state}
    else
      {:reply, :error, state}
    end
  end

  defp is_prefix?([], _) do
    true
  end

  defp is_prefix?(_, []) do
    false
  end

  defp is_prefix?([p_head | p_tail], [l_head | l_tail]) do
    if p_head == l_head do
      is_prefix?(p_tail, l_tail)
    else
      false
    end
  end
end
