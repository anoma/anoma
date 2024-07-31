defmodule EventBroker.Registry do
  @moduledoc """
  I am the Registry for the PubSub system.

  I am the central registry of all the topic subscirptions and filters. I
  am responsible for spawning filter agents, (un)subscribing to them, and
  keeping track of relations between them. All public API other than
  message-sending is handle by me.

  ### Public API

  I have the following public functionality:

  - `subscribe_me/1`
  - `unsubscribe_me/1`
  - `subscribe/2`
  - `unsubscribe/2`
  """

  alias __MODULE__

  use GenServer
  use TypedStruct

  @typedoc """
  I am a filter dependency specification, I am a list of filter specs listed
  in the order in which the filter agents implementing said specs should be
  subscribed to one another.
  """

  @type filter_spec_list :: list(struct())

  @doc """
  I am the type of the registered filters, matching a filter agent to its
  PID.
  """

  @type registered_filters :: %{filter_spec_list => pid()}

  typedstruct enforce: true do
    @typedoc """
    I am the type of the Registry.

    My main functionality is to keep track of all spawned filter actors.

    ### Fields

    - `:registered_filters` - The map whose keys are a filter-spec dependency
                              list and whose values are PID's of filter
                              agents corresponding to said lists.
                              Default: %{}
    """

    field(:registered_filters, registered_filters, default: %{})
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

  @doc """
  I am the subscription function.

  Given a PID and a list of filter specs, I ensure that all the actors
  corresponding to each spec given in the list is launched and subscribed
  to each other in the appropriate order - e.g. given `[spec1, spec2]` I
  ensure that agents implemented `spec1` and `spec2` are launched and that
  the former is subscribed to the top broker, while the latter is subscribed
  to the former.

  I also do this in a minimal fashion, that is, if some starting subchain of
  filter spec dependency has already been registered, I launch the minimal
  chain remaining to build the full dependency.

  Note that each filter actor is hence not only determined by the filtering
  functionality it has but also on the chain of dependencies it is spawned
  with. Hence `[spec1, spec2]` corresponds to an agent different from
  `[spec2]`.

  Afterwards, I subscribe the given PID to the final filter agent in the
  dependency chain and register all the new agents in the map by putting
  the filter agent PIDs as values to their dependency filter specification.

  If I notice that any of the filter structures do not have an appropriate
  public filter functionality exposed, I return the list of such modules
  back to the user and do nothing with respect to agent-spawning or
  registration.
  """

  @spec subscribe(pid(), filter_spec_list) :: :ok | String.t()
  def subscribe(pid, filter_spec_list) do
    GenServer.call(__MODULE__, {:subscribe, pid, filter_spec_list})
  end

  @doc """
  I am a subscription function specifically for `self()`

  I call `subscribe/2` where the first argument is `self()`
  """

  @spec subscribe_me(filter_spec_list) :: :ok | String.t()
  def subscribe_me(filter_spec_list) do
    subscribe(self(), filter_spec_list)
  end

  @doc """
  I am the unsubscription function.

  Given a PID and a list of filter specs, I get the PID of the related
  filter agent by looking in my state, then ask it to unsubscribe the
  given PID from it. In case the agent has other subscribers, I return the
  base state.

  Otherwise, it will shut down, prompting to recursively send unsubscription
  requests to its parent filters and processing their termination
  appropriately in a synchronous manner.

  After all the requests have been sent and terminations recorded, I remove
  all agents which have shut down from my registry map and return `:ok`
  """

  @spec unsubscribe(pid(), filter_spec_list) :: :ok
  def unsubscribe(pid, filter_spec_list) do
    GenServer.call(__MODULE__, {:unsubscribe, pid, filter_spec_list})
  end

  @doc """
  I am the unsubscription function specifically for `self()`

  I call `unsubscribe/2` where the first argument is `self()`
  """

  @spec unsubscribe_me(filter_spec_list) :: :ok
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

        Code.ensure_loaded(struct)

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
    new_registered_filters =
      do_unsubscribe(pid, filter_spec_list, state.registered_filters)

    {:reply, :ok, %{state | registered_filters: new_registered_filters}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  @spec iterate_sub(%{filter_spec_list => pid}, filter_spec_list) ::
          {filter_spec_list, registered_filters}
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

  @spec do_unsubscribe(pid(), filter_spec_list, registered_filters) ::
          registered_filters
  defp do_unsubscribe(pid, filter_spec_list, registered_filters) do
    filter_pid = Map.get(registered_filters, filter_spec_list)

    case GenServer.call(filter_pid, {:unsubscribe, pid}) do
      :ok ->
        registered_filters

      :reap ->
        new_registered_filters =
          Map.delete(registered_filters, filter_spec_list)

        parent_spec_list =
          Enum.take(filter_spec_list, length(filter_spec_list) - 1)

        do_unsubscribe(filter_pid, parent_spec_list, new_registered_filters)
    end
  end
end
