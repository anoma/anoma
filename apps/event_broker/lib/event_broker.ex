defmodule EventBroker do
  @moduledoc """
  I am the EventBroker Application Module.

  I startup the PubSub system as an own OTP application. Moreover I provide
  all the API necessary for the use of the system. I contain all public
  functionality provided by the Broker and the Registry.

  ### Public API

  I have the following public functionality:

  - `event/1`
  - `subscribe_me/1`
  - `unsubscribe_me/1`
  - `subscribe/2`
  - `unsubscribe/2`
  """

  use Application

  @impl true
  def start(_type, args \\ []) do
    EventBroker.Supervisor.start_link(args)
  end

  @typedoc """
  I am a filter dependency specification, I am a list of filter specs listed
  in the order in which the filter agents implementing said specs should be
  subscribed to one another.
  """

  @type filter_spec_list :: list(struct())

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the Event Broker event function.

  I process any incoming events by sending them to all of Broker subscribers
  using the `send/2` functionality.
  """

  @spec event(EventBroker.Event.t()) :: :ok
  @spec event(EventBroker.Event.t(), atom()) :: :ok
  def event(event = %EventBroker.Event{}, broker \\ EventBroker.Broker) do
    GenServer.cast(broker, {:event, event})
  end

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

  Filter Agent spawning is handled via DynamicSupervisor.
  """

  @spec subscribe(pid(), filter_spec_list) :: :ok | String.t()
  @spec subscribe(pid(), filter_spec_list, atom()) :: :ok | String.t()
  def subscribe(pid, filter_spec_list, registry \\ EventBroker.Registry) do
    GenServer.call(registry, {:subscribe, pid, filter_spec_list})
  end

  @doc """
  I am a subscription function specifically for `self()`

  I call `subscribe/2` where the first argument is `self()`
  """

  @spec subscribe_me(filter_spec_list) :: :ok | String.t()
  @spec subscribe_me(filter_spec_list, atom()) :: :ok | String.t()
  def subscribe_me(filter_spec_list, registry \\ EventBroker.Registry) do
    subscribe(self(), filter_spec_list, registry)
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

  @spec unsubscribe(pid(), filter_spec_list, atom()) :: :ok
  def unsubscribe(pid, filter_spec_list, registry \\ EventBroker.Registry) do
    GenServer.call(registry, {:unsubscribe, pid, filter_spec_list})
  end

  @doc """
  I am the unsubscription function specifically for `self()`

  I call `unsubscribe/2` where the first argument is `self()`
  """

  @spec unsubscribe_me(filter_spec_list) :: :ok
  @spec unsubscribe_me(filter_spec_list, atom()) :: :ok
  def unsubscribe_me(filter_spec_list, registry \\ EventBroker.Registry) do
    unsubscribe(self(), filter_spec_list, registry)
  end
end
